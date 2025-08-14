#' Read and filter raw data
prepare_data <- function(path, sheet) {
  dt <- read_xlsx(path, sheet = sheet)
  dt <- as.data.table(dt)
  
  chk <- .check_input_data(dt, sheet)
  
  for (id_col in chk$ids_to_convert) {
    dt[[id_col]] <- as.numeric(dt[[id_col]])
  }
  for (char_col in chk$char_to_factor) {
    dt[[char_col]] <- as.factor(dt[[char_col]])
  }
  show_cols <- intersect(
    c("Study.ID", "Exp.ID", "Reference",
      "Drug", "Outcome", "MD", "SE", "Spooled"),
    names(dt)
  )
  
  dt[, ..show_cols]
}

# utils::globalVariables(
#   c(
#     ".",
#     "Study.ID", "Exp.ID", "Outcome", "MD", "SE",
#     "Spooled", "Drug", "Reference"
#   )
# )
# 
.check_input_data <- function(dt, sheet) {
  # emptiness
  if (nrow(dt) == 0) stop(sprintf("No data in '%s' sheet!", sheet))
  
  # required columns
  req <- c("Outcome", "Study.ID", "Exp.ID", "MD", "SE", "Spooled")
  miss <- setdiff(req, names(dt))
  if (length(miss)) stop(
    sprintf("Missing required columns: %s", paste(miss, collapse = ", ")),
    call. = FALSE
  )
  
  # check the 'Outcome' column: must be character or factor (factor is fine)
  if (!is.character(dt$Outcome) && !is.factor(dt$Outcome)) {
    stop("Column 'Outcome' must be character or factor.", call. = FALSE)
  }
  
  # check numeric columns
  for (num_col in c("MD", "SE", "Spooled")) {
    if (!is.numeric(dt[[num_col]])) {
      stop(sprintf("Column '%s' must be numeric.", num_col), call. = FALSE)
    }
  }
  
  # check ID columns
  ids_to_convert <- character(0)
  for (id_col in c("Study.ID", "Exp.ID")) {
    if (is.numeric(dt[[id_col]]) || is.integer(dt[[id_col]])) next
    if (is.character(dt[[id_col]])) {
      tmp <- suppressWarnings(as.numeric(dt[[id_col]]))
      if (anyNA(tmp)) {
        stop(
          sprintf(
            paste(
              "Column '%s' is character and cannot",
              "be safely converted to numeric."
            ),
            id_col
          ), call. = FALSE
        )
      } else {
        ids_to_convert <- c(ids_to_convert, id_col)
      }
    } else {
      stop(
        sprintf(
          "Column '%s' must be numeric or character with numeric values.",
          id_col
        ), call. = FALSE
      )
    }
  }
  
  # character columns to turn into factors later (if present)
  char_to_factor <- intersect(c("Outcome", "Reference", "Drug"), names(dt))
  
  list(ids_to_convert = ids_to_convert, char_to_factor = char_to_factor)
}

#' Remove influential outliers - experiments, which meet the 2 conditions:
#' influential case: if Cook’s Distance > 4 / total number of experiments
#' outlier: if standardized (deleted) residuals > ± 1.96
remove_influential_outliers <- function(dt) {
  res <- fit_mv(dt)
  
  rst  <- rstudent(res)$z
  cd   <- cooks.distance(res)
  k    <- res$k
  cutz <- qnorm(.975)
  
  influential_outliers <- which(abs(rst) >= cutz & cd > 4 / k)
  dt_filt <- dt[!influential_outliers]
  
  dt_filt
}

#' Fit three-level meta-analytic model with optional robust variance estimation
fit_mv <- function(dt, robust_ve = FALSE) {
  use_full_labels <- all(c("Drug", "Reference") %in% names(dt))
  res <- rma.mv(
    yi     = MD,
    V      = SE^2,
    random = ~ 1 | Study.ID / Exp.ID,
    data   = dt,
    slab   = if (use_full_labels) paste(Drug, Reference, sep = ", ") else Exp.ID
  )
  
  if (robust_ve) {
    return(robust(res, cluster = dt$Study.ID))
  } else {
    return(res)
  }
}

#' Calculate sample size using a 5% non-parametric correction,
#' which gives the same n as G*Power for the Wilcoxon-Mann-Whitney test
#' at two-sided alfa = 0.05, allocation ratio 1:1.
ss_calc <- function(md, sd, power) {
  if (is.na(sd) || sd <= 0 || is.na(md) || md == 0) return(NA)
  n <- power.t.test(
    power = power,
    delta = abs(md),
    sd = sd,
    type = "two.sample",
    alternative = "two.sided"
  )$n
  ceiling(n * 1.05)
}

#' Quantiles vector helper for pooled SD
#'
#' Returns the 20th, 50th, and 80th percentiles of a numeric vector with
#' names aligned to UI selections (p20, p50, p80). NAs are removed.
#' @param x Numeric vector of pooled SD values
#' @return Named numeric vector c(p20, p50, p80)
q_vec <- function(x) {
  q <- stats::quantile(
    x, probs = c(0.20, 0.50, 0.80), na.rm = TRUE, names = FALSE
  )
  names(q) <- c("p20", "p50", "p80")
  q
}