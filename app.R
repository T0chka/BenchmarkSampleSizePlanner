library(shiny)
library(readxl)
library(data.table)
library(metafor)
library(DT)
library(bslib)
library(bsicons)
library(shinycssloaders)
source("utils.R")
source("css.R")

ui <- page_navbar(
  theme = theme,
  title = tags$div(
    class = "app-name",
    tags$div(class = "name-title", "Preclinical Sample Size Planner"),
    tags$div(
      class = "name-subtitle",
      "Goodbye guesswork, hello clinically-proven benchmarks"
    )
  ),
  fillable = TRUE,
  header = tagList(
      shinyjs::useShinyjs(),
      layout_columns(
      col_widths = c(3, 9),
      class = "cols-tight mt-3",
  
      #------------ RIGHT: Settings  ------------
      card(
        class = "sidebar-card",
        card_header(
          "Settings"
        ),
  
        # Dataset choice
        div(
          class = "input-block",
          radioButtons(
            inputId = "data_source",
            label   = tags$h5("Dataset for meta-analysis"),
            choices = c(
              "Use curated EMTVN dataset" = "paper",
              "Upload dataset"            = "custom"
            )
          )
        ),
  
        # Upload UI (only when uploading)
        conditionalPanel(
          condition = "input.data_source == 'custom' && output.data_ready != 1",
          div(
            class = "input-block",
            fileInput("file", "Upload .xlsx file", accept = ".xlsx"),
            div(
              strong("Required columns:"),
              tags$ul(
                tags$li("Numeric: Study.ID, Exp.ID, MD, SE, Spooled"),
                tags$li("Character: Outcome")
              ),
              strong("Optional columns:"),
              tags$ul(tags$li("Character: Drug, Reference"))
            )
          )
        ),
        actionButton(
          inputId = "load_data",
          label = "Load data",
          class = "btn btn-primary w-100"
        ),
  
        # Outcome + planning choices (after data ready)
        conditionalPanel(
          condition = "output.data_ready == 1",
  
          uiOutput("outcome_picker"),
  
          # EFFECT target: discrete buckets
          div(
            class = "input-block",
            radioButtons(
              inputId = "effect_level",
              label   = tags$h5("Target effect to detect"),
              choiceNames = list(
                tagList(
                  "Large ",
                  tags$span(
                    "(≈100%)",
                    title = paste0(
                      "100%: your target effect is at least as large as ",
                      "the benchmark (pooled MD from the dataset)."
                    ),
                    class = "help-hint"
                  )
                ),
                tagList(
                  "Medium ",
                  tags$span(
                    "(≈80%)",
                    title = "80% of the dataset benchmark MD.",
                    class = "help-hint"
                  )
                ),
                tagList(
                  "Small ",
                  tags$span(
                    "(≈50%)",
                    title = "50% of the dataset benchmark MD.",
                    class = "help-hint"
                  )
                )
              ),
              choiceValues = c("large", "medium", "small"),
              selected = "large",
              inline = FALSE
            )
          ),
  
          # POOLED SD: percentile buckets
          div(
            class = "input-block",
            radioButtons(
              inputId = "sd_bucket",
              label   = tags$h5("Expected variability"),
              choiceNames = list(
                tagList(
                  "Low ",
                  tags$span(
                    "(20th percentile)",
                    title = paste0(
                      "20th percentile of pooled SD in the loaded dataset."
                    ),
                    class = "help-hint"
                  )
                ),
                tagList(
                  "Median ",
                  tags$span(
                    "(50th percentile)",
                    title = paste0(
                      "50th percentile (median) of pooled SD in the dataset."
                    ),
                    class = "help-hint"
                  )
                ),
                tagList(
                  "High ",
                  tags$span(
                    "(80th percentile)",
                    title = paste0(
                      "80th percentile of pooled SD in the loaded dataset."
                    ),
                    class = "help-hint"
                  )
                )
              ),
              choiceValues = c("p20", "p50", "p80"),
              selected = "p80",
              inline = FALSE
            )
          ),
  
          # Power
          div(
            class = "input-block",
            radioButtons(
              inputId = "power",
              label = tags$h5(
                tagList(
                  "Power ",
                  tags$span(
                    "\u2139",
                    title = paste0(
                      "80% power is a solid default unless you know exactly ",
                      "why you’d change it"
                    ),
                    class = "info-icon"
                  )
                )
              ),
              choices = c(
                "80%" = "0.80",
                "85%" = "0.85",
                "90%" = "0.90",
                "95%" = "0.95"
              ),
              selected = "0.80"
            )
          ),
          actionButton(
            inputId = "reset_all",
            label = "Reset",
            class = "btn btn-primary w-100"
          )
        )
      ),
  
      #------------ RIGHT: Tabs ------------
      
      layout_columns(
        navset_card_tab(
          nav_panel(
            "Results",
            layout_columns(
              col_widths = c(8, 4),
              uiOutput("calc_summary"),
              uiOutput("n_box")
            )
          ),
  
          nav_panel(
            "Data",
            conditionalPanel(
              condition = "output.data_ready == 1",
              withSpinner(DT::dataTableOutput("raw_table"), type = 4)
            )
          )
        ),
        bslib::accordion(
          bslib::accordion_panel(
            "Quick start",
            p(
              "This app turns a set of experiment-level effect sizes into",
              " a pooled benchmark (via meta-analysis) and then uses it",
              " to compute simple size (animals per group) for a new study."
            ),
            p(
              "The goal is to plan for detecting a biologically meaningful effect — one ",
              "that is large enough to be relevant and, ideally, predictive of ",
              "translating into an effective treatment."
            ),
            p(
              "We set that target effect using human-proven drugs as a benchmark, ",
              "so you compare your planned study’s detectable effect to something with ",
              "known clinical relevance."
            ),
            p("Two ways to use it:"),
            tags$ol(
              tags$li(
                HTML(
                  "Curated dataset — all published experiments testing clinically ",
                  "effective anti-migraine drugs in the EMTVN rat model, collected ",
                  "via systematic review (",
                  "<a href='https://www.crd.york.ac.uk/PROSPERO/view/CRD42021276448' target='_blank'>protocol</a>). ",
                  "Use this only to plan new EMTVN experiments."
                )
              ),
              tags$li(
                "Upload your dataset — same workflow for any animal model if you ",
                "prepare data in the required format."
              )
            )
          ),
          # 2) Prepare your own dataset
          bslib::accordion_panel(
            "Prepare your own dataset",
            p(
              "Use one row per experiment, where an experiment is a treatment ",
              "vs its control on a single animal/neuronal cohort."
            ),
            strong("Required columns:"),
            tags$ul(
              tags$li("Study.ID — integer; unique study identifier."),
              tags$li("Exp.ID — integer; unique experiment ID within each Study.ID."),
              tags$li("Outcome — character; endpoint label for grouping."),
              tags$li("MD — numeric; effect size (mean difference, same units for all rows)."),
              tags$li("SE — numeric; standard error of MD."),
              tags$li("Spooled — numeric; pooled SD used to derive SE.")
            ),
            strong("Optional:"),
            tags$ul(
              tags$li("Drug — character; treatment name."),
              tags$li("Reference — character; citation or study short name.")
            ),
            p(
              "Combine any sub-measures (e.g., different fibre types, multiple readouts) ",
              "into a single MD/SE per experiment before entry."
            )
          ),
          
          # 3) Effect size & Variance calculation
          bslib::accordion_panel(
            "Effect size & variance calculation",
            p("Formulas follow Vesterinen et al., 2014:"),
            tags$pre(
              HTML("MD = M<sub>t</sub> − M<sub>c</sub>"),
              HTML("\nS<sub>pooled</sub> = sqrt(((N<sub>c</sub>−1)·SD<sub>c</sub><sup>2</sup> + (N<sub>t</sub>−1)·SD<sub>t</sub><sup>2</sup>) / (N<sub>c</sub>+N<sub>t</sub>−2))"),
              HTML("\nSE = sqrt((N<sub>c</sub>+N<sub>t</sub>) / (N<sub>c</sub>·N<sub>t</sub>)) · S<sub>pooled</sub>")
            ),
            p(
              "In the curated dataset we used % of baseline neuronal activity as the unit, ",
              "so MD is the absolute difference in percentage points."
            ),
            p(
              HTML(
                "Reference: Vesterinen et al., <em>PLOS Biology</em>, 2014 ",
                "(<a href='https://doi.org/10.1371/journal.pbio.1001779' target='_blank'>link</a>)."
              )
            )
          ),
          
          # 4) Meta-analytic model
          bslib::accordion_panel(
            "Meta-analytic model",
            p(
              "We fit a three-level random-effects meta-analysis with robust variance ",
              "estimation for CIs:"
            ),
            tags$pre(
              "metafor::rma.mv(\n",
              "  yi     = dt$MD,\n",
              "  V      = dt$SE^2,\n",
              "  random = ~ 1 | Study.ID / Exp.ID,\n",
              "  data   = dt\n",
              ")"
            ),
            p(
              "Random intercepts are included for studies and experiments within studies ",
              "to account for dependence. Level-1 variance is the sampling error from SE."
            ),
            p(
              HTML(
                "For details, see Viechtbauer (2010) ",
                "(<a href='https://doi.org/10.18637/jss.v036.i03' target='_blank'>link</a>)."
              )
            )
          ),
          
          # 5) Sample size calculation
          bslib::accordion_panel(
            "Sample size calculation",
            p(
              "The app uses the pooled MD from the meta-analysis as your benchmark. ",
              "You set a target effect as a fraction of that benchmark (100%, 80%, 50%). ",
              "We then calculate the per-group sample size for your chosen power, ",
              "given an assumed pooled SD."
            ),
            p(
              "The pooled SD options — Low, Median, High — correspond to the 20th, 50th, ",
              "and 80th percentiles of pooled SD values in the loaded dataset."
            ),
            p(
              "For example, if the 80th percentile SD is 24.5%, it means 80% of experiments ",
              "in the dataset had pooled SD ≤ 24.5%. Use this if you expect similar or ",
              "worse variability in your planned experiment."
            )
          ),
          bslib::accordion_panel(
            "Sources & data",
            p(
              tags$a(href = "https://doi.org/10.1111/ejn.16030", target = "_blank",
                     "Paper: Dolgorukova et al., Eur J Neurosci (doi:10.1111/ejn.16030)"),
              br(),
              tags$a(href = "https://osf.io/vzjys/", target = "_blank",
                     "OSF project with raw/processed data and scripts")
            )
          )
        ),
        col_widths = c(12, 12)
      )
    )
  )  ,
  footer = div(
    class = "app-footer",
    "Version 1.0 | © 2025 Antonina Dolgorukova"
  )
)


server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)
  load_ok <- reactiveVal(FALSE)
  
  # reset all when data source changes
  observeEvent(input$data_source, {
    load_ok(FALSE)
  })
  
  # update load data button label
  observe({
    lab <- if (identical(input$data_source, "paper"))
      "Load curated dataset" else "Load my dataset"
    updateActionButton(session, "load_data", label = lab)
  })
  
  # show/hide load data button
  observe({
    if (isTRUE(load_ok())) {
      shinyjs::hide("load_data")
      shinyjs::show("reset_all")
    } else {
      shinyjs::show("load_data")
      shinyjs::hide("reset_all")
    }
  })
  
  # reset all
  observeEvent(input$reset_all, {
    raw_data(NULL)
    load_ok(FALSE)
  })
  
  # load data
  observeEvent(input$load_data, {
    use_paper_data <- identical(input$data_source, "paper")
    
    if (use_paper_data) {
      data_path <- "MA_data_set.xlsx"
      sheet <- "MA"
    } else {
      if (is.null(input$file)) {
        showNotification("Please upload your data file.", type = "warning")
        load_ok(FALSE)
        return()
      }
      data_path <- input$file$datapath
      sheet <- 1
    }
    
    dt <- tryCatch(
      prepare_data(data_path, sheet),
      error = function(e) {
        showNotification(
          paste0("Error reading curated dataset: ", e$message),
          type = "error", duration = 6
        )
        NULL
      }
    )
    if (is.null(dt)) {
      load_ok(FALSE)
    } else {
      raw_data(dt)
      load_ok(TRUE)
    }
  }, ignoreInit = TRUE)
  
  output$data_ready <- renderText({ if (isTRUE(load_ok())) 1 else 0 })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  
  raw <- reactive({
    req(load_ok())
    raw_data()
  })
  output$outcome_picker <- renderUI({
    dt <- raw()
    if (is.null(dt)) return(NULL)
    outs <- sort(unique(as.character(dt$Outcome)))
    div(
      class = "input-block",
      radioButtons(
        inputId = "outcome",
        label = tags$h5("Outcome"),
        choices = outs,
        selected = outs[1]
      )
    )
  })
  filter_data <- reactive({
    req(input$outcome)
    dt <- req(raw())
    dt[Outcome %in% input$outcome]
  })
  remove_outliers <- reactive({
    dt <- req(filter_data())
    remove_influential_outliers(dt)
  })
  effects <- reactive({
    dt <- req(remove_outliers())
    list(
      md = fit_mv(dt, robust_ve = TRUE)$b,
      sd = q_vec(dt$Spooled)
    )
  })
  pick_sd <- reactive({
    e <- effects()
    b <- input$sd_bucket
    suppressWarnings(as.numeric(e$sd[[b]]))
  })
  pick_md <- reactive({
    m <- effects()$md
    level <- input$effect_level
    f <- switch(level, large = 1.00, medium = 0.80, small = 0.50, NA_real_)
    m * f
  })
  pick_power <- reactive(as.numeric(input$power))
  
  n_calc <- reactive({
    ss_calc(md = pick_md(), sd = pick_sd(), power = pick_power())
  })
  
  output$n_box <- renderUI({
    req(load_ok())
    n <- n_calc()
    val <- if (is.na(n)) "Insufficient inputs" else as.character(n)
    bslib::value_box(
      title = "Animals per group",
      value = tags$span(val),
      showcase = bsicons::bs_icon("calculator")
    )
  })
  
  output$calc_summary <- renderUI({
    if (!isTRUE(load_ok())) return(tags$p("Load a dataset to begin"))
    
    dt_filt <- filter_data()
    dt <- remove_outliers()
    md_used <- pick_md()
    sd_used <- pick_sd()
    lvl_label <- c(
      large = "(Large)", medium = "(Medium)", small = "(Small)"
    )[input$effect_level]
    sd_label  <- c(
      p20 = "(Low)", p50 = "(Medium)", p80 = "(High)"
    )[input$sd_bucket]
    
    tags$div(
      h5("Selected inputs for calculation"),
      tags$ul(
        tags$li(
          "Studies: ", uniqueN(dt$Study.ID),
          ", experiments: ", uniqueN(dt$Exp.ID),
          paste0(" (", nrow(dt_filt) - nrow(dt), " outliers excluded)")
        ),
        tags$li("Outcome:", paste0(" ", input$outcome)),
        tags$li("Target effect (MD, %): ", paste(round(md_used, 2), lvl_label)),
        tags$li("Expected variability (SD, %): ", paste(round(sd_used, 2), sd_label))
      )
    )
  })
  
  output$raw_table <- DT::renderDataTable({
    if (!isTRUE(load_ok())) return(NULL)
    dt <- raw()
    if (is.null(dt)) return(NULL)
    DT::datatable(
      dt,
      filter = "top",
      options = list(
        pageLength = 35,
        lengthMenu = c(10, 35, 50, 100)
      ),
      class = "nowrap hover compact order-column",
      rownames = FALSE
    ) |> 
      DT::formatRound(
        columns = c("MD", "SE", "Spooled"),
        digits = 2
      )
  })
}

shinyApp(ui, server)
