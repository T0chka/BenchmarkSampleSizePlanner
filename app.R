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
    tags$div(
      class = "name-title",
      "Benchmark-Based Sample Size Planner for Animal Studies"
    ),
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
              "This app takes a set of experiment-level effect sizes, performs a ",
              "meta-analysis to produce a pooled benchmark, and then uses that ",
              "benchmark to calculate how many animals per group you’ll need",
              "for a new study."
            ),
            p(
              "The aim is to plan studies that can detect a biologically meaningful ",
              "effect – large enough to matter and, ideally, predictive of successful ",
              "translation into an effective treatment."
            ),
            p(
              "We define this target effect using data from human-proven drugs.",
              " This ensures that the effect your study is designed to detect",
              " is tied to known clinical relevance."
            ),
            p("Two ways to get started:"),
            tags$ol(
              tags$li(
                HTML(
                  "Use our curated dataset – a prepared set of all published (as of Dec 2021) ",
                  "experiments testing clinically effective anti-migraine drugs in the ",
                  "rat electrophysiological model of trigeminovascular nociception ",
                  "(EMTVN), collected via systematic review ",
                  "(<a href='https://www.crd.york.ac.uk/PROSPERO/view/CRD42021276448' ",
                  "target='_blank'>protocol</a>). Use this only for planning new experiments ",
                  "with this particular model."
                )
              ),
              tags$li(
                "Upload your own dataset – apply the same approach to any animal model, ",
                "by preparing your data in the required format (see below)."
              )
            )
          ),
          
          bslib::accordion_panel(
            "Prepare your own dataset",
            p(
              "Use one row per experiment. An experiment is a single comparison of a ",
              "treatment group and its matched control group within the same study."
            ),
            p("Required columns:"),
            tags$ul(
              tags$li("Study.ID – numeric; unique study identifier."),
              tags$li("Exp.ID – numeric; unique identifier for each experiment within a study."),
              tags$li("Outcome – character; endpoint label for grouping."),
              tags$li("MD – numeric; effect size (mean difference, same units for all rows)."),
              tags$li("SE – numeric; standard error of MD."),
              tags$li("Spooled – numeric; pooled SD used to derive SE.")
            ),
            p("Optional columns:"),
            tags$ul(
              tags$li("Drug – character; treatment name."),
              tags$li("Reference – character; citation or short study name.")
            ),
            p(
              "For an example of how such a dataset can be collected via a systematic ",
              "search, see our ",
              tags$a(
                href = "https://doi.org/10.1111/ejn.16030",
                target = '_blank',
                "published study"
              ), "."
            )
          ),
          bslib::accordion_panel(
            "Data pre-processing",
            p(
              "Before running the meta-analysis, the app automatically screens the dataset ",
              "for potential outliers and influential cases. This helps ensure that the pooled ",
              "benchmark is not distorted by a few atypical experiments."
            ),
            p(
              HTML(
                "Potential outliers are flagged via standardized deleted residuals ",
                "(\\(|\\text{residual}| > 1.96\\)) and influential cases via Cook’s distances (\\(4 / n\\), ",
                "where \\(n\\) is the number of data points; ",
                "<a href='https://doi.org/10.1201/9781315119403' target='_blank'>",
                "Schmid et&nbsp;al., 2020</a>). Influential outliers are removed."
              )
            )
          ),
          bslib::accordion_panel(
            "Meta-analytic model",
            withMathJax(
              p(HTML(
                "Per-experiment effect sizes are observed mean differences ",
                "(\\(MD_{ij} = M_t - M_c\\)) with known standard errors ",
                "(\\(SE_{ij}\\)). To pool these while accounting for clustering, ",
                "the app fits a three-level random-effects model ",
                "(<a href='https://doi.org/10.18637/jss.v036.i03' target='_blank'>Viechtbauer, 2010</a>). ",
                "Let \\(\\theta_{ij}\\) denote the true effect ",
                "for experiment \\(i\\) in study \\(j\\):"
              ))
            ),
            
            div(class = "row equal-row",
                # LEFT: explanation
                div(class = "col-sm-7",
                    div(class = "card-box",
                        withMathJax(
                          div(style = "font-size:85%;",
                              p(
                                "$$\\theta_{ij} = \\mu + u_{study,j} + u_{exp,ij},\\qquad ",
                                "MD_{ij} \\mid \\theta_{ij} \\sim ",
                                "\\mathcal{N}(\\theta_{ij},\\, v_{ij}),\\quad ",
                                "v_{ij} = SE_{ij}^{2}$$"
                              ),
                              tags$ul(
                                tags$li(HTML(
                                  "\\(\\mu\\) is the average true effect; its estimate, ",
                                  "\\(\\hat{\\mu}\\), is the pooled benchmark used for sample-size planning."
                                )),
                                tags$li(HTML(
                                  "between-study heterogeneity: ",
                                  "\\(u_{study,j} \\sim \\mathcal{N}(0,\\, \\tau^2_{study})\\)."
                                )),
                                tags$li(HTML(
                                  "within-study (between-experiment) heterogeneity: ",
                                  "\\(u_{exp,ij} \\sim \\mathcal{N}(0,\\, \\tau^2_{exp})\\)."
                                )),
                                tags$li(HTML(
                                  "sampling variance: \\(v_{ij} = SE_{ij}^{2}\\) (from the input)."
                                )),
                                tags$li(HTML(
                                  "estimation uses REML and the marginal variance of \\(MD_{ij}\\) is ",
                                  "\\(SE_{ij}^{2} + \\tau^2_{exp} + \\tau^2_{study}\\)."
                                ))
                              )
                          )
                        )
                    )
                ),
                
                # RIGHT: wider code box
                div(class = "col-sm-5",
                    div(class = "card-box",
                        {
                          code_r <- paste(
                            "res <- metafor::rma.mv(",
                            "  yi     = MD,",
                            "  V      = SE^2,",
                            "  random = ~ 1 | Study.ID / Exp.ID,",
                            "  method = 'REML',",
                            "  data   = dt",
                            ")",
                            "",
                            "res_rob <- metafor::robust(res, cluster = dt$Study.ID)",
                            sep = "\n"
                          )
                          tags$pre(class = "mono", style = "font-size:72%;", code_r)
                        }
                    )
                )
            ),
            
            p(HTML(
              "We do not require all experiments to be identical – the model explicitly ",
              "allows them to differ with two sources of heterogeneity (between studies ",
              "and between experiments within studies). Study-level cluster-robust ",
              "standard errors are computed with the small-sample adjustment ",
              " in <code>metafor::robust()</code> ",
              "(<a href='https://doi.org/10.1002/jrsm.5' target='_blank'>Hedges et&nbsp;al., 2010</a>). ",
              "Therefore, if your dataset includes multiple experiments per study and/or ",
              "shared control groups, this dependence is handled by the model."
            ))
          ),
          
          bslib::accordion_panel(
            "Sample size calculation",
            withMathJax(
              p(
                "The app uses the pooled estimate \\(\\hat{\\mu}\\) from the meta-analysis ",
                "as the benchmark effect size. The per-group sample size is ",
                "then calculated for your chosen power using the Wilcoxon–Mann–Whitney ",
                "test formula (two-sided \\(\\alpha = 0.05\\), allocation ratio 1:1) with ",
                "a 5% non-parametric correction to match G*Power results."
              )
            ),
            p(
              "In our work and previous preclinical meta-analyses, pooled standard deviations (SDs) ",
              "varied widely across experiments. It is therefore likely that your dataset ",
              "will also show substantial variability. For this reason, the app allows ",
              "you to explore scenarios with different pooled SD values – the 20th, 50th, ",
              "and 80th percentiles of the distribution in the loaded dataset (labelled ",
              "Low, Median, and High variability).",
              "Choosing a higher percentile yields a more conservative (larger) sample size."
            ),
            div(
              style = paste(
                "border-left: 3px solid #ccc;",
                "padding-left: 0.6em;",
                "color: #555;",
                "font-size: 85%;",
                "margin-bottom: 0.8em;"
              ),
              "Example: If the 80th percentile pooled SD (High variability) is 24.5%, ",
              "then 80% of experiments in the dataset had pooled SD ≤ 24.5%."
            ),
            p(
              "When datasets consist only of published experiments, pooled effects are ",
              "often overestimated due to publication bias in preclinical literature ",
              "(e.g., ",
              tags$a(href = "https://doi.org/10.1371/journal.pbio.1000344", target = "_blank",
                     "Sena et al., 2010"),
              "; low power also inflates observed effects, see ",
              tags$a(href = "https://doi.org/10.1038/nrn3475", target = "_blank",
                     "Button et al., 2013"),
              "). To guard against optimism while retaining clinical relevance, the app ",
              "lets you plan for 80% or 50% of \\(\\hat{\\mu}\\). This can reduce the ",
              "required number of animals in line with the 3Rs principle ",
              "(Reduction) yet still target a meaningful, translational effect."
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
        tags$li("Target effect (MD): ", paste(round(md_used, 2), lvl_label)),
        tags$li("Expected variability (SD): ", paste(round(sd_used, 2), sd_label))
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
