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
  
      # --- LEFT: Settings -----------------------------------------------------
      card(
        class = "sidebar-card",
        card_header("Settings"),
  
        # Dataset choice
        div(
          class = "input-block",
          radioButtons(
            inputId = "data_source",
            label   = "Dataset for meta-analysis",
            choices = c(
              "Use curated EMTVN dataset" = "paper",
              "Upload dataset"            = "custom"
            )
          )
        ),
  
        # Upload
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
          label   = "Load data",
          class   = "btn btn-primary w-100"
        ),
  
        # Outcome + planning choices (after data ready)
        conditionalPanel(
          condition = "output.data_ready == 1",
  
          uiOutput("outcome_picker"),
  
          # Target effect: discrete buckets
          div(
            class = "input-block",
            
            radioButtons(
              inputId = "effect_level",
              label   = "Target effect to detect",
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
  
          # Pooled SD: percentile buckets
          div(
            class = "input-block",
            radioButtons(
              inputId = "sd_bucket",
              label   = "Expected variability",
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
              label = tagList(
                "Power ",
                tags$span(
                  "\u2139",
                  title = paste0(
                    "80% power is a solid default unless you know exactly ",
                    "why you’d change it"
                  ),
                  class = "info-icon"
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
  
      # --- RIGHT: Tabs --------------------------------------------------------
      
      tagList(
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
          
          # --- Quick start ----------------------------------------------------
          bslib::accordion_panel(
            "Quick start",
            p(
              "This app aggregates experiment-level effect sizes, runs a meta-analysis ",
              "to form a pooled benchmark, and uses it to compute per-group sample size ",
              "for your new study."
            ),
            p(HTML(
              "The aim is to plan studies that can detect a <strong>biologically",
              " meaningful</strong> effect – large enough to matter and, ideally,",
              " predictive of successful translation into an effective treatment.",
            )),
            
            p(
              "This app suggests defining the target effect from drugs with established ",
              "efficacy in humans that were also tested in the animal model you plan to ",
              "use. A pooled estimate from those experiments shows what a clinically ",
              "relevant effect looks like in that model, providing a solid basis ",
              "for designing your study."
            ),
            p("Two ways to get started:"),
            tags$ol(
              tags$li(
                HTML(
                  "Use our curated dataset – a prepared set of all published (as of December 2021) ",
                  "experiments testing clinically effective anti-migraine drugs in the ",
                  "rat electrophysiological model of trigeminovascular nociception ",
                  "(EMTVN), collected through systematic review ",
                  "(<a href='https://www.crd.york.ac.uk/PROSPERO/view/CRD42021276448' ",
                  "target='_blank'>protocol</a>). Use this only to plan new ",
                  "experiments in this specific model."
                )
              ),
              tags$li(
                "Upload your own dataset prepared in the required format (see below)."
              )
            )
          ),
          
          # --- Prepare your own dataset ---------------------------------------
          
          bslib::accordion_panel(
            "Prepare your own dataset",
            p(
              "Use one row for each experiment. Define an experiment as a single ",
              "comparison between a treatment group and its matched control group ",
              "within the same study."
            ),
            p("Required columns:"),
            tags$ul(
              tags$li("StudyID – numeric; unique study identifier."),
              tags$li(
                "ExpID – numeric; unique identifier for each experiment within a ",
                "study."
              ),
              tags$li("Outcome – character; endpoint label for grouping."),
              tags$li(
                "EffectSize – numeric; effect estimate. Use one metric consistently ",
                "across all rows."
              ),
              tags$li("SE – numeric; standard error of EffectSize."),
              tags$li(
                "Spooled – numeric; pooled (combined) standard deviation across ",
                "treatment and control groups that will be used in sample-size ",
                "calculation."
              )
            ),
            p("Optional columns:"),
            tags$ul(
              tags$li("Drug – character; treatment name."),
              tags$li("Reference – character; citation or short study name.")
            ),
            p(
              "Supported effect-size types can be found in the documentation for ",
              tags$a(
                href = "https://wviechtb.github.io/metafor/reference/rma.mv.html",
                target = "_blank",
                "rma.mv {metafor}"
              ), ". The implementation used by the app is shown in the section below."
            ),
            p(
              "For guidance on calculating EffectSize, SE, and Spooled, including ",
              "mean differences, standardized mean differences, and other common ",
              "effect-size metrics, we recommend ",
              tags$a(
                href = "https://www.sciencedirect.com/science/article/pii/S016502701300321X",
                target = "_blank",
                "Vesterinen et al. (2014)"
              ),
              ". This paper provides step-by-step formulas for most cases you are ",
              "likely to encounter in preclinical datasets."
            )
          ),
          
          # --- Data checks ----------------------------------------------------
          
          bslib::accordion_panel(
            "Data checks",
            withMathJax(
              p(
                "Before running the meta-analysis, the app automatically screens ",
                "the dataset for potential outliers and influential cases. This ",
                "helps ensure that the pooled benchmark is not distorted by a few ",
                "atypical experiments."
              ),
              p(
                HTML(
                  "Potential outliers are flagged via standardized deleted ",
                  "residuals (\\(|\\text{residual}| > 1.96\\)) and influential ",
                  "cases via Cook’s distance (\\( D_i > 4/n \\), where \\(n\\) is ",
                  "the number of data points; <a href=",
                  "'https://doi.org/10.1201/9781315119403' target='_blank'>",
                  "Handbook of Meta-Analysis</a>, Schmid et&nbsp;al., 2020). We ",
                  "remove only influential outliers i.e., points meeting both criteria."
                )
              )
            )
          ),
          
          # --- Meta-analytic model --------------------------------------------
          
          bslib::accordion_panel(
            "Meta-analytic model",
            withMathJax(
              p(HTML(
                "Per-experiment effect sizes are the observed values ",
                "(\\(\\mathit{y}_{ij}\\)) with known standard errors ",
                "(\\(SE_{ij}\\)). Here, \\(\\mathit{y}_{ij}\\) corresponds to the value in ",
                "<code>EffectSize</code> for experiment \\(i\\) in study \\(j\\). ",
                "To pool these while accounting for clustering, the app fits a ",
                "three-level random-effects model ",
                "(<a href='https://doi.org/10.18637/jss.v036.i03' target='_blank'>Viechtbauer, 2010</a>). ",
                "Let \\(\\theta_{ij}\\) denote the true effect for experiment \\(i\\) in study \\(j\\):"
              ))
            ),
            
            div(class = "row equal-row",
                # LEFT: explanation
                div(class = "col-sm-7",
                    div(class = "card-box",
                        withMathJax(
                          div(style = "font-size:85%;",
                              p(
                                "$$\\theta_{ij} = \\mu + u_{\\text{study},j} + u_{\\text{exp},ij},\\qquad ",
                                "\\mathit{y}_{ij} \\mid \\theta_{ij} \\sim ",
                                "\\mathcal{N}(\\theta_{ij},\\, v_{ij}),\\quad ",
                                "v_{ij} = SE_{ij}^{2}$$"
                              ),
                              tags$ul(
                                tags$li(HTML(
                                  "\\(\\mu\\) – average true effect; its estimate ",
                                  "\\(\\hat{\\mu}\\) is the pooled benchmark used for ",
                                  "sample-size planning"
                                )),
                                tags$li(HTML(
                                  "between-study heterogeneity: ",
                                  "\\(u_{\\text{study},j} \\sim \\mathcal{N}(0,\\, \\tau^2_{\\text{study}})\\)"
                                )),
                                tags$li(HTML(
                                  "within-study (between-experiment) heterogeneity: ",
                                  "\\(u_{\\text{exp},ij} \\sim \\mathcal{N}(0,\\, \\tau^2_{\\text{exp}})\\)"
                                )),
                                tags$li(HTML(
                                  "sampling variance: \\(v_{ij} = SE_{ij}^{2}\\) (from input)"
                                )),
                                tags$li(HTML(
                                  "estimation uses REML; the marginal variance of ",
                                  "\\(\\mathit{y}_{ij}\\) is ",
                                  "\\(SE_{ij}^{2} + \\tau^2_{\\text{exp}} + \\tau^2_{\\text{study}}\\)"
                                ))
                              )
                          )
                        )
                    )
                ),
                
                # RIGHT: code box
                div(class = "col-sm-5",
                    div(class = "card-box",
                        {
                          code_r <- paste(
                            "res <- metafor::rma.mv(",
                            "  yi     = EffectSize,",
                            "  V      = SE^2,",
                            "  random = ~ 1 | StudyID / ExpID,",
                            "  method = \"REML\",",
                            "  data   = dt",
                            ")",
                            "",
                            "res_rob <- metafor::robust(res, cluster = dt$StudyID)",
                            sep = "\n"
                          )
                          tags$pre(class = "mono", style = "font-size:72%;", code_r)
                        }
                    )
                )
            ),
            
            p(HTML(
              "We do not require experiments to be identical – the model explicitly ",
              "allows them to differ with two sources of heterogeneity (between studies ",
              "and between experiments within studies). Study-level cluster-robust ",
              "standard errors are computed using <code>metafor::robust()</code> with a ",
              "small-sample correction ",
              "(<a href='https://doi.org/10.1002/jrsm.5' target='_blank'>Hedges et&nbsp;al., 2010</a>). ",
              "Therefore, if your dataset includes multiple experiments per study and/or ",
              "shared control groups, this dependence is accommodated by the modeling."
            ))
          ),
          
          # --- Sample size calculation ----------------------------------------
          
          bslib::accordion_panel(
            "Sample size calculation",
            withMathJax(
              p(
                "The app uses the pooled estimate \\(\\hat{\\mu}\\) from the meta-analysis ",
                "as the benchmark effect size. The per-group sample size is ",
                "then calculated for your chosen power using the Mann–Whitney U ",
                "(Wilcoxon rank-sum) test formula (two-sided, \\(\\alpha = 0.05\\), ",
                "allocation ratio 1:1) with a 5% nonparametric correction to align ",
                "with G*Power results."
              )
            ),
            p(
              "In our work and previous preclinical meta-analyses, standard deviations (SDs) ",
              "varied widely across experiments. It is therefore likely that your dataset ",
              "will also show substantial variability. For this reason, the app allows ",
              "you to explore scenarios with different pooled SD at the 20th, 50th, and ",
              "80th percentiles of the loaded dataset (labeled Low, Median, and High variability).",
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
              "it means 80% of experiments in the dataset had pooled SD ≤ 24.5%."
            ),
            p(HTML(
              "When based only on published experiments, pooled effect sizes may be ",
              "overestimated because positive findings are more likely to be reported ",
              "(e.g., <a href='https://doi.org/10.1371/journal.pbio.1000344' target='_blank'>",
              "Sena et al., 2010</a>); low power, often noted in preclinical studies, ",
              "can also exaggerate observed effects",
              "(<a href='https://doi.org/10.1038/nrn3475' target='_blank'>Button et al., 2013</a>). ",
              "To temper optimism while retaining clinical ",
              "relevance, the app allows you to power the study to detect ",
              "\\(0.8\\hat{\\mu}\\) or \\(0.5\\hat{\\mu}\\). This is more conservative ",
              "(increases the per-group sample size), but it reduces the risk of ",
              "underpowered results and of avoidable follow-up studies, thus supporting ",
              "the 3Rs (Reduction) at the program level."
            ))
          ),
          
          # --- Sources & Further Reading --------------------------------------
          
          bslib::accordion_panel(
            "Sources & data",
            p(
              "Paper: ",
              tags$a(
                href = "https://doi.org/10.1111/ejn.16030",
                target = "_blank",
                "Dolgorukova et al. (2023), ",
                tags$i("European Journal of Neuroscience")
              ),
              br(),
              "Data and scripts for the paper: ",
              tags$a(
                href = "https://osf.io/vzjys/",
                target = "_blank",
                "OSF project"
              ),
              br(),
              "GitHub repo: ",
              tags$a(
                href = "https://github.com/T0chka/BenchmarkSampleSizePlanner.git",
                target = "_blank",
                "BenchmarkSampleSizePlanner"
              )
            ),
            tags$hr(),
            h5("Further reading"),
            tags$ul(
              tags$li(
                tags$a(
                  href = "https://arriveguidelines.org/",
                  target = "_blank",
                  "ARRIVE guidelines 2.0"
                ),
                " – reporting standards for animal studies."
              ),
              tags$li(
                tags$a(
                  href = "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-43",
                  target = "_blank",
                  "SYRCLE risk of bias tool"
                ),
                " – bias assessment tailored to animal experiments."
              ),
              tags$li(
                tags$a(
                  href = "https://www.jstatsoft.org/v36/i03/",
                  target = "_blank",
                  "Viechtbauer (2010), metafor"
                ),
                " – the core reference for meta-analysis in R."
              ),
              tags$li(
                tags$a(
                  href = "https://onlinelibrary.wiley.com/doi/10.1002/jrsm.5",
                  target = "_blank",
                  "Hedges, Tipton & Johnson (2010)"
                ),
                " – robust variance for dependent effects."
              ),
              tags$li(
                tags$a(
                  href = "https://www.nature.com/articles/nrn3475",
                  target = "_blank",
                  "Button et al. (2013)"
                ),
                " – why small samples undermine reliability."
              ),
              tags$li(
                tags$a(
                  href = "https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1000344",
                  target = "_blank",
                  "Sena et al. (2010)"
                ),
                " – publication bias in preclinical literature."
              ),
              tags$li(
                tags$a(
                  href = "https://link.springer.com/article/10.3758/BRM.41.4.1149",
                  target = "_blank",
                  "Faul et al. (2009), G*Power 3.1"
                ),
                " – reference for G*Power’s implementation."
              ),
              tags$li(
                tags$a(
                  href = "https://pubmed.ncbi.nlm.nih.gov/24099992/",
                  target = "_blank",
                  "Vesterinen et al. (2014)"
                ),
                " – meta-analysis of animal studies: a practical guide."
              ),
              tags$li(
                tags$a(
                  href = "https://clinical-brain-sciences.ed.ac.uk/camarades/about-camarades",
                  target = "_blank",
                  "CAMARADES"
                ),
                " – network, training and tools for animal SR & meta-analysis."
              ),
              tags$li(
                tags$a(
                  href = "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020124",
                  target = "_blank",
                  "Ioannidis (2005)"
                ),
                " – metascience on power, bias and false-positive claims."
              ),
              tags$li(
                tags$a(
                  href = "https://onlinelibrary.wiley.com/doi/book/10.1002/9780470723586",
                  target = "_blank",
                  "Senn, Statistical Issues in Drug Development (2nd ed.)"
                ),
                " – classic on trial design; origin of the ‘cynically relevant’ quip."
              )
            )
          )
          # --- Accordeon end --------------------------------------------------
        )
      )
    )
  ),
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
        label = "Outcome",
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
      p("Selected inputs for calculation"),
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
