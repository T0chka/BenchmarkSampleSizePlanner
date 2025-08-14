## Benchmark-Based Sample Size Planner (Shiny)

A Shiny application, which takes a set of experiment-level effect sizes, performs a meta-analysis to produce a pooled benchmark, and then uses that benchmark to calculate how many animals per group you’ll need for a new study.

The aim is to plan studies that can detect a biologically meaningful effect – large enough to matter and, ideally, predictive of successful translation into an effective treatment.

The app remove influential outliers, pools mean differences with a three-level random-effects model and study-cluster robust standard errors ([metafor package](https://www.metafor-project.org/doku.php/metafor)), then calculates sample sizes for the Wilcoxon–Mann–Whitney
(two-sided alpha = 0.05) with a 5% correction aligned to G*Power.

### Key features
- **Curated or custom data**: Use the bundled EMTVN dataset or upload your
  own.
- **Robust meta-analysis**: `metafor::rma.mv` with cluster-robust variance.
- **Planning scenarios**: Target 100%/80%/50% of the pooled benchmark and SD
  percentiles (20th/50th/80th).

### Run locally
1. R >= 4.2 recommended.
2. Install packages:
   ```r
   install.packages(c(
     "shiny","readxl","data.table","metafor","DT","bslib",
     "bsicons","shinycssloaders","shinyjs"
   ))
   ```
3. Launch the app:
   ```r
   shiny::runApp(".", launch.browser = TRUE)
   ```

### Data inputs
- Required columns (types):
  - `Outcome` (character or factor)
  - `Study.ID`, `Exp.ID` (numeric or convertible character)
  - `MD`, `SE`, `Spooled` (numeric)
- Optional: `Drug`, `Reference` (character)
- Example dataset: `MA_data_set.xlsx` (sheet `MA`).

### Method (brief)
- Three-level random-effects over `Study.ID / Exp.ID` (REML) with study-level
  robust SE.
- Influential outliers removed via standardized deleted residuals (|z| ≥ 1.96)
  and Cook’s distance > 4/n.
- Sample size via `power.t.test` with a 5% non-parametric correction; SD
  quantiles reflect variability scenarios.

### Repository structure
- `app.R`: Shiny UI/server
- `utils.R`: Validation, outliers, meta-analysis, sample-size
- `css.R`: Theme and styling
- `MA_data_set.xlsx`: Curated EMTVN dataset

### References
- Dolgorukova et al., Eur J Neurosci. See the
  [paper](https://doi.org/10.1111/ejn.16030) and the
  [OSF project](https://osf.io/vzjys/).
