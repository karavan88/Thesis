# ==============================================================================
# THESIS - PROJECT ENVIRONMENT SETUP
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labour Market Outcomes (PhD Thesis)
# File:         project_config.R
# Purpose:      Configure paths and load required packages for the thesis build.
#
# Layout (this repository):
#   00_documentation/          - Project documentation
#   01_data/                   - All input/intermediate data needed by the book
#       processed/             - RLMS-derived analysis-ready data
#       empl_outputs/          - Pre-computed model outputs for Ch. 4
#       returns_outputs/       - Pre-computed model outputs for Ch. 5
#           thesis/            - Quantile-regression / life-cycle outputs
#       jobsatisf_outputs/     - Pre-computed model outputs for Ch. 6
#   02_codes/                  - R scripts sourced from the qmds
#       01_introduction/
#       02_lit_rev/
#       03_data/               - Data preparation + checks
#       04_ncs_empl/           - Employment models (Ch. 4)
#       05_ncs_wages/          - Wage / returns models (Ch. 5)
#       06_ncs_job_satisf/     - Job satisfaction models (Ch. 6)
#   03_thesis/                 - Quarto book sources (qmds, _quarto.yml, bib)
#
# The variable names below (processedData, codesEmplNcs, etc.) are kept
# IDENTICAL to those used in the upstream NonCognSkillsRLMS repo so that the
# R scripts copied into 02_codes/ run unmodified.
# ==============================================================================

projectFolder <- tryCatch(
  rprojroot::find_root(rprojroot::is_rstudio_project),
  error = function(e) tryCatch(
    rprojroot::find_root(rprojroot::has_file("project_config.R")),
    error = function(e2) getwd()
  )
)

# ---- Data directories --------------------------------------------------------
inputData          <- file.path(projectFolder, "01_data")
processedData      <- file.path(inputData, "processed")

# ---- Output directories (analysis artefacts produced by 02_codes) -----------
outputRoot         <- file.path(projectFolder, "03_output")
outputsEmplNcs     <- file.path(outputRoot, "empl_outputs")
outputsReturnsNcs  <- file.path(outputRoot, "returns_outputs")
outputsJobSatisfNcs <- file.path(outputRoot, "jobsatisf_outputs")

# Sub-folder used by the annex (Ch. 5 quantile + life-cycle models)
returns_thesis_out <- file.path(outputsReturnsNcs, "thesis")

# ---- Code directories --------------------------------------------------------
codesRoot          <- file.path(projectFolder, "02_codes")
rCodes             <- file.path(codesRoot, "03_data")
codesEmplNcs       <- file.path(codesRoot, "04_ncs_empl")
codesReturnsNcs    <- file.path(codesRoot, "05_ncs_wages")
codesJobSatisfNcs  <- file.path(codesRoot, "06_ncs_job_satisf")

# ---- Validation --------------------------------------------------------------
.required_dirs <- c(
  projectFolder, inputData, processedData,
  outputRoot, outputsEmplNcs, outputsReturnsNcs,
  codesRoot, rCodes, codesEmplNcs, codesReturnsNcs, codesJobSatisfNcs
)
.missing <- .required_dirs[!dir.exists(.required_dirs)]
if (length(.missing)) {
  stop("Missing required directories:\n  ", paste(.missing, collapse = "\n  "))
}

# ---- Package loading ---------------------------------------------------------
required_packages <- list(
  "Core Data Science"       = c("dplyr", "ggplot2", "readr", "tidyr",
                                "purrr", "tibble", "stringr", "forcats", "lubridate"),
  "Statistical Analysis"    = c("lme4", "lmerTest", "lmtest", "plm", "lqmm", "mgcv",
                                "broom", "broom.mixed", "psych", "e1071"),
  "Effects & Visualization" = c("ggeffects", "gridExtra", "sjPlot", "ggcorrplot",
                                "ggstats"),
  "Data Import/Export"      = c("haven", "readxl", "here"),
  "Tables & Output"         = c("gtsummary", "modelsummary", "tinytable",
                                "kableExtra", "gt", "flextable"),
  "Statistical Packages"    = c("easystats", "bayestestR", "performance", "parameters",
                                "effectsize", "correlation", "insight"),
  "Utilities"               = c("glue", "igraph", "WeightIt", "showtext", "conflicted")
)

.safe_load <- function(pkg) {
  tryCatch(
    {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE)
      )
      TRUE
    },
    error = function(e) FALSE
  )
}

.failed <- character()
for (cat in names(required_packages)) {
  for (pkg in required_packages[[cat]]) {
    if (!.safe_load(pkg)) .failed <- c(.failed, pkg)
  }
}

if (length(.failed)) {
  message("Packages failed to load: ", paste(.failed, collapse = ", "),
          "\nRun renv::restore() to install them.")
}

if ("conflicted" %in% loadedNamespaces()) {
  conflicted::conflicts_prefer(
    dplyr::filter, dplyr::lag, dplyr::select, dplyr::rename,
    dplyr::mutate, dplyr::summarise, dplyr::slice, dplyr::arrange,
    .quiet = TRUE
  )
  conflicted::conflicts_prefer(lmerTest::lmer, .quiet = TRUE)
}

# ---- User-defined helper functions ------------------------------------------
# Source every *.R file under 99_user_functions/ so the helpers used inside the
# qmds (table styling, model-output formatting, inline coefficient lookups,
# etc.) are available globally.
user_functions_dir <- file.path(projectFolder, "99_user_functions")
if (dir.exists(user_functions_dir)) {
  for (f in list.files(user_functions_dir, pattern = "\\.R$", full.names = TRUE)) {
    source(f, local = FALSE)
  }
}

invisible(NULL)
