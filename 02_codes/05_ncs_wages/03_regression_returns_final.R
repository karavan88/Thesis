# ==============================================================================
# NON-COGNITIVE SKILLS AND RETURNS - QUANTILE REGRESSION ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         03_regression_returns.R
# Purpose:      Quantile regression analysis of returns to non-cognitive skills
# 
# Description:  This script estimates quantile regression models to analyze
#               heterogeneous returns to non-cognitive skills across the wage
#               distribution. Uses linear mixed quantile models (LQMM) to account
#               for individual heterogeneity while examining differential effects
#               at various quantiles (10%, 25%, 50%, 75%, 90%).
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 16-29 years (2016-2019)
# Methodology:  Linear Quantile Mixed Models (LQMM) with random intercepts
#
# Models:       M1 - Baseline quantile regression across all quantiles
#               M2 - Models with IPW (Inverse Probability Weighting)
#               M3 - Models by education levels
#               M4 - Models by gender
#               M5 - Lifecycle analysis models
#
# Key Variables: log_wage (dependent variable)
#               O, C, E, A, ES (Big Five personality traits)
#               exp_imp (work experience)
#               Controls: area, sex, marital_status, region
#
# Author:       Garen Avanesian  
# Institution:  Southern Federal University
# Created:      December 1, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: lqmm, tidyverse, broom
# Runtime:      ~10-15 minutes (quantile models are computationally intensive)
#
# Notes:        Quantile regression allows analysis of heterogeneous effects
#               LQMM accounts for individual-level random effects
#               Results saved as CSV files for output generation
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
outputsReturnsNcsThesis <- file.path(outputsReturnsNcs, "thesis")
dir.create(outputsReturnsNcsThesis, recursive = TRUE, showWarnings = FALSE)

cat(rep("=", 80), "\n")
cat("📊 RETURNS TO NCS - QUANTILE REGRESSION ANALYSIS\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 03_regression_returns.R\n")
cat("🎯 Purpose: Quantile regression analysis of NCS returns\n")
cat("📈 Processing: Youth returns data → LQMM models → Coefficient exports\n\n")

# SECTION 1: DATA PREPARATION AND FACTOR SETUP
cat("🔧 SECTION 1: DATA PREPARATION AND FACTOR SETUP\n")
cat("Preparing categorical variables and factor levels...\n")
factor_start <- Sys.time()

# Load the youth returns dataset
youth_master_returns <-
  readRDS(file.path(outputsReturnsNcs, "youth_master_returns.rds")) %>%
  # exclude 0 occupation code (military) for returns analysis
  filter(occup08 != 0) 

# dataset for the comparative analysis across the age groups
ind_master_returns <-
  readRDS(file.path(outputsReturnsNcs, "ind_master_returns.rds")) %>%
  # exclude 0 occupation code (military for returns analysis
  filter(occup08 != 0) 

youth_master_returns$sex <- factor(youth_master_returns$sex, levels = c("Female", "Male"))
youth_master_returns$edu_lvl <- factor(youth_master_returns$edu_lvl, levels = c("1. No school", 
                                                                "2. Secondary School",
                                                                "3. Secondary Vocational",
                                                                "4. Tertiary"))

factor_end <- Sys.time()
cat("✅ Factor preparation completed in", round(difftime(factor_end, factor_start, units = "secs"), 2), "seconds\n")
cat("   - Sex factor levels:", paste(levels(youth_master_returns$sex), collapse = ", "), "\n")
cat("   - Education factor levels:", paste(levels(youth_master_returns$edu_lvl), collapse = ", "), "\n")
cat("   - Sex distribution:\n")
print(summary(youth_master_returns$sex))
cat("   - Education distribution:\n")
print(summary(youth_master_returns$edu_lvl))
cat("\n")

# SECTION 2: AREA VARIABLE CREATION
cat("🏘️  SECTION 2: AREA VARIABLE CREATION\n")
cat("Creating urban/rural classification...\n")
area_start <- Sys.time()



# Generate urban/rural variable
youth_master_returns$area1 = ifelse( youth_master_returns$area %in% c("Областной центр", "Город"), "urban", "rural")
# Set factor levels with rural as base
youth_master_returns$area1 = factor(youth_master_returns$area1, levels = c("rural", "urban"))

area_end <- Sys.time()
cat("✅ Area classification completed in", round(difftime(area_end, area_start, units = "secs"), 2), "seconds\n")
cat("   - Urban areas: Областной центр, Город\n")
cat("   - Rural areas: All other areas\n")
cat("   - Area distribution:\n")
print(summary(youth_master_returns$area1))
cat("\n")

# SECTION 3: MODEL SPECIFICATION 
cat("📊 SECTION 3: BASELINE QUANTILE REGRESSION MODEL (M1)\n")
cat("Setting up general formula and fitting LQMM across quantiles...\n")
cat("Quantiles: 10%, 25%, 50%, 75%, 90%\n")
model_start <- Sys.time()

# Define general regression formula
general_formula <- log_wage ~ exp_imp + I(exp_imp^2)  + area  + sex  + marital_status + 
  region + O + C + E + A + ES 

cat("   - Formula: log_wage ~ exp_imp + I(exp_imp^2) + area + sex + marital_status + region + NCS\n")
cat("   - NCS variables: O, C, E, A, ES\n")
cat("   - Random effects: individual-level random intercepts\n")
cat("   - Fitting model across 5 quantiles...\n")


# SECTION 4: IPW-WEIGHTED BASELINE MODEL (M1_IPW)
cat("⚖️  SECTION 5: IPW-WEIGHTED BASELINE MODEL (M1_IPW)\n")
cat("Fitting baseline model with Inverse Probability Weighting...\n")
ipw_start <- Sys.time()


ctrl <- lqmmControl(
  method = "df",
  LP_max_iter = 3000,
  UP_max_iter = 3000,
  LP_tol_ll = 1e-5,
  UP_tol = 1e-6
)

ctrl1 <- lqmmControl(
  method = "df",
  LP_max_iter = 5000,
  UP_max_iter = 100,
  LP_tol_ll = 1e-5,
  LP_tol_theta = 1e-5,
  UP_tol = 1e-5,
  check_theta = TRUE,
  startQR = TRUE,
  verbose = TRUE
)

quantile_map <- c(q10 = 0.10, q25 = 0.25, q50 = 0.50, q75 = 0.75, q90 = 0.90)

lqmm_workers <- suppressWarnings(as.integer(Sys.getenv("NCS_LQMM_WORKERS", unset = NA)))

if (is.na(lqmm_workers)) {
  detected_cores <- parallel::detectCores(logical = FALSE)

  if (is.na(detected_cores)) {
    detected_cores <- 2L
  }

  # Keep one core free because each LQMM fit is memory-heavy.
  lqmm_workers <- max(1L, min(length(quantile_map), detected_cores - 1L))
}

cat("   - LQMM worker processes:", lqmm_workers, "\n")

fit_lqmm_by_quantile <- function(fixed_formula, data_frame, weight_vector, tau_values, control_obj,
                                 control_overrides = NULL) {
  tau_labels <- names(tau_values)

  fit_one_quantile <- function(idx) {
    tau_label <- tau_labels[[idx]]
    tau_value <- as.numeric(tau_values[[idx]])
    control_use <- control_obj

    if (!is.null(control_overrides) && tau_label %in% names(control_overrides)) {
      control_use <- control_overrides[[tau_label]]
    }

    lqmm(
      fixed = fixed_formula,
      data = data_frame,
      random = ~1,
      group = idind,
      tau = tau_value,
      weights = weight_vector,
      control = control_use
    )
  }

  if (.Platform$OS.type != "windows" && lqmm_workers > 1L) {
    cat("   - Running quantile fits in parallel via parallel::mclapply()\n")

    model_list <- parallel::mclapply(
      X = seq_along(tau_values),
      FUN = fit_one_quantile,
      mc.cores = min(lqmm_workers, length(tau_values))
    )
  } else {
    if (lqmm_workers > 1L) {
      cat("   - Parallel execution unavailable on this OS in script mode; using serial fallback\n")
    }

    model_list <- lapply(seq_along(tau_values), fit_one_quantile)
  }

  rlang::set_names(model_list, tau_labels)
}

summarize_lqmm_models <- function(model_list) {
  model_labels <- names(model_list)

  summarize_one <- function(idx) {
    summary(model_list[[idx]])
  }

  if (.Platform$OS.type != "windows" && lqmm_workers > 1L) {
    cat("   - Running summary extraction in parallel via parallel::mclapply()\n")

    summary_list <- parallel::mclapply(
      X = seq_along(model_list),
      FUN = summarize_one,
      mc.cores = min(lqmm_workers, length(model_list))
    )
  } else {
    summary_list <- lapply(seq_along(model_list), summarize_one)
  }

  rlang::set_names(summary_list, model_labels)
}

extract_quantile_coefs <- function(summary_list, keep_region = FALSE) {
  purrr::imap(summary_list, function(model_summary, q_label) {
    coef_tbl <-
      model_summary$tTable %>%
      as.data.frame() %>%
      rownames_to_column(var = "variable")

    names(coef_tbl)[2:6] <- c("estimate", "std.error", "ci.low", "ci.upp", "p.value")

    coef_tbl <-
      coef_tbl %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    if (!keep_region) {
      coef_tbl <- coef_tbl %>% filter(!str_detect(variable, "region"))
    }

    coef_tbl %>%
      rename_with(~ paste0(q_label, "_", .x), -variable)
  }) %>%
    purrr::reduce(full_join, by = "variable")
}

save_lqmm_model_objects <- function(model_list, file_prefix, output_dir) {
  purrr::iwalk(model_list, function(model_obj, model_label) {
    saveRDS(
      model_obj,
      file.path(output_dir, paste0(file_prefix, "_", model_label, "_model.rds"))
    )
  })

  cat(
    "✅",
    file_prefix,
    "fitted model objects saved (",
    length(model_list),
    " files)\n",
    sep = ""
  )
}

m1_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula = general_formula,
  data_frame = youth_master_returns,
  weight_vector = youth_master_returns$ipw_empl,
  tau_values = quantile_map,
  control_obj = ctrl,
  control_overrides = list(
    q25 = ctrl1,
    q90 = ctrl1
  )
)

m1_q10_ipw <- m1_ipw_models$q10
m1_q25_ipw <- m1_ipw_models$q25
m1_q50_ipw <- m1_ipw_models$q50
m1_q75_ipw <- m1_ipw_models$q75
m1_q90_ipw <- m1_ipw_models$q90

save_lqmm_model_objects(m1_ipw_models, "m1_ipw", outputsReturnsNcsThesis)

ipw_end <- Sys.time()
cat("✅ IPW baseline model (M1_IPW) completed in", round(difftime(ipw_end, ipw_start, units = "mins"), 2), "minutes\n")

# Extract IPW model results
cat("   - Extracting IPW model summary and coefficients...\n")

m1_ipw_summaries <- summarize_lqmm_models(m1_ipw_models)

saveRDS(m1_ipw_summaries, file.path(outputsReturnsNcsThesis, "m1_ipw_summaries.rds"))
cat("✅ M1_IPW summaries saved to m1_ipw_summaries.rds\n")

m1_ipw_coefs <- extract_quantile_coefs(m1_ipw_summaries)

# Define column names for quantile results
new_names <- c("variable", 
               "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
               "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
               "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
               "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
               "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m1_ipw_coefs) <- new_names

# Export IPW coefficients
write_csv(m1_ipw_coefs, file.path(outputsReturnsNcsThesis, "m1_ipw_coefs.csv"))
cat("✅ M1_IPW coefficients exported to m1_ipw_coefs.csv\n\n")


# SECTION 5: EDUCATION-EXTENDED MODELS (M2_IPW)
cat("🎓 SECTION 5: EDUCATION-EXTENDED QUANTILE MODELS\n")
cat("Fitting models with explicit education level controls...\n")

# Define education-extended formula
formula_edu <- log_wage ~ edu_lvl + exp_imp + I(exp_imp^2)  + sex + area + region + marital_status + 
  O + C + E + A + ES

cat("   - Extended formula includes explicit education levels\n")
cat("   - Education levels: No school, Secondary, Secondary Vocational, Tertiary\n")


# Fit M2_IPW (education-extended with IPW)
cat("   - Fitting M2_IPW (education-extended with IPW)...\n")
m2_ipw_start <- Sys.time()

m2_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula = formula_edu,
  data_frame = youth_master_returns,
  weight_vector = youth_master_returns$ipw_empl,
  tau_values = quantile_map,
  control_obj = ctrl,
  control_overrides = list(
    q25 = ctrl1,
    q90 = ctrl1
  )
)

m2_q10_ipw <- m2_ipw_models$q10
m2_q25_ipw <- m2_ipw_models$q25
m2_q50_ipw <- m2_ipw_models$q50
m2_q75_ipw <- m2_ipw_models$q75
m2_q90_ipw <- m2_ipw_models$q90

save_lqmm_model_objects(m2_ipw_models, "m2_ipw", outputsReturnsNcsThesis)

m2_ipw_end <- Sys.time()
cat("✅ M2_IPW model completed in", round(difftime(m2_ipw_end, m2_ipw_start, units = "mins"), 2), "minutes\n")

# Extract model summaries and coefficients
cat("   - Extracting M2 and M2_IPW coefficients...\n")
coef_extract_start <- Sys.time()

m2_ipw_summaries <- summarize_lqmm_models(m2_ipw_models)
saveRDS(m2_ipw_summaries, file.path(outputsReturnsNcsThesis, "m2_ipw_summaries.rds"))
cat("✅ M2_IPW summaries saved to m2_ipw_summaries.rds\n")


# Extract M2_IPW coefficients  
m2_ipw_coefs <- extract_quantile_coefs(m2_ipw_summaries)

names(m2_ipw_coefs) <- new_names
write_csv(m2_ipw_coefs, file.path(outputsReturnsNcsThesis, "m2_ipw_coefs.csv"))

coef_extract_end <- Sys.time()
cat("✅ M2_IPW coefficients extracted and exported in", round(difftime(coef_extract_end, coef_extract_start, units = "secs"), 2), "seconds\n")
cat("   - Files: m2_ipw_coefs.csv\n")
cat("   - Education effects now explicitly modeled\n\n")


cat("🎓 SECTION 6: EDUCATION-STRATIFIED QUANTILE MODELS\n")

#####################   create a lqmm model  by education level        ####################


### Tertiary
dat_m3_tert <- youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ]
m3_tert_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula = general_formula,
  data_frame = dat_m3_tert,
  weight_vector = dat_m3_tert$ipw_empl,
  tau_values = quantile_map,
  control_obj = ctrl,
  control_overrides = list(
    q25 = ctrl1,
    q50 = ctrl1,
    q75 = ctrl1,
    q90 = ctrl1
  )
)

m3_tert_q10_ipw <- m3_tert_ipw_models$q10
m3_tert_q25_ipw <- m3_tert_ipw_models$q25
m3_tert_q50_ipw <- m3_tert_ipw_models$q50
m3_tert_q75_ipw <- m3_tert_ipw_models$q75
m3_tert_q90_ipw <- m3_tert_ipw_models$q90

save_lqmm_model_objects(m3_tert_ipw_models, "m3_tert_ipw", outputsReturnsNcsThesis)

### Secondary Vocational
dat_m3_voc <- youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ]
m3_voc_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula = general_formula,
  data_frame = dat_m3_voc,
  weight_vector = dat_m3_voc$ipw_empl,
  tau_values = quantile_map,
  control_obj = ctrl,
  control_overrides = list(
    q25 = ctrl1,
    q50 = ctrl1,
    q90 = ctrl1
  )
)

m3_voc_q10_ipw <- m3_voc_ipw_models$q10
m3_voc_q25_ipw <- m3_voc_ipw_models$q25
m3_voc_q50_ipw <- m3_voc_ipw_models$q50
m3_voc_q75_ipw <- m3_voc_ipw_models$q75
m3_voc_q90_ipw <- m3_voc_ipw_models$q90

save_lqmm_model_objects(m3_voc_ipw_models, "m3_voc_ipw", outputsReturnsNcsThesis)

### Secondary or below
dat_m3_sec <- youth_master_returns[youth_master_returns$edu_lvl %in% c("1. No school", "2. Secondary School"), ]
m3_sec_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula = general_formula,
  data_frame = dat_m3_sec,
  weight_vector = dat_m3_sec$ipw_empl,
  tau_values = quantile_map,
  control_obj = ctrl,
  control_overrides = list(
    q25 = ctrl1,
    q75 = ctrl1,
    q90 = ctrl1
  )
)

m3_sec_q10_ipw <- m3_sec_ipw_models$q10
m3_sec_q25_ipw <- m3_sec_ipw_models$q25
m3_sec_q50_ipw <- m3_sec_ipw_models$q50
m3_sec_q75_ipw <- m3_sec_ipw_models$q75
m3_sec_q90_ipw <- m3_sec_ipw_models$q90

save_lqmm_model_objects(m3_sec_ipw_models, "m3_sec_ipw", outputsReturnsNcsThesis)

m3_tert_summary_ipw <- summarize_lqmm_models(m3_tert_ipw_models)
m3_voc_summary_ipw <- summarize_lqmm_models(m3_voc_ipw_models)
m3_sec_summary_ipw <- summarize_lqmm_models(m3_sec_ipw_models)

m3_edu_ipw_summaries <- list(
  Tertiary = m3_tert_summary_ipw,
  Secondary_Vocational = m3_voc_summary_ipw,
  Secondary_or_below = m3_sec_summary_ipw
)

saveRDS(m3_edu_ipw_summaries, file.path(outputsReturnsNcsThesis, "m3_edu_ipw_summaries.rds"))
cat("✅ M3 education-stratified summaries saved to m3_edu_ipw_summaries.rds\n")

m3_tert_coefs_ipw <-
  extract_quantile_coefs(m3_tert_summary_ipw, keep_region = TRUE) %>%
  mutate(model = "Tertiary")

m3_voc_coefs_ipw <-
  extract_quantile_coefs(m3_voc_summary_ipw, keep_region = TRUE) %>%
  mutate(model = "Secondary Vocational")

m3_sec_coefs_ipw <-
  extract_quantile_coefs(m3_sec_summary_ipw, keep_region = TRUE) %>%
  mutate(model = "Secondary or below")

m3_coefs_ipw <-
  bind_rows(m3_tert_coefs_ipw,
            m3_voc_coefs_ipw,
            m3_sec_coefs_ipw)

write_csv(m3_coefs_ipw, file.path(outputsReturnsNcsThesis, "m3_coefs_edu_ipw.csv"))
cat("✅ M3 education-stratified IPW coefficients exported to m3_coefs_edu_ipw.csv\n\n")


# SECTION 7: GENDER-STRATIFIED QUANTILE MODELS (M4)
cat("👫 SECTION 7: GENDER-STRATIFIED QUANTILE MODELS\n")

###-----------------------------------------------------------------------------
### -------------- Interaction 
###-----------------------------------------------------------------------------

# youth_master_returns$sex <- factor(youth_master_returns$sex, levels = c("Male", "Female"))


formula_gend_int <- 
  log_wage ~  exp_imp + I(exp_imp^2)  + area + sex + marital_status + region + 
  O + C + E + A + ES  +
  O*sex + C*sex + E*sex + A*sex + ES*sex 

# we need to make female as a base level in sex variable
# data_merged$sex <- relevel(data_merged$sex, ref = "female")
# levels(data_merged$sex)

m4_quantile_map <- c(q50 = 0.50, q75 = 0.75, q90 = 0.90)

m4_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula = formula_gend_int,
  data_frame = youth_master_returns,
  weight_vector = youth_master_returns$ipw_empl,
  tau_values = m4_quantile_map,
  control_obj = ctrl,
  control_overrides = list(
    q50 = ctrl1
  )
)

m4_q50_ipw <- m4_ipw_models$q50
m4_q75_ipw <- m4_ipw_models$q75
m4_q90_ipw <- m4_ipw_models$q90

save_lqmm_model_objects(m4_ipw_models, "m4_ipw", outputsReturnsNcsThesis)

m4_ipw_summary <- summarize_lqmm_models(m4_ipw_models)
saveRDS(m4_ipw_summary, file.path(outputsReturnsNcsThesis, "m4_ipw_summaries.rds"))
cat("✅ M4 interaction summaries saved to m4_ipw_summaries.rds\n")

m4_ipw_coefs <- extract_quantile_coefs(m4_ipw_summary)

new_names_upp <- c("variable", 
                   "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
                   "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
                   "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

names(m4_ipw_coefs) <- new_names_upp

# View(m4_ipw_coefs)

write.csv(m4_ipw_coefs, file.path(outputsReturnsNcsThesis, "m4_ipw_sex_ncs_int.csv"))

cat("✅ M4 gender-stratified IPW coefficients exported to m4_ipw_sex_ncs_int.csv\n\n")

# SECTION 8: LIFECYCLE ANALYSIS (MEDIAN QUANTILE BY AGE GROUPS)
cat("👶👩👴 SECTION 8: LIFECYCLE ANALYSIS\n")
cat("Fitting median quantile models by age groups for lifecycle perspective...\n")
cat("Age groups: 16-65 (full sample), 30-39, 40-49, 50-65\n")
lifecycle_start <- Sys.time()

# Full sample (16-65) lifecycle model
cat("   - Fitting full sample model (16-65 years)...\n")
m_lc <- lqmm(
  general_formula,
  data = ind_master_returns,
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl,
  control = list(method = "df")
)

m_lc_summary <- 
  summary(m_lc)

m_lc_coefs <-
  m_lc_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "16-65")

m_lc_3039 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 30 & ind_master_returns$age < 40],
  control = ctrl1
)

m_lc_3039_summary <- 
  summary(m_lc_3039)

m_lc_3039_coefs <-
  m_lc_3039_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "30-40")

m_lc_4049 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 40 & ind_master_returns$age < 50],
  control = list(method = "df")
)

m_lc_4049_summary <- 
  summary(m_lc_4049)

m_lc_4049_coefs <-
  m_lc_4049_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "40-50")

m_lc_5065 <- lqmm(
  general_formula,
  data = ind_master_returns[ind_master_returns$age >= 50, ],
  random = ~1,
  group = idind,
  tau = 0.50,
  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 50],
  control = list(method = "df")
)

m_lc_5065_summary <- 
  summary(m_lc_5065)

m_lc_models <- list(
  age_16_65 = m_lc,
  age_30_39 = m_lc_3039,
  age_40_49 = m_lc_4049,
  age_50_65 = m_lc_5065
)

save_lqmm_model_objects(m_lc_models, "m_lc_ipw", outputsReturnsNcsThesis)

m_lc_ipw_summaries <- list(
  age_16_65 = m_lc_summary,
  age_30_39 = m_lc_3039_summary,
  age_40_49 = m_lc_4049_summary,
  age_50_65 = m_lc_5065_summary
)

saveRDS(m_lc_ipw_summaries, file.path(outputsReturnsNcsThesis, "m_lc_ipw_summaries.rds"))
cat("✅ Lifecycle summaries saved to m_lc_ipw_summaries.rds\n")

m_lc_5065_coefs <-
  m_lc_5065_summary$tTable %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  filter(!str_detect(variable, "region")) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(age_group = "50-65")

m_lc <- 
  bind_rows(m_lc_coefs, m_lc_3039_coefs, m_lc_4049_coefs, m_lc_5065_coefs) 

write_csv(m_lc, file.path(outputsReturnsNcsThesis, "m_lc_ipw_coefs.csv"))

lifecycle_end <- Sys.time()
cat("✅ Lifecycle analysis completed in", round(difftime(lifecycle_end, lifecycle_start, units = "mins"), 2), "minutes\n")
cat("   - File: m_lc_ipw_coefs.csv\n")
cat("   - Age groups analyzed: 16-65, 30-39, 40-49, 50-65\n")
cat("   - Focus: Median quantile (0.50) for lifecycle perspective\n\n")

# SECTION 9: SUPPLEMENTARY GAM ANALYSIS
cat("📈 SECTION 9: SUPPLEMENTARY GAM ANALYSIS\n")
cat("Fitting GAM model for non-parametric age effects...\n")
gam_start <- Sys.time()

gam <- gam(log_wage ~ s(age) + sex + region + edu_lvl + area + marital_status, data = ind_master_returns)
age_gam <- plot(gam, se = TRUE, col = "black")
saveRDS(gam, file.path(outputsReturnsNcsThesis, "m_gam_age_model.rds"))
cat("✅ Supplementary GAM model object saved to m_gam_age_model.rds\n")

gam_end <- Sys.time()
cat("✅ GAM analysis completed in", round(difftime(gam_end, gam_start, units = "secs"), 2), "seconds\n")
cat("   - Smooth age function estimated\n")
cat("   - Age effects plot generated\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 RETURNS TO NCS QUANTILE REGRESSION ANALYSIS COMPLETED!\n")
cat(rep("=", 80), "\n")
cat("📊 MODELS FITTED:\n")
cat("   • M1_IPW: Baseline with Inverse Probability Weighting\n")
cat("   • M2_IPW: Education-extended with IPW\n")
cat("   • M6: Gender-stratified models (Male & Female)\n")
cat("   • M4: Education-stratified models (Tertiary, Vocational, Secondary)\n")
cat("   • Lifecycle: Age-group analysis (4 age groups)\n")
cat("   • GAM: Non-parametric age effects\n\n")
cat("📁 FILES EXPORTED:\n")
cat("   • m1_ipw_coefs.csv (Baseline IPW coefficients)\n")
cat("   • m2_ipw_coefs.csv (Education-extended IPW coefficients)\n")
cat("   • m3_coefs_edu_ipw.csv (Education-stratified IPW coefficients)\n")
cat("   • m4_ipw_gender_ncs_int.csv (Gender interaction coefficients)\n")
cat("   • m_lc_ipw_coefs.csv (Lifecycle analysis coefficients)\n\n")
cat("🔍 ANALYSIS FEATURES:\n")
cat("   • Quantile regression across 5 quantiles (10%, 25%, 50%, 75%, 90%)\n")
cat("   • Linear Quantile Mixed Models (LQMM) with random intercepts\n")
cat("   • Inverse Probability Weighting for causal inference\n")
cat("   • Gender and education heterogeneity analysis\n")
cat("   • Lifecycle perspective across age groups\n")
cat("   • Big Five personality traits as key regressors\n\n")
cat("🎯 KEY VARIABLES:\n")
cat("   • Dependent: log_wage (natural log of hourly wage)\n")
cat("   • NCS: O, C, E, A, ES (Big Five traits)\n")
cat("   • Controls: experience, area, sex, marital_status, region\n")
cat("   • Education: explicit education levels in extended models\n\n")
cat("🔄 NEXT STEPS:\n")
cat("   • Run 04_outputs_returns.R to generate publication tables and plots\n")
cat("   • All coefficient files ready for output generation\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")
