# ==============================================================================
# CHAPTER 5 - FIT LQMM REGRESSION MODELS (FIT + SAVE ONLY)
# ==============================================================================
#
# File:         03_fit_models_returns.R
# Purpose:      Fit every LQMM specification used in chapter 5 and save the
#               raw fitted model objects to 03_output/returns_outputs/thesis/.
#               Running summary() on these models is expensive (bootstrap),
#               so it is done separately in 04_summarize_models_returns.R.
#
# Sample inputs (from 01_data_prep_returns.R, in 03_output/returns_outputs/):
#   youth_master_returns.rds  - youth (16-29), military removed, NCS-complete
#   ind_master_returns.rds    - life-course (16-65), military removed
#
# Models fitted:
#   M1_IPW    baseline LQMM, 5 quantiles (10/25/50/75/90)
#   M2_IPW    education-extended (explicit edu_lvl), 5 quantiles
#   M3_*_IPW  education-stratified (Tertiary / Vocational / Secondary or
#             below), 5 quantiles each
#   M4_IPW    sex × NCS interaction, 3 quantiles (50/75/90)
#   M_LC_*    life-course median models for ages 16-65, 30-39, 40-49, 50-65
#   m_gam     supplementary GAM s(age) for figure
#
# Output files (one .rds per fit):
#   m1_ipw_q{10,25,50,75,90}_model.rds
#   m2_ipw_q{10,25,50,75,90}_model.rds
#   m3_tert_ipw_q{...}_model.rds, m3_voc_ipw_q{...}_model.rds,
#   m3_sec_ipw_q{...}_model.rds
#   m4_ipw_q{50,75,90}_model.rds
#   m_lc_ipw_age_{16_65,30_39,40_49,50_65}_model.rds
#   m_gam_age_model.rds
#
# Runtime:      ~5 hours total (LQMM is expensive). Workers can be set via
#               the NCS_LQMM_WORKERS env var; default is detectCores()-1.
#
# Downstream:   04_summarize_models_returns.R reads these files, runs
#               summary() + coefficient extraction, and writes the
#               *_summaries.rds and *_coefs.csv files that
#               05_output_returns_rus.R consumes.
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 5 - FIT LQMM MODELS\n")
cat("Script: 03_fit_models_returns.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# Output directory for the fitted-model RDS files
outputsReturnsNcsThesis <- file.path(outputsReturnsNcs, "thesis")
dir.create(outputsReturnsNcsThesis, recursive = TRUE, showWarnings = FALSE)

# ----- Load samples --------------------------------------------------------
youth_master_returns <-
  readRDS(file.path(outputsReturnsNcs, "youth_master_returns.rds"))
ind_master_returns <-
  readRDS(file.path(outputsReturnsNcs, "ind_master_returns.rds"))

cat("youth_master_returns:", nrow(youth_master_returns), "rows\n")
cat("ind_master_returns:  ", nrow(ind_master_returns), "rows\n\n")

# ----- LQMM controls -------------------------------------------------------
ctrl <- lqmmControl(
  method        = "df",
  LP_max_iter   = 3000,
  UP_max_iter   = 3000,
  LP_tol_ll     = 1e-5,
  UP_tol        = 1e-6
)

ctrl1 <- lqmmControl(
  method        = "df",
  LP_max_iter   = 5000,
  UP_max_iter   = 100,
  LP_tol_ll     = 1e-5,
  LP_tol_theta  = 1e-5,
  UP_tol        = 1e-5,
  check_theta   = TRUE,
  startQR       = TRUE,
  verbose       = TRUE
)

quantile_map <- c(q10 = 0.10, q25 = 0.25, q50 = 0.50, q75 = 0.75, q90 = 0.90)

# ----- Worker count for parallel fits --------------------------------------
lqmm_workers <- suppressWarnings(as.integer(Sys.getenv("NCS_LQMM_WORKERS", unset = NA)))
if (is.na(lqmm_workers)) {
  detected_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(detected_cores)) detected_cores <- 2L
  # Keep one core free — each LQMM fit is memory-heavy
  lqmm_workers <- max(1L, min(length(quantile_map), detected_cores - 1L))
}
cat("LQMM worker processes:", lqmm_workers, "\n\n")

# ----- Helpers: fit a set of quantiles, save raw model objects -------------
fit_lqmm_by_quantile <- function(fixed_formula, data_frame, weight_vector,
                                 tau_values, control_obj, control_overrides = NULL) {
  tau_labels <- names(tau_values)

  fit_one_quantile <- function(idx) {
    tau_label   <- tau_labels[[idx]]
    tau_value   <- as.numeric(tau_values[[idx]])
    control_use <- control_obj
    if (!is.null(control_overrides) && tau_label %in% names(control_overrides)) {
      control_use <- control_overrides[[tau_label]]
    }
    lqmm(fixed   = fixed_formula,
         data    = data_frame,
         random  = ~ 1,
         group   = idind,
         tau     = tau_value,
         weights = weight_vector,
         control = control_use)
  }

  if (.Platform$OS.type != "windows" && lqmm_workers > 1L) {
    model_list <- parallel::mclapply(
      X       = seq_along(tau_values),
      FUN     = fit_one_quantile,
      mc.cores = min(lqmm_workers, length(tau_values))
    )
  } else {
    model_list <- lapply(seq_along(tau_values), fit_one_quantile)
  }

  rlang::set_names(model_list, tau_labels)
}

save_lqmm_model_objects <- function(model_list, file_prefix, output_dir) {
  purrr::iwalk(model_list, function(model_obj, model_label) {
    saveRDS(model_obj,
            file.path(output_dir, paste0(file_prefix, "_", model_label, "_model.rds")))
  })
  cat("✅ saved", length(model_list), "model file(s) with prefix", file_prefix, "\n")
}

# ----- Formulas -----------------------------------------------------------
general_formula <- log_wage ~ exp_imp + I(exp_imp^2) + area + sex +
  marital_status + region + O + C + E + A + ES

formula_edu <- log_wage ~ edu_lvl + exp_imp + I(exp_imp^2) + sex + area +
  region + marital_status + O + C + E + A + ES

formula_gend_int <- log_wage ~ exp_imp + I(exp_imp^2) + area + sex +
  marital_status + region + O + C + E + A + ES +
  O*sex + C*sex + E*sex + A*sex + ES*sex

# ----- M1: BASELINE (5 quantiles, IPW) ------------------------------------
cat("📊 M1_IPW (baseline, 5 quantiles)\n")
t0 <- Sys.time()
m1_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula     = general_formula,
  data_frame        = youth_master_returns,
  weight_vector     = youth_master_returns$ipw_empl,
  tau_values        = quantile_map,
  control_obj       = ctrl,
  control_overrides = list(q25 = ctrl1, q90 = ctrl1)
)
save_lqmm_model_objects(m1_ipw_models, "m1_ipw", outputsReturnsNcsThesis)
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- M2: EDUCATION-EXTENDED (5 quantiles, IPW) --------------------------
cat("🎓 M2_IPW (education-extended, 5 quantiles)\n")
t0 <- Sys.time()
m2_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula     = formula_edu,
  data_frame        = youth_master_returns,
  weight_vector     = youth_master_returns$ipw_empl,
  tau_values        = quantile_map,
  control_obj       = ctrl,
  control_overrides = list(q25 = ctrl1, q90 = ctrl1)
)
save_lqmm_model_objects(m2_ipw_models, "m2_ipw", outputsReturnsNcsThesis)
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- M3: EDUCATION-STRATIFIED (3 subsets × 5 quantiles) -----------------
cat("🎓 M3_IPW (education-stratified, 3 subsets × 5 quantiles)\n")
t0 <- Sys.time()

dat_m3_tert <- youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ]
m3_tert_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula     = general_formula,
  data_frame        = dat_m3_tert,
  weight_vector     = dat_m3_tert$ipw_empl,
  tau_values        = quantile_map,
  control_obj       = ctrl,
  control_overrides = list(q25 = ctrl1, q50 = ctrl1, q75 = ctrl1, q90 = ctrl1)
)
save_lqmm_model_objects(m3_tert_ipw_models, "m3_tert_ipw", outputsReturnsNcsThesis)

dat_m3_voc <- youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ]
m3_voc_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula     = general_formula,
  data_frame        = dat_m3_voc,
  weight_vector     = dat_m3_voc$ipw_empl,
  tau_values        = quantile_map,
  control_obj       = ctrl,
  control_overrides = list(q25 = ctrl1, q50 = ctrl1, q90 = ctrl1)
)
save_lqmm_model_objects(m3_voc_ipw_models, "m3_voc_ipw", outputsReturnsNcsThesis)

dat_m3_sec <- youth_master_returns[youth_master_returns$edu_lvl %in%
                                     c("1. No school", "2. Secondary School"), ]
m3_sec_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula     = general_formula,
  data_frame        = dat_m3_sec,
  weight_vector     = dat_m3_sec$ipw_empl,
  tau_values        = quantile_map,
  control_obj       = ctrl,
  control_overrides = list(q25 = ctrl1, q75 = ctrl1, q90 = ctrl1)
)
save_lqmm_model_objects(m3_sec_ipw_models, "m3_sec_ipw", outputsReturnsNcsThesis)

cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- M4: SEX × NCS INTERACTION (3 quantiles, IPW) -----------------------
cat("👫 M4_IPW (sex × NCS interaction, 3 quantiles)\n")
t0 <- Sys.time()
m4_quantile_map <- c(q50 = 0.50, q75 = 0.75, q90 = 0.90)
m4_ipw_models <- fit_lqmm_by_quantile(
  fixed_formula     = formula_gend_int,
  data_frame        = youth_master_returns,
  weight_vector     = youth_master_returns$ipw_empl,
  tau_values        = m4_quantile_map,
  control_obj       = ctrl,
  control_overrides = list(q50 = ctrl1)
)
save_lqmm_model_objects(m4_ipw_models, "m4_ipw", outputsReturnsNcsThesis)
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- LIFE-COURSE: median quantile by age group --------------------------
cat("👴 Life-course (median quantile, 4 age groups)\n")
t0 <- Sys.time()

m_lc <- lqmm(general_formula,
             data    = ind_master_returns,
             random  = ~ 1, group = idind,
             tau     = 0.50,
             weights = ind_master_returns$ipw_empl,
             control = list(method = "df"))

m_lc_3039 <- lqmm(general_formula,
                  data    = ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ],
                  random  = ~ 1, group = idind,
                  tau     = 0.50,
                  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 30 & ind_master_returns$age < 40],
                  control = ctrl1)

m_lc_4049 <- lqmm(general_formula,
                  data    = ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ],
                  random  = ~ 1, group = idind,
                  tau     = 0.50,
                  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 40 & ind_master_returns$age < 50],
                  control = list(method = "df"))

m_lc_5065 <- lqmm(general_formula,
                  data    = ind_master_returns[ind_master_returns$age >= 50, ],
                  random  = ~ 1, group = idind,
                  tau     = 0.50,
                  weights = ind_master_returns$ipw_empl[ind_master_returns$age >= 50],
                  control = list(method = "df"))

m_lc_models <- list(
  age_16_65 = m_lc,
  age_30_39 = m_lc_3039,
  age_40_49 = m_lc_4049,
  age_50_65 = m_lc_5065
)
save_lqmm_model_objects(m_lc_models, "m_lc_ipw", outputsReturnsNcsThesis)
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- Supplementary GAM (age effects) ------------------------------------
cat("📈 GAM (age, supplementary figure)\n")
t0 <- Sys.time()
m_gam <- gam(log_wage ~ s(age) + sex + region + edu_lvl + area + marital_status,
             data = ind_master_returns)
saveRDS(m_gam, file.path(outputsReturnsNcsThesis, "m_gam_age_model.rds"))
cat("⏱", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- COMPLETION ---------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 5 fits complete in", round(total_time, 2), "min\n")
cat("   → run 04_summarize_models_returns.R next to build summary RDS + CSVs\n")
cat(rep("=", 80), "\n\n")
