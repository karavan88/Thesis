# ==============================================================================
# CHAPTER 5 - SUMMARIZE LQMM MODELS (LOAD + summary() + COEFFICIENT EXPORT)
# ==============================================================================
#
# File:         04_summarize_models_returns.R
# Purpose:      Read every raw LQMM fit produced by 03_fit_models_returns.R,
#               run summary() on each (this is also expensive — bootstrap
#               variance), and save:
#                  *_summaries.rds  - lists of lqmm summary objects
#                  *_coefs.csv      - long/wide coefficient tables for plots
#
# Inputs (in 03_output/returns_outputs/thesis/):
#   m1_ipw_q*_model.rds, m2_ipw_q*_model.rds
#   m3_tert_ipw_q*_model.rds, m3_voc_ipw_q*_model.rds, m3_sec_ipw_q*_model.rds
#   m4_ipw_q*_model.rds
#   m_lc_ipw_age_*_model.rds
#
# Outputs (same directory):
#   m1_ipw_summaries.rds, m1_ipw_coefs.csv
#   m2_ipw_summaries.rds, m2_ipw_coefs.csv
#   m3_edu_ipw_summaries.rds, m3_coefs_edu_ipw.csv
#   m4_ipw_summaries.rds, m4_ipw_sex_ncs_int.csv
#   m_lc_ipw_summaries.rds, m_lc_ipw_coefs.csv
#
# Downstream:   05_output_returns_rus.R reads these files (sourced by qmds).
#
# Runtime:      ~30-60 minutes depending on quantile count and bootstrap.
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 5 - SUMMARIZE LQMM MODELS\n")
cat("Script: 04_summarize_models_returns.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

outputsReturnsNcsThesis <- file.path(outputsReturnsNcs, "thesis")

# ----- Worker count for parallel summary() calls --------------------------
lqmm_workers <- suppressWarnings(as.integer(Sys.getenv("NCS_LQMM_WORKERS", unset = NA)))
if (is.na(lqmm_workers)) {
  detected_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(detected_cores)) detected_cores <- 2L
  lqmm_workers <- max(1L, detected_cores - 1L)
}

# ----- Helpers ------------------------------------------------------------
load_quantile_models <- function(file_prefix, output_dir, quantiles) {
  out <- lapply(quantiles, function(q) {
    f <- file.path(output_dir, paste0(file_prefix, "_", q, "_model.rds"))
    if (!file.exists(f)) stop("Missing model file: ", f)
    readRDS(f)
  })
  rlang::set_names(out, quantiles)
}

summarize_lqmm_models <- function(model_list) {
  model_labels <- names(model_list)
  summarize_one <- function(idx) summary(model_list[[idx]])

  if (.Platform$OS.type != "windows" && lqmm_workers > 1L) {
    summary_list <- parallel::mclapply(
      X       = seq_along(model_list),
      FUN     = summarize_one,
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

    coef_tbl <- coef_tbl %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    if (!keep_region) {
      coef_tbl <- coef_tbl %>% filter(!str_detect(variable, "region"))
    }

    coef_tbl %>%
      rename_with(~ paste0(q_label, "_", .x), -variable)
  }) %>%
    purrr::reduce(full_join, by = "variable")
}

# Column-name pattern for 5-quantile coef tables
coef_names_5q <- c(
  "variable",
  "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
  "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
  "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
  "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
  "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value"
)
coef_names_3q <- c(
  "variable",
  "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
  "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
  "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value"
)

q5  <- c("q10", "q25", "q50", "q75", "q90")
q3  <- c("q50", "q75", "q90")

# ----- M1_IPW -------------------------------------------------------------
cat("📊 M1_IPW summaries\n")
t0 <- Sys.time()
m1_ipw_models    <- load_quantile_models("m1_ipw", outputsReturnsNcsThesis, q5)
m1_ipw_summaries <- summarize_lqmm_models(m1_ipw_models)
saveRDS(m1_ipw_summaries, file.path(outputsReturnsNcsThesis, "m1_ipw_summaries.rds"))

m1_ipw_coefs <- extract_quantile_coefs(m1_ipw_summaries)
names(m1_ipw_coefs) <- coef_names_5q
write_csv(m1_ipw_coefs, file.path(outputsReturnsNcsThesis, "m1_ipw_coefs.csv"))
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- M2_IPW -------------------------------------------------------------
cat("🎓 M2_IPW summaries\n")
t0 <- Sys.time()
m2_ipw_models    <- load_quantile_models("m2_ipw", outputsReturnsNcsThesis, q5)
m2_ipw_summaries <- summarize_lqmm_models(m2_ipw_models)
saveRDS(m2_ipw_summaries, file.path(outputsReturnsNcsThesis, "m2_ipw_summaries.rds"))

m2_ipw_coefs <- extract_quantile_coefs(m2_ipw_summaries)
names(m2_ipw_coefs) <- coef_names_5q
write_csv(m2_ipw_coefs, file.path(outputsReturnsNcsThesis, "m2_ipw_coefs.csv"))
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- M3_IPW (education-stratified) --------------------------------------
cat("🎓 M3_IPW summaries (Tertiary / Vocational / Secondary or below)\n")
t0 <- Sys.time()
m3_tert_models  <- load_quantile_models("m3_tert_ipw", outputsReturnsNcsThesis, q5)
m3_voc_models   <- load_quantile_models("m3_voc_ipw",  outputsReturnsNcsThesis, q5)
m3_sec_models   <- load_quantile_models("m3_sec_ipw",  outputsReturnsNcsThesis, q5)

m3_tert_summary <- summarize_lqmm_models(m3_tert_models)
m3_voc_summary  <- summarize_lqmm_models(m3_voc_models)
m3_sec_summary  <- summarize_lqmm_models(m3_sec_models)

m3_edu_ipw_summaries <- list(
  Tertiary             = m3_tert_summary,
  Secondary_Vocational = m3_voc_summary,
  Secondary_or_below   = m3_sec_summary
)
saveRDS(m3_edu_ipw_summaries,
        file.path(outputsReturnsNcsThesis, "m3_edu_ipw_summaries.rds"))

m3_tert_coefs <- extract_quantile_coefs(m3_tert_summary, keep_region = TRUE) %>%
  mutate(model = "Tertiary")
m3_voc_coefs  <- extract_quantile_coefs(m3_voc_summary,  keep_region = TRUE) %>%
  mutate(model = "Secondary Vocational")
m3_sec_coefs  <- extract_quantile_coefs(m3_sec_summary,  keep_region = TRUE) %>%
  mutate(model = "Secondary or below")

m3_coefs_ipw <- bind_rows(m3_tert_coefs, m3_voc_coefs, m3_sec_coefs)
write_csv(m3_coefs_ipw, file.path(outputsReturnsNcsThesis, "m3_coefs_edu_ipw.csv"))
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- M4_IPW (sex × NCS) -------------------------------------------------
cat("👫 M4_IPW summaries\n")
t0 <- Sys.time()
m4_ipw_models    <- load_quantile_models("m4_ipw", outputsReturnsNcsThesis, q3)
m4_ipw_summaries <- summarize_lqmm_models(m4_ipw_models)
saveRDS(m4_ipw_summaries, file.path(outputsReturnsNcsThesis, "m4_ipw_summaries.rds"))

m4_ipw_coefs <- extract_quantile_coefs(m4_ipw_summaries)
names(m4_ipw_coefs) <- coef_names_3q
write.csv(m4_ipw_coefs, file.path(outputsReturnsNcsThesis, "m4_ipw_sex_ncs_int.csv"))
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- LIFE-COURSE summaries ----------------------------------------------
cat("👴 Life-course summaries\n")
t0 <- Sys.time()

age_groups <- c("age_16_65", "age_30_39", "age_40_49", "age_50_65")
m_lc_models <- lapply(age_groups, function(g) {
  readRDS(file.path(outputsReturnsNcsThesis, paste0("m_lc_ipw_", g, "_model.rds")))
})
names(m_lc_models) <- age_groups

m_lc_ipw_summaries <- lapply(m_lc_models, summary)
saveRDS(m_lc_ipw_summaries,
        file.path(outputsReturnsNcsThesis, "m_lc_ipw_summaries.rds"))

# Long-form coefficient table: one row per (variable, age_group)
to_coefs <- function(s, age_label) {
  s$tTable %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    filter(!str_detect(variable, "region")) %>%
    mutate_if(is.numeric, round, 3) %>%
    mutate(age_group = age_label)
}

age_labels <- c("16-65", "30-40", "40-50", "50-65")
m_lc_long <- do.call(
  rbind,
  Map(to_coefs, m_lc_ipw_summaries, age_labels)
)
write_csv(m_lc_long, file.path(outputsReturnsNcsThesis, "m_lc_ipw_coefs.csv"))
cat("⏱", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n\n")

# ----- COMPLETION ---------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 5 summaries complete in", round(total_time, 2), "min\n")
cat("   → 05_output_returns_rus.R now has all the *_summaries.rds /\n")
cat("     *_coefs.csv files it expects.\n")
cat(rep("=", 80), "\n\n")
