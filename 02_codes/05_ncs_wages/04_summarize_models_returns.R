# ==============================================================================
# CHAPTER 5 - SUMMARIZE LQMM MODELS (LOAD + summary() + COEFFICIENT EXPORT)
# ==============================================================================
#
# Strategy
# --------
# summary() on an lqmm fit triggers a bootstrap variance estimation that is
# usually the second-most expensive step in chapter 5. The previous design
# called summary() five at a time (one model at a time) — the same
# "5-quantile-batch then move on" pattern that limits 03_fit_models_returns.R.
#
# Here we enqueue every per-quantile summary() job into one flat list and
# run them through a single parallel::mclapply with mc.preschedule = FALSE,
# so all worker cores stay busy across model groups. After the queue
# completes we group results back by model and write the
# *_summaries.rds + *_coefs.csv files that 05_output_returns_rus.R consumes.
#
# Inputs (in 03_output/returns_outputs/thesis/):
#   m1_ipw_q*_model.rds, m2_ipw_q*_model.rds, m3_tert_ipw_q*_model.rds,
#   m3_voc_ipw_q*_model.rds, m3_sec_ipw_q*_model.rds, m4_ipw_q*_model.rds,
#   m_lc_ipw_age_*_model.rds
#
# Outputs (same directory):
#   m1_ipw_summaries.rds, m1_ipw_coefs.csv
#   m2_ipw_summaries.rds, m2_ipw_coefs.csv
#   m3_edu_ipw_summaries.rds, m3_coefs_edu_ipw.csv
#   m4_ipw_summaries.rds, m4_ipw_sex_ncs_int.csv
#   m_lc_ipw_summaries.rds, m_lc_ipw_coefs.csv
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 5 - SUMMARIZE LQMM MODELS (parallel queue)\n")
cat("Script: 04_summarize_models_returns.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

outputsReturnsNcsThesis <- file.path(outputsReturnsNcs, "thesis")

# ----- Worker count --------------------------------------------------------
lqmm_workers <- suppressWarnings(as.integer(Sys.getenv("NCS_LQMM_WORKERS",
                                                        unset = NA)))
if (is.na(lqmm_workers)) {
  detected_cores <- parallel::detectCores(logical = TRUE)
  if (is.na(detected_cores)) detected_cores <- 2L
  lqmm_workers <- max(1L, detected_cores - 1L)
}
cat("Worker processes:", lqmm_workers,
    "(of", parallel::detectCores(logical = TRUE), "logical cores)\n\n")

# ----- Job queue: one tuple (group, q_label, file) per saved fit ----------
build_summary_jobs <- function(group, q_labels) {
  lapply(q_labels, function(q) {
    list(group   = group,
         q_label = q,
         file    = file.path(outputsReturnsNcsThesis,
                             paste0(group, "_", q, "_model.rds")))
  })
}

q5 <- c("q10", "q25", "q50", "q75", "q90")
q3 <- c("q50", "q75", "q90")

summary_jobs <- c(
  build_summary_jobs("m1_ipw",      q5),
  build_summary_jobs("m2_ipw",      q5),
  build_summary_jobs("m3_tert_ipw", q5),
  build_summary_jobs("m3_voc_ipw",  q5),
  build_summary_jobs("m3_sec_ipw",  q5),
  build_summary_jobs("m4_ipw",      q3),
  list(
    list(group = "m_lc_ipw", q_label = "age_16_65",
         file = file.path(outputsReturnsNcsThesis, "m_lc_ipw_age_16_65_model.rds")),
    list(group = "m_lc_ipw", q_label = "age_30_39",
         file = file.path(outputsReturnsNcsThesis, "m_lc_ipw_age_30_39_model.rds")),
    list(group = "m_lc_ipw", q_label = "age_40_49",
         file = file.path(outputsReturnsNcsThesis, "m_lc_ipw_age_40_49_model.rds")),
    list(group = "m_lc_ipw", q_label = "age_50_65",
         file = file.path(outputsReturnsNcsThesis, "m_lc_ipw_age_50_65_model.rds"))
  )
)

# Sanity-check that every input file exists before forking
missing <- vapply(summary_jobs, function(j) !file.exists(j$file), logical(1))
if (any(missing)) {
  stop("Missing fit files (run 03_fit_models_returns.R first):\n  ",
       paste(vapply(summary_jobs[missing], `[[`, character(1), "file"),
             collapse = "\n  "))
}

cat("Summary job queue length:", length(summary_jobs), "\n")
cat("  groups:",
    paste(unique(vapply(summary_jobs, function(j) j$group, character(1))),
          collapse = ", "), "\n\n")

# ----- Run all summary() calls in one parallel queue -----------------------
summarize_one_job <- function(job) {
  t0    <- Sys.time()
  model <- readRDS(job$file)
  s     <- summary(model)
  list(
    group   = job$group,
    q_label = job$q_label,
    summary = s,
    secs    = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

cat("🚀 Dispatching", length(summary_jobs), "summary() calls across",
    lqmm_workers, "workers (mc.preschedule = FALSE)\n\n")

queue_start <- Sys.time()

if (.Platform$OS.type != "windows" && lqmm_workers > 1L) {
  summary_results <- parallel::mclapply(
    summary_jobs,
    summarize_one_job,
    mc.cores       = lqmm_workers,
    mc.preschedule = FALSE
  )
} else {
  summary_results <- lapply(summary_jobs, summarize_one_job)
}

errored <- vapply(summary_results, inherits, logical(1), what = "try-error")
if (any(errored)) {
  stop("summary() failed: ", sum(errored), " of ", length(summary_results),
       " — inspect summary_results.")
}

cat("\n✅ All summaries completed.\n")
for (r in summary_results) {
  cat(sprintf("    %-15s %-12s %7.1f s\n", r$group, r$q_label, r$secs))
}
queue_total <- difftime(Sys.time(), queue_start, units = "mins")
cat(sprintf("\n⏱  Summary queue wall-clock: %.2f min\n\n",
            as.numeric(queue_total)))

# ----- Group summaries back by model, save bundles + coef CSVs ------------
group_summaries <- function(results, group_name) {
  grp <- Filter(function(r) r$group == group_name, results)
  out <- lapply(grp, `[[`, "summary")
  rlang::set_names(out, vapply(grp, `[[`, character(1), "q_label"))
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

# ----- M1_IPW -------------------------------------------------------------
m1_ipw_summaries <- group_summaries(summary_results, "m1_ipw")
saveRDS(m1_ipw_summaries, file.path(outputsReturnsNcsThesis, "m1_ipw_summaries.rds"))

m1_ipw_coefs <- extract_quantile_coefs(m1_ipw_summaries)
names(m1_ipw_coefs) <- coef_names_5q
write_csv(m1_ipw_coefs, file.path(outputsReturnsNcsThesis, "m1_ipw_coefs.csv"))

# ----- M2_IPW -------------------------------------------------------------
m2_ipw_summaries <- group_summaries(summary_results, "m2_ipw")
saveRDS(m2_ipw_summaries, file.path(outputsReturnsNcsThesis, "m2_ipw_summaries.rds"))

m2_ipw_coefs <- extract_quantile_coefs(m2_ipw_summaries)
names(m2_ipw_coefs) <- coef_names_5q
write_csv(m2_ipw_coefs, file.path(outputsReturnsNcsThesis, "m2_ipw_coefs.csv"))

# ----- M3 (education-stratified) ------------------------------------------
m3_tert_summary <- group_summaries(summary_results, "m3_tert_ipw")
m3_voc_summary  <- group_summaries(summary_results, "m3_voc_ipw")
m3_sec_summary  <- group_summaries(summary_results, "m3_sec_ipw")

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

# ----- M4_IPW (sex × NCS) -------------------------------------------------
m4_ipw_summaries <- group_summaries(summary_results, "m4_ipw")
saveRDS(m4_ipw_summaries, file.path(outputsReturnsNcsThesis, "m4_ipw_summaries.rds"))
m4_ipw_coefs <- extract_quantile_coefs(m4_ipw_summaries)
names(m4_ipw_coefs) <- coef_names_3q
write_csv(m4_ipw_coefs, file.path(outputsReturnsNcsThesis, "m4_ipw_sex_ncs_int.csv"))

# ----- LIFE-COURSE summaries ----------------------------------------------
m_lc_ipw_summaries <- group_summaries(summary_results, "m_lc_ipw")
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

age_labels <- c(age_16_65 = "16-65",
                age_30_39 = "30-40",
                age_40_49 = "40-50",
                age_50_65 = "50-65")
m_lc_long <- do.call(
  rbind,
  Map(to_coefs,
      m_lc_ipw_summaries[names(age_labels)],
      unname(age_labels))
)
write_csv(m_lc_long, file.path(outputsReturnsNcsThesis, "m_lc_ipw_coefs.csv"))

# ----- COMPLETION ---------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 5 summaries complete in", round(total_time, 2), "min\n")
cat("   → 05_output_returns_rus.R now has all the *_summaries.rds /\n")
cat("     *_coefs.csv files it expects.\n")
cat(rep("=", 80), "\n\n")
