# ==============================================================================
# CHAPTER 5 - FIT LQMM REGRESSION MODELS (FIT + SAVE ONLY)
# ==============================================================================
#
# Strategy
# --------
# Every chapter-5 LQMM fit (5 quantiles × M1, M2, M3-tert, M3-voc, M3-sec;
# 3 quantiles × M4; 4 life-course median fits) and the supplementary GAM are
# enqueued as ONE flat list of jobs and dispatched through a single
# parallel::mclapply with mc.preschedule = FALSE. That gives:
#
#   * full core utilisation — all workers stay busy because slow jobs do
#     not block fast ones (e.g. q90 for M1 typically runs much longer than
#     q50 on the secondary-or-below subsample);
#   * one shared queue across models, so M2 can start before M1 finishes;
#   * a single fork point — workers each get one read-only copy of the
#     youth/life-course frames and write their fit straight to disk.
#
# The previous design ran 5 parallel quantile fits per model in 6 sequential
# model passes. With 7 workers it left ~2 cores idle most of the time and
# created a wait barrier between every model.
#
# Worker count: NCS_LQMM_WORKERS env var, otherwise detectCores(logical=TRUE)-1.
# Leaving one core free keeps the macOS UI/IDE responsive.
#
# Outputs (in 03_output/returns_outputs/thesis/, one .rds per fit):
#   m1_ipw_q{10,25,50,75,90}_model.rds, m2_ipw_q{...}_model.rds,
#   m3_tert_ipw_q{...}_model.rds, m3_voc_ipw_q{...}_model.rds,
#   m3_sec_ipw_q{...}_model.rds, m4_ipw_q{50,75,90}_model.rds,
#   m_lc_ipw_age_{16_65,30_39,40_49,50_65}_model.rds,
#   m_gam_age_model.rds
#
# Downstream:   04_summarize_models_returns.R reads these files, runs
#               summary() (also in parallel), and writes the
#               *_summaries.rds + *_coefs.csv files used by 05.
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 5 - FIT LQMM MODELS (parallel queue)\n")
cat("Script: 03_fit_models_returns.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

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
  verbose       = FALSE  # suppress per-iter chatter from parallel workers
)

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

# ----- Formulas ------------------------------------------------------------
general_formula  <- log_wage ~ exp_imp + I(exp_imp^2) + area + sex +
  marital_status + region + O + C + E + A + ES
formula_edu      <- log_wage ~ edu_lvl + exp_imp + I(exp_imp^2) + sex + area +
  region + marital_status + O + C + E + A + ES
formula_gend_int <- log_wage ~ exp_imp + I(exp_imp^2) + area + sex +
  marital_status + region + O + C + E + A + ES +
  O*sex + C*sex + E*sex + A*sex + ES*sex

# ----- Education-stratified subsamples -------------------------------------
# Precompute once so workers share them read-only via fork CoW.
dat_m3_tert <- youth_master_returns[youth_master_returns$edu_lvl == "4. Tertiary", ]
dat_m3_voc  <- youth_master_returns[youth_master_returns$edu_lvl == "3. Secondary Vocational", ]
dat_m3_sec  <- youth_master_returns[youth_master_returns$edu_lvl %in%
                                      c("1. No school", "2. Secondary School"), ]

# ----- Build the flat job queue --------------------------------------------
make_lqmm_job <- function(prefix, q_label, formula, data, weights, tau, control) {
  list(
    prefix    = prefix,
    q_label   = q_label,
    formula   = formula,
    data      = data,
    weights   = weights,
    tau       = tau,
    control   = control,
    out_file  = file.path(outputsReturnsNcsThesis,
                          paste0(prefix, "_", q_label, "_model.rds"))
  )
}

q5 <- list(q10 = 0.10, q25 = 0.25, q50 = 0.50, q75 = 0.75, q90 = 0.90)
q3 <- list(q50 = 0.50, q75 = 0.75, q90 = 0.90)

m1_overrides     <- list(q25 = ctrl1, q90 = ctrl1)
m2_overrides     <- list(q25 = ctrl1, q90 = ctrl1)
m3_tert_overrides <- list(q25 = ctrl1, q50 = ctrl1, q75 = ctrl1, q90 = ctrl1)
m3_voc_overrides  <- list(q25 = ctrl1, q50 = ctrl1, q90 = ctrl1)
m3_sec_overrides  <- list(q25 = ctrl1, q75 = ctrl1, q90 = ctrl1)
m4_overrides      <- list(q50 = ctrl1)

build_quantile_jobs <- function(prefix, formula, data, weights, taus, overrides) {
  Map(function(q_label, tau) {
    cont <- if (q_label %in% names(overrides)) overrides[[q_label]] else ctrl
    make_lqmm_job(prefix, q_label, formula, data, weights, tau, cont)
  }, names(taus), unname(taus))
}

lqmm_jobs <- c(
  build_quantile_jobs("m1_ipw",      general_formula,  youth_master_returns,
                      youth_master_returns$ipw_empl,           q5, m1_overrides),
  build_quantile_jobs("m2_ipw",      formula_edu,      youth_master_returns,
                      youth_master_returns$ipw_empl,           q5, m2_overrides),
  build_quantile_jobs("m3_tert_ipw", general_formula,  dat_m3_tert,
                      dat_m3_tert$ipw_empl,                    q5, m3_tert_overrides),
  build_quantile_jobs("m3_voc_ipw",  general_formula,  dat_m3_voc,
                      dat_m3_voc$ipw_empl,                     q5, m3_voc_overrides),
  build_quantile_jobs("m3_sec_ipw",  general_formula,  dat_m3_sec,
                      dat_m3_sec$ipw_empl,                     q5, m3_sec_overrides),
  build_quantile_jobs("m4_ipw",      formula_gend_int, youth_master_returns,
                      youth_master_returns$ipw_empl,           q3, m4_overrides),

  # Life-course median fits (4 jobs, each its own age window)
  list(
    make_lqmm_job("m_lc_ipw", "age_16_65", general_formula,
                  ind_master_returns,
                  ind_master_returns$ipw_empl,
                  0.50, list(method = "df")),
    make_lqmm_job("m_lc_ipw", "age_30_39", general_formula,
                  ind_master_returns[ind_master_returns$age >= 30 & ind_master_returns$age < 40, ],
                  ind_master_returns$ipw_empl[ind_master_returns$age >= 30 & ind_master_returns$age < 40],
                  0.50, ctrl1),
    make_lqmm_job("m_lc_ipw", "age_40_49", general_formula,
                  ind_master_returns[ind_master_returns$age >= 40 & ind_master_returns$age < 50, ],
                  ind_master_returns$ipw_empl[ind_master_returns$age >= 40 & ind_master_returns$age < 50],
                  0.50, list(method = "df")),
    make_lqmm_job("m_lc_ipw", "age_50_65", general_formula,
                  ind_master_returns[ind_master_returns$age >= 50, ],
                  ind_master_returns$ipw_empl[ind_master_returns$age >= 50],
                  0.50, list(method = "df"))
  )
)

cat("LQMM job queue length:", length(lqmm_jobs), "\n")
cat("  groups:",
    paste(unique(vapply(lqmm_jobs, function(j) j$prefix, character(1))),
          collapse = ", "), "\n\n")

# ----- Run all LQMM fits in one parallel queue -----------------------------
fit_one_job <- function(job) {
  t0 <- Sys.time()
  fit <- lqmm(
    fixed   = job$formula,
    data    = job$data,
    random  = ~ 1,
    group   = idind,
    tau     = job$tau,
    weights = job$weights,
    control = job$control
  )
  saveRDS(fit, job$out_file)
  list(
    prefix  = job$prefix,
    q_label = job$q_label,
    secs    = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
}

cat("🚀 Dispatching", length(lqmm_jobs), "LQMM fits across",
    lqmm_workers, "workers (mc.preschedule = TRUE; one fork per worker)\n\n")

queue_start <- Sys.time()

# Note: mc.preschedule = TRUE is correct here. See the matching comment in
# 04_summarize_models_returns.R — fork-per-task on macOS multiplies the
# per-fit memory churn and balloons wall-clock to >20h.
if (.Platform$OS.type != "windows" && lqmm_workers > 1L) {
  fit_results <- parallel::mclapply(
    lqmm_jobs,
    fit_one_job,
    mc.cores       = lqmm_workers,
    mc.preschedule = TRUE
  )
} else {
  fit_results <- lapply(lqmm_jobs, fit_one_job)
}

# Surface any worker errors (mclapply collects them rather than throwing)
errored <- vapply(fit_results, inherits, logical(1), what = "try-error")
if (any(errored)) {
  warning("LQMM jobs failed: ", sum(errored), " of ", length(fit_results),
          " — inspect fit_results for details.")
} else {
  cat("\n✅ All LQMM fits completed.\n")
  for (r in fit_results) {
    cat(sprintf("    %-15s %-12s %7.1f s\n", r$prefix, r$q_label, r$secs))
  }
}

queue_total <- difftime(Sys.time(), queue_start, units = "mins")
cat(sprintf("\n⏱  LQMM queue wall-clock: %.2f min\n\n", as.numeric(queue_total)))

# ----- Supplementary GAM (single-threaded; cheap) --------------------------
cat("📈 GAM (age, supplementary figure)\n")
t0 <- Sys.time()
m_gam <- gam(log_wage ~ s(age) + sex + region + edu_lvl + area + marital_status,
             data = ind_master_returns)
saveRDS(m_gam, file.path(outputsReturnsNcsThesis, "m_gam_age_model.rds"))
cat("⏱", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- COMPLETION ----------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 5 fits complete in", round(total_time, 2), "min total\n")
cat("   → run 04_summarize_models_returns.R next to build summary RDS + CSVs\n")
cat(rep("=", 80), "\n\n")
