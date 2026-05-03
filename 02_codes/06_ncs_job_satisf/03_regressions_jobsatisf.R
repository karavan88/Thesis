# ==============================================================================
# CHAPTER 6 - JOB SATISFACTION MODELS (FIT + SAVE)
# ==============================================================================
#
# File:         03_regressions_jobsatisf.R
# Purpose:      Fit chapter-6 mixed-effects job-satisfaction models on the
#               youth_job_satisf data frame produced by 01_data_prep_jobsatisf.R,
#               and save them to 03_output/jobsatisf_outputs/.
#
# Models fitted:
#   gam_job_satisf - GAM, satisf_job ~ s(log hourly_wage) + controls
#   m0_satisf      - intercept-only with random effects (idind, region,
#                    occupation, hourly_wage_quintile)
#   m1_satisf      - reference model: demographics + log wage + work hours
#   m1.5_satisf    - NCS-only model
#   m2_satisf      - full model: demographics + wages + NCS
#   m3_satisf      - random slopes of NCS by hourly_wage_quintile
#   m4_satisf      - career satisfaction
#   m5_satisf      - working-conditions satisfaction
#   m6_satisf      - wage satisfaction
#   m7_satisf      - work-life-balance satisfaction (limited sample)
#
# Output:       03_output/jobsatisf_outputs/jobsatisf_models.rds (one bundle)
#
# Cache logic:  if the bundle already exists, the script skips the heavy fit
#               and loads it back; set options(thesis.refit = TRUE) to force
#               a refit, or simply delete the .rds.
#
# Downstream:   04_outputs_jobsatisf.R reads the bundle and builds every
#               derived object (m3_satisf_coefs, models_satisf, modelsummary
#               tables, plot_wages_ncs, scalar summaries). The qmd sources 04,
#               not 03 — so this fit script only runs when explicitly invoked.
#
# Dependencies: lme4, lmerTest, mgcv, easystats (icc), tidyverse
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 6 - JOB SATISFACTION MODELS\n")
cat("Script: 03_regressions_jobsatisf.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# ----- Cache --------------------------------------------------------------
jobsatisf_cache_path <- file.path(outputsJobSatisfNcs, "jobsatisf_models.rds")
.refit_jobsatisf     <- isTRUE(getOption("thesis.refit", FALSE))
.jobsatisf_cache_hit <- !.refit_jobsatisf && file.exists(jobsatisf_cache_path)

# Reorder edu_lvl with Tertiary as reference (matches the design used by
# the cached models). Runs unconditionally so the data frame stays consistent.
edu_lvl_levels <- c("4. Tertiary", "1. No school",
                    "2. Secondary School", "3. Secondary Vocational")
youth_job_satisf$edu_lvl <- factor(youth_job_satisf$edu_lvl, levels = edu_lvl_levels)

if (.jobsatisf_cache_hit) {
  cat("📦 Cache hit. Loading models from:\n   ", jobsatisf_cache_path, "\n")
  .cached <- readRDS(jobsatisf_cache_path)
  for (.nm in names(.cached)) assign(.nm, .cached[[.nm]], envir = globalenv())
  rm(.cached)
} else {

# ----- SECTION 1: EXPLORATORY GAM -----------------------------------------
cat("📈 SECTION 1: GAM (s(log hourly_wage) + controls)\n")
t0 <- Sys.time()

gam_data <- youth_job_satisf %>%
  mutate(
    region     = as.factor(region),
    occupation = as.factor(occupation),
    year       = as.factor(year)
  )

gam_job_satisf <- gam(
  satisf_job ~
    s(log(hourly_wage)) +
    age + I(age^2) + sex + edu_lvl + area + work_hrs_per_week +
    s(region, bs = "re") +
    s(year, bs = "re") +
    s(occupation, bs = "re"),
  data   = gam_data,
  family = binomial,
  method = "REML"
)

cat("✅ GAM fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- SECTION 2: M0 (intercept-only) -------------------------------------
cat("🏗 SECTION 2: M0 (intercept-only, multi-RE)\n")
t0 <- Sys.time()

m0_satisf <- lmer(satisf_job ~ 1 +
                    (1 | idind) + (1 | region) + (1 | occupation) +
                    (1 | hourly_wage_quintile),
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl   = list(maxfun = 1e6)),
                  data = youth_job_satisf)


cat("✅ M0 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- SECTION 3: M1 / M1.5 / M2 (overall job-satisfaction main models) ---
cat("📊 SECTION 3: M1, M1.5, M2 (main models)\n")
t0 <- Sys.time()

m1_satisf <- lmer(satisf_job ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    log(wages_imp) + work_hrs_per_week +
                    (1 | idind) + (1 | region) + (1 | occupation),
                  weights = ipw_empl, data = youth_job_satisf)

m1.5_satisf <- lmer(satisf_job ~ 1 +
                      O + C + E + A + ES +
                      (1 | idind) + (1 | region) + (1 | occupation),
                    weights = ipw_empl, data = youth_job_satisf)

m2_satisf <- lmer(satisf_job ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    log(wages_imp) + work_hrs_per_week +
                    O + C + E + A + ES +
                    (1 | idind) + (1 | region) + (1 | occupation),
                  weights = ipw_empl, data = youth_job_satisf)

cat("✅ M1, M1.5, M2 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- SECTION 4: M3 (random slopes by wage quintile) ---------------------
cat("📈 SECTION 4: M3 (NCS random slopes by wage quintile)\n")
t0 <- Sys.time()

m3_satisf <- lmer(satisf_job ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    work_hrs_per_week +
                    O + C + E + A + ES +
                    (1 | idind) + (1 | region) + (1 | occupation) +
                    (O + C + E + A + ES | hourly_wage_quintile),
                  weights = ipw_empl, data = youth_job_satisf)

cat("✅ M3 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- SECTION 5: M4-M7 (domain-specific models) --------------------------
cat("🎯 SECTION 5: M4-M7 (domain-specific satisfaction)\n")
t0 <- Sys.time()

m4_satisf <- lmer(satisf_career ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    log(wages_imp) + work_hrs_per_week +
                    O + C + E + A + ES +
                    (1 | idind) + (1 | region) + (1 | occupation),
                  weights = ipw_empl, data = youth_job_satisf)

m5_satisf <- lmer(satisf_labor_cond ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    log(wages_imp) + work_hrs_per_week +
                    O + C + E + A + ES +
                    (1 | idind) + (1 | region) + (1 | occupation),
                  weights = ipw_empl, data = youth_job_satisf)

m6_satisf <- lmer(satisf_wage ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    log(wages_imp) + work_hrs_per_week +
                    O + C + E + A + ES +
                    (1 | idind) + (1 | region) + (1 | occupation),
                  weights = ipw_empl, data = youth_job_satisf)

m7_satisf <- lmer(satisf_wbl ~ 1 +
                    age + I(age^2) + sex + edu_lvl + area +
                    log(wages_imp) + work_hrs_per_week +
                    O + C + E + A + ES +
                    (1 | region) + (1 | occupation),
                  weights = ipw_empl, data = youth_job_satisf)

cat("✅ M4-M7 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ----- SAVE BUNDLE --------------------------------------------------------
saveRDS(
  list(
    gam_job_satisf = gam_job_satisf,
    m0_satisf      = m0_satisf,
    m1_satisf      = m1_satisf,
    m1.5_satisf    = m1.5_satisf,
    m2_satisf      = m2_satisf,
    m3_satisf      = m3_satisf,
    m4_satisf      = m4_satisf,
    m5_satisf      = m5_satisf,
    m6_satisf      = m6_satisf,
    m7_satisf      = m7_satisf
  ),
  jobsatisf_cache_path
)
cat("💾 Saved bundle to:", jobsatisf_cache_path, "\n\n")

}  # end if (.jobsatisf_cache_hit) else { fit + save }

# ----- COMPLETION ---------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 6 models ready (", round(total_time, 2), "min)\n", sep = "")
cat("   → run 04_outputs_jobsatisf.R next to build tables/plots\n")
cat(rep("=", 80), "\n\n")
