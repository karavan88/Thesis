# ==============================================================================
# CHAPTER 6 - JOB SATISFACTION OUTPUTS (LOAD MODELS + BUILD TABLES/PLOTS)
# ==============================================================================
#
# File:         04_outputs_jobsatisf.R
# Purpose:      Read the chapter-6 model bundle saved by
#               03_regressions_jobsatisf.R and build every derived object the
#               qmd needs: random-slope coefficient table, ggplot, modelsummary
#               tables, named lists, scalar summaries.
#
# Dependencies: 01_data_prep_jobsatisf.R must have been sourced first
#               (provides youth_job_satisf and the edu_lvl factor reorder).
#
# Inputs:       03_output/jobsatisf_outputs/jobsatisf_models.rds
#                 - bundle from 03_regressions_jobsatisf.R; auto-built if
#                   missing by sourcing 03 once (cache logic mirrors ch. 4).
#
# Outputs (in scope after sourcing):
#   gam_job_satisf, m0_satisf, m1_satisf, m1.5_satisf, m2_satisf, m3_satisf,
#   m4_satisf .. m7_satisf  - fitted model objects
#   icc_results              - icc(m0_satisf, by_group = TRUE)
#   m3_satisf_coefs          - long-format random-slope coefs (% scale)
#   plot_wages_ncs           - ggplot of NCS coefs by wage quintile
#   models_job_satisf_list   - list("M1"=m1, "M2"=m2, "M3"=m3)
#   rename_vector            - English variable labels for modelsummary
#   models_job_satisf        - flextable: M1/M2/M3 main-models table
#   models_satisf            - list("Career","Working Conditions","Wages")
#   models_satisf2           - flextable: domain-models table
#   n_models, n_variables, n_quintiles, coef_range, n_domain_models, wbl_obs
#   edu_summary, age_edu_table  - lightweight inline-stat helpers
#
# This script is sourced by the qmds (chapter-6 entry point), so keep it
# strictly load + transform. Heavy fitting belongs in 03.
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 6 - LOAD MODELS + BUILD OUTPUTS\n")
cat("Script: 04_outputs_jobsatisf.R\n")
cat(rep("=", 80), "\n\n")

# ----- Load the model bundle ----------------------------------------------
jobsatisf_cache_path <- file.path(outputsJobSatisfNcs, "jobsatisf_models.rds")

if (!file.exists(jobsatisf_cache_path)) {
  cat("⚠ Bundle not found — running 03_regressions_jobsatisf.R to build it...\n")
  source(file.path(codesJobSatisfNcs, "03_regressions_jobsatisf.R"))
} else {
  .cached <- readRDS(jobsatisf_cache_path)
  for (.nm in names(.cached)) assign(.nm, .cached[[.nm]], envir = globalenv())
  rm(.cached)
  cat("📦 Loaded bundle:", basename(jobsatisf_cache_path), "\n\n")
}

# ----- Lightweight inline-stat helpers ------------------------------------
edu_summary    <- summary(factor(youth_job_satisf$edu_lvl))
age_edu_table  <- table(youth_job_satisf$edu_lvl, youth_job_satisf$age)

# ----- ICC of the empty model ---------------------------------------------
icc_results <- icc(m0_satisf, by_group = TRUE)

# ----- M3 random-slope coefficients + plot --------------------------------
m3_satisf_coefs <-
  coef(m3_satisf)$hourly_wage_quintile %>%
  as.data.frame() %>%
  rownames_to_column(var = "hourly_wage_quintile") %>%
  select(hourly_wage_quintile, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -hourly_wage_quintile) %>%
  mutate(Estimate = as.numeric(Estimate) * 100,
         Skill = case_when(Skill == "O"  ~ "Openness",
                           Skill == "C"  ~ "Conscientiousness",
                           Skill == "E"  ~ "Extraversion",
                           Skill == "A"  ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

plot_wages_ncs <-
  ggplot(m3_satisf_coefs, aes(Estimate, hourly_wage_quintile)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill ~ .) +
  ylab("Wage Quintile") +
  xlab("Effect Size (percentage points)") +
  ggtitle("NCS Effects on Job Satisfaction by Wage Level")

n_quintiles <- length(unique(m3_satisf_coefs$hourly_wage_quintile))
coef_range  <- range(m3_satisf_coefs$Estimate)

# ----- Main regression table (M1, M2, M3) ---------------------------------
models_job_satisf_list <- list("M1" = m1_satisf, "M2" = m2_satisf, "M3" = m3_satisf)

rename_vector <- c(
  `(Intercept)`                           = "Intercept",
  age                                     = "Age",
  `I(age^2)`                              = "Age Squared",
  sexMale                                 = "Sex: Male",
  `edu_lvl1. No school`                   = "Education: No School",
  `edu_lvl2. Secondary School`            = "Education: Secondary",
  `edu_lvl3. Secondary Vocational`        = "Education: Vocational",
  `edu_lvl4. Tertiary`                    = "Education: Tertiary",
  `areaUrban-Type Settlement`             = "Area: Urban-Type Settlement",
  areaCity                                = "Area: City",
  `areaRegional Center`                   = "Area: Regional Center",
  `log(wages_imp)`                        = "Hourly Wage (Log)",
  work_hrs_per_week                       = "Working Hours Per Week",
  O                                       = "Openness",
  C                                       = "Conscientiousness",
  E                                       = "Extraversion",
  A                                       = "Agreeableness",
  ES                                      = "Emotional Stability"
)

models_job_satisf <-
  modelsummary(models_job_satisf_list,
               statistic   = "({std.error}) {stars}",
               gof_omit    = "ICC|RMSE|cond|AIC|BIC",
               coef_omit   = "SD|Cor",
               coef_rename = rename_vector,
               notes       = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.",
               output      = "flextable")

n_models    <- length(models_job_satisf_list)
n_variables <- length(rename_vector)

# ----- Domain-specific models (Career / Conditions / Wages) ---------------
models_satisf <- list(
  "Career"             = m4_satisf,
  "Working Conditions" = m5_satisf,
  "Wages"              = m6_satisf
)

models_satisf2 <- modelsummary(
  models_satisf,
  output      = "flextable",
  stars       = TRUE,
  coef_omit   = "Intercept",
  gof_omit    = "IC|Log.Lik",
  coef_rename = rename_vector,
  notes       = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы."
)

n_domain_models <- length(models_satisf)
wbl_obs         <- sum(!is.na(youth_job_satisf$satisf_wbl))

cat("✅ Chapter 6 derived outputs ready\n")
cat("   • models_job_satisf_list (M1, M2, M3)\n")
cat("   • models_satisf (Career, Working Conditions, Wages)\n")
cat("   • m3_satisf_coefs + plot_wages_ncs\n")
cat("   • icc_results, scalar summaries\n\n")
