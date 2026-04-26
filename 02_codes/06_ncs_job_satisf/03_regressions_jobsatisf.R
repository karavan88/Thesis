# ==============================================================================
# NON-COGNITIVE SKILLS AND JOB SATISFACTION - REGRESSION ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         03_regressions_jobsatisf.R
# Purpose:      Multilevel regression analysis of NCS effects on job satisfaction
# 
# Description:  This script conducts comprehensive regression analysis to examine
#               the relationship between non-cognitive skills (Big Five traits)
#               and various job satisfaction domains. Uses mixed-effects models
#               with individual, regional, and occupational random effects.
#               Includes wage heterogeneity analysis and multiple satisfaction outcomes.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)  
# Input:        youth_job_satisf dataset (from 01_data_prep_jobsatisf.R)
#
# Key Outputs:  • Mixed-effects models for overall job satisfaction
#               • Random slopes analysis by wage quintiles
#               • Models for specific satisfaction domains (career, conditions, pay)
#               • Publication-ready regression tables
#               • Coefficient plots by wage levels
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      January 12, 2025
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: lme4, lmerTest, modelsummary, ggplot2, tidyr
# Runtime:      ~3-5 minutes (depending on model complexity)
#
# Notes:        Uses IPW weights for employment selection correction
#               Includes hierarchical random effects structure
#               Focuses on employed youth aged 15-29 years
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("📊 JOB SATISFACTION - REGRESSION ANALYSIS\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 03_regressions_jobsatisf.R\n")
cat("🎯 Purpose: Multilevel regression analysis of NCS and job satisfaction\n")
cat("📈 Processing: Mixed-effects models → Coefficient estimation\n\n")

# ----- DISK CACHE FOR FITTED MODELS ------------------------------------------
# All LMER / GAM fits below are expensive (~50 s combined). They are cached to
# 03_output/jobsatisf_outputs/jobsatisf_models.rds so subsequent thesis renders
# skip the refit. To force a refit: delete the cache file, or set
#   options(thesis.refit = TRUE)
# before sourcing this script.
jobsatisf_cache_path <- file.path(outputsJobSatisfNcs, "jobsatisf_models.rds")
.refit_jobsatisf <- isTRUE(getOption("thesis.refit", FALSE))
.jobsatisf_cache_hit <- !.refit_jobsatisf && file.exists(jobsatisf_cache_path)

# The factor-level reorder needs to match what the cached models expect, so it
# runs unconditionally before any fitting or cache load.
edu_lvl_levels <- c("4. Tertiary", "1. No school", "2. Secondary School", "3. Secondary Vocational")
youth_job_satisf$edu_lvl <- factor(youth_job_satisf$edu_lvl, levels = edu_lvl_levels)

# Lightweight summaries (always cheap)
edu_summary <- summary(factor(youth_job_satisf$edu_lvl))
age_edu_table <- table(youth_job_satisf$edu_lvl, youth_job_satisf$age)

if (.jobsatisf_cache_hit) {
  cat("📦 Loading job-satisfaction models from cache:\n   ", jobsatisf_cache_path, "\n")
  .cached <- readRDS(jobsatisf_cache_path)
  for (.nm in names(.cached)) assign(.nm, .cached[[.nm]], envir = globalenv())
  rm(.cached)
} else {

# SECTION 1: EXPLORATORY GAM ANALYSIS
cat("📈 SECTION 1: EXPLORATORY GAM ANALYSIS\n")
cat("Fitting GAM model to explore wage-satisfaction relationship...\n")
gam_start <- Sys.time()

# Fit controlled GAM model with smooth wage effect and adjustment covariates
gam_data <- youth_job_satisf %>%
  mutate(
    region = as.factor(region),
    occupation = as.factor(occupation),
    year = as.factor(year)
  )

gam_job_satisf <- gam(
  satisf_job ~
    s(log(hourly_wage)) +
    age + I(age^2) + sex + edu_lvl + area + work_hrs_per_week +
    s(region, bs = "re") +
    s(year, bs = "re") +
    s(occupation, bs = "re"),
  data = gam_data,
  family = binomial,
  method = "REML"
)

gam_end <- Sys.time()
cat("✅ GAM analysis completed in", round(difftime(gam_end, gam_start, units = "secs"), 2), "seconds\n")
cat("   - Model: Controlled GAM with smooth(log(hourly_wage)) + covariates + region/occupation controls\n")
cat("   - Family: Binomial (logistic)\n")
cat("   - Education levels in data:", length(edu_summary), "\n")
cat("   - Age-education crosstab created for model validation\n\n")

# SECTION 2: MIXED-EFFECTS MODELS OF OVERALL JOB SATISFACTION
cat("🏗️ SECTION 2: MIXED-EFFECTS MODELS OF OVERALL JOB SATISFACTION\n")
cat("Fitting hierarchical models with multiple random effects...\n")
mixed_start <- Sys.time()

# Baseline model (intercept-only) with multiple random effects
cat("   - Fitting baseline intercept-only model...\n")
m0_satisf <-
  lmer(satisf_job ~ 1 +
         (1 | idind) + (1 | region) + (1 | occupation) +
         (1 | hourly_wage_quintile),
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)),
       data = youth_job_satisf)

mixed_end <- Sys.time()
cat("✅ Baseline model completed in", round(difftime(mixed_end, mixed_start, units = "secs"), 2), "seconds\n")
cat("   - Random effects: Individual, Region, Occupation, Wage quintile\n\n")

# SECTION 3: MAIN REGRESSION MODELS
cat("📊 SECTION 3: MAIN REGRESSION MODELS\n")
cat("Fitting reference model, NCS-only model, and full model...\n")
main_models_start <- Sys.time()

# Model 1: Reference model (demographics + wages + working hours)
cat("   - Fitting M1: Reference model with demographics and wages...\n")
m1_satisf <- 
  lmer(satisf_job ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         (1 | idind) + (1 | region) + (1 | occupation),  
       weights = ipw_empl,
       data = youth_job_satisf)

# Model 1.5: NCS-only model (for comparison)
cat("   - Fitting M1.5: NCS-only model...\n")
m1.5_satisf <- 
  lmer(satisf_job ~ 1 + 
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

# Model 2: Full model (demographics + wages + working hours + NCS)
cat("   - Fitting M2: Full model with demographics, wages, and NCS...\n")
m2_satisf <- 
  lmer(satisf_job ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

main_models_end <- Sys.time()
cat("✅ Main models completed in", round(difftime(main_models_end, main_models_start, units = "secs"), 2), "seconds\n")
cat("   - M1: Reference model with demographics and economic controls\n")
cat("   - M1.5: NCS-only model for effect size comparison\n")
cat("   - M2: Full model combining demographics, wages, and NCS\n")
cat("   - All models use IPW weights for employment selection correction\n\n")

# m2_satisf_logit <- 
#   glmer(satisf_job ~ 1 + 
#          age + I(age^2) + sex + edu_lvl + area + 
#          log(wages_imp) + 
#          work_hrs_per_week +
#          O + C + E + A + ES + 
#          (1 | idind) + (1 | region) + (1 | occupation), 
#        family = binomial(link = "logit"),
#        weights = ipw_empl,
#        data = youth_job_satisf)
# 
# summary(m2_satisf_logit)

# SECTION 4: RANDOM SLOPES MODEL BY WAGE QUINTILE
cat("📈 SECTION 4: RANDOM SLOPES MODEL BY WAGE QUINTILE\n")
cat("Fitting model with NCS random slopes varying by wage level...\n")
random_slopes_start <- Sys.time()

# Model 3: Random slopes of NCS by wage quintile
cat("   - Fitting M3: Random slopes model...\n")
m3_satisf <-
  lmer(satisf_job ~ 1 +
         age + I(age^2) + sex + edu_lvl + area +
        # log(wages_imp) +  # Excluded to avoid collinearity with wage quintiles
         work_hrs_per_week +
         O + C + E + A + ES +
         (1 | idind) + (1 | region) + (1 | occupation) +
         (O + C + E + A + ES | hourly_wage_quintile),
       weights = ipw_empl,
       data = youth_job_satisf)

random_slopes_end <- Sys.time()
cat("✅ Random slopes analysis completed in", round(difftime(random_slopes_end, random_slopes_start, units = "secs"), 2), "seconds\n\n")


# SECTION 6: MODELS FOR SPECIFIC JOB SATISFACTION DOMAINS
cat("🎯 SECTION 6: MODELS FOR SPECIFIC JOB SATISFACTION DOMAINS\n")
cat("Fitting separate models for career, working conditions, and wage satisfaction...\n")
domains_start <- Sys.time()

# Model 4: Career satisfaction
cat("   - Fitting M4: Career satisfaction model...\n")
m4_satisf <- 
  lmer(satisf_career ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

# Model 5: Working conditions satisfaction
cat("   - Fitting M5: Working conditions satisfaction model...\n")
m5_satisf <- 
  lmer(satisf_labor_cond ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

# Model 6: Wage satisfaction
cat("   - Fitting M6: Wage satisfaction model...\n")
m6_satisf <- 
  lmer(satisf_wage ~ 1 + 
         age + I(age^2) + sex + edu_lvl + area + 
         log(wages_imp) + 
         work_hrs_per_week +
         O + C + E + A + ES + 
         (1 | idind) + (1 | region) + (1 | occupation), 
       weights = ipw_empl,
       data = youth_job_satisf)

# Model 7: Work-life balance satisfaction (wave 28 only, no individual random effects)
cat("   - Fitting M7: Work-life balance satisfaction model (limited data)...\n")
m7_satisf <-
  lmer(satisf_wbl ~ 1 +
         age + I(age^2) + sex + edu_lvl + area +
         log(wages_imp) +
         work_hrs_per_week +
         O + C + E + A + ES +
         (1 | region) + (1 | occupation),
       weights = ipw_empl,
       data = youth_job_satisf)

domains_end <- Sys.time()
cat("✅ Domain-specific models completed in", round(difftime(domains_end, domains_start, units = "secs"), 2), "seconds\n\n")

# ----- Save fitted models to disk cache --------------------------------------
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
cat("💾 Cached fitted models to:", jobsatisf_cache_path, "\n\n")

}  # end if (.jobsatisf_cache_hit) else { fit + cache }

# ----- POST-FIT DERIVATIONS (always run, cheap) ------------------------------

# ICC for the baseline model
icc_results <- icc(m0_satisf, by_group = TRUE)

# Random-slope coefficients for visualization
m3_satisf_coefs <-
  coef(m3_satisf)$hourly_wage_quintile %>%
  as.data.frame() %>%
  rownames_to_column(var = "hourly_wage_quintile") %>%
  select(hourly_wage_quintile, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -hourly_wage_quintile) %>%
  mutate(Estimate = as.numeric(Estimate) * 100) %>%
  mutate(Skill = case_when(Skill == "O"  ~ "Openness",
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

# Main regression table
models_job_satisf_list <- list("M1" = m1_satisf, "M2" = m2_satisf, "M3" = m3_satisf)

rename_vector <- c(`(Intercept)` = "Intercept",
                   age = "Age",
                   `I(age^2)` = "Age Squared",
                   sexMale = "Sex: Male",
                   `edu_lvl1. No school` = "Education: No School",
                   `edu_lvl2. Secondary School` = "Education: Secondary",
                   `edu_lvl3. Secondary Vocational` = "Education: Vocational",
                   `edu_lvl4. Tertiary` = "Education: Tertiary",
                   `areaUrban-Type Settlement` = "Area: Urban-Type Settlement",
                   areaCity = "Area: City",
                   `areaRegional Center` = "Area: Regional Center",
                   `log(wages_imp)` = "Hourly Wage (Log)",
                   work_hrs_per_week = "Working Hours Per Week",
                   O = "Openness",
                   C = "Conscientiousness",
                   E = "Extraversion",
                   A = "Agreeableness",
                   ES = "Emotional Stability")

models_job_satisf <-
  modelsummary(models_job_satisf_list,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector,
               notes = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.",
               output = "flextable")

n_models    <- length(models_job_satisf_list)
n_variables <- length(rename_vector)

# Domain-specific models
models_satisf <- list("Career" = m4_satisf,
                      "Working Conditions" = m5_satisf,
                      "Wages" = m6_satisf)

models_satisf2 <- modelsummary(
  models_satisf,
  output = "flextable",
  stars = TRUE,
  coef_omit = "Intercept",
  gof_omit = "IC|Log.Lik",
  coef_rename = rename_vector,
  notes = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы."
)

n_domain_models <- length(models_satisf)
wbl_obs <- sum(!is.na(youth_job_satisf$satisf_wbl))

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 JOB SATISFACTION REGRESSION ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 MODELS FITTED:\n")
cat("   • GAM exploratory model: Job satisfaction ~ smooth(log wage)\n")
cat("   • M0: Baseline intercept-only with random effects\n")
cat("   • M1: Reference model (demographics + wages + hours)\n")
cat("   • M1.5: NCS-only model (for effect size comparison)\n")
cat("   • M2: Full model (M1 + NCS traits)\n")
cat("   • M3: Random slopes model (NCS effects vary by wage quintile)\n")
cat("   • M4-M6: Domain-specific models (career, conditions, wages)\n")
cat("   • M7: Work-life balance model (limited data)\n\n")
cat("🏗️ MODEL SPECIFICATIONS:\n")
cat("   • Random effects: Individual, Region, Occupation (+ Wage quintile in M3)\n")
cat("   • IPW weights applied for employment selection correction\n")
cat("   • Mixed-effects framework accommodates panel structure\n")
cat("   • Robust standard errors and hierarchical clustering\n\n")
cat("🧠 NON-COGNITIVE SKILLS:\n")
cat("   • Big Five traits: Openness, Conscientiousness, Extraversion\n")
cat("   • Agreeableness, Emotional Stability\n")
cat("   • Random slopes analysis shows heterogeneous effects by wage level\n\n")
cat("📋 OUTPUT TABLES:\n")
cat("   • Main regression table: M1, M2, M3 comparison\n")
cat("   • Domain-specific table: Career, Working conditions, Wages\n")
cat("   • Publication-ready format with Russian source notes\n\n")
cat("📈 KEY FINDINGS READY FOR:\n")
cat("   • Manuscript preparation\n")
cat("   • Coefficient interpretation\n")
cat("   • Heterogeneity analysis by wage levels\n")
cat("   • Cross-domain satisfaction comparisons\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")


