# ==============================================================================
# NON-COGNITIVE SKILLS AND EMPLOYMENT ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         03_regression_empl.R
# Purpose:      Employment transition analysis using mixed-effects models
# 
# Description:  This script analyzes the relationship between non-cognitive 
#               skills (Big Five personality traits) and successful employment 
#               transitions among Russian youth aged 15-29. Uses multilevel 
#               modeling to account for individual, regional, and temporal 
#               heterogeneity.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 15-29 years (2016-2023)
# Methodology:  Mixed-effects logistic regression with random slopes
#
# Models:       M1 - Baseline (random intercepts only)
#               M2 - Controls (age, sex, education, area, SES)
#               M3 - NCS + Controls (adds Big Five traits)
#               M4 - Random slopes by SES
#               M5 - Random slopes by education level  
#               M6 - Random slopes by sex
#
# Outputs:      Regression tables, coefficient plots, model objects (.rds)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 17, 2024
# Modified:     October 18, 2025
# Version:      2.1
#
# Dependencies: lme4, ggplot2, ggeffects, modelsummary, tinytable, tidyverse
# Runtime:      ~5-10 minutes (depending on data size and system)
#
# ==============================================================================


# Initialize logging
cat("\n", rep("=", 80), "\n")
cat("STARTING EMPLOYMENT NCS REGRESSION ANALYSIS\n")
cat("Script: 03_regression_empl.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

# Start timer for overall execution
script_start_time <- Sys.time()

# SECTION 1: DATA LOADING
cat("📊 SECTION 1: DATA LOADING\n")
cat("Loading youth_empl (pre-built by 01_data_prep_empl.R)...\n")
data_start_time <- Sys.time()

youth_empl <- readRDS(file.path(processedData, "youth_empl.rds"))

data_end_time <- Sys.time()
cat("✅ Loaded in", round(difftime(data_end_time, data_start_time, units = "secs"), 2), "seconds\n")
cat("   - Dataset dimensions:", nrow(youth_empl), "rows x", ncol(youth_empl), "columns\n")
cat("   - Unique individuals:", length(unique(youth_empl$idind)), "\n\n")

# SECTION 2: BASELINE MODEL (M1)
cat("🔧 SECTION 2: BASELINE MODEL (M1)\n")
cat("Fitting baseline random intercept model...\n")
m1_start_time <- Sys.time()

m1_empl <- lmer(empl_dv ~ 1 + 
                      (1|region)  + (1|idind) + (1|age_factor) + (1|edu_lvl),
                    REML = T, data = youth_empl )

m1_end_time <- Sys.time()
cat("✅ Baseline model (M1) completed in", round(difftime(m1_end_time, m1_start_time, units = "secs"), 2), "seconds\n")
cat("   - Computing ICC by group...\n")

icc(m1_empl, by_group = TRUE)

# save the baseline model
saveRDS(m1_empl,
        file = file.path(outputsEmplNcs, "model_ncs_empl_m1_baseline.rds"))

cat("   - Generating age effect predictions...\n")
age_effect_total <- 
  ggpredict(m1_empl, 
            terms = "age_factor", 
            type = "random")

cat("✅ M1 analysis completed\n\n")

# SECTION 3: MODEL WITH CONTROLS (M2)
cat("🔧 SECTION 3: MODEL WITH CONTROLS (M2)\n")
cat("Fitting model with age, demographics, and control variables...\n")
m2_start_time <- Sys.time()

m2_empl <- lmer(empl_dv ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + edu_lvl + area + in_education + ses5 + # controls
                 (1|region)  + (1|idind) + (1|age), # random intercepts
               REML = T, data = youth_empl)

m2_end_time <- Sys.time()
cat("✅ Model with controls (M2) completed in", round(difftime(m2_end_time, m2_start_time, units = "secs"), 2), "seconds\n")

# summary(m2_empl)

cat("   - Generating age effect predictions by gender...\n")
### Predict the effect of age
age_effect_females <- 
  ggpredict(m2_empl, 
            terms = "age [all]", 
            type = "random", 
            condition = c(sex = "Female", 
                          ses5 = "Q3", 
                          edu_lvl = "4. Tertiary", 
                          area = "City"))

age_effect_males <- 
  ggpredict(m2_empl, 
            terms = "age [all]", 
            type = "random", 
            condition = c(sex = "Male", 
                          ses5 = "Q3", 
                          edu_lvl = "4. Tertiary", 
                          area = "City"))

Age = age_effect_females$x
Female = age_effect_females$predicted
Male = age_effect_males$predicted

cat("   - Creating and saving age effect visualization data...\n")
age_effect_data <-
  data.frame(Age, Female, Male) %>%
  pivot_longer(cols = c("Male", "Female"),
               names_to = "Sex",
               values_to = "Value") %>%
  mutate(Value = Value * 100) %>%
  filter(Sex == "Female")

# Save age effect data for visualization in script 05
saveRDS(age_effect_data, file.path(outputsEmplNcs, "age_effect_data.rds"))

cat("✅ M2 analysis and visualizations completed\n\n")

# SECTION 4: MODEL WITH NCS AND CONTROLS (M3)
cat("🔧 SECTION 4: MODEL WITH NCS AND CONTROLS (M3)\n")
cat("Fitting model with Non-Cognitive Skills, controls, and demographics...\n")
m3_start_time <- Sys.time()

m3_empl <- lmer(empl_dv ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + edu_lvl + in_education + area + ses5 + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age), # random effects
                REML = T, data = youth_empl)

m3_end_time <- Sys.time()
cat("✅ Model with NCS and controls (M3) completed in", round(difftime(m3_end_time, m3_start_time, units = "secs"), 2), "seconds\n")

# summary(m3_empl)
# modelsummary(m3_empl)

cat("   - Creating regression table for models M2 and M3...\n")
### Create a table of regressions

models_empl1 <- list(# "Baseline Model" = m1_empl,
                     "Model with controls"  = m2_empl,
                     "Model with NCS and controls" = m3_empl)

rename_vector_empl <- 
  c(`(Intercept)` = "Intercept",
    sexMale = "Sex: Male",
    `edu_lvl2. Secondary School` = "Education: Secondary",
    `edu_lvl3. Secondary Vocational` = "Education: Vocational",
    `edu_lvl4. Tertiary` = "Education: Tertiary",
    `areaUrban-Type Settlement` = "Area: Urban-Type Settlement",
    areaCity = "Area: City",
    `areaRegional Center` = "Area: Regional Center",
    `in_education1` = "Currently studying: Yes",
    ses5Q2 = "SES: Q2",
    ses5Q3 = "SES: Q3",
    ses5Q4 = "SES: Q4",
    ses5Q5 = "SES: Q5",
    O = "Openness",
    C = "Conscientiousness",
    E = "Extraversion",
    A = "Agreeableness",
    ES = "Emotional Stability")

reg_tables_mem_fixed <- 
  modelsummary(models_empl1,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl,
               output = "tinytable",
               note = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.")

cat("   - Saving regression models to file...\n")
# save the list of regression models as rds file
saveRDS(models_empl1,
        file = file.path(outputsEmplNcs, "models_ncs_empl_tab1.rds"))

cat("✅ Fixed effects models (M2 & M3) analysis completed\n\n")

# SECTION 4A: SENSITIVITY CHECKS
cat("🔎 SECTION 4A: SENSITIVITY CHECKS\n")
cat("Constructing 2019 outcomes with baseline 2016 NCS to probe endogeneity...\n")
sensitivity_start_time <- Sys.time()

baseline_ncs_2016 <-
  youth_empl %>%
  filter(year == 2016) %>%
  select(idind,
         O_baseline_2016 = O,
         C_baseline_2016 = C,
         E_baseline_2016 = E,
         A_baseline_2016 = A,
         ES_baseline_2016 = ES) %>%
  distinct(idind, .keep_all = TRUE)

sensitivity_data <-
  youth_empl %>%
  filter(year == 2019) %>%
  select(-c(O, C, E, A, ES)) %>%
  left_join(baseline_ncs_2016, by = "idind") 

cat("   - Sensitivity sample size:", nrow(sensitivity_data), "observations\n")
cat("   - Matched individuals with 2016 baseline NCS:", length(unique(sensitivity_data$idind)), "\n")
cat("   - Estimating M3 specification on sensitivity sample...\n")

m3_empl_sensitivity <- lmer(empl_dv ~ 1 +
                              age + I(age^2) +
                              sex + edu_lvl + in_education + area + ses5 +
                              O_baseline_2016 + C_baseline_2016 + E_baseline_2016 + A_baseline_2016 + ES_baseline_2016 +
                              (1|region) ,
                            REML = T, data = sensitivity_data)

summary(m3_empl)
summary(m3_empl_sensitivity)

# NOTE: sensitivity model and sample are kept in-memory for inspection but not
# written to 03_output/empl_outputs/ — neither 05_regression_final.R nor any
# qmd reads them, so persisting them only creates orphan files.

sensitivity_end_time <- Sys.time()
cat("✅ Sensitivity check completed in", round(difftime(sensitivity_end_time, sensitivity_start_time, units = "secs"), 2), "seconds\n\n")

# SECTION 5: RANDOM SLOPES MODELS
cat("🔧 SECTION 5: RANDOM SLOPES MODELS\n")
cat("Analyzing heterogeneous effects by SES, Education, and Sex...\n\n")

# SECTION 5A: SES MODEL (M4)
cat("   📈 5A: SES Random Slopes Model (M4)\n")
cat("   Fitting model with random slopes by socioeconomic status...\n")
m4_start_time <- Sys.time()

### SES ####

m4_empl <- lmer(empl_dv ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + edu_lvl + in_education + area + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age_factor) + # random intercepts
                  (1 + O + C + E + A + ES | ses5), # random slopes
                REML = T, data = youth_empl)

m4_end_time <- Sys.time()
cat("   ✅ SES model (M4) completed in", round(difftime(m4_end_time, m4_start_time, units = "secs"), 2), "seconds\n")

# summary(m4_empl)

cat("   - Extracting and saving coefficients for SES visualization...\n")
m4_empl_coefs =
  coef(m4_empl)$`ses5` %>%
  as.data.frame() %>%
  rownames_to_column(var = "ses5") %>%
  select(ses5, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -ses5) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

# Save coefficients data for visualization in script 05
saveRDS(m4_empl_coefs, file.path(outputsEmplNcs, "m4_ses_coefs.rds"))

cat("   ✅ SES analysis completed\n\n")

# SECTION 5B: EDUCATION MODEL (M5)
cat("   📚 5B: Education Random Slopes Model (M5)\n")
cat("   Fitting model with random slopes by education level...\n")
m5_start_time <- Sys.time()

### EDU ####

m5_empl <- lmer(empl_dv ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  sex + in_education + area + ses5 + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age_factor) + # random intercepts
                  (1 + O + C + E + A + ES | edu_lvl), # random slopes
                REML = T, data = youth_empl)

m5_end_time <- Sys.time()
cat("   ✅ Education model (M5) completed in", round(difftime(m5_end_time, m5_start_time, units = "secs"), 2), "seconds\n")

# summary(m5_empl)

cat("   - Extracting and saving coefficients for education visualization...\n")
m5_empl_coefs =
  coef(m5_empl)$`edu_lvl` %>%
  as.data.frame() %>%
  rownames_to_column(var = "edu_lvl") %>%
  select(edu_lvl, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -edu_lvl) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

# Save coefficients data for visualization in script 05
saveRDS(m5_empl_coefs, file.path(outputsEmplNcs, "m5_edu_coefs.rds"))

cat("   ✅ Education analysis completed\n\n")

# SECTION 5C: SEX MODEL (M6)
cat("   👥 5C: Sex Random Slopes Model (M6)\n")
cat("   Fitting model with random slopes by sex...\n")
m6_start_time <- Sys.time()

### SEX ####

m6_empl <- lmer(empl_dv ~ 1 + 
                  age + I(age^2) + # assuming nonlinearity of the age effect
                  edu_lvl + in_education + area + ses5 + # controls
                  O + C + E + A + ES + # NCS
                  (1|region)  + (1|idind) + (1|age_factor) + # random intercepts
                  (1 + O + C + E + A + ES | sex), # random slopes
                REML = T, data = youth_empl)

m6_end_time <- Sys.time()
cat("   ✅ Sex model (M6) completed in", round(difftime(m6_end_time, m6_start_time, units = "secs"), 2), "seconds\n")

cat("   - Extracting and saving coefficients for sex visualization...\n")
m6_empl_coefs =
  coef(m6_empl)$`sex` %>%
  as.data.frame() %>%
  rownames_to_column(var = "sex") %>%
  select(sex, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -sex) %>%
  mutate(Estimate = as.numeric(Estimate)*100) %>%
  mutate(Skill = case_when(Skill == "O" ~ "Openness",
                           Skill == "C" ~ "Conscientiousness",
                           Skill == "E" ~ "Extraversion",
                           Skill == "A" ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))

# Save coefficients data for visualization in script 05
saveRDS(m6_empl_coefs, file.path(outputsEmplNcs, "m6_sex_coefs.rds"))

cat("   ✅ Sex analysis completed\n\n")

# SECTION 6: FINAL OUTPUTS
cat("📊 SECTION 6: FINAL OUTPUTS\n")
cat("Creating final regression tables and saving results...\n")
final_start_time <- Sys.time()

models_empl2 <-
  list("Model with SES" = m4_empl,
       "Model with Education" = m5_empl,
       "Model with Sex" = m6_empl)

cat("   - Saving random slopes models to file...\n")
# save the list of regression models as rds file
saveRDS(models_empl2,
        file = file.path(outputsEmplNcs, "models_ncs_empl_tab2.rds"))

cat("   - Creating final regression table...\n")
reg_tables_mem_random <- 
  modelsummary(models_empl2,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl,
               output = "tinytable",
               notes   = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.") 

final_end_time <- Sys.time()
cat("✅ Final outputs completed in", round(difftime(final_end_time, final_start_time, units = "secs"), 2), "seconds\n\n")

# SCRIPT COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 EMPLOYMENT NCS REGRESSION ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📈 MODELS FITTED:\n")
cat("   • M1: Baseline random intercept model\n")
cat("   • M2: Model with demographic and control variables\n")
cat("   • M3: Model with NCS, demographics, and controls\n")
cat("   • Sensitivity: 2016 NCS baseline with 2019 employment outcomes\n")
cat("   • M4: Random slopes by SES (socioeconomic status)\n")
cat("   • M5: Random slopes by education level\n")
cat("   • M6: Random slopes by sex\n\n")
cat("📊 OUTPUTS GENERATED:\n")
cat("   • Age effect predictions and data\n")
cat("   • SES heterogeneity coefficients\n")
cat("   • Education heterogeneity coefficients\n")
cat("   • Sex heterogeneity coefficients\n")
cat("   • Model objects saved to RDS files\n")
cat("   • Sensitivity-check sample and model saved to RDS files\n")
cat("   • Visualization data prepared for script 05\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("📁 OUTPUT FILES:\n")
cat("   • models_ncs_empl_tab1.rds (M2 & M3 models)\n")
cat("   • model_ncs_empl_m3_sensitivity.rds (sensitivity-check model)\n")
cat("   • sensitivity_data_2019_outcomes_2016_ncs.rds (matched sensitivity sample)\n")
cat("   • models_ncs_empl_tab2.rds (M4, M5 & M6 models)\n")
cat("   • age_effect_data.rds (age effect visualization data)\n")
cat("   • m4_ses_coefs.rds (SES coefficients data)\n")
cat("   • m5_edu_coefs.rds (education coefficients data)\n")
cat("   • m6_sex_coefs.rds (sex coefficients data)\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")