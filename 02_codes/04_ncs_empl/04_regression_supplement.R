# ==============================================================================
# NON-COGNITIVE SKILLS AND EMPLOYMENT - SUPPLEMENTARY ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         04_regression_supplement.R
# Purpose:      Supplementary employment analysis with occupational outcomes
# 
# Description:  This script analyzes the relationship between non-cognitive 
#               skills and occupational outcomes including white-collar employment,
#               skill-based employment categories, and educational mismatch
#               among Russian youth. Extends main employment transition analysis
#               with additional outcome measures.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 15-29 years (2016-2023)
# Methodology:  Mixed-effects logistic regression with occupational outcomes
#
# Models:       MS1 - White-collar high-skilled employment
#               MS2 - White-collar high-skilled by SES (random slopes)
#               MS3 - Educational mismatch (over-education)
#
# Outputs:      Model objects and coefficient data for visualization creation
#               Data saved to outputs folder for 05_regression_final.R
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      April 27, 2025
# Modified:     October 19, 2025
# Version:      2.0 (Separated models from visualization creation)
#
# Dependencies: lme4, tidyverse, project configuration
# Runtime:      ~3-5 minutes (depending on data size and system)
#
# Notes:        Models are fitted and saved here, tables and plots created
#               in 05_regression_final.R for consistent output management
#
# ==============================================================================


# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat("\n", rep("=", 80), "\n")
cat("STARTING SUPPLEMENTARY EMPLOYMENT ANALYSIS\n")
cat("Script: 04_regression_supplement.R\n")
cat("Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

# SECTION 1: DATA PREPARATION
cat("📊 SECTION 1: SUPPLEMENTARY DATA PREPARATION\n")
cat("Loading and preparing youth employment data for occupational analysis...\n")
data_start_time <- Sys.time()

youth_empl_suppl <- 
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, 
         edu_lvl, region, sex, employed, year, wave, occupation, 
         in_education, hh_inc_quintile, area_binary, area, ipw_empl,
         #empl_offic, 
         employed_officially,
         self_employed, satisf_job, j1_1_1, j5_2,
         O, C, E, A, ES) %>%
  # create transition_successful variable
  # should be OFFICIALLy self-employed and satisfied with the job
  mutate(self_empl_offic_and_saisf = case_when(self_employed == 1 & j1_1_1 == 1 & employed_officially == 1 ~ 1,
                                               TRUE ~ 0),
         # exclude self-employed from those employed officially as successfully transitioned
         transition_successful1 = case_when(self_employed == 1 ~ 0,
                                            TRUE ~ employed_officially)) %>%
  # create a final transition successful variable
  mutate(transition_successful = case_when(transition_successful1 == 1 |
                                             self_empl_offic_and_saisf == 1 ~ 1,
                                           TRUE                             ~ 0)) %>%
  rename(ses5 = hh_inc_quintile) %>%
  # create age groups based on 15-29
  mutate(age_group = case_when(age >= 15 & age < 20 ~ "1. 15-19",
                               age >= 20 & age < 25 ~ "2. 20-24",
                               age >= 25 & age < 30 ~ "3. 25-29")) %>%
  mutate(in_education = factor(in_education)) %>%
  mutate(white_collar_hs = ifelse(occupation %in% c(1, 2, 3), 1, 0)) %>%
  mutate(white_collar_ls = ifelse(occupation %in% c(4, 5), 1, 0)) %>%
  mutate(blue_collar_hs = ifelse(occupation %in% c(6, 7), 1, 0)) %>%
  mutate(blue_collar_ls = ifelse(occupation %in% c(8, 9), 1, 0)) %>%
  mutate(white_collar = ifelse(occupation %in% c(1, 2, 3, 4, 5), 1, 0)) %>%
  mutate(age_factor = factor(age)) %>%
  mutate(skill_mismatch_overeduc = ifelse(edu_lvl == "4. Tertiary" & white_collar_hs !=1, 1, 0)) 

# Data preparation completed
data_end_time <- Sys.time()
cat("✅ Data preparation completed in", round(difftime(data_end_time, data_start_time, units = "secs"), 2), "seconds\n")
cat("   - Dataset dimensions:", nrow(youth_empl_suppl), "rows x", ncol(youth_empl_suppl), "columns\n")
cat("   - Unique individuals:", length(unique(youth_empl_suppl$idind)), "\n")
cat("   - Age range:", min(youth_empl_suppl$age, na.rm = TRUE), "to", max(youth_empl_suppl$age, na.rm = TRUE), "years\n")
cat("   - White-collar workers:", sum(youth_empl_suppl$white_collar, na.rm = TRUE), "observations\n")
cat("   - High-skilled white-collar:", sum(youth_empl_suppl$white_collar_hs, na.rm = TRUE), "observations\n")
cat("   - Educational mismatch cases:", sum(youth_empl_suppl$skill_mismatch_overeduc, na.rm = TRUE), "observations\n\n")

# Configure variable rename vector for tables
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

cat("✅ Variable rename vector configured for", length(rename_vector_empl), "variables\n\n")

# SECTION 2: WHITE-COLLAR HIGH-SKILLED EMPLOYMENT MODEL (MS1)
cat("🔧 SECTION 2: WHITE-COLLAR HIGH-SKILLED MODEL (MS1)\n")
cat("Fitting white-collar high-skilled employment model...\n")
ms1_start_time <- Sys.time()

m_occup_wc_hs <- lmer(white_collar_hs ~ 1 + 
                        age + I(age^2) +
                        sex + edu_lvl + area + ses5 + in_education + # controls
                        O + C + E + A + ES + # NCS
                        (1|region)  + (1|idind) + (1|edu_lvl), # random intercepts
                      weights = ipw_empl,
                      REML = T, data = youth_empl_suppl)

ms1_end_time <- Sys.time()
cat("✅ White-collar high-skilled model (MS1) completed in", 
    round(difftime(ms1_end_time, ms1_start_time, units = "secs"), 2), "seconds\n")

# Display model summary
summary(m_occup_wc_hs)
cat("✅ MS1 model summary displayed\n\n")

# SECTION 3: WHITE-COLLAR HIGH-SKILLED BY SES MODEL (MS2)
cat("🔧 SECTION 3: WHITE-COLLAR HIGH-SKILLED BY SES MODEL (MS2)\n")
cat("Fitting white-collar model with SES random slopes...\n")
ms2_start_time <- Sys.time()

m_occup_wc_hs_ses <- lmer(white_collar_hs ~ 1 + 
                            age + I(age^2) +
                            sex + edu_lvl + area + in_education + # controls
                            O + C + E + A + ES + # NCS
                            (1|idind) + (1|edu_lvl) + # random intercepts
                            (1 + O + C + E + A + ES | ses5), # random effects
                          weights = ipw_empl,
                          REML = T, 
                          data = youth_empl_suppl)

ms2_end_time <- Sys.time()
cat("✅ White-collar by SES model (MS2) completed in", 
    round(difftime(ms2_end_time, ms2_start_time, units = "secs"), 2), "seconds\n")

# Display model summary
summary(m_occup_wc_hs_ses)

# Extract SES coefficients for visualization
cat("   - Extracting SES coefficients for visualization...\n")
m_ocup_ses_coefs =
  coef(m_occup_wc_hs_ses)$`ses5` %>%
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

cat("✅ MS2 coefficients extracted for visualization\n\n")


# SECTION 4: EDUCATIONAL MISMATCH MODEL (MS3)
cat("🔧 SECTION 4: EDUCATIONAL MISMATCH MODEL (MS3)\n")
cat("Fitting educational mismatch (over-education) model...\n")
cat("   - Restricting sample to tertiary education graduates...\n")
ms3_start_time <- Sys.time()

# Filter data to tertiary education only
tertiary_data <- youth_empl_suppl[youth_empl_suppl$edu_lvl == "4. Tertiary", ]
cat("   - Tertiary education sample size:", nrow(tertiary_data), "observations\n")

m_overeduc <- lmer(skill_mismatch_overeduc ~ 1 + 
                         age + I(age^2) +
                         sex + area + ses5 + # in_education + # controls
                         O + C + E + A + ES + # NCS
                         (1|region)  + (1|idind), # random intercepts
                       REML = T, 
                       data = tertiary_data)

ms3_end_time <- Sys.time()
cat("✅ Educational mismatch model (MS3) completed in", 
    round(difftime(ms3_end_time, ms3_start_time, units = "secs"), 2), "seconds\n")

# Display model summary
summary(m_overeduc)
cat("✅ MS3 model summary displayed\n\n")

# SECTION 5: SAVE MODELS AND DATA FOR OUTPUT GENERATION
cat("💾 SECTION 5: SAVING MODELS AND VISUALIZATION DATA\n")
cat("Saving supplementary models and coefficients for 05_regression_final.R...\n")

# Save supplementary models
models_suppl <- list(
  "White Collar (High-Skilled)" = m_occup_wc_hs,
  "White Collar (High-Skilled) by SES" = m_occup_wc_hs_ses,
  "Educational Mismatch" = m_overeduc
)

saveRDS(models_suppl, file.path(outputsEmplNcs, "models_ncs_empl_suppl.rds"))
cat("✅ Supplementary models saved to models_ncs_empl_suppl.rds\n")

# Save SES coefficients for occupation plot
saveRDS(m_ocup_ses_coefs, file.path(outputsEmplNcs, "m_occup_ses_coefs.rds"))
cat("✅ Occupational SES coefficients saved to m_occup_ses_coefs.rds\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 SUPPLEMENTARY ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 MODELS FITTED:\n")
cat("   • MS1: White-collar high-skilled employment\n")
cat("   • MS2: White-collar high-skilled by SES (random slopes)\n")
cat("   • MS3: Educational mismatch (over-education)\n\n")
cat("💾 FILES SAVED:\n")
cat("   • models_ncs_empl_suppl.rds (All supplementary models)\n")
cat("   • m_occup_ses_coefs.rds (SES coefficients for visualization)\n\n")
cat("📈 ANALYSIS FEATURES:\n")
cat("   • Occupational outcome measures\n")
cat("   • Educational mismatch analysis\n")
cat("   • SES heterogeneity in occupational outcomes\n")
cat("   • Mixed-effects modeling with random slopes\n")
cat("   • IPW weighting for causal inference\n\n")
cat("🔄 NEXT STEPS:\n")
cat("   • Run 05_regression_final.R to generate tables and plots\n")
cat("   • Models and data ready for publication outputs\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")


