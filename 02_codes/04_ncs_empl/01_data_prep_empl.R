# ==============================================================================
# NON-COGNITIVE SKILLS AND EMPLOYMENT ANALYSIS - DATA PREPARATION
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         01_data_prep_empl.R
# Purpose:      Data preparation for the youth-employment analysis (Ch. 4)
#
# Description:  Prepares the RLMS-HSE dataset for the chapter-4 employment
#               analysis: builds the Big-Five (NCS) measures, derives
#               household-income quintiles assigned at age 15, imputes work
#               experience, and computes employment-selection IPW weights.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 15-29 years (Waves 25-28: 2016-2019)
# Processing:   - Big Five personality trait construction from survey items
#               - Household income quintile assignment
#               - Experience imputation using LOESS regression
#               - Inverse probability weighting for selection bias
#
# Key Variables Created:
#               - O, C, E, A, ES: Standardized Big Five personality traits
#               - hh_inc_quintile: Household income quintiles
#               - exp_imp: Imputed work experience
#               - ipw_empl: Inverse probability weights for employment
#
# Outputs:      ind_master_empl.rds (master analytical dataset)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 17, 2024
# Modified:     October 19, 2025
# ==============================================================================


cat("\n", rep("=", 80), "\n")
cat("EMPLOYMENT DATA PREPARATION - RLMS-HSE ANALYSIS\n")
cat(rep("=", 80), "\n")
cat("Script:", "01_data_prep_empl.R\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

cat("📋 PHASE 1: VARIABLE DEFINITIONS\n")
cat(rep("-", 50), "\n")

# Variables for non-cognitive skills
ncs_vars <- 
  c("j445_3", "j445_11", "j445_14",
    "j445_2", "j445_12", "j445_17",
    "j445_1", "j445_4",  "j445_20",
    "j445_9", "j445_16", "j445_19",
    "j445_5", "j445_10", "j445_18")

openness <- c("o1", "o2", "o3") 
con <- c("c1", "c2", "c3")
ex <- c("e1", "e2", "e3")
ag <- c("a1", "a2", "a3")
em_st <- c("es1", "es2", "es3") 

cat("✓ Defined NCS variables:\n")
cat("  • Raw survey items:", length(ncs_vars), "variables\n")
cat("  • Big Five dimensions: O, C, E, A, ES (3 items each)\n")
cat("  • Total personality items:", length(ncs_vars), "\n\n")

cat("📊 PHASE 2: MAIN DATA LOADING AND PROCESSING\n")
cat(rep("-", 50), "\n") 

cat("📊 PHASE 2: MAIN DATA LOADING AND PROCESSING\n")
cat(rep("-", 50), "\n")

cat("Loading individual data (2016-2019 waves)...")
start_time <- Sys.time()

ind_2016_2019_empl <-
  readRDS(file.path(processedData, "rlms_ind_sel_2001_2023.rds")) %>%
  filter(id_w %in% c("25", "28")) %>%
  # NCS items come in from haven as <haven_labelled>; strip the class so
  # the >= comparison and downstream rowMeans()/scale() work on every
  # vctrs version (newer vctrs refuses to coerce haven_labelled to double).
  # Base-R unclass()+as.numeric() avoids depending on haven being loaded.
  mutate(across(all_of(ncs_vars), ~ as.numeric(unclass(.)))) %>%
  mutate(across(all_of(ncs_vars), ~ ifelse(. >= 88888888, NA, .))) %>%
  # produce NCS measures
  mutate(o1 = 5 - j445_3,
         o2 = 5 - j445_11,
         o3 = 5 - j445_14,
         c1 = 5 - j445_2,
         c2 = j445_12,
         c3 = 5 - j445_17,
         e1 = 5 - j445_1,
         e2 = j445_4, 
         e3 = 5 - j445_20,
         a1 = 5 - j445_9,
         a2 = 5 - j445_16,
         a3 = 5 - j445_19,
         es1 = 5 - j445_5,
         es2 = j445_10,
         es3 = j445_18) %>%
  mutate(O = scale(rowMeans(select(., all_of(openness)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         C = scale(rowMeans(select(., all_of(con)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         E = scale(rowMeans(select(., all_of(ex)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         A = scale(rowMeans(select(., all_of(ag)), na.rm = T),center = TRUE, scale = TRUE)[,1],
         ES = scale(rowMeans(select(., all_of(em_st)), na.rm = T),center = TRUE, scale = TRUE)[,1]) %>%
  select(-all_of(ncs_vars), -all_of(openness), -all_of(con), -all_of(ex), -all_of(ag), -all_of(em_st)) %>%
  group_by(idind) %>%
  arrange(id_w) %>%
  fill(O, C, E, A, ES, .direction = "downup") %>%
  ungroup() 
  # drop_na(O, C, E, A, ES)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Individual data loaded and processed:\n")
cat("  • Observations:", nrow(ind_2016_2019_empl), "\n")
cat("  • Unique individuals:", length(unique(ind_2016_2019_empl$idind)), "\n")
cat("  • Waves included: 25 (2016), 28 (2019)\n")
cat("  • Big Five traits created and standardized\n")
cat("  • Dropout indicators created\n")
cat("  • Missing NCS values forward/backward filled within individuals\n\n")

cat("🎓 PHASE 3: AGE-15 REFERENCE YEAR\n")
cat(rep("-", 50), "\n")

cat("Calculating the calendar year when each respondent turned 15...")
start_time <- Sys.time()

# Find the calendar year in which each respondent was 15. Used downstream
# to look up the household they belonged to at age 15 -> background SES.
year_15_data <-
  ind_2016_2019_empl %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, age, id_w, year) %>%
  mutate(age_diff = age - 15) %>%
  mutate(year_15 = year - age_diff) %>%
  select(idind, year_15) %>%
  distinct() %>%
  mutate(year = as.integer(year_15)) %>%
  select(-year_15) %>%
  mutate(year_note = "year individual turned 15",
         year_copy = year)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Reference year computed:\n")
cat("  • Individuals with NCS data:", length(unique(year_15_data$idind)), "\n")
cat("  • Age range analyzed: 15-29 years\n\n")

idind_age15 <- unique(year_15_data$idind)

cat("🏠 PHASE 4: HOUSEHOLD MATCHING\n")
cat(rep("-", 50), "\n")

cat("Loading longitudinal data for household matching...")
start_time <- Sys.time()

# Find which household an individual belonged to in the year they turned 15.
rlms_ind_2001_2023 <-
  readRDS(file.path(processedData, "rlms_ind_2001_2023.rds")) %>%
  select(idind, id_w, year, id_h) %>%
  filter(idind %in% idind_age15) %>%
  mutate(idind = as.factor(idind))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")

cat("Performing household ID matching...")
start_time <- Sys.time()

matching_data <-
  rlms_ind_2001_2023 %>%
  full_join(year_15_data) %>%
  mutate(matched = ifelse(!is.na(id_h) & !is.na(year_copy), "matched", "unmatched"),
         idind = as.factor(idind))

# create the vector of those who were matched
matched = 
  matching_data %>%
  filter(matched == "matched") %>%
  select(idind, id_h, id_w, year) %>%
  mutate(matching_method = "original") %>%
  group_by(idind) %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  mutate(n = n()) %>%
  distinct() %>%
  select(-year, -n)

# View(matched)

idind_matched <- unique(matched$idind)
# length(idind_matched)

unmatched <- 
  matching_data %>%
  filter(!idind %in% idind_matched) %>%
  group_by(idind) %>%
  arrange(year_copy) %>%
  mutate(
    id_h_next = dplyr::lead(id_h),     # Use lead safely now
    id_w_next = dplyr::lead(id_w)
  ) %>%
  select(idind, id_h_next, id_w_next) %>%
  filter(id_w_next == min(id_w_next, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(id_h = id_h_next,
         id_w = id_w_next) %>%
  mutate(matching_method = "lead") 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Household matching results:\n")
cat("  • Direct matches:", length(idind_matched), "\n")
cat("  • Lead method matches:", nrow(unmatched), "\n")
cat("  • Total matched individuals:", length(idind_matched) + nrow(unmatched), "\n\n")

# View(unmatched)

idh_at_15 <-
  matched %>%
  bind_rows(unmatched)

cat("💰 PHASE 5: HOUSEHOLD INCOME DATA\n")
cat(rep("-", 50), "\n")

cat("Loading and processing household income data...")
start_time <- Sys.time()

hh_data_selected <-
  # readRDS("~/Documents/GitHub/Thesis/01_input_data/processed/hh_1994_2023_selected.rds") %>%
  readRDS(file.path(processedData, "hh_1994_2023_selected.rds")) %>%
  select(id_w, id_h, hh_income_per_cap, hh_per_cap_quantile) %>%
  group_by(id_w) %>%
  # impute average income per capita for those households where it is missing - it adds 11 obs
  mutate(hh_income_per_cap_imp = ifelse(is.na(hh_income_per_cap), 
                                    median(hh_income_per_cap, na.rm = T), hh_income_per_cap)) %>%
  # create a percentile rank based on a new distribution
  mutate(hh_per_cap_quantile_imp = percent_rank(hh_income_per_cap_imp)) %>%
  # create hh income quintile groups
  mutate(hh_inc_quintile = case_when(hh_per_cap_quantile_imp <= 0.2                                  ~ "Q1",
                                     hh_per_cap_quantile_imp >  0.2 & hh_per_cap_quantile_imp <= 0.4 ~ "Q2",
                                     hh_per_cap_quantile_imp >  0.4 & hh_per_cap_quantile_imp <= 0.6 ~ "Q3",
                                     hh_per_cap_quantile_imp >  0.6 & hh_per_cap_quantile_imp <= 0.8 ~ "Q4",
                                     hh_per_cap_quantile_imp >  0.8                                  ~ "Q5")) %>%
  # create deciles for descriptive purposes
  mutate(hh_inc_decile = factor(ntile(hh_income_per_cap_imp, 10))) %>%
  # create 20 groups
  mutate(hh_inc_20group = factor(ntile(hh_income_per_cap_imp, 20))) %>%
  ungroup() %>%
  right_join(idh_at_15) %>%
  select(-id_w, -id_h) 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Household income data processed:\n")
cat("  • Income imputation applied for missing values\n")
cat("  • Income quintiles created (Q1-Q5)\n")
cat("  • Income deciles created (D1-D10)\n")
cat("  • Income 20 groups created (G1-G20)\n")
cat("  • Merged with individual-household matches\n\n")

summary(hh_data_selected$hh_income_per_cap)
summary(hh_data_selected$hh_income_per_cap_imp)

# check the correlation between hh_per_cap_quantile11 and hh_per_cap_quantile
#cor(hh_data_selected$hh_per_cap_quantile, hh_data_selected$hh_per_cap_quantile1, use = "complete.obs")

# View(hh_data_selected)

cat("🔗 PHASE 6: MASTER DATASET CREATION\n")
cat(rep("-", 50), "\n")

cat("Merging individual and household data...")
start_time <- Sys.time()

# Now we need to merge the household data with the individual data
ind_master_empl <-
  ind_2016_2019_empl %>%
  full_join(hh_data_selected) %>%
  mutate(area_binary = case_when(area %in% c("City", "Regional Center") ~ "2. Urban",
                                 TRUE ~ "1. Rural"),
         hh_inc_quintile = factor(hh_inc_quintile)) %>%
  # if NA in edu_lvv that assign No School - this will fill 4 observations
  mutate(edu_lvl = ifelse(is.na(edu_lvl), "1. No school", edu_lvl),
         employed_officially = ifelse(is.na(employed_officially), 0, employed_officially)) %>%
  # fix NAs in self-employed
  mutate(self_employed = ifelse(is.na(self_employed), 0, self_employed)) 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Master dataset created:\n")
cat("  • Individual + household data merged\n")
cat("  • Area binary variable created (Rural/Urban)\n")
cat("  • Education and employment NAs handled\n")
cat("  • Final observations:", nrow(ind_master_empl), "\n\n")

summary(ind_master_empl$area)
# summary(ind_master_empl$self_employed[ind_master_empl$age >= 15 & ind_master_empl$age < 30])])])
summary(factor(ind_master_empl$hh_inc_quintile))
summary(factor(ind_master_empl$id_w))

summary(ind_master_empl$experience)

cat("🎯 PHASE 7: AGE FILTERING AND WAVE PREPARATION\n")
cat(rep("-", 50), "\n")

cat("Filtering to working age population (15-65)...")
start_time <- Sys.time()

# Make sure we have only population in the working age
ind_master_empl_pre <- 
  ind_master_empl %>%
  filter(age >= 15 & age < 66) %>%
  # we need to convert id_w tio numerric and then to factor again to get rid of other levels with 0 obs
  mutate(wave = ifelse(id_w == "25", "Wave: 25 (2016)", "Wave: 28 (2019)")) %>%
  mutate(wave = as.factor(wave)) 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Working age population filtered:\n")
cat("  • Age range: 15-65 years\n")
cat("  • Wave labels created\n")
cat("  • Observations after filtering:", nrow(ind_master_empl_pre), "\n\n")

summary(ind_master_empl_pre$wave)
summary(ind_master_empl_pre$id_w)

cat("⚙️ PHASE 8: EXPERIENCE IMPUTATION & IPW CALCULATION\n")
cat(rep("-", 50), "\n")

cat("Applying LOESS regression for experience imputation and IPW calculation...")
start_time <- Sys.time()

# Split the dataset by `id_w`
data_split_empl <- 
  split(ind_master_empl_pre, ind_master_empl_pre$wave)

# Apply the loess model and extract predicted values
predicted_data_empl <- 
  lapply(data_split_empl, function(subset) {
    
    # Fit loess model for each subset
    exp_model <- loess(experience ~ age, data = subset)
    
    # Add predicted values to the subset
    subset$exp_pred <- predict(exp_model, newdata = subset)
    
    # Calculate inverse probability weight to control for the self-selection into employment
    ipw_model <- weightit(employed ~ age + sex + edu_lvl + region + area ,
                          data = subset,
                          method = "ps",  # Propensity score (logit model)
                          estimand = "ATE")  # Average treatment effect

    # Add predicted values to the subset
    subset$ipw_empl <- ipw_model$weights
    
    return(subset)
  })

# Combine the data back into a single dataset
ind_master_empl <- 
  bind_rows(predicted_data_empl)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Advanced modeling completed:\n")
cat("  • LOESS regression for experience imputation\n")
cat("  • Inverse probability weighting (IPW) for employment selection\n")
cat("  • Models fitted separately by wave\n")
cat("  • Final dataset reconstructed\n\n")

# View(ind_master_empl)

summary(ind_master_empl$ipw_empl)
summary(ind_master_empl$exp_pred)
summary(ind_master_empl$id_w)
summary(ind_master_empl$wave)
summary(ind_master_empl$age)

cat("📊 PHASE 9: DATA VALIDATION AND VISUALIZATION\n")
cat(rep("-", 50), "\n")

cat("Creating experience validation plot...")
start_time <- Sys.time()

# check the correlation between observed and predicted via plot
ggplot(ind_master_empl, aes(x = experience, y = exp_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Experience vs Predicted Experience",
       x = "Experience",
       y = "Predicted Experience") +
  facet_wrap(~id_w) +
  theme_minimal()

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n\n")

cat("🔧 PHASE 10: FINAL DATA CLEANING\n")
cat(rep("-", 50), "\n")

cat("Applying final data corrections...")
start_time <- Sys.time()

cat("Applying final data corrections...")
start_time <- Sys.time()

# experience cannot be below 0, so all the negative values should be 0
ind_master_empl$exp_pred <- ifelse(ind_master_empl$exp_pred < 0, 0, ind_master_empl$exp_pred)

# replace NAs of exp with the predicted values from loess
ind_master_empl$exp_imp <- 
  ifelse(is.na(ind_master_empl$experience), ind_master_empl$exp_pred, 
         ind_master_empl$experience)

# just to check to which quantile does the value of 10 as a threshold belongs
percentile <- quantile(ind_master_empl$ipw_empl, 0.99) #5.88
#ind_master_empl$ipw_empl <- ifelse(ind_master_empl$ipw_empl > percentile, percentile, ind_master_empl$ipw_empl)
ind_master_empl$ipw_empl <- ifelse(ind_master_empl$ipw_empl > 10, 10, ind_master_empl$ipw_empl)

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Final corrections applied:\n")
cat("  • Negative experience values set to 0\n")
cat("  • Missing experience imputed with LOESS predictions\n")
cat("  • IPW weights capped at 10 (99th percentile:", round(percentile, 2), ")\n\n")

# check summary of ipw_empl for aged 15-30
summary(ind_master_empl$ipw_empl[ind_master_empl$age >= 15 & ind_master_empl$age < 30])

youth <- 
  ind_master_empl %>%
  filter(age >= 15 & age < 30) 

cat("📈 PHASE 11: YOUTH SAMPLE ANALYSIS\n")
cat(rep("-", 50), "\n")

cat("Creating IPW distribution plot for youth (15-29)...")
start_time <- Sys.time()
  
# plot histogram of these weights for the select age range
ggplot(youth, aes(x = ipw_empl)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Inverse Probability Weights",
       x = "Inverse Probability Weights",
       y = "Frequency") +
  theme_minimal()

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Youth sample (15-29) analyzed:\n")
cat("  • Observations:", nrow(youth), "\n")
cat("  • IPW distribution visualized\n\n")

# summary(ind_master_empl$wages)

cat("💾 PHASE 12: SAVE FINAL DATASET\n")
cat(rep("-", 50), "\n")

cat("Saving master employment dataset...")
start_time <- Sys.time()

saveRDS(ind_master_empl, file.path(processedData, "ind_master_empl.rds"))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n\n")

# ============================================================================
# PHASE 13: YOUTH ANALYTIC DATASET
# ============================================================================
# Build the chapter-4-ready dataset once: youth filter, NCS-complete cases,
# all renames/recodes/derived variables that downstream scripts (02_descr_empl,
# 03_regression_empl, 04_regression_supplement) used to recompute every time.
# After this script runs, those scripts only readRDS() + analyse.
# ============================================================================

cat("👥 PHASE 13: YOUTH ANALYTIC DATASET\n")
cat(rep("-", 50), "\n")

cat("Building youth_empl analytic dataset...")
start_time <- Sys.time()

youth_empl <-
  ind_master_empl %>%
  filter(age >= 15 & age < 30) %>%
  drop_na(O, C, E, A, ES) %>%
  # Drop military respondents (ISCO-08 major group 0). Keep NA occupation
  # rows since those represent the not-employed population we still want
  # to model in M1-M3 (outcome = empl_dv, with 0 for non-employed).
  filter(is.na(occupation) | occupation != 0) %>%
  rename(ses5 = hh_inc_quintile) %>%
  mutate(
    age_group     = case_when(
      age >= 15 & age < 20 ~ "1. 15-19",
      age >= 20 & age < 25 ~ "2. 20-24",
      age >= 25 & age < 30 ~ "3. 25-29"
    ),
    age_factor    = factor(age),
    in_education  = factor(in_education),
    empl_dv       = case_when(
      employed_officially == 1 & self_employed == 0 ~ 1,
      TRUE ~ 0
    ),
    # Occupation-based outcomes (ISCO-08 major groups)
    white_collar_hs         = ifelse(occupation %in% c(1, 2, 3), 1, 0),
    white_collar_ls         = ifelse(occupation %in% c(4, 5), 1, 0),
    blue_collar_hs          = ifelse(occupation %in% c(6, 7), 1, 0),
    blue_collar_ls          = ifelse(occupation %in% c(8, 9), 1, 0),
    white_collar            = ifelse(occupation %in% c(1, 2, 3, 4, 5), 1, 0),
    skill_mismatch_overeduc = ifelse(edu_lvl == "4. Tertiary" & white_collar_hs != 1, 1, 0),
    satisfied_with_job      = case_when(j1_1_1 == 1 ~ 1, TRUE ~ 0)
  )

saveRDS(youth_empl, file.path(processedData, "youth_empl.rds"))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Youth analytic dataset saved:\n")
cat("  • File: youth_empl.rds\n")
cat("  • Observations:", nrow(youth_empl), "\n")
cat("  • Unique individuals:", length(unique(youth_empl$idind)), "\n")
cat("  • Variables:", ncol(youth_empl), "\n\n")

cat(rep("=", 80), "\n")
cat("🎉 DATA PREPARATION COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 FINAL DATASET SUMMARY:\n")
cat("  • File saved: ind_master_empl.rds\n")
cat("  • Total observations:", nrow(ind_master_empl), "\n")
cat("  • Unique individuals:", length(unique(ind_master_empl$idind)), "\n")
cat("  • Waves included: 25 (2016), 28 (2019)\n")
cat("  • Youth sample (15-29):", nrow(youth), "observations\n")
cat("  • Variables created: Big Five traits, IPW weights, imputed experience\n")
cat("  • Ready for chapter-4 employment analysis\n")
cat(rep("=", 80), "\n")
cat("Script completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")
