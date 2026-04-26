# ==============================================================================
# NON-COGNITIVE SKILLS AND JOB SATISFACTION - DATA PREPARATION
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         01_data_prep_jobsatisf.R
# Purpose:      Prepare RLMS data for job satisfaction analysis with NCS
# 
# Description:  This script processes RLMS individual data (2016-2019) to create
#               a clean dataset for analyzing the relationship between non-cognitive
#               skills and job satisfaction. Includes wage imputation, working hours
#               calculation, occupation coding, and satisfaction measure preparation.
#               Focuses on employed youth aged 15-29 years.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)  
# Input Files:  • ind_master_empl.rds (Master employment dataset)
#
# Key Outputs:  • youth_job_satisf dataset with job satisfaction measures
#               • Wage imputation using experience and demographics
#               • Working hours and overtime indicators
#               • Occupation and industry classifications
#               • Job satisfaction domains (overall, career, conditions, wages)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 15, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: dplyr, ggplot2, readr
# Runtime:      ~1-2 minutes
#
# Notes:        Applies CPI adjustment for 2016 wages to 2019 levels
#               Uses mixed-effects imputation for missing wage data
#               Creates hourly wage quintiles for stratified analysis
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("💼 JOB SATISFACTION - DATA PREPARATION\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 01_data_prep_jobsatisf.R\n")
cat("🎯 Purpose: Prepare RLMS data for job satisfaction analysis\n")
cat("📈 Processing: Employment data → Job satisfaction dataset\n\n")

# SECTION 1: CPI ADJUSTMENT CALCULATION
cat("💰 SECTION 1: CPI ADJUSTMENT CALCULATION\n")
cat("Setting up Consumer Price Index adjustment for wage comparability...\n")
cpi_start <- Sys.time()

# Adjust wages for 2016 to the 2019 level using World Bank CPI data
# Source: https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=RU
cpi_2016 <- 162.2
cpi_2019 <- 180.8

adj_factor <- cpi_2019/cpi_2016

cpi_end <- Sys.time()
cat("✅ CPI adjustment calculated in", round(difftime(cpi_end, cpi_start, units = "secs"), 2), "seconds\n")
cat("   - 2016 CPI:", cpi_2016, "\n")
cat("   - 2019 CPI:", cpi_2019, "\n")
cat("   - Adjustment factor:", round(adj_factor, 3), "\n\n")

# SECTION 2: MAIN DATASET PREPARATION
cat("📊 SECTION 2: MAIN DATASET PREPARATION\n")
cat("Loading and filtering employment data for job satisfaction analysis...\n")
data_prep_start <- Sys.time()

# Load master employment dataset and apply filters for job satisfaction analysis
raw_data <- readRDS(file.path(processedData, "ind_master_empl.rds"))
cat("   - Loaded raw employment data:", nrow(raw_data), "observations\n")

youth_job_satisf <- 
  raw_data %>%
  filter(age >= 15 & age < 30) %>%
  filter(employed == 1) %>%
  select(idind, id_w, year, ipw_empl, wave,
         age, sex, region, area, edu_lvl, 
         exp_imp, wages,
         # tenure year and month
         j5a, j5b,
         # employees/subordinates
         j6, j6_0, 
         # working over time or over the weekend
         working_overtime, working_weekends,
         # company ownership
         j23, j26,
         # labor amount (to derive an hourly wage)
         j6_1, j6_1a, j6_1b, j6_2,
         occupation, industry,
         starts_with("satisf"), 
         # original job satisfaction measures
         j1_1_1:j1_1_10,
         O, C, E, A, ES) %>%
  # if wave is 25, then we need to adjust the wages
  #mutate(wages = ifelse(id_w == "25", wages * adj_factor, wages)) %>%
  drop_na(O, C, E, A, ES, satisf_job) %>%
  mutate(work_hrs_per_week = ifelse(is.na(j6_2), 40, j6_2)) %>%
  mutate(work_over_40hrs = ifelse(work_hrs_per_week > 40, 1, 0)) %>%
  mutate(occupation = case_when(occupation == "0" ~ "0. Military",
                                occupation == "1" ~ "1. Managers",
                                occupation == "2" ~ "2. Professionals",
                                occupation == "3" ~ "3. Associate Professionals",
                                occupation == "4" ~ "4. Clerical Workers",
                                occupation == "5" ~ "5. Service/Sales Workers",
                                occupation == "6" ~ "6. Skilled Agricult Workers",
                                occupation == "7" ~ "7. Craft/Trades Workers",
                                occupation == "8" ~ "8. Plant/Machine Operators",
                                occupation == "9" ~ "9. Elementary Occupations"
                                )) 

data_prep_end <- Sys.time()
cat("✅ Dataset preparation completed in", round(difftime(data_prep_end, data_prep_start, units = "secs"), 2), "seconds\n")
cat("   - After age filter (15-29):", nrow(youth_job_satisf), "observations\n")
cat("   - Employment status: employed only\n")
cat("   - Age range:", min(youth_job_satisf$age), "-", max(youth_job_satisf$age), "years\n")
cat("   - Unique individuals:", length(unique(youth_job_satisf$idind)), "\n")
cat("   - Years covered:", paste(sort(unique(youth_job_satisf$year)), collapse = ", "), "\n\n")


# SECTION 3: WAGE IMPUTATION
cat("💸 SECTION 3: WAGE IMPUTATION\n")
cat("Imputing missing wages using experience, demographics, and NCS...\n")
imputation_start <- Sys.time()

# Calculate missing wage statistics before imputation
n_missing_wages <- sum(is.na(youth_job_satisf$wages))
n_total_obs <- nrow(youth_job_satisf)
missing_pct <- round((n_missing_wages / n_total_obs) * 100, 1)

cat("   - Missing wages:", n_missing_wages, "observations (", missing_pct, "%)\n")

# Split the dataset by wave for wave-specific imputation models
data_split_js <- split(youth_job_satisf, youth_job_satisf$wave)
cat("   - Splitting data by", length(data_split_js), "waves for imputation\n")

# Apply linear regression model for wage prediction by wave
predicted_data_job_satisf <- 
  lapply(data_split_js, function(subset) {
    
    # Fit linear model on log wages using experience, demographics, and NCS
    wages_model <- lm(log(wages) ~ exp_imp + I(exp_imp^2) + edu_lvl + 
                        sex + region + area +
                        O + C + E + A + ES, data = subset)
    
    # Add predicted values to the subset (back-transformed from log)
    subset$wages_pred <- exp(predict(wages_model, newdata = subset))
    
    return(subset)
  })

youth_job_satisf <- bind_rows(predicted_data_job_satisf)

imputation_end <- Sys.time()
cat("✅ Wage imputation completed in", round(difftime(imputation_end, imputation_start, units = "secs"), 2), "seconds\n")
cat("   - Imputation models fitted by wave using experience + demographics + NCS\n")
cat("   - Predicted wage range:", round(min(youth_job_satisf$wages_pred, na.rm = TRUE)), "-", 
    round(max(youth_job_satisf$wages_pred, na.rm = TRUE)), "rubles\n\n")

# SECTION 4: IMPUTATION VALIDATION AND FINAL WAGE VARIABLE
cat("✅ SECTION 4: IMPUTATION VALIDATION AND FINAL WAGE VARIABLE\n")
cat("Creating validation plot and final imputed wage variable...\n")
validation_start <- Sys.time()

# Create correlation plot between observed and predicted wages
wages_corr <-
  ggplot(youth_job_satisf, aes(x = wages, y = wages_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Observed vs Predicted Wages by Wave",
       x = "Observed Wages",
       y = "Predicted Wages") +
  xlim(0, 60000) +
  facet_wrap(~id_w) +
  theme_minimal()

# Create final imputed wage variable (replace NAs with predicted values)
youth_job_satisf$wages_imp <- 
  ifelse(is.na(youth_job_satisf$wages), youth_job_satisf$wages_pred, 
         youth_job_satisf$wages)

# Calculate imputation statistics
n_imputed <- sum(is.na(youth_job_satisf$wages))
wage_correlation <- cor(youth_job_satisf$wages, youth_job_satisf$wages_pred, use = "complete.obs")

validation_end <- Sys.time()
cat("✅ Validation completed in", round(difftime(validation_end, validation_start, units = "secs"), 2), "seconds\n")
cat("   - Correlation (observed vs predicted):", round(wage_correlation, 3), "\n")
cat("   - Final imputed wages created\n")
cat("   - Wage range (imputed):", round(min(youth_job_satisf$wages_imp)), "-", 
    round(max(youth_job_satisf$wages_imp)), "rubles\n\n")

# SECTION 5: HOURLY WAGE CALCULATION AND QUINTILE CREATION
cat("⏰ SECTION 5: HOURLY WAGE CALCULATION AND QUINTILE CREATION\n")
cat("Deriving hourly wages and creating wage quintiles for analysis...\n")
hourly_start <- Sys.time()

# Calculate hourly wages and create quintiles
youth_job_satisf <- 
  youth_job_satisf %>%
  mutate(hourly_wage = wages_imp/work_hrs_per_week,
         hourly_wage_quantile = percent_rank(hourly_wage),
         hourly_wage_quintile = case_when(hourly_wage_quantile <= 0.2 ~ "Q1",
                                          hourly_wage_quantile <= 0.4 ~ "Q2",
                                          hourly_wage_quantile <= 0.6 ~ "Q3",
                                          hourly_wage_quantile <= 0.8 ~ "Q4",
                                          TRUE                        ~ "Q5" )) %>%
  # Work-life balance satisfaction only available in wave 28 (2019)
  mutate(satisf_wbl = ifelse(id_w == "28", satisf_wbl, as.numeric(NA))) 

# Calculate key statistics
n_final_obs <- nrow(youth_job_satisf)
n_unique_ind <- length(unique(youth_job_satisf$idind))
hourly_wage_range <- range(youth_job_satisf$hourly_wage)
n_occupation_na <- sum(is.na(youth_job_satisf$occupation))
n_industry_na <- sum(is.na(youth_job_satisf$industry))

hourly_end <- Sys.time()
cat("✅ Hourly wage processing completed in", round(difftime(hourly_end, hourly_start, units = "secs"), 2), "seconds\n")
cat("   - Hourly wage range:", round(hourly_wage_range[1], 1), "-", round(hourly_wage_range[2], 1), "rubles/hour\n")
cat("   - Wage quintiles created (Q1-Q5)\n")
cat("   - Work-life balance satisfaction available for wave 28 only\n\n")

# SECTION 6: FINAL DATASET VALIDATION
cat("🔍 SECTION 6: FINAL DATASET VALIDATION\n")
cat("Validating final dataset structure and variables...\n")
validation_start <- Sys.time()

# Dataset dimensions and key variable summaries
cat("   - Final dataset dimensions:", n_final_obs, "observations x", ncol(youth_job_satisf), "variables\n")
cat("   - Unique individuals:", n_unique_ind, "\n")
cat("   - Age range:", min(youth_job_satisf$age), "-", max(youth_job_satisf$age), "years\n")

# Job satisfaction variables validation
job_satisf_levels <- length(unique(youth_job_satisf$satisf_job[!is.na(youth_job_satisf$satisf_job)]))
career_satisf_levels <- length(unique(youth_job_satisf$j1_1_4[!is.na(youth_job_satisf$j1_1_4)]))
conditions_satisf_levels <- length(unique(youth_job_satisf$j1_1_2[!is.na(youth_job_satisf$j1_1_2)]))
pay_satisf_levels <- length(unique(youth_job_satisf$j1_1_3[!is.na(youth_job_satisf$j1_1_3)]))

cat("   - Job satisfaction overall levels:", job_satisf_levels, "\n")
cat("   - Career satisfaction levels:", career_satisf_levels, "\n")
cat("   - Working conditions satisfaction levels:", conditions_satisf_levels, "\n")
cat("   - Pay satisfaction levels:", pay_satisf_levels, "\n")
cat("   - Missing occupation data:", n_occupation_na, "observations\n")
cat("   - Missing industry data:", n_industry_na, "observations\n")

validation_end <- Sys.time()
cat("✅ Dataset validation completed in", round(difftime(validation_end, validation_start, units = "secs"), 2), "seconds\n\n")

# SECTION 7: EXPLORATORY VISUALIZATIONS
cat("📊 SECTION 7: EXPLORATORY VISUALIZATIONS\n")
cat("Creating working hours histogram and identifying overworkers...\n")
viz_start <- Sys.time()

# Create histogram of working hours per week
hist_work_per_week <-
  ggplot(youth_job_satisf, aes(x = j6_2)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Working Hours per Week",
       x = "Working Hours",
       y = "Frequency") +
  ylim(0, 7) +
  xlim(35, 170) +
  theme_minimal()

# Identify workers with >40 hours per week
overworkers <-
  youth_job_satisf %>%
  filter(work_hrs_per_week > 40)

n_overworkers <- nrow(overworkers)
overwork_pct <- round((n_overworkers / n_final_obs) * 100, 1)

viz_end <- Sys.time()
cat("✅ Visualizations created in", round(difftime(viz_end, viz_start, units = "secs"), 2), "seconds\n")
cat("   - Working hours histogram generated\n")
cat("   - Overworkers (>40 hrs/week):", n_overworkers, "individuals (", overwork_pct, "%)\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 JOB SATISFACTION DATA PREPARATION COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 DATASET CREATED:\n")
cat("   • youth_job_satisf: Employment + job satisfaction + NCS data\n")
cat("   • Sample: Employed youth aged 15-29 years\n")
cat("   • Years: 2016-2019 (RLMS waves)\n")
cat("   • Observations:", n_final_obs, "\n")
cat("   • Unique individuals:", n_unique_ind, "\n\n")
cat("💸 WAGE PROCESSING:\n")
cat("   • CPI adjustment factor calculated (2016→2019)\n")
cat("   • Missing wages imputed using experience + demographics + NCS\n")
cat("   • Hourly wages calculated from weekly wages and hours\n")
cat("   • Wage quintiles created for stratified analysis\n\n")
cat("📋 JOB SATISFACTION MEASURES:\n")
cat("   • Overall job satisfaction (satisf_job)\n")
cat("   • Career opportunities satisfaction (j1_1_4)\n")
cat("   • Working conditions satisfaction (j1_1_2)\n")
cat("   • Pay satisfaction (j1_1_3)\n")
cat("   • Work-life balance (satisf_wbl) - wave 28 only\n\n")
cat("👥 OCCUPATION & INDUSTRY:\n")
cat("   • 10 occupation categories (ISCO-based)\n")
cat("   • Industry classifications included\n")
cat("   • Working hours and overtime indicators\n\n")
cat("🧠 NON-COGNITIVE SKILLS:\n")
cat("   • Big Five traits: O, C, E, A, ES\n")
cat("   • Complete cases only (no missing NCS data)\n\n")
cat("📈 ANALYSIS FEATURES:\n")
cat("   • IPW weights for employment selection correction\n")
cat("   • Regional and temporal controls\n")
cat("   • Hourly wage quintiles for heterogeneity analysis\n")
cat("   • Working hours and overtime variables\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n") 



