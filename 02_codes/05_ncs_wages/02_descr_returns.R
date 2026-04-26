# ==============================================================================
# NON-COGNITIVE SKILLS AND RETURNS - DESCRIPTIVE ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         02_descr_returns.R
# Purpose:      Descriptive statistics and visualizations for returns analysis
# 
# Description:  This script generates comprehensive descriptive statistics,
#               summary tables, and visualizations for the returns to 
#               non-cognitive skills analysis. Includes wage distributions,
#               NCS profiles, and demographic characteristics of the youth
#               sample from RLMS 2016-2019 data.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 16-29 years (2016-2019)
# Input:        youth_master_returns.rds
#
# Key Outputs:  • Wage distribution plots and summary statistics
#               • NCS descriptive statistics by demographics
#               • Sample characteristics tables
#               • Year-over-year wage comparisons
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 3, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: ggplot2, tinytable, tidyverse
# Runtime:      ~30 seconds - 1 minute
#
# Notes:        Focuses on youth wage patterns and NCS distributions
#               Prepares summary statistics for regression analysis context
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("📈 RETURNS TO NCS - DESCRIPTIVE ANALYSIS\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 02_descr_returns.R\n")
cat("🎯 Purpose: Generate descriptive statistics for returns analysis\n")
cat("📈 Processing: Youth returns data → Summary tables & visualizations\n\n")

# SECTION 1: LOAD AND PREPARE DATA
cat("📊 SECTION 1: LOADING RETURNS ANALYSIS DATA\n")
cat("Loading youth returns dataset for descriptive analysis...\n")
data_start_time <- Sys.time()

# Load the youth returns dataset
youth_master_returns <-
  readRDS(file.path(outputsReturnsNcs, "youth_master_returns.rds"))

youth_descr <- youth_master_returns

data_end_time <- Sys.time()
cat("✅ Data loading completed in", round(difftime(data_end_time, data_start_time, units = "secs"), 2), "seconds\n")
cat("   - Dataset dimensions:", nrow(youth_descr), "rows x", ncol(youth_descr), "columns\n")
cat("   - Age range:", min(youth_descr$age, na.rm = TRUE), "to", max(youth_descr$age, na.rm = TRUE), "years\n")
cat("   - Years included:", paste(sort(unique(youth_descr$year)), collapse = ", "), "\n")
cat("   - Complete wage observations:", sum(!is.na(youth_descr$hourly_wage)), "\n\n")

# SECTION 2: WAGE DISTRIBUTION ANALYSIS
cat("💰 SECTION 2: WAGE DISTRIBUTION ANALYSIS\n")
cat("Creating wage distribution visualizations and statistics...\n")
wage_viz_start <- Sys.time()

# Display basic wage statistics
cat("   - Hourly wage range:", round(min(youth_descr$hourly_wage, na.rm = TRUE), 2), 
    "to", round(max(youth_descr$hourly_wage, na.rm = TRUE), 2), "RUB\n")
cat("   - Mean hourly wage:", round(mean(youth_descr$hourly_wage, na.rm = TRUE), 2), "RUB\n")
cat("   - Median hourly wage:", round(median(youth_descr$hourly_wage, na.rm = TRUE), 2), "RUB\n")

# Probability density plot of hourly wage by year
cat("   - Generating wage density plot by year...\n")
wage_density <-
  ggplot(youth_descr, aes(x = hourly_wage, color = as.factor(year), fill = as.factor(year))) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(x = "Hourly Wage (RUB)", y = "Density", color = "Year", fill = "Year") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  ggtitle("Hourly Wage Distribution by Year")

wage_viz_end <- Sys.time()
cat("✅ Wage distribution analysis completed in", round(difftime(wage_viz_end, wage_viz_start, units = "secs"), 2), "seconds\n\n")

# SECTION 3: WAGE SUMMARY STATISTICS TABLE
cat("📊 SECTION 3: GENERATING WAGE SUMMARY STATISTICS TABLE\n")
cat("Creating comprehensive wage statistics by year...\n")
table_start <- Sys.time()

wage_summary <-
  youth_descr %>%
  group_by(year) %>%
  summarise(
    N = n(),
    Min = min(hourly_wage, na.rm = TRUE),
    Q1 = quantile(hourly_wage, 0.25, na.rm = TRUE),
    Median = median(hourly_wage, na.rm = TRUE),
    Mean = mean(hourly_wage, na.rm = TRUE),
    Q3 = quantile(hourly_wage, 0.75, na.rm = TRUE),
    Max = max(hourly_wage, na.rm = TRUE),
    SD = sd(hourly_wage, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -year, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = year, values_from = Value) %>%
  arrange(match(Statistic, c("N", "Min", "Q1", "Median", "Mean", "Q3", "Max", "SD"))) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  tinytable::tt(
    caption = "Hourly Wage Summary Statistics by Year (2019 RUB)",
    notes = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы."
  )

table_end <- Sys.time()
cat("✅ Wage summary table generated in", round(difftime(table_end, table_start, units = "secs"), 2), "seconds\n")
cat("   - Statistics included: N, Min, Q1, Median, Mean, Q3, Max, SD\n")
cat("   - Years compared:", paste(sort(unique(youth_descr$year)), collapse = ", "), "\n")
cat("   - Currency: 2019 RUB (inflation-adjusted)\n\n")
  # 
  # cols_label(
  #   Statistic = "Statistic",
  #   `2016` = "2016",
  #   `2019` = "2019"
  # ) %>%
  # fmt_number(
  #   columns = vars(`2016`, `2019`),
  #   decimals = 2
  # ) %>%
  # tab_source_note(md("Source: Author's calculations based on RLMS-HSE data"))

# SECTION 4: DEMOGRAPHIC CHARACTERISTICS TABLE
cat("👥 SECTION 4: GENERATING DEMOGRAPHIC CHARACTERISTICS TABLE\n")
cat("Creating sample characteristics summary by year...\n")
demo_start <- Sys.time()

summary_stats <-
  youth_master_returns %>%
  select(sex, age, area, edu_lvl, exp_imp, marital_status, year) %>%
  tbl_summary(by = year,
              type = list( age ~ 'continuous2',
                           exp_imp ~ 'continuous2',
                           c(sex, area, edu_lvl, marital_status) ~ 'categorical'),
              label = list(age ~ "Age",
                           exp_imp ~ "Experience",
                           area ~ "Area",
                           sex ~ "Sex",
                           edu_lvl ~ "Highest Level of Education",
                           marital_status ~ "Marital Status"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_source_note(md("Source: Author's calculations based on RLMS-HSE data"))

demo_end <- Sys.time()
cat("✅ Demographic characteristics table completed in", round(difftime(demo_end, demo_start, units = "secs"), 2), "seconds\n")
cat("   - Variables included: age, experience, sex, area, education, marital status\n")
cat("   - Comparison by year (2016 vs 2019)\n")
cat("   - Continuous variables shown as mean (SD)\n\n")


# SECTION 5: NON-COGNITIVE SKILLS DISTRIBUTION ANALYSIS
cat("🧠 SECTION 5: NON-COGNITIVE SKILLS DISTRIBUTION ANALYSIS\n")
cat("Creating NCS distribution plots and statistics...\n")
ncs_start <- Sys.time()

youth_long <- 
  youth_descr %>%
  # create a variable NCS and add there all big five traits
  pivot_longer(cols = c(O, C, E, A, ES), 
               names_to = "NCS", values_to = "Value") %>%
  # recode NCS into full names
  mutate(NCS = case_when(
    NCS == "O" ~ "Openness",
    NCS == "C" ~ "Conscientiousness",
    NCS == "E" ~ "Extraversion",
    NCS == "A" ~ "Agreeableness",
    NCS == "ES" ~ "Emotional Stability"
  ))

cat("   - Data reshaped for NCS analysis\n")
cat("   - NCS variables: Openness, Conscientiousness, Extraversion, Agreeableness, Emotional Stability\n")

# Plot the distribution of non-cognitive skills
cat("   - Generating NCS distribution histogram...\n")
ncs_hist <-
  ggplot(youth_long, aes(x = Value)) +
  geom_histogram(binwidth = 0.5, fill = "#3498DB", alpha = 0.7, color = "white") +
  facet_wrap(~ NCS, scales = "free_x") +
  labs(x = "NCS Score",
       y = "Frequency") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Distribution of Non-Cognitive Skills")

ncs_end <- Sys.time()
cat("✅ NCS distribution analysis completed in", round(difftime(ncs_end, ncs_start, units = "secs"), 2), "seconds\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 RETURNS TO NCS DESCRIPTIVE ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 ANALYSES COMPLETED:\n")
cat("   • Wage distribution analysis by year\n")
cat("   • Comprehensive wage summary statistics table\n")
cat("   • Demographic characteristics comparison table\n")
cat("   • Non-cognitive skills distribution analysis\n\n")
cat("📈 VISUALIZATIONS CREATED:\n")
cat("   • Hourly wage density plot by year\n")
cat("   • NCS distribution histograms (Big Five traits)\n\n")
cat("📋 TABLES GENERATED:\n")
cat("   • Wage summary statistics (2016 vs 2019)\n")
cat("   • Sample demographic characteristics\n\n")
cat("🔍 KEY FINDINGS SUMMARY:\n")
cat("   • Sample size:", nrow(youth_descr), "observations\n")
cat("   • Years covered: 2016, 2019\n")
cat("   • Mean hourly wage:", round(mean(youth_descr$hourly_wage, na.rm = TRUE), 2), "RUB (2019)\n")
cat("   • Age range:", min(youth_descr$age, na.rm = TRUE), "to", max(youth_descr$age, na.rm = TRUE), "years\n")
cat("   • Complete NCS data for regression analysis\n\n")
cat("🔄 NEXT STEPS:\n")
cat("   • Run 03_regression_returns.R for quantile regression analysis\n")
cat("   • Descriptive statistics ready for manuscript tables\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

