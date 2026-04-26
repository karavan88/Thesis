# ==============================================================================
# NON-COGNITIVE SKILLS AND JOB SATISFACTION - DESCRIPTIVE ANALYSIS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         02_descriptive_jobsatisf.R
# Purpose:      Generate descriptive statistics and visualizations for job satisfaction analysis
# 
# Description:  This script creates comprehensive descriptive statistics and 
#               correlation analysis for job satisfaction domains and non-cognitive
#               skills. Produces summary tables by year, correlation matrices,
#               and exploratory visualizations to understand relationships
#               between NCS and various job satisfaction dimensions.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)  
# Input:        youth_job_satisf dataset (from 01_data_prep_jobsatisf.R)
#
# Key Outputs:  • Sample summary tables by year and overall
#               • Descriptive statistics for NCS and satisfaction domains
#               • Correlation matrix of job satisfaction domains
#               • Publication-ready summary tables
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 17, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Added comprehensive logging and professional documentation)
#
# Dependencies: gtsummary, flextable, ggcorrplot, ggplot2
# Runtime:      ~30-45 seconds
#
# Notes:        Creates publication-ready descriptive tables
#               Focuses on employed youth aged 15-29 years
#               Includes Spearman correlation analysis
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()
cat(rep("=", 80), "\n")
cat("📊 JOB SATISFACTION - DESCRIPTIVE ANALYSIS\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 02_descriptive_jobsatisf.R\n")
cat("🎯 Purpose: Generate descriptive statistics and correlations\n")
cat("📈 Processing: Job satisfaction data → Tables & visualizations\n\n")


# SECTION 1: ANALYTICAL SAMPLE PREPARATION
cat("📋 SECTION 1: ANALYTICAL SAMPLE PREPARATION\n")
cat("Creating clean analytical sample for descriptive analysis...\n")
sample_start <- Sys.time()

# Create analytical sample with complete NCS data
original_n <- nrow(youth_job_satisf)

youth_js <- 
  youth_job_satisf %>%
  drop_na(O, C, E, A, ES) %>%
  select(idind, id_w, age, edu_lvl, region, sex, year, wages_imp,
        area,
      hourly_wage_quintile,
        work_hrs_per_week, 
        occupation, industry,
        starts_with("satisf"), 
        j1_1_1, j1_1_2, j1_1_3, j1_1_4,
        O, C, E, A, ES) %>%
  # Fill missing education levels (affects 4 observations)
  mutate(edu_lvl = ifelse(is.na(edu_lvl), "1. No school", edu_lvl)) 

final_n <- nrow(youth_js)
n_dropped <- original_n - final_n
unique_individuals <- length(unique(youth_js$idind))
years_covered <- sort(unique(youth_js$year))

sample_end <- Sys.time()
cat("✅ Sample preparation completed in", round(difftime(sample_end, sample_start, units = "secs"), 2), "seconds\n")
cat("   - Original observations:", original_n, "\n")
cat("   - Final analytical sample:", final_n, "\n")
cat("   - Dropped due to missing NCS:", n_dropped, "\n")
cat("   - Unique individuals:", unique_individuals, "\n")
cat("   - Years covered:", paste(years_covered, collapse = ", "), "\n\n")

# SECTION 2: SAMPLE SUMMARY TABLE BY YEAR
cat("📊 SECTION 2: SAMPLE SUMMARY TABLE BY YEAR\n")
cat("Creating demographic and occupational summary table...\n")
summary_start <- Sys.time()

sample_summary_js <-
  youth_js %>%
  select(age, sex, edu_lvl, area, year, occupation) %>%
  tbl_summary(by = year, 
              type = list( c(age) ~ 'continuous2',
                           c(area, edu_lvl, occupation) ~ 'categorical'),
              label = list(age ~ "Age",
                           area ~ "Area",
                           sex ~ "Sex",
                           edu_lvl ~ "Highest Level of Education",
                           occupation ~ "Occupation") , 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_flex_table()

# Calculate summary statistics for reporting
age_stats <- youth_js %>% 
  summarise(mean_age = round(mean(age), 1),
            sd_age = round(sd(age), 1),
            min_age = min(age),
            max_age = max(age))

edu_distribution <- youth_js %>% 
  count(edu_lvl) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

summary_end <- Sys.time()
cat("✅ Sample summary table created in", round(difftime(summary_end, summary_start, units = "secs"), 2), "seconds\n")
cat("   - Age: Mean", age_stats$mean_age, "(SD", age_stats$sd_age, "), Range", age_stats$min_age, "-", age_stats$max_age, "\n")
cat("   - Education levels:", nrow(edu_distribution), "categories\n")
cat("   - Table includes: demographics, education, area, occupation by year\n\n") 


# SECTION 3: DESCRIPTIVE STATISTICS FOR NCS AND JOB SATISFACTION
cat("🧠 SECTION 3: DESCRIPTIVE STATISTICS FOR NCS AND JOB SATISFACTION\n")
cat("Creating comprehensive descriptive table for key variables...\n")
descr_start <- Sys.time()

# Calculate NCS summary statistics before table creation
ncs_stats <- youth_js %>%
  select(O, C, E, A, ES) %>%
  summarise(across(everything(), list(mean = ~round(mean(., na.rm = TRUE), 2),
                                      sd = ~round(sd(., na.rm = TRUE), 2))))

descr_stats_js <-
  youth_js %>%
  select(O, C, E, A, ES,
    hourly_wage_quintile, work_hrs_per_week,
    j1_1_1, j1_1_2, j1_1_3, j1_1_4, year) %>%
  # Recode job satisfaction variables with meaningful labels
  mutate(across(starts_with("j1_1_"), ~ case_when(. == 1 ~ "1. Very satisfied",
                                                  . == 2 ~ "2. Satisfied",
                                                  . == 3 ~ "3. Neutral",
                                                  . == 4 ~ "4. Dissatisfied",
                                                  . == 5 ~ "5. Very dissatisfied"))) %>%
  tbl_summary(by = year, 
    type = list( c(O, C, E, A, ES, work_hrs_per_week) ~ 'continuous2',
            c(hourly_wage_quintile, j1_1_1, j1_1_2, j1_1_3, j1_1_4) ~ 'categorical'),
              label = list(O ~ "Openness",
                           C ~ "Conscientiousness",
                           E ~ "Extraversion",
                           A ~ "Agreeableness",
                           ES ~ "Emotional Stability",
            hourly_wage_quintile ~ "Hourly Wage Quintile",
            work_hrs_per_week ~ "Weekly Working Hours",
                           j1_1_1 ~ "Satisf: Job Overall", 
                           j1_1_2 ~ "Satisf: Labor Conditions",
                           j1_1_3 ~ "Satisf: Pay", 
                           j1_1_4 ~ "Satisf: Career Opport") , 
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{min}",
                                                    "{median}",
                                                    "{max}"))) %>%
  add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_flex_table()

# Append number of regions directly in the descriptive table (overall and by year)
ft_keys <- descr_stats_js$col_keys
stat_keys <- grep("^stat_", ft_keys, value = TRUE)

region_row <- as.list(rep("", length(ft_keys)))
names(region_row) <- ft_keys

if ("label" %in% ft_keys) {
  region_row[["label"]] <- "Number of regions"
}

if (length(stat_keys) > 0) {
  region_row[[stat_keys[1]]] <- as.character(dplyr::n_distinct(youth_js$region))

  year_levels <- youth_js %>%
    mutate(year = as.character(year)) %>%
    distinct(year) %>%
    arrange(year) %>%
    pull(year)

  region_by_year <- youth_js %>%
    mutate(year = as.character(year)) %>%
    group_by(year) %>%
    summarise(n_regions = dplyr::n_distinct(region), .groups = "drop")

  for (i in seq_along(year_levels)) {
    stat_index <- i + 1
    if (stat_index <= length(stat_keys)) {
      n_regions_i <- region_by_year %>%
        filter(year == year_levels[i]) %>%
        pull(n_regions)
      region_row[[stat_keys[stat_index]]] <- ifelse(length(n_regions_i) == 0, "", as.character(n_regions_i))
    }
  }
}

descr_stats_js <- flextable::add_body_row(
  x = descr_stats_js,
  values = unname(region_row[ft_keys]),
  colwidths = rep(1, length(ft_keys))
)

# Calculate satisfaction distribution
satisf_distribution <- youth_js %>%
  select(starts_with("j1_1_")) %>%
  summarise(across(everything(), ~sum(!is.na(.))))

descr_end <- Sys.time()
cat("✅ Descriptive statistics table created in", round(difftime(descr_end, descr_start, units = "secs"), 2), "seconds\n")
cat("   - Big Five traits: Mean scores and standard deviations by year\n")
cat("   - Job satisfaction domains: Distribution of responses (5-point scale)\n")
cat("   - Complete observations for satisfaction measures:", 
    paste(names(satisf_distribution), satisf_distribution, sep = ":", collapse = ", "), "\n\n") 


# SECTION 4: JOB SATISFACTION CORRELATION ANALYSIS
cat("🔗 SECTION 4: JOB SATISFACTION CORRELATION ANALYSIS\n")
cat("Creating correlation matrix of job satisfaction domains...\n")
corr_start <- Sys.time()

# Prepare data for correlation analysis with meaningful variable names
youth_js_corr_dat <-
  youth_js %>%
  select(starts_with("j1_1_")) %>%
  rename(`Job Satisfaction` = j1_1_1, 
         `Labor Conditions` = j1_1_2,
         `Pay` = j1_1_3, 
         `Career Opportunities` = j1_1_4)

# do the same but with Russian variable names for the plot of coor matrix
youth_js_corr_dat_ru <-
  youth_js %>%
  select(starts_with("j1_1_")) %>%
  rename(`Работа (в целом)` = j1_1_1, 
         `Условия труда` = j1_1_2,
         `Оплата труда` = j1_1_3, 
         `Карьерные возможности` = j1_1_4)

# Calculate Spearman rank correlations (appropriate for ordinal satisfaction scales)
youth_js_corr <-
  cor(youth_js_corr_dat, use = "pairwise.complete.obs", method = "spearman") %>%
  round(2) %>%
  as.data.frame()

# Calculate Spearman rank correlations for Russian variable names
youth_js_corr_ru <-
  cor(youth_js_corr_dat_ru, use = "pairwise.complete.obs", method = "spearman") %>%
  round(2) %>%
  as.data.frame()

# Extract key correlation statistics
n_pairs <- ncol(youth_js_corr_dat) * (ncol(youth_js_corr_dat) - 1) / 2
corr_range <- range(youth_js_corr[lower.tri(youth_js_corr)])
mean_corr <- round(mean(youth_js_corr[lower.tri(youth_js_corr)]), 3)

# Create correlation matrix visualization
js_corr_matrix <-
  ggcorrplot(
  youth_js_corr |> as.matrix(),         # correlation matrix
  hc.order = TRUE,                      # hierarchical clustering of variables
  type = "lower",                       # show only lower triangle
  lab = TRUE,                           # add correlation values
  lab_size = 3,                         # label size
  colors = c("red", "white", "blue"),   # color scheme for negative/positive
  ggtheme = ggplot2::theme_minimal(),
  title = "Job Satisfaction Domains - Spearman Correlations"
)

# create correlation matrix in Russian
js_corr_matrix_ru <-
  ggcorrplot(
    youth_js_corr_ru |> as.matrix(),         # correlation matrix
    hc.order = TRUE,                      # hierarchical clustering of variables
    type = "lower",                       # show only lower triangle
    lab = TRUE,                           # add correlation values
    lab_size = 3,                         # label size
    colors = c("red", "white", "blue"),   # color scheme for negative/positive
    ggtheme = ggplot2::theme_minimal(),
    title = ""
  )

corr_end <- Sys.time()
cat("✅ Correlation analysis completed in", round(difftime(corr_end, corr_start, units = "secs"), 2), "seconds\n")
cat("   - Method: Spearman rank correlation (appropriate for ordinal data)\n")
cat("   - Domains analyzed: Job satisfaction, Labor conditions, Pay, Career opportunities\n")
cat("   - Correlation pairs:", n_pairs, "\n")
cat("   - Correlation range:", round(corr_range[1], 3), "to", round(corr_range[2], 3), "\n")
cat("   - Mean correlation:", mean_corr, "\n")
cat("   - Visualization: Lower triangle with hierarchical clustering\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 JOB SATISFACTION DESCRIPTIVE ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 TABLES GENERATED:\n")
cat("   • Sample summary table: Demographics and occupation by year\n")
cat("   • Descriptive statistics: NCS traits and job satisfaction domains\n")
cat("   • Both tables include overall statistics and year-specific breakdowns\n\n")
cat("🔗 CORRELATION ANALYSIS:\n")
cat("   • Spearman correlation matrix for job satisfaction domains\n")
cat("   • Hierarchically clustered visualization\n")
cat("   • Lower triangle display with correlation coefficients\n\n")
cat("📋 SAMPLE CHARACTERISTICS:\n")
cat("   • Final sample size:", final_n, "observations\n")
cat("   • Unique individuals:", unique_individuals, "\n")
cat("   • Age range:", age_stats$min_age, "-", age_stats$max_age, "years\n")
cat("   • Years covered:", paste(years_covered, collapse = ", "), "\n\n")
cat("🧠 NCS VARIABLES:\n")
cat("   • Big Five personality traits: O, C, E, A, ES\n")
cat("   • Complete cases only (no missing NCS data)\n")
cat("   • Continuous measures with means and standard deviations\n\n")
cat("💼 JOB SATISFACTION DOMAINS:\n")
cat("   • Overall job satisfaction (j1_1_1)\n")
cat("   • Labor conditions satisfaction (j1_1_2)\n")
cat("   • Pay satisfaction (j1_1_3)\n")
cat("   • Career opportunities satisfaction (j1_1_4)\n")
cat("   • 5-point ordinal scales (Very satisfied → Very dissatisfied)\n\n")
cat("📈 READY FOR:\n")
cat("   • Regression analysis with job satisfaction outcomes\n")
cat("   • Multilevel modeling with individual and regional effects\n")
cat("   • Publication-ready descriptive tables\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")



