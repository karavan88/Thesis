# ==============================================================================
# NON-COGNITIVE SKILLS AND RETURNS - OUTPUT GENERATION (RUSSIAN VERSION)
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         05_output_returns_rus.R (Russian translation of 04_outputs_returns.R)
# Purpose:      Generate publication-ready tables and plots for returns analysis
# 
# Description:  This script processes quantile regression results to create
#               formatted publication tables and coefficient plots. Transforms
#               raw coefficient CSV files into publication-ready outputs with
#               proper formatting, significance stars, and Russian labels for
#               domestic academic manuscripts and reports.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)  
# Input Files:  • m1_coefs.csv (Baseline model coefficients)
#               • m1_ipw_coefs.csv (IPW baseline coefficients)
#               • m2_coefs.csv (Education-extended coefficients)
#               • m2_ipw_coefs.csv (Education-extended IPW coefficients)
#               • m4_coefs_edu.csv (Education-stratified coefficients)
#               • m7_ipw_gender_ncs_int.csv (Gender interaction coefficients)
#               • m_lc_ipw_coefs.csv (Lifecycle analysis coefficients)
#
# Key Outputs:  • Publication-ready quantile regression tables with Russian labels
#               • Coefficient plots across quantiles
#               • Gender and education heterogeneity visualizations
#               • Lifecycle analysis tables and plots
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      October 31, 2024
# Modified:     October 19, 2025
# Version:      2.0 (Russian version - variable labels translated for domestic publication)
#
# Dependencies: tidyverse, tinytable, ggplot2
# Runtime:      ~2-3 minutes
#
# Notes:        Identical to 04_outputs_returns.R but with Russian variable labels
#               for domestic publication in Russian academic journals
#               Creates coefficient plots for visual presentation
#
# ==============================================================================

# SCRIPT INITIALIZATION
script_start_time <- Sys.time()

cat(rep("=", 80), "\n")
cat("📊 RETURNS TO NCS - OUTPUT GENERATION\n")
cat(rep("=", 80), "\n")
cat("📅 Start time:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("📊 Script: 05_output_returns_rus.R\n")
cat("🎯 Purpose: Generate publication-ready tables and plots\n")
cat("📈 Processing: Regression coefficients → Tables & Visualizations\n\n")

# SECTION 1: SETUP AND SAMPLE STATISTICS
cat("🔧 SECTION 1: SETUP AND SAMPLE STATISTICS\n")
cat("Configuring output paths and extracting sample statistics...\n")
setup_start <- Sys.time()

# All chapter-5 model outputs (summaries + coef CSVs + raw fits) live under
# the `thesis` subfolder, written by 03_fit_models_returns.R and
# 04_summarize_models_returns.R. The two analytical samples themselves stay
# at the top level of returns_outputs/.
youthOutput <- returns_thesis_out

youth_master_returns <- read_rds(file.path(outputsReturnsNcs, "youth_master_returns.rds"))
ind_master_returns   <- readRDS(file.path(outputsReturnsNcs, "ind_master_returns.rds"))

# ---------------------------------------------------------------------------
# Guard: only build the chapter-5 publication tables if 03_fit_models_returns.R
# and 04_summarize_models_returns.R have already populated youthOutput.
# Otherwise emit placeholder tables/plots (matching the shapes the qmd
# consumes) so the book still renders before the 5-hour fit has run.
# ---------------------------------------------------------------------------
.ch5_summary_files <- file.path(youthOutput, c(
  "m1_ipw_summaries.rds", "m2_ipw_summaries.rds",
  "m3_edu_ipw_summaries.rds", "m4_ipw_summaries.rds",
  "m_lc_ipw_summaries.rds"
))
.ch5_summaries_ready <- all(file.exists(.ch5_summary_files))

if (.ch5_summaries_ready) {

m1_ipw_summaries <- readRDS(file.path(youthOutput, "m1_ipw_summaries.rds"))
m2_ipw_summaries <- readRDS(file.path(youthOutput, "m2_ipw_summaries.rds"))
m3_edu_ipw_summaries <- readRDS(file.path(youthOutput, "m3_edu_ipw_summaries.rds"))
m4_ipw_summaries <- readRDS(file.path(youthOutput, "m4_ipw_summaries.rds"))
m_lc_ipw_summaries <- readRDS(file.path(youthOutput, "m_lc_ipw_summaries.rds"))

# Extract sample statistics
nrow_base <- nrow(youth_master_returns)
ngrp_base <- as.character(length(unique(youth_master_returns$idind)))

setup_end <- Sys.time()
cat("✅ Setup completed in", round(difftime(setup_end, setup_start, units = "secs"), 2), "seconds\n")  
cat("   - Output directory:", youthOutput, "\n")
cat("   - Sample size:", nrow_base, "observations\n")
cat("   - Unique individuals:", ngrp_base, "\n\n")

# SECTION 2: DEFINE COLUMN STRUCTURE AND VARIABLE LABELS
cat("🔤 SECTION 2: DEFINING COLUMN STRUCTURE AND VARIABLE LABELS\n")
cat("Setting up quantile regression output format...\n")
labels_start <- Sys.time()

# Define standard column names for quantile regression results
new_names <- c("variable", 
               "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
               "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
               "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
               "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
               "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value")

cat("✅ Column structure defined for 5 quantiles (10%, 25%, 50%, 75%, 90%)\n")
cat("   - Each quantile includes: estimate, std.error, CI bounds, p.value\n\n")

format_model_stat <- function(x, digits = 3) {
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

extract_lqmm_metadata <- function(summary_obj) {
  list(
    random_var = as.numeric(lqmm:::VarCorr.lqmm(summary_obj))[1],
    residual = as.numeric(summary_obj[["scale"]]),
    loglik = as.numeric(summary_obj[["logLik"]]),
    nobs = as.integer(summary_obj[["nobs"]]),
    ngroups = as.integer(summary_obj[["ngroups"]])
  )
}

make_info_row <- function(label, columns, values) {
  tibble(variable = label, !!!stats::setNames(as.list(unname(values)), columns))
}

make_section_row <- function(label, columns) {
  tibble(variable = label, !!!stats::setNames(as.list(rep("", length(columns))), columns))
}

build_model_info_rows <- function(summary_list, column_map, fixed_effects = c("Регион" = "Контролируется")) {
  metadata <- purrr::map(summary_list, extract_lqmm_metadata)

  values_for <- function(field, formatter) {
    purrr::map_chr(names(column_map), function(model_name) {
      formatter(metadata[[model_name]][[field]])
    })
  }

  display_columns <- unname(column_map)

  fixed_rows <- purrr::imap_dfr(fixed_effects, function(value, term) {
    make_info_row(term, display_columns, rep(value, length(display_columns)))
  })

  random_var_vals <- purrr::map_chr(names(column_map), function(model_name) {
    md <- metadata[[model_name]]
    total <- md$random_var + md$residual
    share <- ifelse(total > 0, 100 * md$random_var / total, NA_real_)
    sprintf("%s (%.1f%%)", format_model_stat(md$random_var, 3), share)
  })

  residual_vals <- purrr::map_chr(names(column_map), function(model_name) {
    md <- metadata[[model_name]]
    total <- md$random_var + md$residual
    share <- ifelse(total > 0, 100 * md$residual / total, NA_real_)
    sprintf("%s (%.1f%%)", format_model_stat(md$residual, 3), share)
  })

  random_rows <- bind_rows(
    make_info_row("Var(u0): Индивидуальный ID", display_columns, random_var_vals),
    make_info_row("Var(Residual)", display_columns, residual_vals)
  )

  quality_rows <- bind_rows(
    make_info_row("Log-likelihood", display_columns, values_for("loglik", function(x) format_model_stat(x, 2))),
    make_info_row("N", display_columns, values_for("nobs", as.character)),
    make_info_row("Количество групп", display_columns, values_for("ngroups", as.character))
  )

  bind_rows(
    fixed_rows,
    make_section_row("Случайные эффекты", display_columns),
    random_rows,
    make_section_row("Качество модели", display_columns),
    quality_rows
  )
}

add_section_blocks <- function(table_df, summary_list, column_map, fixed_effects = c("Регион" = "Контролируется")) {
  display_columns <- unname(column_map)
  bind_rows(
    make_section_row("Фиксированные эффекты", display_columns),
    table_df %>% mutate(variable = as.character(variable)),
    build_model_info_rows(summary_list, column_map, fixed_effects = fixed_effects)
  )
}

# SECTION 3: BASELINE MODEL TABLE GENERATION
cat("📊 SECTION 3: BASELINE MODEL TABLE GENERATION\n")
cat("Processing baseline quantile regression results (M1)...\n")
baseline_start <- Sys.time()

# Dead non-IPW table (base_reg) that read m1_coefs.csv was removed: that CSV
# is no longer produced by the new 03/04 pipeline (only IPW results land in
# 03_output/returns_outputs/thesis/), and no qmd ever consumed base_reg.

base_reg_ipw <-
  read_csv(file.path(youthOutput, "m1_ipw_coefs.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q10 = paste0(round(q10_estimate, 3), " (", round(q10_std.error, 2), ")", q10_p.value),
         Q25 = paste0(round(q25_estimate, 3), " (", round(q25_std.error, 2), ")", q25_p.value),
         Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q10, Q25, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)"  ~ "Константа",
                              variable == "exp_imp"      ~ "Опыт",
                              variable == "I(exp_imp^2)" ~ "Опыт²",
                              variable == "areaCity"     ~ "Тип поселения: Город",
                              variable == "areaUrban-Type Settlement"      ~ "Тип поселения: ПГТ",
                              variable == "areaRegional Center"            ~ "Тип поселения: Райцентр",
                              variable == "sexMale"      ~ "Пол: Мужской",
                              variable == "marital_status2. Married/Civil partnership" ~ "Семья: Женат/замужем",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Семья: Разведен/вдовец",
                              variable == "O"           ~ "Открытость",
                              variable == "C"           ~ "Добросовестность",
                              variable == "E"           ~ "Экстраверсия",
                              variable == "A"           ~ "Доброжелательность",
                              variable == "ES"          ~ "Эмоциональная стабильность",
                              TRUE ~ variable)) %>%
  mutate(variable = as.character(variable)) %>%
  add_section_blocks(
    m1_ipw_summaries,
    c(q10 = "Q10", q25 = "Q25", q50 = "Q50", q75 = "Q75", q90 = "Q90"),
    fixed_effects = c("Регион" = "Контролируется")
  )

# View(base_reg_ipw)

### Table 3 and 4: Extended model ####

# write_csv(m3_coefs, file.path(youthOutput, "m3_coefs.csv"))
# write_csv(m3_ipw_coefs, file.path(youthOutput, "m3_ipw_coefs.csv"))


# Dead non-IPW extended table (extd_reg) that read m2_coefs.csv was removed:
# the new pipeline writes only IPW results to thesis/, and no qmd consumed it.

extd_reg_ipw <-
  read_csv(file.path(youthOutput, "m2_ipw_coefs.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q10 = paste0(round(q10_estimate, 3), " (", round(q10_std.error, 2), ")", q10_p.value),
         Q25 = paste0(round(q25_estimate, 3), " (", round(q25_std.error, 2), ")", q25_p.value),
         Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q10, Q25, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)"                     ~ "Константа",
                              variable == "exp_imp"                         ~ "Опыт",
                              variable == "I(exp_imp^2)"                    ~ "Опыт²",
                              variable == "areaCity"     ~ "Тип поселения: Город",
                              variable == "areaUrban-Type Settlement"      ~ "Тип поселения: ПГТ",
                              variable == "areaRegional Center"            ~ "Тип поселения: Райцентр",
                              variable == "sexMale"      ~ "Пол: Мужской",
                              variable == "marital_status2. Married/Civil partnership" ~ "Семья: Женат/замужем",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Семья: Разведен/вдовец",
                              variable == "edu_lvl2. Secondary School"      ~ "Образование: Среднее",
                              variable == "edu_lvl3. Secondary Vocational"  ~ "Образование: Среднее проф.",
                              variable == "edu_lvl4. Tertiary"              ~ "Образование: Высшее",
                              variable == "O"           ~ "Открытость",
                              variable == "C"           ~ "Добросовестность",
                              variable == "E"           ~ "Экстраверсия",
                              variable == "A"           ~ "Доброжелательность",
                              variable == "ES"          ~ "Эмоциональная стабильность")) %>%
  mutate(variable = factor(variable, levels = c("Константа", "Опыт", "Опыт²",
                                                "Тип поселения: ПГТ", "Тип поселения: Город", "Тип поселения: Райцентр",
                                                "Пол: Мужской", "Семья: Женат/замужем", "Семья: Разведен/вдовец",
                                                "Образование: Среднее", "Образование: Среднее проф.", "Образование: Высшее", 
                                                "Открытость", "Добросовестность",
                                                "Экстраверсия", "Доброжелательность", "Эмоциональная стабильность"))) %>%
  mutate(variable = as.character(variable)) %>%
  add_section_blocks(
    m2_ipw_summaries,
    c(q10 = "Q10", q25 = "Q25", q50 = "Q50", q75 = "Q75", q90 = "Q90"),
    fixed_effects = c("Регион" = "Контролируется")
  )



# View(extd_reg_ipw)

### Figure 1 NCS by Edu Level ####


# returns to NCS by education level
extd_reg_ncs_edu <- 
  read_csv(file.path(youthOutput, "m3_coefs_edu_ipw.csv")) %>%
  # filter only NCS in variable
  filter(!str_detect(variable, "area")) %>%
  filter(str_detect(variable, "O|C|E|A|ES")) %>%
  # remove from the variable everything after capital letters
  mutate(variable = str_extract(variable, "[A-Z]+")) %>%
  # mutate by changing the NCS values in variable
  mutate(variable = case_when(variable == "O" ~ "Открытость",
                              variable == "C" ~ "Добросовестность",
                              variable == "E" ~ "Экстраверсия",
                              variable == "A" ~ "Доброжелательность",
                              variable == "ES" ~ "Эмоциональная стабильность")) %>%
  drop_na() 

names(extd_reg_ncs_edu) <- c(new_names, "model")

# View(extd_reg_ncs_edu)

# Assuming your data is stored in a dataframe called `data`
edu_models_long <- 
  extd_reg_ncs_edu %>%
  pivot_longer(
    cols = starts_with("q"),       # Select all columns starting with "q"
    names_to = c("quantile", ".value"), # Split column names into 'model' and actual variable names
    names_pattern = "q(\\d+)_(.+)"  # Regex to extract quantile (q10, q25, etc.) and variable names
  ) 

# add the values to plot for the chart if the p is <0.1
edu_models_long$text = ifelse(edu_models_long$p.value < 0.1, 
                              round(edu_models_long$estimate*100, 1), as.numeric(NA))

# View(edu_models_long)

t <-
  edu_models_long %>%
  select(variable, model, quantile, estimate, p.value) %>%
  mutate_if(is.numeric, round, 3)

# View(t)

write_csv(t, file.path(youthOutput, "edu_ncs_fig.csv"))

# View(t)

fig_edu_ncs <-
  ggplot(edu_models_long, aes(x = estimate * 100, y = variable)) +
  geom_errorbarh(aes(xmin = ci.low * 100, xmax = ci.upp * 100),
                 height = 0.3) +                                        # Smaller whiskers on error bars
  geom_point(aes(fill = p.value < 0.1), shape = 21, color = "black", size = 6) +  # Use fill with black outline
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "white"),    # Use TRUE/FALSE for manual fill
                    labels = c("Insig", "Sig (p<0.1)")) +                       # Set legend labels
  # add geom text for the variable text
  geom_text(aes(label = text), color = "black", size = 3) +            # Add text for significant variables
  #geom_vline(xintercept = 0, linetype = "dashed") +                     # Add vertical dashed line at 0
  facet_grid(quantile ~ model) +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(axis.text.y = element_text(color = "black"),                    # Set y-axis text to black
        legend.position = "bottom",                                     # Position legend at the bottom
        legend.title = element_blank())                                 # Remove legend title


### Figure 2 Returns to NCs by Gender ####

# gender_regs <-
#   read_csv(file.path(youthOutput, "m6_gender_coefs.csv")) %>%
#   mutate(text = ifelse(p.value < 0.1, estimate*100, as.numeric(NA))) %>%
#   # filter only NCS in variable
#   filter(str_detect(variable, "O|C|E|A|ES")) %>%
#   # mutate by changing the NCS values in variable
#   mutate(variable = case_when(variable == "O" ~ "Openness",
#                               variable == "C" ~ "Conscientiousness",
#                               variable == "E" ~ "Extraversion",
#                               variable == "A" ~ "Agreeableness",
#                               variable == "ES" ~ "Emotional Stability"))
# 
# 
# fig_gender_ncs <-
#   ggplot(gender_regs, aes(x = estimate * 100, y = variable)) +
#   geom_errorbarh(aes(xmin = ci.low * 100, xmax = ci.upp * 100),
#                  height = 0.3) +                                        # Smaller whiskers on error bars
#   geom_point(aes(fill = p.value < 0.1), shape = 21, color = "black", size = 4.5) +  # Use fill with black outline
#   scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"),    # Use TRUE/FALSE for manual fill
#                     labels = c("Insig", "Sig")) +                       # Set legend labels
#   # add geom text for the variable text
#   geom_text(aes(label = text), color = "white", size = 2.2) +            # Add text for significant variables
#   #geom_vline(xintercept = 0, linetype = "dashed") +                     # Add vertical dashed line at 0
#   facet_grid(quantile ~ gender) +
#   theme_bw() +
#   xlab("") +
#   ylab("") +
#   theme(axis.text.y = element_text(color = "black"),                    # Set y-axis text to black
#         legend.position = "bottom",                                     # Position legend at the bottom
#         legend.title = element_blank())                                 # Remove legend title

### Table 5 Interaction between NCS and Gender

gend_int_tab <-
  read_csv(file.path(youthOutput, "m4_ipw_sex_ncs_int.csv")) %>%
  select(variable, contains("estimate"), contains("std.error"), contains("p.value")) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q50 = paste0(round(q50_estimate, 3), " (", round(q50_std.error, 2), ")", q50_p.value),
         Q75 = paste0(round(q75_estimate, 3), " (", round(q75_std.error, 2), ")", q75_p.value),
         Q90 = paste0(round(q90_estimate, 3), " (", round(q90_std.error, 2), ")", q90_p.value)) %>%
  select(variable, Q50, Q75, Q90) %>%
  mutate(variable = case_when(variable == "(Intercept)" ~ "Константа",
                              variable == "exp_imp"         ~ "Опыт",
                              variable == "I(exp_imp^2)"    ~ "Опыт²",
                              variable == "areaCity"     ~ "Тип поселения: Город",
                              variable == "areaUrban-Type Settlement"      ~ "Тип поселения: ПГТ",
                              variable == "areaRegional Center"            ~ "Тип поселения: Райцентр",
                              variable == "sexMale"      ~ "Пол: Мужской",
                              variable == "genderMale"      ~ "Пол: Мужской",
                              variable == "marital_status2. Married/Civil partnership" ~ "Семья: Женат/замужем",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Семья: Разведен/вдовец",
                              variable == "O"           ~ "Открытость",
                              variable == "C"           ~ "Добросовестность",
                              variable == "E"           ~ "Экстраверсия",
                              variable == "A"           ~ "Доброжелательность",
                              variable == "ES"          ~ "Эмоциональная стабильность",
                              variable == "sexMale:O"   ~ "Мужской * Открытость",
                              variable == "sexMale:C"   ~ "Мужской * Добросовестность",
                              variable == "sexMale:E"   ~ "Мужской * Экстраверсия",
                              variable == "sexMale:A"   ~ "Мужской * Доброжелательность",
                              variable == "sexMale:ES"  ~ "Мужской * Эмоциональная стабильность",
                              variable == "genderMale:O"   ~ "Мужской * Открытость",
                              variable == "genderMale:C"   ~ "Мужской * Добросовестность",
                              variable == "genderMale:E"   ~ "Мужской * Экстраверсия",
                              variable == "genderMale:A"   ~ "Мужской * Доброжелательность",
                              variable == "genderMale:ES"  ~ "Мужской * Эмоциональная стабильность",
                              TRUE ~ variable)) %>%
  filter(!is.na(variable), nzchar(variable)) %>%
  mutate(variable = as.character(variable)) %>%
  add_section_blocks(
    m4_ipw_summaries,
    c(q50 = "Q50", q75 = "Q75", q90 = "Q90"),
    fixed_effects = c("Регион" = "Контролируется")
  )

# View(gend_int_tab)

### Life course models ####

lc_models <-
  read_csv(file.path(youthOutput, "m_lc_ipw_coefs.csv")) %>%
  rename(estimate = Value,
         std.error = `Std. Error`,
         p.value = `Pr(>|t|)`) %>%
  select(variable, estimate, std.error, p.value, age_group) %>%
  mutate(across(
    contains("p.value"), 
    ~ case_when(
      . < 0.001 ~ "***",
      . < 0.01  ~ "**",
      . < 0.05  ~ "*",
      . < 0.1   ~ ".",
      is.na(.)  ~ "",    # Handle NA values
      TRUE      ~ ""))) %>%
  mutate(Q50 = paste0(round(estimate, 3), " (", round(std.error, 2), ")", p.value)) %>%
  select(variable, Q50, age_group) %>%
  mutate(variable = case_when(variable == "(Intercept)" ~ "Константа",
                              variable == "exp_imp"         ~ "Опыт",
                              variable == "I(exp_imp^2)"    ~ "Опыт²",
                              variable == "areaCity"     ~ "Тип поселения: Город",
                              variable == "areaUrban-Type Settlement"      ~ "Тип поселения: ПГТ",
                              variable == "areaRegional Center"            ~ "Тип поселения: Райцентр",
                              variable == "sexMale"      ~ "Пол: Мужской",
                              variable == "marital_status2. Married/Civil partnership" ~ "Семья: Женат/замужем",
                              variable == "marital_status3. Divorced/Separated/Widowed" ~ "Семья: Разведен/вдовец",
                              variable == "O"           ~ "Открытость",
                              variable == "C"           ~ "Добросовестность",
                              variable == "E"           ~ "Экстраверсия",
                              variable == "A"           ~ "Доброжелательность",
                              variable == "ES"          ~ "Эмоциональная стабильность")) %>%
  mutate(age_group = case_when(age_group == "30-40" ~ "30-39",
                               age_group == "40-50" ~ "40-49",
                               TRUE                 ~ age_group)) %>%
  pivot_wider(names_from = age_group, values_from = Q50) %>%
  mutate(variable = as.character(variable)) %>%
  add_section_blocks(
    m_lc_ipw_summaries,
    c(age_16_65 = "16-65", age_30_39 = "30-39", age_40_49 = "40-49", age_50_65 = "50-65"),
    fixed_effects = c("Регион" = "Контролируется")
  )


# View(lc_models)




# FINAL SECTION: SUPPLEMENTARY GAM (loaded from disk)
# The GAM is fit by 03_fit_models_returns.R and saved to
# thesis/m_gam_age_model.rds; load it here so the qmd plot binds without
# re-fitting on every render.
gam <- readRDS(file.path(youthOutput, "m_gam_age_model.rds"))

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 RETURNS TO NCS OUTPUT GENERATION COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 TABLES GENERATED:\n")
cat("   • Baseline Model Table (M1): 5 quantiles with Russian labels\n")
cat("   • IPW Baseline Table (M1_IPW): Weighted quantile regression\n")
cat("   • Education-Extended Tables (M2, M2_IPW): With education controls\n")
cat("   • Education-Stratified Tables: By education level\n")
cat("   • Gender Analysis Tables: Male vs Female coefficients\n")
cat("   • Lifecycle Analysis Table: Age-group comparisons\n\n")
cat("🎨 VISUALIZATIONS CREATED:\n")
cat("   • Coefficient plots across quantiles\n")
cat("   • Gender comparison visualizations\n")
cat("   • Education heterogeneity plots\n")
cat("   • Age-wage relationship (GAM smoothing)\n\n")
cat("📁 INPUT FILES PROCESSED:\n")
cat("   • m1_coefs.csv → Baseline quantile table\n")
cat("   • m1_ipw_coefs.csv → IPW baseline table\n")
cat("   • m2_coefs.csv → Education-extended table\n")
cat("   • m2_ipw_coefs.csv → Education-extended IPW table\n")
cat("   • m3_coefs_edu_ipw.csv → Education-stratified tables\n")
cat("   • m4_ipw_sex_ncs_int.csv → Gender interaction tables\n")
cat("   • m_lc_ipw_coefs.csv → Lifecycle analysis table\n\n")
cat("🔍 OUTPUT FEATURES:\n")
cat("   • Publication-ready formatting with significance stars\n")
cat("   • Russian variable labels for domestic publication\n")
cat("   • Quantile-specific coefficient display\n")
cat("   • Sample size and group information included\n")
cat("   • Standard errors in parentheses\n")
cat("   • Controlled variables notation\n\n")
cat("📈 ANALYSIS SCOPE:\n")
cat("   • Sample size:", nrow_base, "observations\n")
cat("   • Unique individuals:", ngrp_base, "\n")
cat("   • Quantiles analyzed: 10%, 25%, 50%, 75%, 90%\n")
cat("   • Models: Baseline, IPW-weighted, Education-extended, Stratified\n")
cat("   • Age range: 16-29 years (youth focus)\n")
cat("   • Extended: 16-65 years (lifecycle analysis)\n\n")
cat("🔄 PUBLICATION READY:\n")
cat("   • Tables formatted for academic manuscripts\n")
cat("   • Plots ready for inclusion in papers\n")
cat("   • All outputs saved and documented\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

} else {

# ---------------------------------------------------------------------------
# Placeholder branch: 03_fit_models_returns.R + 04_summarize_models_returns.R
# have not been run yet, so no *_summaries.rds files exist in
# 03_output/returns_outputs/thesis/. Build empty / "[Не подогнано]" stubs
# matching the shapes the qmd consumes, so chapter 5 still renders.
# ---------------------------------------------------------------------------
message("[ch5] Model summaries not found in ", youthOutput,
        " — chapter 5 tables will render as placeholders. ",
        "Run 03_fit_models_returns.R + 04_summarize_models_returns.R ",
        "and re-render to fill them in.")

.ph_label  <- "[Модели ещё не подогнаны — запустите 03_fit_models_returns.R + 04_summarize_models_returns.R]"
.ph_5q_tab <- data.frame(
  variable = .ph_label,
  Q10 = "—", Q25 = "—", Q50 = "—", Q75 = "—", Q90 = "—",
  stringsAsFactors = FALSE
)
.ph_3q_tab <- data.frame(
  variable = .ph_label,
  Q50 = "—", Q75 = "—", Q90 = "—",
  stringsAsFactors = FALSE
)

base_reg_ipw <- .ph_5q_tab
extd_reg_ipw <- .ph_5q_tab
gend_int_tab <- .ph_3q_tab
lc_models    <- data.frame(
  variable = .ph_label,
  `16-65` = "—", `30-40` = "—", `40-50` = "—", `50-65` = "—",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Long-format placeholder for fig-edu-ncs (ggplot)
edu_models_long <- data.frame(
  variable  = factor(rep(c("Openness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "Emotional Stability"), times = 5)),
  estimate  = NA_real_,
  std.error = NA_real_,
  ci.low    = NA_real_,
  ci.upp    = NA_real_,
  p.value   = NA_real_,
  quantile  = factor(rep(c("Q10", "Q25", "Q50", "Q75", "Q90"), each = 5)),
  model     = factor(rep("[Не подогнано]", 25)),
  stringsAsFactors = FALSE
)

gam <- NULL

# Inline-text scalars used in the chapter narrative
nrow_base <- 0L
ngrp_base <- "0"

}  # end if (.ch5_summaries_ready) else { placeholders }
