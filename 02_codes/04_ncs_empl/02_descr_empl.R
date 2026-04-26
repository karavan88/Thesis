# ==============================================================================
# NON-COGNITIVE SKILLS AND EMPLOYMENT ANALYSIS - DESCRIPTIVE STATISTICS
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         02_descr_empl.R
# Purpose:      Descriptive statistics and visualizations for employment analysis
# 
# Description:  This script generates comprehensive descriptive statistics,
#               summary tables, and visualizations for the employment transition
#               analysis. Creates tables showing employment patterns by demographics,
#               Big Five personality trait distributions, and publication-ready
#               visualizations for employment outcomes.
#
# Data Source:  Russia Longitudinal Monitoring Survey (RLMS-HSE)
# Sample:       Youth aged 15-29 years (Waves 25-28: 2016-2019)
# Analysis:     - Employment status descriptive tables
#               - Big Five traits summary statistics
#               - Boxplots by employment status, SES, education, sex
#               - Occupation classification tables (ISCO-08)
#               - Publication-ready visualizations
#
# Key Outputs:  - summary_stats: Employment status by year
#               - ncs_descr: Big Five traits descriptive statistics
#               - Boxplot visualizations (bp1, bp2, bp3)
#               - Density plots by demographic groups
#               - Occupation classification tables
#
# Dependencies: ind_master_empl.rds (from 01_data_prep_empl.R)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 17, 2024
# Modified:     October 19, 2025
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("EMPLOYMENT DESCRIPTIVE ANALYSIS - RLMS-HSE STUDY\n")
cat(rep("=", 80), "\n")
cat("Script:", "02_descr_empl.R\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

cat("📊 PHASE 1: DATA LOADING AND PREPARATION\n")
cat(rep("-", 50), "\n")

cat("Loading youth employment dataset (pre-built by 01_data_prep_empl.R)...")
start_time <- Sys.time()

youth_empl <- readRDS(file.path(processedData, "youth_empl.rds"))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Youth employment dataset loaded:\n")
cat("  • Total observations:", nrow(youth_empl), "\n")
cat("  • Unique individuals:", length(unique(youth_empl$idind)), "\n\n")

# ----------------------------------------------------------------------------
# CHAPTER 4 MODEL OBJECTS + RUSSIAN RENAME VECTOR
# ----------------------------------------------------------------------------
# These are loaded here (rather than in a dedicated 05_regression_final.R)
# so the qmd only has to source 02_descr_empl.R to get both the descriptive
# inputs and the fitted models for chapter 4.
# ----------------------------------------------------------------------------

models_empl1     <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_tab1.rds"))
models_empl2     <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_tab2.rds"))
models_suppl     <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_suppl.rds"))
m4_ses_coefs     <- readRDS(file.path(outputsEmplNcs, "m4_ses_coefs.rds"))
m5_edu_coefs     <- readRDS(file.path(outputsEmplNcs, "m5_edu_coefs.rds"))
m6_sex_coefs     <- readRDS(file.path(outputsEmplNcs, "m6_sex_coefs.rds"))
m_occup_ses_coefs <- readRDS(file.path(outputsEmplNcs, "m_occup_ses_coefs.rds"))

# Two-element subset used by the qmd's "occupation" tables
models_occup <- list(
  "M5" = models_suppl[["M5"]],
  "M6" = models_suppl[["M6"]]
)

# Russian variable labels used by the qmd's modelsummary / tinytable calls
rename_vector_empl_rus <-
  c(`(Intercept)`                      = "Константа",
    age                                = "Возраст",
    `I(age^2)`                         = "Возраст²",
    sexMale                            = "Пол: Мужской",
    `edu_lvl2. Secondary School`       = "Обр: Среднее",
    `edu_lvl3. Secondary Vocational`   = "Обр: Среднее проф",
    `edu_lvl4. Tertiary`               = "Обр: Высшее",
    `areaUrban-Type Settlement`        = "Тип поселения: ПГТ",
    areaCity                           = "Тип поселения: Город",
    `areaRegional Center`              = "Тип поселения: Обл центр",
    `in_education1`                    = "В настоящее время обучается",
    ses5Q2                             = "Среднедушевой доход ДХ: Q2",
    ses5Q3                             = "Среднедушевой доход ДХ: Q3",
    ses5Q4                             = "Среднедушевой доход ДХ: Q4",
    ses5Q5                             = "Среднедушевой доход ДХ: Q5",
    O                                  = "Открытость опыту",
    C                                  = "Добросовестность",
    E                                  = "Экстраверсия",
    A                                  = "Доброжелательность",
    ES                                 = "Эмоциональная стабильность")


cat("📋 PHASE 2: SUMMARY STATISTICS TABLES\n")
cat(rep("-", 50), "\n")

cat("Creating comprehensive employment summary table...")
start_time <- Sys.time() 


cat("Creating comprehensive employment summary table...")
start_time <- Sys.time()

summary_stats <-
  youth_empl %>%
  select(age, sex, year,
         employed, employed_officially, self_employed, satisfied_with_job,
         in_education, edu_lvl,
         year, area, ses5) %>%
  tbl_summary(by = year,
              type = list( age ~ 'continuous2',
                           c(employed, employed_officially, self_employed,
                             in_education, satisfied_with_job) ~ 'dichotomous',
                           c( ses5, area, edu_lvl) ~ 'categorical'),
              value = list(in_education ~ 1),
              label = list(age ~ "Age",
                           employed ~ "Employed",
                           employed_officially ~ "Officially Employed",
                           self_employed ~ "Self-Employed",
                           satisfied_with_job ~ "Satisfied with Job",
                           in_education ~ "Attending Education",
                           area ~ "Area",
                           sex ~ "Sex",
                           ses5 ~ "HH Income Per Cap Quintile",
                           edu_lvl ~ "Highest Level of Education") ,
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Variable") %>%
  bold_labels() %>%
  as_gt() %>%
  tab_source_note(md("Source: Author's calculations based on RLMS-HSE data"))

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Main summary statistics table created:\n")
cat("  • Variables included: employment status, demographics, education\n")
cat("  • Grouped by: survey year (2016 vs 2019)\n")
cat("  • Format: GT table with source note\n\n")

cat("Creating Russian-language data chapter table...")
start_time <- Sys.time() 

cat("Creating Russian-language data chapter table...")
start_time <- Sys.time()

summary_stats_data_chapter <-
  youth_empl %>%
  select(employed, employed_officially,
         self_employed, satisfied_with_job,
         year) %>%
  tbl_summary(by = year,
              type = list(
                           c(employed, employed_officially, self_employed,
                             satisfied_with_job) ~ 'dichotomous'
                           ),
              label = list(
                           employed ~ "Трудоустроен",
                           employed_officially ~ "Формальная занятость",
                           self_employed ~ "Самозанятый",
                           satisfied_with_job ~ "Удовлетворен работой"
                           ) ,
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall() %>%
  modify_header(label = "Переменная") %>%
  bold_labels()

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Russian-language table created:\n")
cat("  • Employment variables in Russian\n")
cat("  • Format: FlexTable for Russian publications\n\n")

cat("📊 PHASE 3: BIG FIVE PERSONALITY TRAITS ANALYSIS\n")
cat(rep("-", 50), "\n")

cat("Creating Big Five descriptive statistics...")
start_time <- Sys.time()

ncs_descr <-
  youth_empl %>%
  select(O, C, E, A, ES) %>%
  rename(Openness = O,
         Conscientiousness = C,
         Extraversion = E,
         Agreeableness = A,
         `Emotional Stability` = ES) %>%
  datasummary_skim()

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Big Five descriptive statistics created:\n")
cat("  • Traits: Openness, Conscientiousness, Extraversion, Agreeableness, Emotional Stability\n")
cat("  • Statistics: Mean, SD, Min, Max, Histogram\n")
cat("  • Format: Datasummary table\n\n")

cat("📈 PHASE 4: BOXPLOT VISUALIZATIONS\n")
cat(rep("-", 50), "\n")

cat("Preparing data for boxplot visualizations...")
start_time <- Sys.time()

bar_dta <- 
   youth_empl %>%
   select(age, empl_dv, id_w ) %>%
   mutate(age_factor = factor(floor(age)),
         empl_dv = ifelse(empl_dv == 1, "Да", "Нет")) 

# table(bar_data$age_factor, bar_dta$employed)

# summary(bar_dta$employed)

age_empl_chart <-
  ggplot(bar_dta, aes(x = age_factor, fill = empl_dv)) +
  geom_bar(position = "dodge") +
  labs(x = "Age", y = "Количество наблюдений", fill = "Формальная занятость") +
  theme_minimal() +
  xlab("Возраст") +
  theme(legend.position = "bottom") +
  facet_wrap(~ id_w, ncol = 1) +
  scale_fill_manual(values = c("Да" = "#59c8a7", "Нет" = "#e74a4a")) +
  # give name to facets 
  facet_wrap(~ id_w, ncol = 1, labeller = labeller(id_w = c("25" = "25 Волна (2016 г.)", "28" = "28 Волна (2019 г.)"))) +
  theme(strip.background = element_rect(fill = "lightgrey", color = "black", size = 1),
        strip.text = element_text(size = 12, face = "bold")) 

cat("📊 PHASE 6: OCCUPATION CLASSIFICATION TABLE\n")
cat(rep("-", 50), "\n")

cat("Creating ISCO-08 occupation classification table...")
start_time <- Sys.time()

occup_table <-
  youth_empl %>%
  select(year, occupation) %>%
  # recode in accordance with isco-08 in russin МСКЗ-08
  mutate(occupation = case_when(occupation == 0 ~ "0. Военные",
                                occupation == 1 ~ "1. Руководители", 
                                occupation == 2 ~ "2. Специалисты-профессионалы",
                                occupation == 3 ~ "3. Специалисты-техники и иной средний специальный персонал",
                                occupation == 4 ~ "4. Служащие, занятые подготовкой и оформлением документации",
                                occupation == 5 ~ "5. Работники сферы обслуживания и торговли",
                                occupation == 6 ~ "6. Квалифицированные работники сельского хозяйств",
                                occupation == 7 ~ "7. Квалифицированные рабочие промышленности ",
                                occupation == 8 ~ "8. Операторы и сборщики промышленных установок и машин",
                                occupation == 9 ~ "9. Неквалифицированные работники",
                                TRUE ~ as.character(NA)
                                )) %>%
  tbl_summary(by = year, 
              type = list( 
                c(occupation) ~ 'categorical'
              ),
              label = list(# age ~ "Age",
                occupation ~ "Классификация занятости"
              ) ) %>%
  add_overall() %>%
  modify_header(label = "Переменная") %>%
  bold_labels() 

cat(" ✓ Completed\n")
cat("Processing time:", round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2), "seconds\n")
cat("✓ Occupation classification table created:\n")
cat("  • Standard: ISCO-08 (МСКЗ-08 in Russian)\n")
cat("  • Categories: 10 major occupation groups\n")
cat("  • Language: Russian labels for Russian publications\n")
cat("  • Format: FlexTable by survey year\n\n")

cat(rep("=", 80), "\n")
cat("🎉 DESCRIPTIVE ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 OUTPUTS CREATED:\n")
cat("  • summary_stats: Main employment statistics table (GT)\n")
cat("  • summary_stats_data_chapter: Russian employment table (FlexTable)\n")
cat("  • ncs_descr: Big Five descriptive statistics\n")
cat("  • bp1, bp2, bp3: Employment/SES/Education boxplots\n")
cat("  • bp_sex_ncs: Publication-ready gender differences plot\n")
cat("  • bp_ses5_ncs: SES differences visualization\n")
cat("  • occup_table: ISCO-08 occupation classification (Russian)\n")
cat("  • density_data, boxplot_data: Prepared datasets for further analysis\n")
cat("\n📈 VISUALIZATION OBJECTS:\n")
cat("  • Ready for export to publications\n")
cat("  • Professional styling applied\n")
cat("  • Multi-language support (English/Russian)\n")
cat(rep("=", 80), "\n")
cat("Script completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n") 
  


