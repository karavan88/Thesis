# ==============================================================================
# CHAPTER 4 - EMPLOYMENT MODELS (MAIN)
# ==============================================================================
#
# File:         03_regression_empl.R
# Purpose:      Fit chapter-4 mixed-effects employment models on the youth
#               sample produced by 01_data_prep_empl.R.
#
# Models fitted in this script:
#   M1 - Reference: empl_dv ~ NCS + controls + region/idind/age REs
#   M2 - Random slopes of NCS by SES quintile
#   M3 - Random slopes of NCS by education level
#   M4 - Random slopes of NCS by sex
#
# Outputs (saved to 03_output/empl_outputs/):
#   models_ncs_empl_tab1.rds   - list("M1" = m1_empl)
#   models_ncs_empl_tab2.rds   - list("M2"=m2, "M3"=m3, "M4"=m4)
#   m4_ses_coefs.rds           - long-format SES random-slope coefs (% scale)
#   m5_edu_coefs.rds           - long-format education random-slope coefs
#   m6_sex_coefs.rds           - long-format sex random-slope coefs
#
# (File names retained for backward compatibility with the qmd; the model
# numbering inside follows the chapter-4 sequence above.)
#
# Dependencies: lme4, lmerTest, modelsummary, tinytable, tidyverse
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 4 - EMPLOYMENT MODELS\n")
cat("Script: 03_regression_empl.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# ---- Russian rename vector for publication tables ---------------------------
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

# ---- SECTION 1: DATA LOADING -----------------------------------------------
cat("📊 SECTION 1: DATA LOADING\n")
youth_empl <- readRDS(file.path(processedData, "youth_empl.rds"))
cat("✅ Loaded youth_empl:", nrow(youth_empl), "rows,",
    length(unique(youth_empl$idind)), "individuals\n\n")

# ---- SECTION 2: M1 - REFERENCE MODEL ---------------------------------------
cat("🔧 SECTION 2: M1 (NCS + controls)\n")
t0 <- Sys.time()

m1_empl <- lmer(empl_dv ~ 1 +
                  age + I(age^2) +
                  sex + edu_lvl + in_education + area + ses5 +
                  O + C + E + A + ES +
                  (1|region) + (1|idind) + (1|age),
                REML = TRUE, data = youth_empl)

cat("✅ M1 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

models_empl1 <- list("M1" = m1_empl)
saveRDS(models_empl1, file.path(outputsEmplNcs, "models_ncs_empl_tab1.rds"))

# Build the Russian-labelled main-model table (consumed via models_empl1
# in the qmd, but pre-formatted here so 05_regression_final.R is no longer
# needed downstream).
reg_tables_empl_main <-
  modelsummary(models_empl1,
               statistic = "({std.error}) {stars}",
               gof_omit  = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl_rus,
               output    = "tinytable",
               notes     = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.")

# ---- SECTION 3: RANDOM-SLOPE MODELS ----------------------------------------
cat("🔧 SECTION 3: random-slope models M2-M4\n\n")

# M2: SES random slopes
cat("   📈 M2: NCS random slopes by SES\n")
t0 <- Sys.time()
m2_empl <- lmer(empl_dv ~ 1 +
                  age + I(age^2) +
                  sex + edu_lvl + in_education + area +
                  O + C + E + A + ES +
                  (1|region) + (1|idind) + (1|age_factor) +
                  (1 + O + C + E + A + ES | ses5),
                REML = TRUE, data = youth_empl)
cat("   ✅ M2 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n")

m2_empl_coefs <-
  coef(m2_empl)$ses5 %>%
  as.data.frame() %>%
  rownames_to_column(var = "ses5") %>%
  select(ses5, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -ses5) %>%
  mutate(Estimate = as.numeric(Estimate) * 100,
         Skill = case_when(Skill == "O"  ~ "Openness",
                           Skill == "C"  ~ "Conscientiousness",
                           Skill == "E"  ~ "Extraversion",
                           Skill == "A"  ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))
saveRDS(m2_empl_coefs, file.path(outputsEmplNcs, "m4_ses_coefs.rds"))

# M3: Education random slopes
cat("   📚 M3: NCS random slopes by education\n")
t0 <- Sys.time()
m3_empl <- lmer(empl_dv ~ 1 +
                  age + I(age^2) +
                  sex + in_education + area + ses5 +
                  O + C + E + A + ES +
                  (1|region) + (1|idind) + (1|age_factor) +
                  (1 + O + C + E + A + ES | edu_lvl),
                REML = TRUE, data = youth_empl)
cat("   ✅ M3 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n")

m3_empl_coefs <-
  coef(m3_empl)$edu_lvl %>%
  as.data.frame() %>%
  rownames_to_column(var = "edu_lvl") %>%
  select(edu_lvl, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -edu_lvl) %>%
  mutate(Estimate = as.numeric(Estimate) * 100,
         Skill = case_when(Skill == "O"  ~ "Openness",
                           Skill == "C"  ~ "Conscientiousness",
                           Skill == "E"  ~ "Extraversion",
                           Skill == "A"  ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))
saveRDS(m3_empl_coefs, file.path(outputsEmplNcs, "m5_edu_coefs.rds"))

# M4: Sex random slopes
cat("   👥 M4: NCS random slopes by sex\n")
t0 <- Sys.time()
m4_empl <- lmer(empl_dv ~ 1 +
                  age + I(age^2) +
                  edu_lvl + in_education + area + ses5 +
                  O + C + E + A + ES +
                  (1|region) + (1|idind) + (1|age_factor) +
                  (1 + O + C + E + A + ES | sex),
                REML = TRUE, data = youth_empl)
cat("   ✅ M4 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n")

m4_empl_coefs <-
  coef(m4_empl)$sex %>%
  as.data.frame() %>%
  rownames_to_column(var = "sex") %>%
  select(sex, O, C, E, A, ES) %>%
  gather(Skill, Estimate, -sex) %>%
  mutate(Estimate = as.numeric(Estimate) * 100,
         Skill = case_when(Skill == "O"  ~ "Openness",
                           Skill == "C"  ~ "Conscientiousness",
                           Skill == "E"  ~ "Extraversion",
                           Skill == "A"  ~ "Agreeableness",
                           Skill == "ES" ~ "Emotional Stability"))
saveRDS(m4_empl_coefs, file.path(outputsEmplNcs, "m6_sex_coefs.rds"))

# ---- SECTION 4: SAVE RANDOM-SLOPE BUNDLE -----------------------------------
models_empl2 <- list("M2" = m2_empl, "M3" = m3_empl, "M4" = m4_empl)
saveRDS(models_empl2, file.path(outputsEmplNcs, "models_ncs_empl_tab2.rds"))

reg_tables_empl_random <-
  modelsummary(models_empl2,
               statistic = "({std.error}) {stars}",
               gof_omit  = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl_rus,
               output    = "tinytable",
               notes     = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.")

# ---- COMPLETION ------------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 4 main models fitted in", round(total_time, 2), "min\n")
cat("   M1 (reference), M2 (SES), M3 (edu), M4 (sex)\n")
cat(rep("=", 80), "\n\n")
