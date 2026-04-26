# ==============================================================================
# NON-COGNITIVE SKILLS AND EMPLOYMENT - FINAL OUTPUTS (TABLES & PLOTS)
# ==============================================================================
#
# Project:      Non-Cognitive Skills and Labor Market Outcomes
# File:         05_regression_final.R
# Purpose:      Generate publication-ready regression tables and visualizations
# 
# Description:  This script loads pre-fitted regression models and coefficient
#               data to create formatted publication tables with Russian labels
#               and publication-ready visualizations for academic manuscripts
#               and reports. Separates analysis from visualization creation.
#
# Input Files:  models_ncs_empl_tab1.rds (Fixed effects models M2 & M3)
#               models_ncs_empl_tab2.rds (Random slopes models M4, M5 & M6)
#               models_ncs_empl_suppl.rds (Supplementary models MS1, MS2, MS3)
#               age_effect_data.rds (Age effect visualization data)
#               m4_ses_coefs.rds (SES coefficients data)
#               m5_edu_coefs.rds (Education coefficients data)
#               m6_sex_coefs.rds (Sex coefficients data)
#               m_occup_ses_coefs.rds (Occupational SES coefficients data)
#
# Outputs:      • Publication-ready regression tables with Russian labels
#               • Age effect visualization 
#               • SES heterogeneity coefficient plots
#               • Education heterogeneity coefficient plot
#               • Sex heterogeneity coefficient plot
#               • Occupational outcomes table and plot
#               • Educational mismatch analysis table
#               • Saved plot files (PNG format)
#
# Models:       Main Analysis:
#               - Model with controls (M2)
#               - Model with NCS and controls (M3)
#               - Model with SES random slopes (M4)
#               - Model with Education random slopes (M5)
#               - Model with Sex random slopes (M6)
#               
#               Supplementary Analysis:
#               - White-collar high-skilled employment (MS1)
#               - White-collar by SES random slopes (MS2)
#               - Educational mismatch analysis (MS3)
#
# Author:       Garen Avanesian
# Institution:  Southern Federal University
# Created:      December 17, 2024
# Modified:     October 19, 2025
# Version:      3.0 (Added visualizations)
#
# Dependencies: modelsummary, tinytable, ggplot2, tidyverse
# Runtime:      ~2-3 minutes
#
# Notes:        Requires models and data to be pre-fitted using 03_regression_empl.R
#               All variable labels translated to Russian for publication
#               All plots saved to outputs folder with publication quality
#
# ==============================================================================


# Initialize logging
cat("\n", rep("=", 80), "\n")
cat("GENERATING FINAL REGRESSION OUTPUTS (TABLES & VISUALIZATIONS)\n")
cat("Script: 05_regression_final.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

# Start timer for overall execution
script_start_time <- Sys.time()

# SECTION 1: LOAD PRE-FITTED MODELS
cat("📁 SECTION 1: LOADING PRE-FITTED MODELS\n")
cat("Loading fixed effects models (M2 & M3)...\n")

# Upload the regression models saved in the folder
models_empl1 <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_tab1.rds"))



cat("✅ Fixed effects models loaded successfully\n")
cat("   - Model with controls (M2)\n")
cat("   - Model with NCS and controls (M3)\n\n")

# SECTION 2: DEFINE RUSSIAN VARIABLE LABELS
cat("🔤 SECTION 2: SETTING UP RUSSIAN VARIABLE LABELS\n")
cat("Configuring translation mappings for publication tables...\n")

rename_vector_empl_rus <-
    c(`(Intercept)`                       = "Константа",
        age                               = "Возраст",
        `I(age^2)`                        = "Возраст²",
        sexMale                           = "Пол: Мужской",
        `edu_lvl2. Secondary School`      = "Обр: Среднее",
        `edu_lvl3. Secondary Vocational`  = "Обр: Среднее проф",
        `edu_lvl4. Tertiary`              = "Обр: Высшее",
        `areaUrban-Type Settlement`       = "Тип поселения: ПГТ",
        areaCity                          = "Тип поселения: Город",
        `areaRegional Center`             = "Тип поселения: Обл центр",
        `in_education1`                   = "В настоящее время обучается",
        ses5Q2                            = "Среднедушевой доход ДХ: Q2",
        ses5Q3                            = "Среднедушевой доход ДХ: Q3",
        ses5Q4                            = "Среднедушевой доход ДХ: Q4",
        ses5Q5                            = "Среднедушевой доход ДХ: Q5",
        O                                 = "Открытость опыту",
        C                                 = "Добросовестность",
        E                                 = "Экстраверсия",
        A                                 = "Доброжелательность",
        ES                                = "Эмоциональная стабильность")

cat("✅ Russian labels configured for", length(rename_vector_empl_rus), "variables\n\n")

# SECTION 3: GENERATE FIXED EFFECTS TABLE
cat("📊 SECTION 3: GENERATING FIXED EFFECTS TABLE\n")
cat("Creating publication table for models M2 and M3...\n")

reg_tables_mem_fixed <- 
  modelsummary(models_empl1,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl_rus,
               output = "tinytable",
               note = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.")

cat("✅ Fixed effects table generated successfully\n\n")

m1_empl_base <- readRDS(file.path(outputsEmplNcs, "model_ncs_empl_m1_baseline.rds"))


# SECTION 4: LOAD AND PROCESS RANDOM SLOPES MODELS
cat("📁 SECTION 4: LOADING RANDOM SLOPES MODELS\n")
cat("Loading random slopes models (M4, M5, M6)...\n")

# Upload the regression models saved in the folder
models_empl2 <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_tab2.rds"))


models_empl2 <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_tab2.rds"))

cat("✅ Random slopes models loaded successfully\n")
cat("   - Model with SES random slopes (M4)\n")
cat("   - Model with Education random slopes (M5)\n")
cat("   - Model with Sex random slopes (M6)\n\n")

# SECTION 5: GENERATE RANDOM SLOPES TABLE
cat("📊 SECTION 5: GENERATING RANDOM SLOPES TABLE\n")
cat("Creating publication table for heterogeneous effects models...\n")

reg_tables_mem_random <- 
  modelsummary(models_empl2,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl_rus,
               output = "tinytable",
               note = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.")

cat("✅ Random slopes table generated successfully\n\n")

# SECTION 6: LOAD VISUALIZATION DATA
cat("📊 SECTION 6: LOADING VISUALIZATION DATA\n")
cat("Loading coefficient data and visualization datasets...\n")

# Load age effect data
age_effect_data <- readRDS(file.path(outputsEmplNcs, "age_effect_data.rds"))
cat("✅ Age effect data loaded\n")

# Load SES coefficients data
m4_ses_coefs <- readRDS(file.path(outputsEmplNcs, "m4_ses_coefs.rds"))
cat("✅ SES coefficients data loaded\n")

# Load education coefficients data  
m5_edu_coefs <- readRDS(file.path(outputsEmplNcs, "m5_edu_coefs.rds"))
cat("✅ Education coefficients data loaded\n")

# Load sex coefficients data
m6_sex_coefs <- readRDS(file.path(outputsEmplNcs, "m6_sex_coefs.rds"))
cat("✅ Sex coefficients data loaded\n\n")

# SECTION 7: CREATE VISUALIZATIONS
cat("🎨 SECTION 7: CREATING PUBLICATION-READY VISUALIZATIONS\n\n")

# SECTION 7A: AGE EFFECT PLOT
cat("   📈 7A: Age Effect Visualization\n")
cat("   Creating age effect plot...\n")
age_plot_start <- Sys.time()

age_effect_plot <-
  ggplot(age_effect_data, aes(Age, Value)) +
  geom_line(linewidth = 1.2, color = "#2C3E50") +
  scale_x_continuous(breaks = c(21, 22, 23, 24, 25, 26, 27, 28, 29),
                     limits = c(21, 29.5)) +
  theme_bw() +
  ylim(45, 100) +
  ylab("Probability of Completed Transition (%)") +
  xlab("Age") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) 

# Save the age effect plot
ggsave(file.path(outputsEmplNcs, "age_effect_plot.png"), 
       plot = age_effect_plot, 
       width = 8, height = 6, dpi = 300)

cat("   ✅ Age effect plot completed in", 
    round(difftime(Sys.time(), age_plot_start, units = "secs"), 2), "seconds\n\n")

# SECTION 7B: SES HETEROGENEITY PLOT
cat("   💰 7B: SES Heterogeneity Visualization\n")
cat("   Creating SES effects plot...\n")
ses_plot_start <- Sys.time()

plot_ses <-
  ggplot(m4_ses_coefs, aes(Estimate, ses5)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")

# Save the SES plot
ggsave(file.path(outputsEmplNcs, "ses_ncs_plot.png"), 
       plot = plot_ses, 
       width = 12, height = 8, dpi = 300)

cat("   ✅ SES effects plot completed in", 
    round(difftime(Sys.time(), ses_plot_start, units = "secs"), 2), "seconds\n\n")

# SECTION 7C: EDUCATION HETEROGENEITY PLOT
cat("   🎓 7C: Education Heterogeneity Visualization\n")
cat("   Creating education effects plot...\n")
edu_plot_start <- Sys.time()

plot_edu <-
  ggplot(m5_edu_coefs, aes(Estimate, edu_lvl)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")

# Save the education plot
ggsave(file.path(outputsEmplNcs, "education_ncs_plot.png"), 
       plot = plot_edu, 
       width = 12, height = 8, dpi = 300)

cat("   ✅ Education effects plot completed in", 
    round(difftime(Sys.time(), edu_plot_start, units = "secs"), 2), "seconds\n\n")

# SECTION 7D: SEX HETEROGENEITY PLOT
cat("   👥 7D: Sex Heterogeneity Visualization\n")
cat("   Creating sex effects plot...\n")
sex_plot_start <- Sys.time()

plot_sex <-
  ggplot(m6_sex_coefs, aes(Estimate, Skill)) +
  geom_point(size = 9, aes(color = sex)) +
  geom_text(aes(label = round(Estimate, 2)), 
            size = 2, color = "black",
            check_overlap = TRUE) +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  ylab("") +
  xlab("") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# Save the sex plot
ggsave(file.path(outputsEmplNcs, "sex_ncs_plot.png"), 
       plot = plot_sex, 
       width = 10, height = 6, dpi = 300)

cat("   ✅ Sex effects plot completed in", 
    round(difftime(Sys.time(), sex_plot_start, units = "secs"), 2), "seconds\n\n")

# SECTION 8: SUPPLEMENTARY MODELS AND OUTPUTS
cat("🔬 SECTION 8: SUPPLEMENTARY MODELS AND OUTPUTS\n\n")

# SECTION 8A: LOAD SUPPLEMENTARY MODELS
cat("   📁 8A: Loading Supplementary Models\n")
cat("   Loading occupational and educational mismatch models...\n")

models_suppl <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_suppl.rds"))
cat("   ✅ Supplementary models loaded successfully\n")
cat("      • White-collar high-skilled employment model\n")
cat("      • White-collar by SES random slopes model\n")
cat("      • Educational mismatch model\n\n")

# SECTION 8B: GENERATE SUPPLEMENTARY TABLES
cat("   📊 8B: Generating Supplementary Tables\n")
cat("   Creating occupational outcomes table...\n")
suppl_table_start <- Sys.time()

# Occupational models table (MS1 and MS2)
models_occup <- list(
  "White Collar (High-Skilled)" = models_suppl[["White Collar (High-Skilled)"]],
  "White Collar (High-Skilled) by SES" = models_suppl[["White Collar (High-Skilled) by SES"]]
)




reg_tables_occup <-
  modelsummary(models_occup,
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl_rus,
               notes = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.",
               output = "tinytable")

cat("   ✅ Occupational outcomes table generated\n")

# Educational mismatch table (MS3)
cat("   Creating educational mismatch table...\n")

reg_tab_overeduc <-
  modelsummary(models_suppl[["Educational Mismatch"]],
               statistic = "({std.error}) {stars}",
               gof_omit = "ICC|RMSE|cond|AIC|BIC",
               coef_omit = "SD|Cor",
               coef_rename = rename_vector_empl_rus,
               notes = "Источник: расчеты автора на основе данных РМЭЗ за 2016 и 2019 годы.",
               output = "tinytable")

cat("   ✅ Educational mismatch table generated\n")

suppl_table_end <- Sys.time()
cat("   ✅ Supplementary tables completed in", 
    round(difftime(suppl_table_end, suppl_table_start, units = "secs"), 2), "seconds\n\n")

# SECTION 8C: SUPPLEMENTARY VISUALIZATIONS
cat("   🎨 8C: Creating Supplementary Visualizations\n")
cat("   Creating occupational SES heterogeneity plot...\n")
suppl_plot_start <- Sys.time()

# Load occupational SES coefficients
m_occup_ses_coefs <- readRDS(file.path(outputsEmplNcs, "m_occup_ses_coefs.rds"))

plot_ses_occup <-
  ggplot(m_occup_ses_coefs, aes(Estimate, ses5)) +
  geom_point(size = 7, color = "lightblue") +
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid",  color = "black") +
  theme_bw() +
  scale_y_discrete(limits = rev) +
  facet_wrap(Skill~.) +
  ylab("") +
  xlab("")

# Save the occupational SES plot
ggsave(file.path(outputsEmplNcs, "occupational_ses_ncs_plot.png"), 
       plot = plot_ses_occup, 
       width = 12, height = 8, dpi = 300)

suppl_plot_end <- Sys.time()
cat("   ✅ Supplementary plot completed in", 
    round(difftime(suppl_plot_end, suppl_plot_start, units = "secs"), 2), "seconds\n\n")

# COMPLETION SUMMARY
script_end_time <- Sys.time()
total_time <- difftime(script_end_time, script_start_time, units = "mins")

cat(rep("=", 80), "\n")
cat("🎉 FINAL REGRESSION OUTPUTS COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 80), "\n")
cat("📊 MAIN TABLES GENERATED:\n")
cat("   • Fixed Effects Table: Models M2 & M3 with Russian labels\n")
cat("   • Random Slopes Table: Models M4, M5 & M6 with Russian labels\n\n")
cat("📊 SUPPLEMENTARY TABLES GENERATED:\n")
cat("   • Occupational Outcomes Table: White-collar employment models\n")
cat("   • Educational Mismatch Table: Over-education analysis\n\n")
cat("🎨 MAIN VISUALIZATIONS CREATED:\n")
cat("   • Age Effect Plot: Employment transition probability by age\n")
cat("   • SES Heterogeneity Plot: NCS effects by socioeconomic status\n")
cat("   • Education Heterogeneity Plot: NCS effects by education level\n")
cat("   • Sex Heterogeneity Plot: NCS effects by gender\n\n")
cat("🎨 SUPPLEMENTARY VISUALIZATIONS CREATED:\n")
cat("   • Occupational SES Plot: NCS effects on white-collar employment by SES\n\n")
cat("📁 FILES SAVED:\n")
cat("   Main Analysis:\n")
cat("   • age_effect_plot.png (8x6, 300 DPI)\n")
cat("   • ses_ncs_plot.png (12x8, 300 DPI)\n")
cat("   • education_ncs_plot.png (12x8, 300 DPI)\n")
cat("   • sex_ncs_plot.png (10x6, 300 DPI)\n")
cat("   Supplementary Analysis:\n")
cat("   • occupational_ses_ncs_plot.png (12x8, 300 DPI)\n\n")
cat("📝 TABLE FEATURES:\n")
cat("   • Standard errors in parentheses with significance stars\n")
cat("   • Russian variable labels for publication\n")
cat("   • Omitted technical statistics (ICC, RMSE, AIC, BIC)\n")
cat("   • Publication-ready formatting with tinytable\n")
cat("   • RLMS source attribution in Russian\n\n")
cat("🎨 PLOT FEATURES:\n")
cat("   • Publication-ready quality (300 DPI)\n")
cat("   • Professional color schemes and typography\n")
cat("   • Clear labels and titles\n")
cat("   • Consistent styling across all plots\n")
cat("   • Ready for manuscript inclusion\n\n")
cat("⏱️  TOTAL EXECUTION TIME:", round(total_time, 2), "minutes\n")
cat("✅ End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")
