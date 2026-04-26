# ==============================================================================
# CHAPTER 4 - SUPPLEMENTARY EMPLOYMENT MODELS
# ==============================================================================
#
# File:         04_regression_supplement.R
# Purpose:      Fit chapter-4 supplementary mixed-effects models on the youth
#               sample produced by 01_data_prep_empl.R.
#
# Models fitted in this script (continuing the chapter-4 sequence):
#   M5 - High-skilled white-collar employment, NCS + controls
#   M6 - High-skilled white-collar with NCS random slopes by SES quintile
#   M7 - Educational mismatch (overeducation) among tertiary-educated youth
#
# Outputs (saved to 03_output/empl_outputs/):
#   models_ncs_empl_suppl.rds  - list("M5"=..., "M6"=..., "Educational Mismatch"=...)
#   m_occup_ses_coefs.rds      - long-format M6 random-slope coefs (% scale)
#
# Dependencies: lme4, lmerTest, modelsummary, tinytable, tidyverse
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 4 - SUPPLEMENTARY MODELS\n")
cat("Script: 04_regression_supplement.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# ---- SECTION 1: DATA LOADING -----------------------------------------------
cat("📊 SECTION 1: DATA LOADING\n")
youth_empl_suppl <- readRDS(file.path(processedData, "youth_empl.rds"))
cat("✅ Loaded youth_empl_suppl:", nrow(youth_empl_suppl), "rows\n")
cat("   - White-collar workers:", sum(youth_empl_suppl$white_collar, na.rm = TRUE), "\n")
cat("   - High-skilled white-collar:", sum(youth_empl_suppl$white_collar_hs, na.rm = TRUE), "\n")
cat("   - Educational-mismatch cases:", sum(youth_empl_suppl$skill_mismatch_overeduc, na.rm = TRUE), "\n\n")

# ---- SECTION 2: M5 - HIGH-SKILLED WHITE-COLLAR -----------------------------
cat("🔧 SECTION 2: M5 (high-skilled white-collar)\n")
t0 <- Sys.time()

m5_empl <- lmer(white_collar_hs ~ 1 +
                  age + I(age^2) +
                  sex + edu_lvl + area + ses5 + in_education +
                  O + C + E + A + ES +
                  (1|region) + (1|idind) + (1|edu_lvl),
                weights = ipw_empl,
                REML = TRUE, data = youth_empl_suppl)

cat("✅ M5 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ---- SECTION 3: M6 - WHITE-COLLAR x SES RANDOM SLOPES ----------------------
cat("🔧 SECTION 3: M6 (high-skilled white-collar, random slopes by SES)\n")
t0 <- Sys.time()

m6_empl <- lmer(white_collar_hs ~ 1 +
                  age + I(age^2) +
                  sex + edu_lvl + area + in_education +
                  O + C + E + A + ES +
                  (1|idind) + (1|edu_lvl) +
                  (1 + O + C + E + A + ES | ses5),
                weights = ipw_empl,
                REML = TRUE, data = youth_empl_suppl)

cat("✅ M6 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n")

# Extract SES random-slope coefficients (percentage-point scale)
m6_empl_ses_coefs <-
  coef(m6_empl)$ses5 %>%
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

saveRDS(m6_empl_ses_coefs, file.path(outputsEmplNcs, "m_occup_ses_coefs.rds"))
cat("   ✓ M6 SES coefficients saved\n\n")

# ---- SECTION 4: M7 - EDUCATIONAL MISMATCH (OVEREDUCATION) ------------------
cat("🔧 SECTION 4: M7 (educational mismatch among tertiary-educated)\n")
t0 <- Sys.time()

tertiary_data <- youth_empl_suppl[youth_empl_suppl$edu_lvl == "4. Tertiary", ]
cat("   - Tertiary sample size:", nrow(tertiary_data), "\n")

m7_empl <- lmer(skill_mismatch_overeduc ~ 1 +
                  age + I(age^2) +
                  sex + area + ses5 +
                  O + C + E + A + ES +
                  (1|region) + (1|idind),
                REML = TRUE, data = tertiary_data)

cat("✅ M7 fit in", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n\n")

# ---- SECTION 5: SAVE BUNDLE ------------------------------------------------
# List keyed so the qmd's existing references (e.g. models_suppl[["Educational
# Mismatch"]]) keep working after the M5/M6/M7 renumbering.
models_suppl <- list(
  "M5"                    = m5_empl,
  "M6"                    = m6_empl,
  "Educational Mismatch"  = m7_empl
)

saveRDS(models_suppl, file.path(outputsEmplNcs, "models_ncs_empl_suppl.rds"))
cat("✅ Supplementary bundle saved (M5, M6, Educational Mismatch)\n\n")

# Convenience: 'occupational' two-model list expected by the qmd. Built here
# (was previously built in 05_regression_final.R, now eliminated).
models_occup <- list(
  "M5" = m5_empl,
  "M6" = m6_empl
)

# ---- COMPLETION ------------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 4 supplementary models fitted in", round(total_time, 2), "min\n")
cat("   M5 (white-collar HS), M6 (white-collar HS x SES), M7 (overeducation)\n")
cat(rep("=", 80), "\n\n")
