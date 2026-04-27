# ==============================================================================
# CHAPTER 5 - RETURNS TO NCS - DATA PREPARATION
# ==============================================================================
#
# File:         01_data_prep_returns.R
# Purpose:      Build the two analytical samples used in chapter 5.
#
# Two samples:
#   youth_master_returns  - youth analysis (ages 16-29). Built from
#                           01_data/processed/youth_empl.rds, which is the
#                           project-wide youth source-of-truth (age 15-29,
#                           NCS-complete, military removed); chapter 5 trims
#                           to age >= 16 and adds wage / hourly-wage variables.
#   ind_master_returns    - life-course analysis (ages 16-65). Built from
#                           01_data/processed/ind_master_empl.rds; military
#                           is removed and the same wage / hourly-wage
#                           variables are added.
#
# Wage adjustment:  wages from wave 25 (2016) are converted to 2019 rubles
#                   via World-Bank CPI (162.2 -> 180.8).
#
# Outputs (in 03_output/returns_outputs/):
#   youth_master_returns.rds, ind_master_returns.rds
#
# Dependencies: tidyverse, project configuration
# ==============================================================================

cat(rep("=", 80), "\n")
cat("CHAPTER 5 - DATA PREPARATION\n")
cat("Script: 01_data_prep_returns.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# ----- CPI adjustment (2016 -> 2019 rubles, World Bank) ----------------------
cpi_2016   <- 162.2
cpi_2019   <- 180.8
adj_factor <- cpi_2019 / cpi_2016

cat("CPI adjustment: 2016 -> 2019, factor =", round(adj_factor, 4), "\n\n")

# Common wage-processing helper. Adds wages_adj, work_hrs_per_week,
# working_hrs_per_month, hourly_wage, log_wage. Hours fall back to 40 when j6_2
# is missing.
add_wage_vars <- function(df) {
  df %>%
    mutate(
      wages_adj             = ifelse(id_w == 25, wages * adj_factor, wages),
      work_hrs_per_week     = ifelse(is.na(j6_2), 40, j6_2),
      working_hrs_per_month = work_hrs_per_week * 4,
      hourly_wage           = wages_adj / working_hrs_per_month,
      log_wage              = log(hourly_wage)
    )
}

# ----- Youth sample (16-29) --------------------------------------------------
cat("👥 Youth sample (16-29) — read youth_empl.rds, trim to age >= 16\n")
t0 <- Sys.time()

youth_master_returns <-
  readRDS(file.path(processedData, "youth_empl.rds")) %>%
  filter(age >= 16) %>%
  add_wage_vars() %>%
  drop_na(wages, log_wage) %>%
  mutate(occup_rus = as_factor(occup08))

# Factor levels matched to what the chapter-5 regressions expect
youth_master_returns$sex <- factor(youth_master_returns$sex,
                                   levels = c("Female", "Male"))
youth_master_returns$edu_lvl <- factor(
  youth_master_returns$edu_lvl,
  levels = c("1. No school", "2. Secondary School",
             "3. Secondary Vocational", "4. Tertiary")
)
# Urban/rural binary used by the LQMM regressions
youth_master_returns$area1 <- ifelse(
  youth_master_returns$area %in% c("Областной центр", "Город"),
  "urban", "rural"
)
youth_master_returns$area1 <- factor(youth_master_returns$area1,
                                     levels = c("rural", "urban"))

cat("✅ youth_master_returns:", nrow(youth_master_returns), "rows,",
    length(unique(youth_master_returns$idind)), "individuals (",
    round(difftime(Sys.time(), t0, units = "secs"), 2), "s)\n\n")

# ----- Life-course sample (16-65) -------------------------------------------
cat("👴 Life-course sample (16-65) — read ind_master_empl.rds\n")
t0 <- Sys.time()

ind_master_returns <-
  readRDS(file.path(processedData, "ind_master_empl.rds")) %>%
  filter(age >= 16 & age < 66) %>%
  # Drop military (ISCO-08 group 0) for consistency with the youth sample
  filter(is.na(occup08) | occup08 != 0) %>%
  drop_na(O, C, E, A, ES, wages) %>%
  add_wage_vars()

cat("✅ ind_master_returns:", nrow(ind_master_returns), "rows,",
    length(unique(ind_master_returns$idind)), "individuals (",
    round(difftime(Sys.time(), t0, units = "secs"), 2), "s)\n\n")

# ----- SAVE -----------------------------------------------------------------
saveRDS(youth_master_returns,
        file.path(outputsReturnsNcs, "youth_master_returns.rds"))
saveRDS(ind_master_returns,
        file.path(outputsReturnsNcs, "ind_master_returns.rds"))

cat("💾 Saved both samples to 03_output/returns_outputs/\n\n")

total_time <- difftime(Sys.time(), script_start_time, units = "secs")
cat(rep("=", 80), "\n")
cat("✅ Chapter 5 data prep complete in",
    round(total_time, 2), "s\n")
cat(rep("=", 80), "\n\n")
