#' @title Build a wide table of skill-by-wage-quintile random slopes
#' @description Reshapes the long random-slope coefficient frame for the
#'   job-satisfaction chapter into a wide table with one row per skill (in
#'   Russian) and one column per wage quintile (Q1..Q5). Recodes skill names via
#'   `skill_labels_ru` and quintile labels via `wage_q_labels_ru` from the
#'   calling environment.
#' @param df Data frame with columns `Skill`, `hourly_wage_quintile` and
#'   `Estimate`.
#' @return A wide data frame with first column `Навык` (skill, Russian) and one
#'   column per wage quintile holding rounded estimates.
make_wage_slope_table <- function(df) {
  df |>
    dplyr::transmute(
      `Навык` = dplyr::recode(Skill, !!!skill_labels_ru, .default = Skill),
      `Квинтиль заработной платы` = dplyr::recode(as.character(hourly_wage_quintile), !!!wage_q_labels_ru, .default = as.character(hourly_wage_quintile)),
      `Оценка` = round(Estimate, 3)
    ) |>
    tidyr::pivot_wider(names_from = `Квинтиль заработной платы`, values_from = `Оценка`) |>
    dplyr::arrange(`Навык`)
}
