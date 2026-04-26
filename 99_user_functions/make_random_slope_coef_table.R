#' @title Build a wide table of random-slope coefficients by group
#' @description Reshapes a long random-slope coefficient data frame (with
#'   columns `Skill`, the user-supplied `group_col`, and `Estimate`) into a
#'   wide data frame, with one row per skill and one column per group level.
#'   Recodes skill names to Russian via `skill_labels_ru` from the calling
#'   environment; optionally recodes group labels.
#' @param df Data frame with at least columns `Skill`, `Estimate` and the
#'   column named by `group_col`.
#' @param group_col Character name of the column whose levels become wide
#'   columns.
#' @param group_labels Optional named character vector mapping group level
#'   strings to display labels. Defaults to `NULL` (no recoding).
#' @return A wide data frame with first column `Навык` (skill, Russian) and
#'   one column per group level holding rounded estimates.
make_random_slope_coef_table <- function(df, group_col, group_labels = NULL) {
  out <- df |>
    dplyr::transmute(
      skill = dplyr::recode(Skill, !!!skill_labels_ru),
      group = as.character(.data[[group_col]]),
      estimate = round(Estimate, 3)
    )

  if (!is.null(group_labels)) {
    out <- out |> dplyr::mutate(group = dplyr::recode(group, !!!group_labels))
  }

  out |>
    tidyr::pivot_wider(names_from = group, values_from = estimate) |>
    dplyr::rename(`Навык` = skill)
}
