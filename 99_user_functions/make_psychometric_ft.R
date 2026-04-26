#' @title Build a psychometric flextable for one Big-Five trait
#' @description Filters `big5_item_table` by trait label, recodes item codes to
#'   Russian wording via `big5_item_labels_ru`, rounds all numeric columns to
#'   two decimals, and renders a `flextable` with Russian column headers via
#'   `df_to_flex()`. Depends on the chunk-level objects `big5_item_table` and
#'   `big5_item_labels_ru` being defined in the calling environment.
#' @param trait_label Character. Trait code as it appears in the `trait` column
#'   of `big5_item_table` (e.g. `"Openness"`).
#' @return A styled `flextable` summarising psychometric properties for the
#'   chosen trait.
make_psychometric_ft <- function(trait_label) {
  big5_item_table %>%
    filter(trait == trait_label) %>%
    mutate(item = recode(item, !!!big5_item_labels_ru)) %>%
    transmute(
      item = item,
      missings = round(missings, 2),
      mean = round(mean, 2),
      sd = round(sd, 2),
      skew = round(skew, 2),
      item_difficulty = round(item_difficulty, 2),
      item_discrimination = round(item_discrimination, 2),
      alpha_if_deleted = round(alpha_if_deleted, 2),
      cronbach_alpha = round(cronbach_alpha, 2),
      mean_inter_item_corr = round(mean_inter_item_corr, 2)
    ) %>%
    df_to_flex(
      col_labels = c(
        item = "Пункт",
        missings = "Пропуски, %",
        mean = "Среднее",
        sd = "SD",
        skew = "Асимметрия",
        item_difficulty = "Сложность вопроса",
        item_discrimination = "Дискриминация вопроса",
        alpha_if_deleted = "alpha если вопрос удален",
        cronbach_alpha = "alpha Кронбаха",
        mean_inter_item_corr = "Средняя межпунктовая корреляция"
      )
    )
}
