#' @title Convert a gtsummary table to a styled flextable
#' @description Renders a `gtsummary` table object via `as_flex_table()` (or
#'   `as_kable_extra()` for LaTeX output) and applies the thesis styling.
#' @param tbl A `gtsummary` table object.
#' @param note Optional character vector of footnote lines passed through to
#'   `style_flex_table()`. Defaults to `NULL`.
#' @return A styled `flextable` for HTML/Word output, or a `kableExtra`
#'   `kable_styling` object for LaTeX output.
gtsummary_to_flex <- function(tbl, note = NULL) {
  if (knitr::is_latex_output()) {
    return(
      gtsummary::as_kable_extra(tbl, format = "latex", booktabs = TRUE, linesep = "") |>
        kableExtra::kable_styling(font_size = 9)
    )
  }
  style_flex_table(gtsummary::as_flex_table(tbl), note = note)
}
