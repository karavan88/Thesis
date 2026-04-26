#' @title Build a styled flextable from a data frame
#' @description Wraps a data frame in a `flextable`, optionally applies header
#'   relabelling, and dispatches to `style_flex_table()` for consistent thesis
#'   styling and an optional footnote.
#' @param data A data frame (or tibble) to render.
#' @param note Optional character vector of footnote lines passed through to
#'   `style_flex_table()`. Defaults to `NULL`.
#' @param col_labels Optional named character vector mapping current column names
#'   to display labels (e.g. `c(year = "Год")`). Defaults to `NULL`.
#' @param latex_total_width Numeric total table width in inches passed through to
#'   `style_flex_table()` for LaTeX output. Defaults to `5.6`.
#' @return A styled `flextable` object.
df_to_flex <- function(data, note = NULL, col_labels = NULL, latex_total_width = 5.6) {
  ft <- flextable::flextable(data)
  if (!is.null(col_labels)) {
    ft <- flextable::set_header_labels(ft, values = col_labels)
  }
  style_flex_table(ft, note = note, latex_total_width = latex_total_width)
}
