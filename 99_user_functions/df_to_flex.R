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
  # Force a UTF-8 locale at the moment flextable processes the table.
  # flextable's LaTeX writer formats each cell under the active LC_CTYPE;
  # if it isn't UTF-8 the writer emits Cyrillic characters as
  # "<U+041E>..." escape strings. Setting it here guarantees the right
  # locale right before flextable() is invoked, even if upstream chunks
  # or hooks reset it.
  for (loc in c("ru_RU.UTF-8", "en_US.UTF-8")) {
    res <- try(Sys.setlocale("LC_CTYPE", loc), silent = TRUE)
    if (!inherits(res, "try-error") && nzchar(res)) break
  }

  ft <- flextable::flextable(data)
  if (!is.null(col_labels)) {
    ft <- flextable::set_header_labels(ft, values = col_labels)
  }
  style_flex_table(ft, note = note, latex_total_width = latex_total_width)
}
