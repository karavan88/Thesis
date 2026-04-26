#' @title Render a modelsummary table as a styled thesis flextable
#' @description Wrapper around `modelsummary::modelsummary()` with
#'   `output = "flextable"` that pipes the result through `style_flex_table()`
#'   so the table picks up the standard thesis styling and an optional footnote.
#' @param models A model or named list of fitted models accepted by
#'   `modelsummary::modelsummary()`.
#' @param note Optional character vector of footnote lines.
#' @param add_rows Optional data frame of extra rows to append, passed through
#'   to `modelsummary::modelsummary()`.
#' @param ... Further arguments passed on to `modelsummary::modelsummary()`.
#' @return A styled `flextable` object.
render_modelsummary_flex <- function(models, note = NULL, add_rows = NULL, ...) {
  ft <- modelsummary::modelsummary(models, output = "flextable", add_rows = add_rows, ...)
  style_flex_table(ft, note = note)
}
