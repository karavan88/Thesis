#' @title Print a model summary in the annex
#' @description Convenience wrapper around `render_model_output()` that prints
#'   `summary(model)` underneath the embedded code snippet. The `label` argument
#'   is currently ignored (kept for backward compatibility with call sites).
#' @param label Character label for the model (currently unused but retained
#'   for API stability).
#' @param model A fitted model object.
#' @param source_file Character. Absolute path to the source file.
#' @param start_line Integer. First line of the code snippet.
#' @param end_line Integer. Last line of the code snippet.
#' @return Invisibly `NULL`; called for side-effects.
print_model_output <- function(label, model, source_file, start_line, end_line) {
  render_model_output(
    model = model,
    source_file = source_file,
    start_line = start_line,
    end_line = end_line,
    render_code = function(model_name) paste0("summary(", model_name, ")"),
    render_output = function(model_obj) summary(model_obj)
  )
}
