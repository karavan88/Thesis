#' @title Extract a fitted model's formula as a single character string
#' @description Calls `stats::formula()` and `deparse()`s the result. Returns a
#'   fallback string when the formula cannot be retrieved.
#' @param model A fitted model object.
#' @return Character scalar with the deparsed model formula, or
#'   `"Formula is unavailable for this model object."` on error.
get_model_formula <- function(model) {
  f <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(f)) return("Formula is unavailable for this model object.")
  paste(deparse(f, width.cutoff = 500), collapse = "\n")
}
