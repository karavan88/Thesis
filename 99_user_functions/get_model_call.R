#' @title Extract a fitted model's call as a single character string
#' @description Calls `stats::getCall()` and `deparse()`s the result on a single
#'   line. Returns a fallback string when the call cannot be retrieved.
#' @param model A fitted model object.
#' @return Character scalar with the deparsed model call, or
#'   `"Call is unavailable for this model object."` on error.
get_model_call <- function(model) {
  call_obj <- tryCatch(stats::getCall(model), error = function(e) NULL)
  if (is.null(call_obj)) return("Call is unavailable for this model object.")
  paste(deparse(call_obj, width.cutoff = 500), collapse = "\n")
}
