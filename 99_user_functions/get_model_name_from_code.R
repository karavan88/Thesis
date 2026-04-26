#' @title Parse a model-object name from a code snippet
#' @description Looks for the first `name <- ...` assignment in a code snippet
#'   and returns the assigned name. Falls back to `fallback` when no assignment
#'   is matched. Used by the annex to give the bound model object a meaningful
#'   variable name in rendered code blocks.
#' @param code_snippet Character scalar containing R source code.
#' @param fallback Character. Name to return if no `<-` assignment is found.
#'   Defaults to `"model_obj"`.
#' @return Character scalar with the extracted variable name.
get_model_name_from_code <- function(code_snippet, fallback = "model_obj") {
  m <- regexec("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*<-", code_snippet)
  r <- regmatches(code_snippet, m)
  if (length(r) > 0 && length(r[[1]]) > 1 && nzchar(r[[1]][2])) {
    return(r[[1]][2])
  }
  fallback
}
