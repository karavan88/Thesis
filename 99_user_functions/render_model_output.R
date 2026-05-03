#' @title Render a model-fitting code snippet alongside its output
#' @description Reads a code snippet via `get_source_code()`, parses the model
#'   variable name with `get_model_name_from_code()`, assigns the model object
#'   to that name in the knitr global environment, then knits a child chunk
#'   that displays the code and writes the captured `render_output(model)` text
#'   inside a fenced `text` code block. Used by the annex to print
#'   `summary(model)` next to its source code.
#' @param model A fitted model object to bind into the knit environment.
#' @param source_file Character. Absolute path to the source file.
#' @param start_line Integer. First line of the code snippet.
#' @param end_line Integer. Last line of the code snippet.
#' @param render_code Function `(model_name) -> character` returning the line(s)
#'   to append to the code chunk (e.g. `summary(model_name)`).
#' @param render_output Function `(model_obj) -> any` whose printed output is
#'   captured and emitted under the code chunk.
#' @return Invisibly `NULL`; called for its side-effect of writing to stdout.
render_model_output <- function(model, source_file, start_line, end_line, render_code, render_output) {
  code_snippet <- get_source_code(source_file, start_line, end_line)
  model_name <- get_model_name_from_code(code_snippet, fallback = "model_obj")
  assign(model_name, model, envir = knitr::knit_global())

  code_chunk <- paste0(
    "```{r eval=FALSE, echo=TRUE}\n",
    code_snippet, "\n\n",
    render_code(model_name), "\n",
    "```\n"
  )
  code_rendered <- knitr::knit_child(text = code_chunk, quiet = TRUE, envir = knitr::knit_global())
  out <- capture.output(render_output(model))

  # Strip lme4's "boundary (singular) fit" diagnostic from summary.merMod —
  # the appendix should show the spec + estimates, not the near-zero-variance
  # warning.
  out <- out[!grepl("boundary \\(singular\\) fit|isSingular", out)]

  cat("\n\n")
  cat(code_rendered, sep = "\n")
  cat("\n")
  cat("```text\n")
  cat(out, sep = "\n")
  cat("\n```\n")
}
