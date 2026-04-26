#' @title Read a contiguous block of source-code lines from a file
#' @description Reads a file via `readLines(..., encoding = "UTF-8")` and
#'   returns lines `start_line:end_line` joined with newlines. Used by the
#'   annex chapter to embed model-fitting code snippets next to model output.
#' @param file_path Character. Absolute path to the source file.
#' @param start_line Integer. First line (1-based) to include.
#' @param end_line Integer. Last line (inclusive) to include.
#' @return Character scalar with the requested lines joined by `\n`.
get_source_code <- function(file_path, start_line, end_line) {
  code_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  paste(code_lines[start_line:end_line], collapse = "\n")
}
