#' @title Print a raw model object in the annex
#' @description Convenience wrapper around `render_model_output()` that prints
#'   `print(model)` underneath the embedded code snippet, using a custom
#'   recursive printer that:
#'   - For `lqmm` models, prints the call, formula, quantile and fixed effects
#'     while suppressing region dummy coefficients.
#'   - For lm/glm/gam/merMod-style objects, prints `summary(x)` instead of the
#'     raw object to avoid dumping internals.
#'   - For lists, recurses into elements with parent-name annotations.
#'   - For other objects, prints them but filters out lines starting with
#'     "region" so region fixed effects are hidden in the annex.
#' @param label Character label for the model (currently unused but retained
#'   for API stability).
#' @param model A fitted model object (or a list of them).
#' @param source_file Character. Absolute path to the source file.
#' @param start_line Integer. First line of the code snippet.
#' @param end_line Integer. Last line of the code snippet.
#' @return Invisibly `NULL`; called for side-effects.
print_raw_model_output <- function(label, model, source_file, start_line, end_line) {
  render_model_output(
    model = model,
    source_file = source_file,
    start_line = start_line,
    end_line = end_line,
    render_code = function(model_name) paste0("print(", model_name, ")"),
    render_output = function(model_obj) {
      print_lqmm_without_regions <- function(one_model) {
        coefs <- coef(one_model)
        coef_names <- names(coefs)
        keep <- !grepl("^region", coef_names)
        coefs <- coefs[keep]

        formula_txt <- tryCatch(
          paste(deparse(stats::formula(one_model), width.cutoff = 120), collapse = " "),
          error = function(e) "formula unavailable"
        )
        cat("Call: lqmm(...)\n")
        cat("Formula: ", formula_txt, "\n\n", sep = "")
        cat("Quantile ", one_model$tau, "\n\n", sep = "")
        cat("Fixed effects:\n")
        for (nm in names(coefs)) {
          cat(sprintf("%-45s %12.7f\n", nm, unname(coefs[[nm]])))
        }
      }

      print_raw_object <- function(x, parent_name = NULL) {
        if (inherits(x, "lqmm")) {
          if (!is.null(parent_name)) {
            cat(parent_name, "\n", strrep("-", nchar(parent_name)), "\n", sep = "")
          }
          print_lqmm_without_regions(x)
          cat("\n")
          return(invisible(NULL))
        }

        # Many model objects (e.g., gam/glm/lm) are lists internally.
        # Handle them before list recursion to avoid dumping massive internals.
        if (inherits(x, c("gam", "glm", "lm", "merMod", "lmerMod", "lmerModLmerTest"))) {
          if (!is.null(parent_name)) {
            cat(parent_name, "\n", strrep("-", nchar(parent_name)), "\n", sep = "")
          }
          cat(paste(capture.output(summary(x)), collapse = "\n"), "\n\n", sep = "")
          return(invisible(NULL))
        }

        if (is.list(x)) {
          nms <- names(x)
          if (is.null(nms) || any(!nzchar(nms))) {
            nms <- paste0("[[", seq_along(x), "]]")
          }
          for (i in seq_along(x)) {
            child_name <- if (is.null(parent_name)) nms[[i]] else paste0(parent_name, "$", nms[[i]])
            print_raw_object(x[[i]], child_name)
          }
          return(invisible(NULL))
        }

        printed_lines <- capture.output(print(x))
        filtered_lines <- printed_lines[!grepl("^[[:space:]]*region", printed_lines)]
        cat(paste(filtered_lines, collapse = "\n"), "\n", sep = "")
      }

      print_raw_object(model_obj)
    }
  )
}
