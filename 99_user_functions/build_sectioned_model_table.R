#' @title Build a sectioned coefficient + diagnostics table for mixed models
#' @description Combines `modelsummary::modelsummary()` fixed-effect output with
#'   random-effects diagnostics (`make_re_diagnostics_rows`) and model-quality
#'   metrics (`make_model_quality_rows`) into a single data frame, with three
#'   section header rows ("Фиксированные эффекты", "Случайные эффекты",
#'   "Качество модели") used by `style_flex_table()` for visual grouping.
#' @param models_list Named list of fitted mixed-effects models.
#' @param coef_rename Optional named character vector passed to
#'   `modelsummary::modelsummary()` for relabelling coefficient terms. Defaults
#'   to `NULL`.
#' @param single_model_inline Logical. When `TRUE` and the list has a single
#'   model, the standard error is rendered on the same row as the estimate.
#'   Defaults to `FALSE`.
#' @param inline_se Logical. When `TRUE`, always render the standard error on
#'   the same row as the estimate (overrides `single_model_inline` for
#'   multi-model tables). Defaults to `FALSE`.
#' @param coef_omit Regex passed to `modelsummary::modelsummary()`'s
#'   `coef_omit`. Defaults to `"SD|Cor"`. Pass `"SD|Cor|Intercept"` to suppress
#'   the model intercept (used for the job-satisfaction tables).
#' @param include_random_slopes Logical. Forwarded to
#'   `make_re_diagnostics_rows()`. When `FALSE`, omits `Var(u1)` random-slope
#'   rows from the random-effects block. Defaults to `TRUE`.
#' @param include_random_correlations Logical. Forwarded to
#'   `make_re_diagnostics_rows()`. When `TRUE`, appends `Cor:` rows for the
#'   off-diagonals of each random-effect covariance matrix. Defaults to `FALSE`.
#' @return A data frame with columns `term` plus one per model, ready to be
#'   passed to `df_to_flex()`.
build_sectioned_model_table <- function(models_list, coef_rename = NULL, single_model_inline = FALSE, inline_se = FALSE, coef_omit = "SD|Cor", include_random_slopes = TRUE, include_random_correlations = FALSE) {
  ms <- modelsummary::modelsummary(
    models_list,
    output = "dataframe",
    statistic = "({std.error})",
    stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
    gof_omit = ".*",
    coef_omit = coef_omit,
    coef_rename = coef_rename
  )

  est <- ms |> dplyr::filter(part == "estimates")
  model_cols <- setdiff(names(est), c("part", "term", "statistic"))

  groups <- split(est, cumsum(est$statistic == "estimate"))
  fixed_rows <- lapply(groups, function(g) {
    e <- g[1, , drop = FALSE]
    s <- if (nrow(g) > 1) g[2, , drop = FALSE] else NULL

    combine_inline <- inline_se || (single_model_inline && length(model_cols) == 1)

    vals <- lapply(model_cols, function(col) {
      ev <- as.character(e[[col]])
      sv <- if (!is.null(s)) as.character(s[[col]]) else ""
      if (combine_inline) {
        if (!nzchar(ev)) return("")
        if (nzchar(sv)) return(paste(ev, sv))
        return(ev)
      }
      ev
    })
    names(vals) <- model_cols
    row_main <- as.data.frame(c(list(term = as.character(e$term)), vals), stringsAsFactors = FALSE, check.names = FALSE)

    if (combine_inline) {
      return(row_main)
    }

    if (!is.null(s)) {
      se_vals <- lapply(model_cols, function(col) as.character(s[[col]]))
      names(se_vals) <- model_cols
      row_se <- as.data.frame(c(list(term = ""), se_vals), stringsAsFactors = FALSE, check.names = FALSE)
      return(dplyr::bind_rows(row_main, row_se))
    }
    row_main
  })
  fixed_block <- dplyr::bind_rows(fixed_rows)

  re_block <- make_re_diagnostics_rows(models_list, include_random_slopes = include_random_slopes, include_random_correlations = include_random_correlations)
  qual_block <- make_model_quality_rows(models_list)

  sec_row <- function(lbl) {
    x <- as.list(c(lbl, rep("", length(model_cols))))
    names(x) <- c("term", model_cols)
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  }

  dplyr::bind_rows(
    sec_row("Фиксированные эффекты"),
    fixed_block,
    sec_row("Случайные эффекты"),
    re_block,
    sec_row("Качество модели"),
    qual_block
  )
}
