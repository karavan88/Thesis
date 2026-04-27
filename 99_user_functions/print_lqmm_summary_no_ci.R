#' @title Print lqmm summary output without confidence-interval columns
#' @description Helper for the chapter-5 annex. Accepts a pre-computed
#'   `summary.lqmm` object, a list of them (one per quantile or age band),
#'   or a raw `lqmm` model. Prints the fixed-effects table — coefficient,
#'   standard error, p-value only — for each. Lower- and upper-bound CI
#'   columns produced by `summary.lqmm()` are dropped to keep the table
#'   narrow enough for a portrait page; region dummies are hidden by
#'   default. The variance of the random intercept is appended as a
#'   single-line summary so random effects remain visible.
#'
#'   Output is `cat()`-ed inside one fenced ``` ``` block; intended for a
#'   chunk with `#| results: asis`. Pass NULL for a "[не подогнано]"
#'   placeholder so the book renders before the chapter-5 fits exist.
#'
#'   Prefer summaries over models — they are pre-computed by
#'   `04_summarize_models_returns.R` and avoid re-running the bootstrap on
#'   every render. If a raw `lqmm` model is passed, the helper does call
#'   `summary()` on it, which is slow.
#'
#' @param label Character label used as the placeholder marker when input
#'   is `NULL`.
#' @param summaries Either a `summary.lqmm` object, a list of them, an
#'   `lqmm` model, a list of `lqmm` models, or `NULL`.
#' @param drop_region Logical; drop coefficients whose name starts with
#'   `"region"`. Defaults to `TRUE`.
#' @param digits Integer; rounding for printed values. Defaults to 4.
#' @return Invisibly `NULL`.
print_lqmm_summary_no_ci <- function(label, summaries,
                                     drop_region = TRUE, digits = 4) {
  if (is.null(summaries)) {
    cat("```\n[", label, " — модель ещё не подогнана. ",
        "Запустите 03_fit_models_returns.R + 04_summarize_models_returns.R.]\n```\n",
        sep = "")
    return(invisible(NULL))
  }

  # Coerce the various accepted shapes to a named list of summary objects.
  is_summary <- function(x) is.list(x) && !is.null(x$tTable)

  if (inherits(summaries, "lqmm")) {
    summaries <- list(summary(summaries))
  } else if (is_summary(summaries)) {
    summaries <- list(summaries)
  } else if (is.list(summaries)) {
    # Already a list. Each element may be an lqmm model, a summary, or NULL.
    summaries <- lapply(summaries, function(x) {
      if (is.null(x))                 NULL
      else if (is_summary(x))         x
      else if (inherits(x, "lqmm"))   summary(x)
      else                            x  # let the printer fall back to "[unavailable]"
    })
  }

  labels <- names(summaries)
  if (is.null(labels) || any(!nzchar(labels))) {
    labels <- vapply(seq_along(summaries), function(i) {
      s <- summaries[[i]]
      tau <- if (is_summary(s)) s$tau else if (inherits(s, "lqmm")) s$tau else NA_real_
      if (is.finite(tau)) paste0("q", round(100 * tau)) else paste0("[[", i, "]]")
    }, character(1))
  }

  cat("```\n")
  for (i in seq_along(summaries)) {
    s  <- summaries[[i]]
    nm <- labels[[i]]

    if (is.null(s) || !is_summary(s)) {
      cat("== ", nm, " ==\n[summary unavailable]\n\n", sep = "")
      next
    }

    t  <- s$tTable
    keep_cols <- setdiff(colnames(t), c("lower bound", "upper bound"))
    t2 <- as.data.frame(t[, keep_cols, drop = FALSE])
    if (drop_region) t2 <- t2[!grepl("^region", rownames(t2)), , drop = FALSE]
    t2 <- round(t2, digits)

    tau <- if (!is.null(s$tau)) round(as.numeric(s$tau), 2) else NA_real_
    cat("== ", nm,
        if (is.finite(tau)) paste0("  (tau = ", tau, ")") else "",
        " ==\n", sep = "")
    print(t2)

    re_var <- tryCatch({
      vc <- s$VarCov
      if (!is.null(vc)) as.numeric(vc)[1] else NA_real_
    }, error = function(e) NA_real_)

    cat("\nRandom intercept variance: ",
        if (is.finite(re_var)) round(re_var, digits) else "NA", "\n\n",
        sep = "")
  }
  cat("```\n")
  invisible(NULL)
}
