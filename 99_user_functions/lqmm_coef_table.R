#' @title Coefficient-only table for one or more lqmm fits
#' @description Build a tidy data frame of point estimates from a single
#'   `lqmm` object or a named list of them (e.g. one bundle per quantile).
#'   Region dummies are dropped by default so the table fits on a page,
#'   and the variance of the random intercept (`m$theta_z`, if present) is
#'   appended as a final row labelled `"Var(random intercept)"`. No
#'   standard errors, p-values, confidence intervals, or other inferential
#'   quantities are included — this helper is intended for the annex,
#'   where the inferential summary lives in the chapter table.
#' @param models Either a single `lqmm` model or a (possibly named) list
#'   of `lqmm` models. `NULL` is accepted and produces a one-row tibble
#'   so the calling chunk does not error before the fits exist.
#' @param drop_region Logical; drop coefficients whose name starts with
#'   `"region"` (defaults to `TRUE`).
#' @param digits Integer; rounding for printed estimates. Defaults to 4.
#' @return A `tibble` with one row per coefficient and one column per
#'   model in `models`. Column names default to `q{round(100*tau)}` for
#'   unnamed list elements; otherwise they take their list names.
lqmm_coef_table <- function(models, drop_region = TRUE, digits = 4) {
  if (is.null(models)) {
    return(tibble::tibble(variable = "[not yet fit]"))
  }
  if (inherits(models, "lqmm")) models <- list(models)

  per_model <- function(m, label) {
    coefs <- stats::coef(m)
    if (drop_region) coefs <- coefs[!grepl("^region", names(coefs))]
    re_var <- if (!is.null(m$theta_z)) as.numeric(m$theta_z)[1] else NA_real_
    out <- tibble::tibble(
      variable = c(names(coefs), "Var(random intercept)"),
      value    = round(c(unname(coefs), re_var), digits)
    )
    names(out)[2] <- label
    out
  }

  labels <- names(models)
  if (is.null(labels) || any(!nzchar(labels))) {
    labels <- vapply(models, function(m) {
      paste0("q", round(100 * as.numeric(m$tau)))
    }, character(1))
  }

  per_tab <- Map(per_model, models, labels)
  Reduce(function(x, y) dplyr::full_join(x, y, by = "variable"), per_tab)
}
