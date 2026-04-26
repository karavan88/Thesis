#' @title Look up a life-cycle coefficient as a percentage
#' @description Extracts a single life-cycle quantile coefficient from a data
#'   frame keyed by `variable` and `age_group`, multiplies by 100 and rounds to
#'   one decimal. Refactored from the original closure form so the data frame is
#'   passed explicitly rather than captured from the enclosing environment.
#' @param df A data frame with at least columns `variable`, `age_group` and
#'   `Value` (e.g. the output of reading `m_lc_ipw_coefs.csv`).
#' @param var Character name of the coefficient (matched against `df$variable`).
#' @param grp Character age-group label (matched against `df$age_group`).
#' @return A numeric scalar with the coefficient expressed as a percentage,
#'   rounded to one decimal.
get_lc <- function(df, var, grp) {
  round(df[df$variable == var & df$age_group == grp, "Value", drop = TRUE] * 100, 1)
}
