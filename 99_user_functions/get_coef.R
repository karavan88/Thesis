#' @title Look up a wage-model IPW coefficient as a percentage
#' @description Extracts a single coefficient cell from a data frame produced by
#'   `read_ipw_coefs()`, converts it to a percentage (multiplies by 100) and
#'   rounds to one decimal. Used to render inline numeric values in the wages
#'   chapter.
#' @param df A data frame produced by `read_ipw_coefs()`.
#' @param var Character name of the coefficient (matched against `df$variable`).
#' @param col Character column name to read (e.g. `"q10_est"`).
#' @return A numeric scalar with the coefficient expressed as a percentage,
#'   rounded to one decimal.
get_coef <- function(df, var, col) {
  val <- suppressWarnings(as.numeric(df[df$variable == var, col, drop = TRUE][1]))
  round(val * 100, 1)
}
