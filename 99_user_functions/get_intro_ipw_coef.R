#' @title Look up an IPW coefficient as a percentage for the introduction
#' @description Extracts a single coefficient cell from a data frame produced by
#'   `read_intro_ipw_coefs()`, converts it to a percentage (multiplies by 100)
#'   and rounds to one decimal. Used to render inline numeric values in the
#'   introduction chapter.
#' @param df A data frame produced by `read_intro_ipw_coefs()`.
#' @param var Character name of the coefficient (matched against `df$variable`).
#' @param col Character column name to read (e.g. `"q10_est"`).
#' @return A numeric scalar with the coefficient expressed as a percentage,
#'   rounded to one decimal.
get_intro_ipw_coef <- function(df, var, col) {
	round(as.numeric(df[df$variable == var, col][1]) * 100, 1)
}
