#' @title Read an IPW quantile-coefficient CSV used in the introduction
#' @description Reads a header-less IPW coefficient CSV from
#'   `outputsReturnsNcs` and assigns canonical column names: `variable` plus
#'   estimate / SE / lower / upper / p-value columns for each of the five
#'   quantiles (Q10, Q25, Q50, Q75, Q90).
#' @param fname Character file name (relative to `outputsReturnsNcs`) of the
#'   coefficient CSV to read.
#' @return A data frame with 26 named columns ready for downstream coefficient
#'   look-ups by `get_intro_ipw_coef()`.
read_intro_ipw_coefs <- function(fname) {
	df <- utils::read.csv(file.path(outputsReturnsNcs, fname), header = FALSE, stringsAsFactors = FALSE)
	names(df) <- c(
		"variable",
		"q10_est", "q10_se", "q10_lo", "q10_hi", "q10_p",
		"q25_est", "q25_se", "q25_lo", "q25_hi", "q25_p",
		"q50_est", "q50_se", "q50_lo", "q50_hi", "q50_p",
		"q75_est", "q75_se", "q75_lo", "q75_hi", "q75_p",
		"q90_est", "q90_se", "q90_lo", "q90_hi", "q90_p"
	)
	df
}
