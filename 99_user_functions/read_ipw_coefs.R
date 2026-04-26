#' @title Read an IPW quantile-coefficient CSV from the wages outputs
#' @description Reads a header-less IPW coefficient CSV from the `out_returns`
#'   directory (a chunk-level alias for `outputsReturnsNcs`) and assigns the
#'   canonical 26 column names: `variable` plus estimate / SE / lower / upper /
#'   p-value columns for each of the five quantiles (Q10, Q25, Q50, Q75, Q90).
#' @param fname Character file name (relative to `out_returns`) of the
#'   coefficient CSV to read.
#' @return A tibble with 26 named columns ready for downstream lookups by
#'   `get_coef()`.
read_ipw_coefs <- function(fname) {
  readr::read_csv(
    file.path(out_returns, fname), show_col_types = FALSE,
    col_names = c("variable",
      "q10_est","q10_se","q10_lo","q10_hi","q10_p",
      "q25_est","q25_se","q25_lo","q25_hi","q25_p",
      "q50_est","q50_se","q50_lo","q50_hi","q50_p",
      "q75_est","q75_se","q75_lo","q75_hi","q75_p",
      "q90_est","q90_se","q90_lo","q90_hi","q90_p"))
}
