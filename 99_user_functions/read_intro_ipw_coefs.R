#' @title Read an IPW quantile-coefficient CSV used in the introduction
#' @description Reads a chapter-5 IPW coefficient CSV produced by
#'   `04_summarize_models_returns.R` (one row per coefficient, columns
#'   `q10_estimate`, `q10_std.error`, `q10_ci.low`, `q10_ci.upp`,
#'   `q10_p.value`, ... up to q90). The columns are renamed to the short
#'   aliases the introduction chunk expects: `q10_est`, `q10_se`,
#'   `q10_lo`, `q10_hi`, `q10_p`, ... — so existing call sites keep working.
#'
#'   If the CSV does not exist yet (chapter-5 fits not yet run), returns a
#'   one-row placeholder with NA values so the intro renders without
#'   erroring; numeric extracts via `get_intro_ipw_coef()` will return NA
#'   and the inline figures will appear as `NA` in the rendered text.
#' @param fname Character file name (relative to `returns_thesis_out`) of
#'   the coefficient CSV to read, e.g. `"m1_ipw_coefs.csv"`.
#' @return A data frame with 26 named columns ready for `get_intro_ipw_coef()`.
read_intro_ipw_coefs <- function(fname) {
  short_names <- c(
    "variable",
    "q10_est", "q10_se", "q10_lo", "q10_hi", "q10_p",
    "q25_est", "q25_se", "q25_lo", "q25_hi", "q25_p",
    "q50_est", "q50_se", "q50_lo", "q50_hi", "q50_p",
    "q75_est", "q75_se", "q75_lo", "q75_hi", "q75_p",
    "q90_est", "q90_se", "q90_lo", "q90_hi", "q90_p"
  )

  path <- file.path(returns_thesis_out, fname)

  if (!file.exists(path)) {
    out <- as.data.frame(matrix(NA_real_, nrow = 1, ncol = length(short_names) - 1))
    names(out) <- short_names[-1]
    out$variable <- "[Не подогнано]"
    return(out[, short_names])
  }

  df <- utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  long_names <- c(
    "variable",
    "q10_estimate", "q10_std.error", "q10_ci.low", "q10_ci.upp", "q10_p.value",
    "q25_estimate", "q25_std.error", "q25_ci.low", "q25_ci.upp", "q25_p.value",
    "q50_estimate", "q50_std.error", "q50_ci.low", "q50_ci.upp", "q50_p.value",
    "q75_estimate", "q75_std.error", "q75_ci.low", "q75_ci.upp", "q75_p.value",
    "q90_estimate", "q90_std.error", "q90_ci.low", "q90_ci.upp", "q90_p.value"
  )

  if (all(long_names %in% names(df))) {
    df <- df[, long_names]
    names(df) <- short_names
  }

  df
}
