#' @title Build model-quality summary rows for a list of fitted models
#' @description For each fitted model, computes RMSE from residuals, marginal
#'   and conditional R-squared via `safe_r2()`, and N (`stats::nobs`), returning
#'   a wide data frame with one row per metric and one column per model.
#' @param models_list Named list of fitted models supported by `stats::residuals`,
#'   `stats::nobs` and `performance::r2_nakagawa()`.
#' @return A data frame with first column `term` (RMSE, R2 marginal,
#'   R2 conditional, N) and one further column per model.
make_model_quality_rows <- function(models_list) {
  one_model <- function(m) {
    r2_vals <- safe_r2(m)
    c(
      "RMSE" = sprintf("%.3f", sqrt(mean(stats::residuals(m)^2, na.rm = TRUE))),
      "R2 marginal" = ifelse(is.na(r2_vals[["marginal"]]), "", sprintf("%.3f", r2_vals[["marginal"]])),
      "R2 conditional" = ifelse(is.na(r2_vals[["conditional"]]), "", sprintf("%.3f", r2_vals[["conditional"]])),
      "N" = as.character(stats::nobs(m))
    )
  }

  stat_list <- lapply(models_list, one_model)
  stat_names <- c("RMSE", "R2 marginal", "R2 conditional", "N")

  rows <- lapply(stat_names, function(term) {
    vals <- sapply(stat_list, function(x) x[[term]])
    c(list(term = term), as.list(vals))
  })

  out <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  names(out) <- c("term", names(models_list))
  out
}
