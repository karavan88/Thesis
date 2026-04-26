#' @title Extract an ICC value for a grouping factor as a percentage
#' @description Looks up the row of an `icc_result` data frame whose `Group`
#'   matches `group_name`, multiplies the ICC by 100 and rounds. Used to render
#'   inline ICC percentages in the employment chapter.
#' @param icc_result Data frame with at least columns `Group` and `ICC` (e.g.
#'   the output of `performance::icc(model, by_group = TRUE)`).
#' @param group_name Character grouping factor name to look up in `Group`.
#' @param digits Integer number of decimal digits to round to. Defaults to `0`.
#' @return Numeric scalar ICC on the percentage scale.
get_icc_pct <- function(icc_result, group_name, digits = 0) {
  # Extract ICC value for specific group
  icc_val <- icc_result[icc_result$Group == group_name, "ICC"] * 100
  return(round(icc_val, digits))
}
