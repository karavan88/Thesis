#' @title Extract a fixed-effect coefficient as a percentage
#' @description Pulls a single fixed-effect coefficient from a fitted
#'   mixed-effects model via `fixef()`, multiplies by 100 and rounds. Used to
#'   render inline percentage-point values in the employment chapter.
#' @param model A fitted model accepted by `lme4::fixef()`.
#' @param var_name Character name of the fixed-effect coefficient to extract.
#' @param digits Integer number of decimal digits to round to. Defaults to `1`.
#' @return Numeric scalar coefficient on the percentage scale.
get_coef_pct <- function(model, var_name, digits = 1) {
  # Extract coefficient (assuming it's in marginal effects or probability form)
  coef_val <- fixef(model)[var_name] * 100  # Convert to percentage
  return(round(coef_val, digits))
}
