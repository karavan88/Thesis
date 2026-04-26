#' @title Format a percentage scalar for inline rendering
#' @description Returns an empty string for NA, an integer (rounded) when the
#'   value is at least 1, and a one-decimal value otherwise. Used to produce
#'   inline percentages that read naturally in Russian text.
#' @param x Numeric scalar. A percentage value (already on the 0-100 scale).
#' @return Either a character `""` (for NA), an integer, or a numeric rounded
#'   to one decimal.
fmt_pct <- function(x) {
  if (is.na(x)) return("")
  if (x >= 1) as.integer(round(x)) else round(x, 1)
}
