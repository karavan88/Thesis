#' @title Format a numeric value to two decimal places, with empty string for NA
#' @description One-liner helper used in psychometric tables: returns a
#'   formatted character string with two decimals, or an empty string when the
#'   input is NA.
#' @param x Numeric vector (or scalar) to format.
#' @return Character vector of the same length as `x` with two-decimal
#'   formatting; NA values become `""`.
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", x))
