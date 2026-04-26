#' @title Safely compute Nakagawa marginal and conditional R-squared
#' @description Wraps `performance::r2_nakagawa()` so that errors and warnings
#'   are suppressed, returning a length-2 named numeric vector with NA values
#'   when the calculation fails.
#' @param model A fitted model object accepted by `performance::r2_nakagawa()`,
#'   typically a mixed-effects model from `lme4`.
#' @return A named numeric vector of length 2 with names `marginal` and
#'   `conditional`. NA values are returned when computation fails.
safe_r2 <- function(model) {
  out <- NULL
  tryCatch({
    suppressWarnings(suppressMessages({
      capture.output({
        out <- performance::r2_nakagawa(model, verbose = FALSE)
      })
    }))
  }, error = function(e) {
    out <<- NULL
  })
  if (is.null(out)) return(c(marginal = NA_real_, conditional = NA_real_))
  c(marginal = unname(out$R2_marginal), conditional = unname(out$R2_conditional))
}
