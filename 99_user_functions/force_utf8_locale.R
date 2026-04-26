#' @title Force a UTF-8 locale for the current R session
#' @description Tries to set the LC_CTYPE locale to a UTF-8 locale (Russian first,
#'   then US English) so that Cyrillic table and figure labels render correctly in
#'   downstream output (DOCX, PDF, plots). Emits a warning if no UTF-8 locale is
#'   available and returns NULL invisibly.
#' @return Invisibly, the locale string that was successfully set, or NULL if
#'   none could be applied.
force_utf8_locale <- function() {
  for (loc in c("ru_RU.UTF-8", "en_US.UTF-8")) {
    res <- try(Sys.setlocale("LC_CTYPE", loc), silent = TRUE)

    if (!inherits(res, "try-error") && is.character(res) && nzchar(res)) {
      Sys.setenv(LANG = res, LC_CTYPE = res)
      options(encoding = "UTF-8")
      return(invisible(res))
    }
  }

  warning("UTF-8 locale is unavailable; DOCX table and figure labels may render incorrectly.")
  invisible(NULL)
}
