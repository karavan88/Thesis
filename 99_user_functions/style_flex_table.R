#' @title Apply consistent thesis styling to a flextable
#' @description Applies the standard thesis visual style to a flextable: Times New
#'   Roman font, output-aware sizes/padding, bold header, left-aligned first
#'   column, centred remaining columns, optional footnote, and section-header
#'   formatting (merge + bold + grey background) for the rows whose first column
#'   matches one of the standard Russian section labels ("Фиксированные эффекты",
#'   "Случайные эффекты", "Качество модели"). When rendering to LaTeX the table
#'   is given a fixed layout with column widths derived from `latex_total_width`.
#' @param ft A `flextable` object to style.
#' @param note Optional character vector of footnote lines added to the footer.
#'   Defaults to `NULL` (no footer).
#' @param latex_total_width Numeric total table width in inches used when
#'   rendering to LaTeX. Defaults to `5.6`.
#' @return The styled `flextable` object.
style_flex_table <- function(ft, note = NULL, latex_total_width = 5.6) {
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = if (knitr::is_latex_output()) 9 else 10, part = "all")
  ft <- flextable::padding(ft, padding = if (knitr::is_latex_output()) 1 else 4, part = "all")
  ft <- flextable::line_spacing(ft, space = 1, part = "all")
  ft <- flextable::bold(ft, part = "header")
  if ("term" %in% ft$col_keys) {
    ft <- flextable::set_header_labels(ft, term = "")
  }

  if (length(ft$col_keys) > 1) {
    ft <- flextable::align(ft, j = ft$col_keys[-1], align = "center", part = "all")
  }

  ft <- flextable::align(ft, j = ft$col_keys[1], align = "left", part = "all")

  section_titles <- c("Фиксированные эффекты", "Случайные эффекты", "Качество модели")
  ds <- ft$body$dataset
  if (!is.null(ds)) {
    section_col <- if ("term" %in% names(ds)) "term" else if ("variable" %in% names(ds)) "variable" else NULL
    if (!is.null(section_col)) {
      sec_rows <- which(ds[[section_col]] %in% section_titles)
      if (length(sec_rows) > 0) {
        for (r in sec_rows) {
          ft <- flextable::merge_at(ft, i = r, j = seq_along(ft$col_keys), part = "body")
        }
        ft <- flextable::bold(ft, i = sec_rows, part = "body")
        ft <- flextable::bg(ft, i = sec_rows, bg = "#F2F2F2", part = "body")
        ft <- flextable::align(ft, i = sec_rows, align = "left", part = "body")
      }
    }
  }

  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(ft, values = note)
    ft <- flextable::font(ft, fontname = "Times New Roman", part = "footer")
    ft <- flextable::fontsize(ft, size = 9, part = "footer")
  }

  ft <- flextable::set_table_properties(
    ft,
    layout = if (knitr::is_latex_output()) "fixed" else "autofit",
    opts_word = list(split = TRUE),
    opts_pdf = list(float = "none", tabcolsep = 1, arraystretch = 1)
  )
  if (knitr::is_latex_output()) {
    ncols <- length(ft$col_keys)
    total_w <- latex_total_width
    if (ncols >= 2) {
      first_w <- max(min(2.2, total_w - 0.6 * (ncols - 1)), 1.6)
      other_w <- max((total_w - first_w) / (ncols - 1), 0.55)
      ft <- flextable::width(ft, j = 1, width = first_w)
      ft <- flextable::width(ft, j = 2:ncols, width = other_w)
    } else {
      ft <- flextable::width(ft, j = 1, width = total_w)
    }
    ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
    return(ft)
  }

  flextable::autofit(ft)
}
