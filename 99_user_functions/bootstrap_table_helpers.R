#' @title Build a compact bootstrap CI table for the chapter body
#' @description Takes a tidy bootstrap CI data frame (from
#'   `m*_bootCI.rds`) and returns a wide table with traits as rows and
#'   groups as columns. Each cell displays the point estimate followed by
#'   the requested CI, in the form `"+1.71 [-2.28, +5.51]"`. Suitable for
#'   the chapter body where space is limited and only one CI level is
#'   shown.
#' @param boot Data frame loaded from a `*_bootCI.rds` cache. Must have
#'   columns `trait`, `estimate`, and either `group` (chapter 4) or
#'   `quintile` (chapter 6), plus `ci_low_<level>` / `ci_high_<level>`.
#' @param ci_level Integer, one of `90`, `95`, `99`. Defaults to `90`.
#' @param group_labels Optional named character vector mapping group
#'   level codes to display labels (e.g. `c("Q1" = "Q1 (нижний)", ...)`).
#' @param trait_labels Optional named character vector mapping trait codes
#'   (`O`, `C`, `E`, `A`, `ES`) to display labels.
#' @param trait_order Character vector controlling row order. Defaults to
#'   `c("O","C","E","A","ES")`.
#' @param digits Integer number of decimal digits in the printed values.
#'   Defaults to `1` so each `[low, high]` string fits comfortably in a
#'   narrow body-table column without further line wrapping. The annex
#'   table (`bootstrap_to_full_table`) uses 2 digits.
#' @return Data frame with one row per trait and one column per group,
#'   plus a leading `"Черта"` column. Cell values are character strings
#'   ready for rendering via `df_to_flex()`.
bootstrap_to_compact_table <- function(boot,
                                       ci_level     = 90,
                                       group_labels = NULL,
                                       trait_labels = NULL,
                                       trait_order  = c("O","C","E","A","ES"),
                                       digits       = 1) {
  stopifnot(ci_level %in% c(90, 95, 99))
  ci_low_col  <- paste0("ci_low_",  ci_level)
  ci_high_col <- paste0("ci_high_", ci_level)
  group_col <- intersect(c("group", "quintile"), colnames(boot))[1]
  if (is.na(group_col)) stop("boot must have a 'group' or 'quintile' column.")

  est <- boot$estimate
  lo  <- boot[[ci_low_col]]
  hi  <- boot[[ci_high_col]]
  # Two lines per cell: estimate on top, CI on bottom. With digits = 1,
  # cells like "+1.7\n[-1.8, +4.9]" fit comfortably in a narrow column
  # without further wrapping.
  fmt <- paste0("%+.", digits, "f\n[%+.", digits, "f, %+.", digits, "f]")
  cell <- sprintf(fmt, est, lo, hi)

  group_codes <- unique(boot[[group_col]])
  group_disp  <- if (!is.null(group_labels))
    unname(group_labels[group_codes]) else group_codes

  trait_codes <- intersect(trait_order, unique(boot$trait))
  trait_disp  <- if (!is.null(trait_labels))
    unname(trait_labels[trait_codes]) else trait_codes

  out <- data.frame(`Черта` = trait_disp,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  for (i in seq_along(group_codes)) {
    g <- group_codes[i]; col <- group_disp[i]
    vals <- vapply(trait_codes, function(tr) {
      idx <- which(boot$trait == tr & boot[[group_col]] == g)
      if (length(idx) == 0) "" else cell[idx[1]]
    }, character(1))
    out[[col]] <- vals
  }
  out
}


#' @title Build a detailed bootstrap CI table for the annex
#' @description Long-format table with one row per trait × group slope and
#'   columns for the point estimate, all three CI levels (90/95/99%), the
#'   three p-values (`p_boot`, `p_holm`, `p_maxT`), and conventional star
#'   markers. Designed for annex placement where comprehensive statistical
#'   detail is appropriate.
#' @inheritParams bootstrap_to_compact_table
#' @return Data frame with one row per trait × group combination and the
#'   following columns: `Черта`, `Группа`, `Оценка`, `90% ДИ`, `95% ДИ`,
#'   `99% ДИ`, `p_boot`, `p_holm`, `p_maxT`, `Sig.`. Numeric values are
#'   formatted as character strings with two decimal digits.
bootstrap_to_full_table <- function(boot,
                                    group_labels = NULL,
                                    trait_labels = NULL,
                                    trait_order  = c("O","C","E","A","ES")) {
  # Force a UTF-8 locale at the moment the data frame is being built.
  # flextable's LaTeX writer uses format() under whatever locale is active
  # when it processes each cell, and if that locale isn't UTF-8 the Cyrillic
  # text comes out as "<U+041E>..." escape sequences inside per-character
  # \setmainfont{...} wrappers. Setting it here guarantees the right locale
  # at call time even if upstream chunks/setups didn't.
  for (loc in c("ru_RU.UTF-8", "en_US.UTF-8")) {
    res <- try(Sys.setlocale("LC_CTYPE", loc), silent = TRUE)
    if (!inherits(res, "try-error") && nzchar(res)) break
  }

  group_col <- intersect(c("group", "quintile"), colnames(boot))[1]
  if (is.na(group_col)) stop("boot must have a 'group' or 'quintile' column.")

  # Helper: explicitly mark UTF-8 on character vectors. R reads the .qmd
  # source as bytes and stores Russian string literals with Encoding =
  # "unknown"; under a C locale that surfaces as "<d0><9e>..." byte
  # escapes in the rendered LaTeX. Setting the encoding tag explicitly
  # tells downstream renderers (knitr, kableExtra) to treat the bytes
  # as UTF-8.
  utf8 <- function(x) { Encoding(x) <- "UTF-8"; x }

  trait_codes <- intersect(trait_order, unique(boot$trait))
  trait_disp  <- if (!is.null(trait_labels))
    unname(trait_labels[trait_codes]) else trait_codes
  trait_map   <- stats::setNames(utf8(trait_disp), trait_codes)

  group_codes <- unique(boot[[group_col]])
  group_disp  <- if (!is.null(group_labels))
    unname(group_labels[group_codes]) else group_codes
  group_map   <- stats::setNames(utf8(group_disp), group_codes)

  fmt_p <- function(p) {
    out <- character(length(p))
    out[is.na(p)] <- ""
    out[!is.na(p) & p < 0.001] <- "<0.001"
    ord <- !is.na(p) & p >= 0.001
    out[ord] <- sprintf("%.3f", p[ord])
    out
  }

  ord <- order(match(boot$trait, trait_codes),
               match(boot[[group_col]], group_codes))
  b   <- boot[ord, ]

  # Use the same construction pattern as bootstrap_to_compact_table:
  # backtick-quoted Russian column names with check.names = FALSE, and
  # column-by-column assignment for cell values. This approach renders
  # cleanly in flextable's LaTeX output where the data.frame(...)+names()<-
  # pattern produces "<U+041E>..." escape sequences instead.
  out <- data.frame(
    `Черта`  = utf8(unname(trait_map[b$trait])),
    `Группа` = utf8(unname(group_map[b[[group_col]]])),
    `Оценка` = sprintf("%+.2f", b$estimate),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Also mark the Russian column names as UTF-8.
  names(out) <- utf8(names(out))
  # Use "\\%" in column names so LaTeX renders the percent sign rather
  # than treating it as a comment marker (which would swallow the row
  # terminator "\\\\" and break the tabular's \midrule placement).
  out[["90\\% ДИ"]] <- sprintf("[%+.2f, %+.2f]", b$ci_low_90, b$ci_high_90)
  out[["95\\% ДИ"]] <- sprintf("[%+.2f, %+.2f]", b$ci_low_95, b$ci_high_95)
  out[["99\\% ДИ"]] <- sprintf("[%+.2f, %+.2f]", b$ci_low_99, b$ci_high_99)
  # Underscores would be parsed as LaTeX math subscripts outside math mode.
  # Use a non-breaking-hyphen unicode glyph (or a plain hyphen) instead.
  out[["p (boot)"]] <- fmt_p(b$p_boot)
  out[["p (Holm)"]] <- fmt_p(b$p_holm)
  out[["p (max-T)"]] <- fmt_p(b$p_maxT)
  out[["Sig."]]     <- b$stars
  out
}
