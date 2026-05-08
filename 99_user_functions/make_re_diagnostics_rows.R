#' @title Build random-effects diagnostic rows for a list of mixed models
#' @description For each fitted mixed-effects model, extracts variance components
#'   via `lme4::VarCorr()`, formats each as `value (share%)` of total variance,
#'   relabels grouping factors and slope variables to Russian labels, and
#'   returns a wide data frame with one row per random-effects term and one
#'   column per model. Rows are ordered: `Var(u0)` (random intercepts) first,
#'   then `Var(u1)` (random slopes), then `Var(Residual)`.
#' @param models_list Named list of fitted `merMod` objects from `lme4`/`lmerTest`.
#' @param digits Integer number of decimal digits used to format variance
#'   estimates. Defaults to `3`.
#' @param include_random_slopes Logical. When `FALSE`, drops `Var(u1)` rows
#'   (random-slope variance components) from the output. Defaults to `TRUE`.
#' @param include_random_correlations Logical. When `TRUE`, appends correlation
#'   rows (`Cor: grp | var1, var2`) between random effects sharing a grouping
#'   factor — i.e. the off-diagonals of each random-effect covariance matrix —
#'   formatted as the correlation coefficient (signed, three decimals).
#'   Defaults to `FALSE`.
#' @return A data frame with first column `term` and one further column per
#'   model in `models_list` containing formatted variance strings.
make_re_diagnostics_rows <- function(models_list, digits = 3, include_random_slopes = TRUE, include_random_correlations = FALSE) {
  var_labels <- c(
    "idind" = "Индивидуальный ID",
    "region" = "Регион",
    "occupation" = "Профессия",
    "hourly_wage_quintile" = "Квинтиль заработной платы",
    "age" = "Возраст",
    "age_factor" = "Возраст",
    "edu_lvl" = "Уровень образования",
    "ses5" = "Среднедушевой доход ДХ",
    "sex" = "Пол",
    "O" = "Открытость",
    "C" = "Добросовестность",
    "E" = "Экстраверсия",
    "A" = "Доброжелательность",
    "ES" = "Эмоциональная стабильность"
  )

  intercept_lab <- "u0"

  format_one <- function(m) {
    vc <- as.data.frame(lme4::VarCorr(m))
    vc <- vc |>
      dplyr::mutate(
        grp_lab = dplyr::recode(grp, !!!var_labels, .default = grp),
        var1_lab = dplyr::recode(var1, !!!var_labels, .default = var1),
        var2_lab = dplyr::recode(dplyr::coalesce(var2, ""), !!!var_labels, .default = dplyr::coalesce(var2, "")),
        var1_lab = dplyr::if_else(var1 == "(Intercept)", intercept_lab, var1_lab),
        var2_lab = dplyr::if_else(var2 %in% "(Intercept)", intercept_lab, var2_lab),
        cor_first  = dplyr::if_else(var1 == "(Intercept)" | (!is.na(var2) & var2 != "(Intercept)" & var1_lab < var2_lab),
                                    var1_lab, var2_lab),
        cor_second = dplyr::if_else(cor_first == var1_lab, var2_lab, var1_lab),
        comp = dplyr::case_when(
          grp == "Residual" ~ "Var(Residual)",
          is.na(var2) & var1 == "(Intercept)" ~ paste0("Var(u0): ", grp_lab),
          is.na(var2) ~ paste0("Var(u1): ", grp_lab, " | ", var1_lab),
          TRUE ~ paste0("Cor: ", grp_lab, " | ", cor_first, ", ", cor_second)
        )
      )

    var_rows <- vc[is.na(vc$var2), , drop = FALSE]
    cor_rows <- vc[!is.na(vc$var2), , drop = FALSE]

    total_var <- sum(var_rows$vcov, na.rm = TRUE)
    vals_var <- setNames(
      sprintf(paste0("%.", digits, "f (", "%.1f%%", ")"), var_rows$vcov, 100 * var_rows$vcov / total_var),
      var_rows$comp
    )

    if (include_random_correlations && nrow(cor_rows) > 0) {
      vals_cor <- setNames(
        sprintf(paste0("%+.", digits, "f"), cor_rows$sdcor),
        cor_rows$comp
      )
      c(vals_var, vals_cor)
    } else {
      vals_var
    }
  }

  diag_list <- lapply(models_list, format_one)
  all_terms <- unique(unlist(lapply(diag_list, names)))
  int_terms <- grep("^Var\\(u0\\)", all_terms, value = TRUE)
  slope_terms <- if (include_random_slopes) grep("^Var\\(u1\\)", all_terms, value = TRUE) else character(0)
  cor_terms <- if (include_random_correlations) grep("^Cor:", all_terms, value = TRUE) else character(0)
  resid_terms <- grep("^Var\\(Residual\\)$", all_terms, value = TRUE)
  ordered_terms <- c(sort(int_terms), sort(slope_terms), sort(cor_terms), resid_terms)
  ordered_terms <- ordered_terms[ordered_terms %in% all_terms]

  rows <- lapply(ordered_terms, function(term) {
    vals <- sapply(diag_list, function(x) ifelse(term %in% names(x), x[[term]], ""))
    c(list(term = term), as.list(vals))
  })

  out <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  names(out) <- c("term", names(models_list))
  out
}
