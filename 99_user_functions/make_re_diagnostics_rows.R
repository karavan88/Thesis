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
#' @return A data frame with first column `term` and one further column per
#'   model in `models_list` containing formatted variance strings.
make_re_diagnostics_rows <- function(models_list, digits = 3) {
  var_labels <- c(
    "idind" = "Индивидуальный ID",
    "region" = "Регион",
    "occupation" = "Профессия",
    "hourly_wage_quintile" = "Квинтиль заработной платы",
    "age" = "Возраст",
    "age_factor" = "Возраст",
    "edu_lvl" = "Уровень образования",
    "ses5" = "СЭС",
    "sex" = "Пол",
    "O" = "Открытость",
    "C" = "Добросовестность",
    "E" = "Экстраверсия",
    "A" = "Доброжелательность",
    "ES" = "Эмоциональная стабильность"
  )

  format_one <- function(m) {
    vc <- as.data.frame(lme4::VarCorr(m))
    vc <- vc |>
      dplyr::filter(is.na(var2)) |>
      dplyr::mutate(
        grp_lab = dplyr::recode(grp, !!!var_labels, .default = grp),
        var1_lab = dplyr::recode(var1, !!!var_labels, .default = var1),
        comp = dplyr::case_when(
          grp == "Residual" ~ "Var(Residual)",
          var1 == "(Intercept)" ~ paste0("Var(u0): ", grp_lab),
          TRUE ~ paste0("Var(u1): ", grp_lab, " | ", var1_lab)
        )
      )

    total_var <- sum(vc$vcov, na.rm = TRUE)
    vals <- setNames(
      sprintf(paste0("%.", digits, "f (", "%.1f%%", ")"), vc$vcov, 100 * vc$vcov / total_var),
      vc$comp
    )

    vals
  }

  diag_list <- lapply(models_list, format_one)
  all_terms <- unique(unlist(lapply(diag_list, names)))
  int_terms <- grep("^Var\\(u0\\)", all_terms, value = TRUE)
  slope_terms <- grep("^Var\\(u1\\)", all_terms, value = TRUE)
  resid_terms <- grep("^Var\\(Residual\\)$", all_terms, value = TRUE)
  ordered_terms <- c(sort(int_terms), sort(slope_terms), resid_terms)
  ordered_terms <- ordered_terms[ordered_terms %in% all_terms]

  rows <- lapply(ordered_terms, function(term) {
    vals <- sapply(diag_list, function(x) ifelse(term %in% names(x), x[[term]], ""))
    c(list(term = term), as.list(vals))
  })

  out <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  names(out) <- c("term", names(models_list))
  out
}
