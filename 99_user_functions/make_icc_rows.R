#' @title Build ICC rows for a list of fitted mixed models
#' @description For each model, computes the variance partition coefficient
#'   (ICC) per grouping factor as the group variance divided by the total
#'   variance (group variances + residual variance, with residual variance set
#'   to pi^2/3 for binomial models). Returns a wide data frame with one row
#'   per grouping factor (plus a residual row) and one column per model.
#' @param models_list Named list of fitted mixed-effects models supported by
#'   `lme4::VarCorr()`.
#' @return A data frame with first column `term` (Russian ICC labels) and one
#'   further column per model.
make_icc_rows <- function(models_list) {
  lbl_map <- c(
    idind                = "ICC: Индивидуальный ID",
    region               = "ICC: Регион",
    age                  = "ICC: Возраст",
    age_factor           = "ICC: Возраст",
    edu_lvl              = "ICC: Образование",
    ses5                 = "ICC: Среднедушевой доход ДХ",
    sex                  = "ICC: Пол",
    occupation           = "ICC: Профессия",
    hourly_wage_quintile = "ICC: Квинтиль зарплаты"
  )
  get_var_comps <- function(m) {
    tryCatch({
      vc <- lme4::VarCorr(m)
      fam <- tryCatch(stats::family(m)$family, error = function(e) "gaussian")
      sigma2_e <- if (identical(fam, "binomial")) pi^2 / 3 else attr(vc, "sc")^2
      group_vars <- sapply(vc, function(mat) as.numeric(mat)[1])
      total_var <- sum(group_vars) + sigma2_e
      list(
        group    = round(group_vars / total_var, 3),
        residual = round(sigma2_e   / total_var, 3)
      )
    }, error = function(e) NULL)
  }
  results <- lapply(models_list, get_var_comps)
  all_groups <- unique(unlist(lapply(results, function(r) if (!is.null(r)) names(r$group))))
  rows <- lapply(all_groups, function(g) {
    lbl  <- if (g %in% names(lbl_map)) lbl_map[[g]] else paste0("ICC: ", g)
    vals <- sapply(results, function(r) {
      if (is.null(r)) return("")
      if (g %in% names(r$group)) as.character(r$group[[g]]) else ""
    })
    c(list(term = lbl), as.list(vals))
  })
  resid_vals <- sapply(results, function(r) if (is.null(r)) "" else as.character(r$residual))
  rows <- c(rows, list(c(list(term = "ICC: Остаток"), as.list(resid_vals))))
  df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  names(df) <- c("term", names(models_list))
  df
}
