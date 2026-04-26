#' @title Plot random-slope predicted curves for non-cognitive skills
#' @description For each Big-Five skill in `skill_vars`, calls
#'   `ggeffects::ggpredict()` with `type = "random"` over a fixed grid
#'   `[-2, 2]`, recodes skill names to Russian via `skill_code_labels_ru` from
#'   the calling environment, and faceted-plots the resulting curves with
#'   ribbons by group. Returns a placeholder ggplot when no predictions can be
#'   computed.
#' @param model A fitted mixed-effects model with random slopes for the skills.
#' @param group_var Character name of the grouping factor (e.g. `"sex"`,
#'   `"edu_lvl"`) used both as a `ggpredict` term and the plot's colour
#'   aesthetic.
#' @param group_labels Optional named character vector recoding group level
#'   strings to display labels. Defaults to `NULL`.
#' @param skill_vars Character vector of skill column names. Defaults to
#'   `c("O", "C", "E", "A", "ES")`.
#' @param y_label Character. Y-axis label. Defaults to `"Pr(занятость)"`.
#' @return A `ggplot` object.
plot_random_slope_curves <- function(model, group_var, group_labels = NULL,
                                     skill_vars = c("O", "C", "E", "A", "ES"),
                                     y_label = "Pr(занятость)") {
  pred_list <- lapply(skill_vars, function(sv) {
    term1 <- sprintf("%s [-2:2 by=0.1]", sv)
    p <- tryCatch(
      ggeffects::ggpredict(model, terms = c(term1, group_var), type = "random"),
      error = function(e) NULL
    )
    if (is.null(p)) return(NULL)
    d <- as.data.frame(p)
    d$skill <- skill_code_labels_ru[[sv]]
    d
  })

  pred <- dplyr::bind_rows(pred_list)
  if (nrow(pred) == 0) {
    return(ggplot() + theme_void() +
             labs(title = "Не удалось построить кривые: проверьте спецификацию модели"))
  }

  pred <- pred |>
    dplyr::mutate(group = as.character(group))
  if (!is.null(group_labels)) {
    pred <- pred |> dplyr::mutate(group = dplyr::recode(group, !!!group_labels))
  }

  p <- ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_line(linewidth = 0.8)

  if (all(c("conf.low", "conf.high") %in% names(pred))) {
    p <- p + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA)
  }

  p +
    facet_wrap(~ skill, scales = "free_y") +
    labs(x = "Значение навыка (стандартизованная шкала)",
         y = y_label,
         color = NULL,
         fill = NULL) +
    theme_bw(base_family = plot_text_family, base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
}
