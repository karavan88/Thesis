#' @title Plot random-slope predicted trends for non-cognitive skills (job satisfaction)
#' @description Job-satisfaction-chapter version of the random-slope plotter:
#'   for each skill, calls `ggeffects::ggpredict()` with `type = "random"` over
#'   `[-2, 2]` and faceted-plots the resulting curves with ribbons. Differs
#'   from `plot_random_slope_curves()` in that skill labels and group labels
#'   are passed in as arguments rather than read from the calling environment,
#'   and the legend titles refer to wage quintiles.
#' @param model A fitted mixed-effects model with random slopes for the skills.
#' @param group_var Character name of the grouping factor used as the colour
#'   aesthetic. Defaults to `"hourly_wage_quintile"`.
#' @param skill_vars Character vector of skill column names. Defaults to
#'   `c("O", "C", "E", "A", "ES")`.
#' @param skill_labels Optional named character vector recoding skill codes to
#'   display labels. Defaults to `NULL`.
#' @param group_labels Optional named character vector recoding group level
#'   strings to display labels. Defaults to `NULL`.
#' @param y_label Character. Y-axis label. Defaults to
#'   `"Pr(удовлетворенность работой)"`.
#' @return A `ggplot` object.
plot_random_slope_trends_js <- function(model,
                                        group_var = "hourly_wage_quintile",
                                        skill_vars = c("O", "C", "E", "A", "ES"),
                                        skill_labels = NULL,
                                        group_labels = NULL,
                                        y_label = "Pr(удовлетворенность работой)") {
  pred_list <- lapply(skill_vars, function(sv) {
    term1 <- sprintf("%s [-2:2 by=0.1]", sv)
    p <- tryCatch(
      ggeffects::ggpredict(model, terms = c(term1, group_var), type = "random"),
      error = function(e) NULL
    )
    if (is.null(p)) return(NULL)
    d <- as.data.frame(p)
    d$skill <- sv
    d
  })

  pred <- dplyr::bind_rows(pred_list)
  if (nrow(pred) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "Не удалось построить предсказанные тренды: проверьте спецификацию модели"))
  }

  pred <- pred |>
    dplyr::mutate(
      skill = if (!is.null(skill_labels)) dplyr::recode(skill, !!!skill_labels, .default = skill) else skill,
      group = as.character(group)
    )

  if (!is.null(group_labels)) {
    pred <- pred |> dplyr::mutate(group = dplyr::recode(group, !!!group_labels, .default = group))
  }

  p <- ggplot2::ggplot(pred, ggplot2::aes(x = x, y = predicted, color = group, fill = group)) +
    ggplot2::geom_line(linewidth = 0.8)

  if (all(c("conf.low", "conf.high") %in% names(pred))) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA)
  }

  p +
    ggplot2::facet_wrap(~ skill, scales = "free_y") +
    ggplot2::labs(
      x = "Значение навыка (стандартизованная шкала)",
      y = y_label,
      color = "Квинтиль заработной платы",
      fill = "Квинтиль заработной платы"
    ) +
    ggplot2::theme_bw(base_family = plot_text_family, base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
