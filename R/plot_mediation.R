#' Plot mediation paths
#'
#' Draw the exposure, mediator, and outcome path from a
#' \code{mediation_analysis()} result.
#'
#' @param mediation_object Object returned by \code{mediation_analysis()}.
#' @param show_estimates Logical; if \code{TRUE}, show direct and indirect
#'   effect estimates on the plot.
#' @param base_size Base font size.
#'
#' @return A \code{ggplot2} object.
#' @importFrom rlang .data
#'
#' @examples
#' med <- mediation_analysis(
#'   data = data_diabetes_mediation,
#'   exposure = obesity,
#'   mediator = glucose,
#'   outcome = diabetes,
#'   covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
#'   outcome_approach = logit,
#'   sims = 50,
#'   seed = 123
#' )
#' plot_mediation(med)
#' plot_mediation(med, show_estimates = FALSE)
#'
#' @export
plot_mediation <- function(mediation_object,
                           show_estimates = TRUE,
                           base_size = 13) {
  if (!inherits(mediation_object, "mediation_analysis")) {
    stop("`mediation_object` must be returned by `mediation_analysis()`.", call. = FALSE)
  }
  if (!is.logical(show_estimates) || length(show_estimates) != 1L || is.na(show_estimates)) {
    stop("`show_estimates` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(base_size) || length(base_size) != 1L || is.na(base_size) || base_size <= 0) {
    stop("`base_size` must be a single positive number.", call. = FALSE)
  }

  labels <- mediation_object$variable_labels
  if (is.null(labels)) {
    labels <- .var_label_map(
      mediation_object$complete_data,
      c(mediation_object$exposure, mediation_object$mediator, mediation_object$outcome)
    )
  }
  exposure_lab <- .label_var(mediation_object$exposure, labels)
  mediator_lab <- .label_var(mediation_object$mediator, labels)
  outcome_lab <- .label_var(mediation_object$outcome, labels)

  nodes <- data.frame(
    node = c("Exposure", "Mediator", "Outcome"),
    label = c(exposure_lab, mediator_lab, outcome_lab),
    x = c(0, 1, 2),
    y = c(0, 0.7, 0),
    stringsAsFactors = FALSE
  )
  paths <- data.frame(
    x = c(0.18, 1.18, 0.18),
    y = c(0.08, 0.62, -0.06),
    xend = c(0.82, 1.82, 1.82),
    yend = c(0.62, 0.08, -0.06),
    path = c("Exposure -> mediator", "Mediator -> outcome", "Direct path"),
    stringsAsFactors = FALSE
  )

  effects <- mediation_object$table_body
  direct <- effects$estimate[effects$effect == "direct"]
  indirect <- effects$estimate[effects$effect == "indirect"]
  total <- effects$estimate[effects$effect == "total"]

  ann <- data.frame(
    x = c(0.5, 1.5, 1.0),
    y = c(0.5, 0.5, -0.18),
    label = c(
      if (show_estimates) paste0("Indirect path\npart 1") else "",
      if (show_estimates) paste0("Indirect path\npart 2") else "",
      if (show_estimates) paste0("Direct = ", formatC(direct, digits = 3, format = "f"),
                                 "\nIndirect = ", formatC(indirect, digits = 3, format = "f"),
                                 "\nTotal = ", formatC(total, digits = 3, format = "f")) else ""
    ),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = paths,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$xend,
        yend = .data$yend
      ),
      arrow = grid::arrow(length = grid::unit(0.22, "cm")),
      linewidth = 0.8,
      lineend = "round",
      color = "#2B5C8A"
    ) +
    ggplot2::geom_label(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = base_size / 4,
      linewidth = 0.35,
      fill = "white",
      color = "#222222"
    ) +
    ggplot2::geom_text(
      data = ann,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = base_size / 5,
      color = "#444444",
      lineheight = 0.9
    ) +
    ggplot2::coord_cartesian(xlim = c(-0.25, 2.25), ylim = c(-0.42, 0.98), expand = FALSE) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::labs(
      title = "Mediation path diagram",
      subtitle = "Interpret causally only when mediation assumptions are justified"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#555555")
    )
}
