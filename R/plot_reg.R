#' Visualize a Regression Model (Forest Plot)
#'
#' Handles both univariate and multivariate models with hierarchical labels.
#'
#' @param tbl A gtsummary object (from uni_reg, multi_reg, etc.).
#' @param title Optional title.
#' @param ref_line Reference line (default = 1).
#' @param order_y Optional character vector for custom y-axis header order.
#' @param log_x Logical. Log-transform the x-axis (default = FALSE).
#' @param xlim Optional x-axis limits.
#' @param breaks Optional x-axis breaks.
#' @param point_color Point color.
#' @param errorbar_color Error bar color.
#' @param base_size Font size.
#' @param show_ref Logical. Whether to show reference categories. Default is TRUE.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom dplyr mutate case_when filter row_number arrange if_else
#' @importFrom tidyr fill
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline
#'   scale_y_discrete labs theme_minimal element_blank element_text margin
#'   coord_cartesian scale_x_continuous scale_x_log10
#' @importFrom ggtext element_markdown
plot_reg <- function(tbl,
                     title = NULL,
                     ref_line = 1,
                     order_y = NULL,
                     log_x = FALSE,
                     xlim = NULL,
                     breaks = NULL,
                     point_color = "#1F77B4",
                     errorbar_color = "#4C4C4C",
                     base_size = 14,
                     show_ref = TRUE) {

  df <- tbl$table_body

  # Extract the model source type and approach
  source_type <- attr(tbl, "source_type")
  approach <- attr(tbl, "approach")

  # Auto-identify uni or multi regression
  is_multi <- identical(source_type, "multi_reg") || identical(source_type, "multi_reg_nbin")

  # Decide base label based on approach
  base_label <- dplyr::case_when(
    approach == "logit" ~ "Odds Ratio",
    approach == "log-binomial" ~ "Risk Ratio",
    approach == "poisson" ~ "Incidence Rate Ratio",
    approach == "robpoisson" ~ "Risk Ratio",
    approach == "negbin" ~ "Incidence Rate Ratio",
    approach == "linear" ~ "Beta Coefficient",
    TRUE ~ "Effect Size" # fallback
  )
  # Adjusted label if multivariable
  x_axis_label <- if (is_multi) paste("Adjusted", base_label) else base_label
  # log scale for log transformed x axis
  if (log_x) x_axis_label <- paste0(x_axis_label, " (log scale)")

  df <- dplyr::mutate(df,
                      is_header = is.na(.data$reference_row),
                      label_clean = dplyr::case_when(
                        is_header ~ paste0("**", .data$variable, "**"),
                        .data$reference_row & show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label, " <span style='color:gray'>(ref)</span>"),
                        !.data$reference_row ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label),
                        TRUE ~ NA_character_
                      )
  )

  df <- dplyr::filter(df, .data$is_header | !is.na(.data$estimate) | (.data$reference_row & show_ref))

  if (!is.null(order_y)) {
    df <- dplyr::mutate(df,
                        header_order = dplyr::case_when(
                          .data$is_header ~ match(.data$variable, order_y),
                          TRUE ~ NA_real_
                        )
    )
    df <- tidyr::fill(df, header_order, .direction = "down")
    df <- dplyr::arrange(df, header_order, dplyr::row_number())
  }

  df <- dplyr::mutate(df, row_id = factor(dplyr::row_number(), levels = rev(dplyr::row_number())))
  label_map <- df$label_clean
  names(label_map) <- df$row_id

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row_id)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high),
      height = 0.2,
      color = errorbar_color
    ) +
    ggplot2::geom_point(
      data = df[!is.na(df$estimate), ],
      shape = 21,
      fill = point_color,
      size = 3,
      stroke = 0.6
    ) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray60") +
    ggplot2::scale_y_discrete(labels = label_map) +
    ggplot2::labs(
      title = title,
      x = x_axis_label,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(family = "mono", hjust = 0),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 40, 10, 10)
    )

  if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)
  if (!is.null(breaks)) p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  if (log_x) p <- p + ggplot2::scale_x_log10()

  return(p)
}
