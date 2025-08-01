#' Visualize a Regression Model as a Forest Plot
#'
#' Creates a forest plot from a `gtsummary` object.
#' Supports both univariate and multivariable models
#' with hierarchical labels for categorical variables.
#' Designed to work seamlessly with outputs from
#' functions like `uni_reg()` and `multi_reg()`.
#'
#' @param tbl A `gtsummary` object from regression functions
#' @param title Optional plot title (character).
#' @param ref_line Numeric value for the reference line (default = 1).
#' @param order_y Optional character vector to the customise y-axis order
#' @param log_x Logical. If `TRUE`, uses a logarithmic x-axis (default = FALSE).
#' @param xlim Optional numeric vector specifying x-axis limits
#' @param breaks Optional numeric vector for x-axis tick breaks.
#' @param point_color Color of the points (default is automatic).
#' @param errorbar_color Color of the error bars (default is automatic).
#' @param base_size Base font size for text elements.
#' @param show_ref Logical. If `TRUE`, includes reference in the plot.
#'
#' @return A `ggplot2` object representing the forest plot.
#' @importFrom dplyr mutate case_when filter row_number arrange if_else
#' @importFrom tidyr fill
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline
#'   scale_y_discrete labs theme_minimal element_blank element_text margin
#'   coord_cartesian scale_x_continuous scale_x_log10
#' @importFrom ggtext element_markdown
#'
#' @examples
#' \donttest{
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   data("PimaIndiansDiabetes2", package = "mlbench")
#'   library(dplyr)
#'   library(gtregression)
#'
#'   # Prepare data
#'   pima <- PimaIndiansDiabetes2 |>
#'     mutate(
#'       diabetes = ifelse(diabetes == "pos", 1, 0),
#'       bmi_cat = cut(
#'         mass,
#'         breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
#'         labels = c("Underweight", "Normal", "Overweight", "Obese"))
#'   )
#'
#'   # Univariate logistic regression
#'   uni_rr <- uni_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "bmi_cat"),
#'     approach = "logit"
#'   )
#'plot_reg(uni_rr)
#'}
#'}
#' @export
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

  # Extract the relevant meta data, source type and approach
  source_type <- attr(tbl, "source")
  approach <- attr(tbl, "approach")

  # Auto-identify uni or multi regression using metadata
  valid_uni_types <- c("uni_reg", "uni_reg_nbin")
  valid_multi_types <- c("multi_reg", "multi_reg_nbin")

  is_multi <- source_type %in% valid_multi_types

  # Decide base label based on approach
  base_label <- dplyr::case_when(
    approach == "logit" ~ "Odds Ratio",
    approach == "log-binomial" ~ "Risk Ratio",
    approach == "poisson" ~ "Incidence Rate Ratio",
    approach == "robpoisson" ~ "Risk Ratio",
    approach == "negbin" ~ "Incidence Rate Ratio",
    approach == "linear" ~ "Beta Coefficient",
    TRUE ~ "Effect Size" # rescue
  )
  # Adjusted label if multivariable
  x_axis_label <- if (is_multi) paste("Adjusted", base_label) else base_label
  # log scale for log transformed x axis
  if (log_x) x_axis_label <- paste0(x_axis_label, " (log scale)")

  df <- dplyr::mutate(df,
    is_header = is.na(.data$reference_row),
    label_clean = dplyr::case_when(
      is_header ~ paste0("**", .data$label, "**"),
      .data$reference_row &
        show_ref ~ paste0("&nbsp;&nbsp;&nbsp;",
            .data$label, " <span style='color:gray'>(ref)</span>"),
      !.data$reference_row ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label),
      TRUE ~ NA_character_
    )
  )

  df <- dplyr::filter(df, .data$is_header | !is.na(.data$estimate) |
                        (.data$reference_row & show_ref))

  # order y axis
  if (!is.null(order_y)) {
    df <- dplyr::mutate(df,
      header_order = dplyr::case_when(
        .data$is_header ~ match(.data$label, order_y),
        TRUE ~ NA_real_
      )
    )
    df <- tidyr::fill(df, header_order, .direction = "down")
    df <- dplyr::arrange(df, header_order, dplyr::row_number())
  }

  df <- dplyr::mutate(df, row_id = factor(dplyr::row_number(),
                                          levels = rev(dplyr::row_number())))
  label_map <- df$label_clean
  names(label_map) <- df$row_id

  # plot
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
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed",
                        color = "gray60") +
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
