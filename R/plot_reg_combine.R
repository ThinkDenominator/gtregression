#' Visualize Univariate and Multivariate Regression Side-by-Side
#'
#' Generates side-by-side plots to compare univariate & multivariable results
#'
#' @param tbl_uni A `gtsummary` object from `uni_reg()` etc.,
#' @param tbl_multi A `gtsummary` object from `multi_reg()`.
#' @param title_uni Optional plot title for the univariate model
#' @param title_multi Optional plot title for the multivariable mode
#' @param ref_line Numeric value for the reference line (default = 1).
#' @param order_y Optional character vector to manually order the y-axis labels.
#' @param log_x Logical. If `TRUE`, x-axis is log-transformed (default = FALSE).
#' @param point_color Optional color for plot points.
#' @param errorbar_color Optional color for error bars.
#' @param base_size Numeric. Base font size for plot text elements.
#' @param show_ref Logical. If `TRUE`, includes reference categories
#' @param xlim_uni Optional numeric vector to set x-axis limits for uni plot.
#' @param breaks_uni Optional numeric vector to set x-axis breaks for uni plot.
#' @param xlim_multi Optional numeric vector to set x-axis limits for multi plot
#' @param breaks_multi Optional numeric vector to set x-axis breaks- multi plot.
#'
#' @return A `ggplot2` object with two forest plots displayed side-by-side.
#'
#' @export
#' @importFrom patchwork wrap_plots plot_layout
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
#'         labels = c("Underweight", "Normal", "Overweight", "Obese")
#'       ),
#'       age_cat = cut(
#'         age,
#'         breaks = c(-Inf, 29, 49, Inf),
#'         labels = c("Young", "Middle-aged", "Older")
#'       )
#'     )
#'
#'   # Univariate logistic regression
#'   uni_rr <- uni_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age_cat", "bmi_cat"),
#'     approach = "logit"
#'   )
#'
#'   # Multivariable logistic regression
#'   multi_rr <- multi_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age_cat", "bmi_cat"),
#'     approach = "logit"
#'   )
#'
#'   # Combined plot
#'   plot_reg_combine(uni_rr, multi_rr)
#' }
#' }
plot_reg_combine <- function(tbl_uni,
                             tbl_multi,
                             title_uni = NULL,
                             title_multi = NULL,
                             ref_line = 1,
                             order_y = NULL,
                             log_x = FALSE,
                             point_color = "#1F77B4",
                             errorbar_color = "#4C4C4C",
                             base_size = 14,
                             show_ref = TRUE,
                             xlim_uni = NULL,
                             breaks_uni = NULL,
                             xlim_multi = NULL,
                             breaks_multi = NULL) {
  # Extract relevant metadata
  # uni
  approach_uni <- attr(tbl_uni, "approach")
  source_type_uni <- attr(tbl_uni, "source")
  # multi
  approach_multi <- attr(tbl_multi, "approach")
  source_type_multi <- attr(tbl_multi, "source")

  # Define valid uni and multi source types
  valid_uni_types <- c("uni_reg", "uni_reg_nbin")
  valid_multi_types <- c("multi_reg", "multi_reg_nbin")

  is_uni <- source_type_uni %in% valid_uni_types
  is_multi <- source_type_multi %in% valid_multi_types

  # Label generator based on approach and source_type
  get_label <- function(approach, source_type) {
    base_label <- dplyr::case_when(
      approach == "logit" ~ "Odds Ratio",
      approach == "log-binomial" ~ "Risk Ratio",
      approach == "poisson" ~ "Incidence Rate Ratio",
      approach == "robpoisson" ~ "Risk Ratio",
      approach == "negbin" ~ "Incidence Rate Ratio",
      approach == "linear" ~ "Beta Coefficient",
      TRUE ~ "Effect Size" # fallback
    )
    if (source_type %in% valid_multi_types) {
      paste("Adjusted", base_label)
    } else {
      base_label
    }
  }

  # x lab
  x_axis_label_uni <- get_label(approach_uni, source_type_uni)
  x_axis_label_multi <- get_label(approach_multi, source_type_multi)

  # log x lab
  if (log_x) {
    x_axis_label_uni <- paste0(x_axis_label_uni, " (log scale)")
    x_axis_label_multi <- paste0(x_axis_label_multi, " (log scale)")
  }

  # Helper function to build a single plot
  build_plot <- function(tbl, plot_title, xlim = NULL, breaks = NULL,
                         x_label = "Effect Size") {
    df <- tbl$table_body
    df <- dplyr::mutate(df,
      is_header = is.na(.data$reference_row),
      label_clean = dplyr::case_when(
        is_header ~ paste0("**", .data$label, "**"),
        .data$reference_row &
          show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label,
                            " <span style='color:gray'>(ref)</span>"),
        !.data$reference_row ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label),
        TRUE ~ NA_character_
      )
    )
    df <- dplyr::filter(df, .data$is_header | !is.na(.data$estimate) |
                          (.data$reference_row & show_ref))

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

    # plots
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate,
                                          y = .data$row_id)) +
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
      ggplot2::geom_vline(xintercept = ref_line,
                          linetype = "dashed", color = "gray60") +
      ggplot2::scale_y_discrete(labels = label_map) +
      ggplot2::labs(
        title = plot_title,
        x = x_label,
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

  # uni plot
  p1 <- build_plot(tbl_uni, plot_title = title_uni, xlim = xlim_uni,
                   breaks = breaks_uni, x_label = x_axis_label_uni)
  # multi plot
  p2 <- build_plot(tbl_multi, plot_title = title_multi, xlim = xlim_multi,
                   breaks = breaks_multi, x_label = x_axis_label_multi) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())
  # patch them
  patchwork::wrap_plots(p1, p2, ncol = 2, widths = c(1.2, 1))
}
