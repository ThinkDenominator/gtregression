#' Visualize Univariate and Multivariate Regression Side-by-Side
#'
#' Creates side-by-side forest plots from univariate and multivariate gtsummary objects.
#'
#' @param tbl_uni A `gtsummary` object from `uni_reg()`.
#' @param tbl_multi A `gtsummary` object from `multi_reg()`.
#' @param title_uni Optional title for univariate plot.
#' @param title_multi Optional title for multivariate plot.
#' @param ref_line Reference line (default = 1).
#' @param order_y Optional character vector for custom y-axis header order.
#' @param log_x Logical. Log-transform x-axis (default = FALSE).
#' @param point_color Point color.
#' @param errorbar_color Error bar color.
#' @param base_size Font size.
#' @param show_ref Logical. Whether to show reference categories. Default is TRUE.
#' @param xlim_uni,breaks_uni,xlim_multi,breaks_multi Axis customizations.
#' @importFrom patchwork wrap_plots plot_layout
#' @return A combined `ggplot` object.
#' @export
#'
plot_reg_combine <- function(tbl_uni,
                             tbl_multi,
                             title_uni = "Unadjusted",
                             title_multi = "Adjusted",
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

  if (log_x && ref_line != 1) {
    warning("Reference line should be at 1 when log_x = TRUE for log-scaled plots.")
  }

  df_uni <- tbl_uni$table_body
  df_multi <- tbl_multi$table_body

  label_df <- df_uni %>%
    dplyr::mutate(
      is_header = is.na(reference_row),
      label_clean = dplyr::case_when(
        is_header ~ paste0("**", variable, "**"),
        reference_row & show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", label, " <span style='color:gray'>(ref)</span>"),
        !reference_row ~ paste0("&nbsp;&nbsp;&nbsp;", label),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::distinct(variable, label_clean)

  if (!is.null(order_y)) {
    label_df <- label_df %>%
      dplyr::mutate(
        header_order = dplyr::case_when(
          grepl("\\*\\*", label_clean) ~ match(gsub("\\*\\*", "", variable), order_y),
          TRUE ~ NA_real_
        )
      ) %>%
      tidyr::fill(header_order, .direction = "down") %>%
      dplyr::arrange(header_order, dplyr::row_number())
  }

  label_df <- label_df %>%
    dplyr::mutate(row_id = factor(dplyr::row_number(), levels = rev(dplyr::row_number())))

  build_plot <- function(df, label_df, plot_title, xlim = NULL, breaks = NULL, x_label = "Effect Size") {
    df <- df %>%
      dplyr::mutate(
        is_header = is.na(reference_row),
        label_clean = dplyr::case_when(
          is_header ~ paste0("**", variable, "**"),
          reference_row & show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", label, " <span style='color:gray'>(ref)</span>"),
          !reference_row ~ paste0("&nbsp;&nbsp;&nbsp;", label),
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::left_join(label_df, by = c("variable", "label_clean")) %>%
      dplyr::filter(!is.na(row_id))

    label_map <- label_df$label_clean
    names(label_map) <- label_df$row_id

    p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = row_id)) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
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
      ggplot2::scale_y_discrete(labels = label_map, drop = FALSE) +
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

  get_label <- function(approach, adj = FALSE) {
    label <- switch(
      approach,
      "log-binomial" = "Risk Ratio",
      "robpoisson"   = "Risk Ratio",
      "poisson"      = "Incidence Rate Ratio",
      "logit"        = "Odds Ratio",
      "linear"       = "Coefficient",
      "Estimate"
    )
    if (adj) paste("Adjusted", label) else label
  }

  approach_uni <- na.omit(df_uni$model)[1]
  approach_multi <- na.omit(df_multi$model)[1]

  x_axis_label_uni <- get_label(approach_uni, adj = FALSE)
  x_axis_label_multi <- get_label(approach_multi, adj = TRUE)

  if (log_x) {
    x_axis_label_uni <- paste0(x_axis_label_uni, " (log scale)")
    x_axis_label_multi <- paste0(x_axis_label_multi, " (log scale)")
  }

  p1 <- build_plot(df_uni, label_df, title_uni, xlim_uni, breaks_uni, x_axis_label_uni)
  p2 <- build_plot(df_multi, label_df, title_multi, xlim_multi, breaks_multi, x_axis_label_multi) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  patchwork::wrap_plots(p1, p2, ncol = 2, widths = c(1.2, 1))
}
