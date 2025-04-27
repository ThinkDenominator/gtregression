#' Visualize a Regression Model (Forest Plot)
#'
#' Handles both univariate and multivariate models with hierarchical labels.
#'
#' @param tbl A `gtsummary` object (from `uni_reg`, `multi_reg`, etc.).
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
#' @return A `ggplot2` object.
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
  # Ensure ggtext is installed
  if (!requireNamespace("ggtext", quietly = TRUE)) {
    stop("Please install the ggtext package: install.packages('ggtext')")
  }

  if (log_x && ref_line != 1) {
    warning("Reference line should be at 1 when log_x = TRUE for log-scaled plots.")
  }

  df <- tbl$table_body

  # Safe fallback if no model column present
  model_col <- if ("model" %in% names(df)) df$model else rep(NA, nrow(df))
  approach <- na.omit(model_col)[1]

  # Identify multivariate
  is_multi <- !"tbls" %in% names(tbl)

  # Determine base label from approach
  base_label <- switch(
    approach,
    "log-binomial" = "Risk Ratio",
    "robpoisson"   = "Risk Ratio",
    "poisson"      = "Incidence Rate Ratio",
    "logit"        = "Odds Ratio",
    "linear"       = "Coefficient",
    "Estimate"
  )

  # Add (log scale) if needed
  x_axis_label <- if (is_multi) paste("Adjusted", base_label) else base_label
  if (log_x) x_axis_label <- paste0(x_axis_label, " (log scale)")

  # Add clean labels
  df <- df %>%
    dplyr::mutate(
      is_header = is.na(reference_row),
      label_clean = dplyr::case_when(
        is_header ~ paste0("**", variable, "**"),
        reference_row & show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", label, " <span style='color:gray'>(ref)</span>"),
        !reference_row ~ paste0("&nbsp;&nbsp;&nbsp;", label),
        TRUE ~ NA_character_
      )
    )

  # Filter rows
  df <- df %>%
    dplyr::filter(is_header | !is.na(estimate) | (reference_row & show_ref))

  # Now reorder properly if order_y is given
  if (!is.null(order_y)) {
    df <- df %>%
      dplyr::mutate(
        header_order = dplyr::case_when(
          is_header ~ match(variable, order_y),
          TRUE ~ NA_real_
        )
      ) %>%
      tidyr::fill(header_order, .direction = "down") %>%
      dplyr::arrange(header_order, dplyr::row_number())
  }

  # Always reassign row IDs AFTER filtering and ordering
  df <- df %>%
    dplyr::mutate(row_id = factor(dplyr::row_number(), levels = rev(dplyr::row_number())))

  label_map <- df$label_clean
  names(label_map) <- df$row_id

  # Plot
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
