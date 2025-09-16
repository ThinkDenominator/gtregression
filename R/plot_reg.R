#' Visualize a Regression Model as a Forest Plot
#'
#' Creates a forest plot from a `gtsummary` object.
#' Supports both univariate and multivariable models
#' with hierarchical labels for categorical variables.
#' Designed to work seamlessly with outputs from
#' functions like `uni_reg()` and `multi_reg()`.
#' @param tbl A gtsummary object from regression functions
#' @param title Optional plot title
#' @param ref_line Reference line; if NULL, uses 0 for linear and 1 otherwise
#' @param order_y Optional character vector to customise y-axis order
#' @param log_x Logical. If TRUE, log x-axis (ignored for linear)
#' @param xlim Optional numeric vector for x-axis limits
#' @param breaks Optional numeric vector for x-axis tick breaks
#' @param point_color Fill color for points (default "#1F77B4")
#' @param errorbar_color Color for all error bars (default "#4C4C4C")
#' @param base_size Base font size
#' @param show_ref Logical. If TRUE, includes reference in the plot
#' @param sig_color Optional fill color for significant points; if "" (default),
#'   significance highlighting is disabled
#' @param sig_errorbar_color Optional color for significant error bars; if "" (default),
#'   bars stay `errorbar_color`
#'
#' @return A `ggplot2` object representing the forest plot.
#' @importFrom dplyr mutate case_when filter row_number arrange if_else
#' @importFrom tidyr fill
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline
#'   scale_y_discrete labs theme_minimal element_blank element_text margin
#'   coord_cartesian scale_x_continuous scale_x_log10
#' @importFrom ggtext element_markdown
#' @importFrom rlang .data
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
#'       )
#'     )
#'
#'   # Univariate logistic regression
#'   uni_rr <- uni_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "bmi_cat"),
#'     approach = "logit"
#'   )
#'   plot_reg(uni_rr)
#' }
#' }
#' @export
plot_reg <- function(tbl,
                     title = NULL,
                     ref_line = NULL,
                     order_y = NULL,
                     log_x = FALSE,
                     xlim = NULL,
                     breaks = NULL,
                     point_color = "#1F77B4",
                     errorbar_color = "#4C4C4C",
                     base_size = 14,
                     show_ref = TRUE,
                     sig_color = "",
                     sig_errorbar_color = "") {

  df <- tbl$table_body

  # ---- metadata ----
  source_type <- attr(tbl, "source")
  approach    <- attr(tbl, "approach")
  is_multi    <- source_type %in% c("multi_reg", "multi_reg_nbin")

  # Axis label
  base_label <- dplyr::case_when(
    approach == "logit"        ~ "Odds Ratio",
    approach == "log-binomial" ~ "Risk Ratio",
    approach == "poisson"      ~ "Incidence Rate Ratio",
    approach == "robpoisson"   ~ "Risk Ratio",
    approach == "negbin"       ~ "Incidence Rate Ratio",
    approach == "linear"       ~ "Beta Coefficient",
    TRUE                       ~ "Effect Size"
  )
  x_axis_label <- if (is_multi) paste("Adjusted", base_label) else base_label

  # Linear models: never log-scaled
  if (identical(approach, "linear")) log_x <- FALSE
  if (isTRUE(log_x)) x_axis_label <- paste0(x_axis_label, " (log scale)")

  # Auto reference line if not provided
  if (is.null(ref_line)) ref_line <- if (identical(approach, "linear")) 0 else 1

  # ---- labels: robust header + ref detection ----
  # Only TRUE is "reference"; FALSE/NA are treated as non-reference
  ref_flag <- df$reference_row %in% TRUE

  df <- dplyr::mutate(
    df,
    # Treat as header only when both reference_row and estimate are NA
    is_header = is.na(.data$reference_row) & is.na(.data$estimate),
    label_clean = dplyr::case_when(
      is_header ~ paste0("**", .data$label, "**"),
      ref_flag & show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label,
                                   " <span style='color:gray'>(ref)</span>"),
      !ref_flag ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label),
      TRUE ~ .data$label
    )
  )

  # Keep headers, estimate rows, and optional reference rows
  df <- dplyr::filter(
    df,
    .data$is_header | !is.na(.data$estimate) | (ref_flag & show_ref)
  )

  # Optional custom order (group by last seen header)
  if (!is.null(order_y)) {
    df <- dplyr::mutate(
      df,
      header_order = dplyr::case_when(.data$is_header ~ match(.data$label, order_y),
                                      TRUE ~ NA_real_)
    )
    df <- tidyr::fill(df, header_order, .direction = "down")
    df <- dplyr::arrange(df, header_order, dplyr::row_number())
  }

  # Y mapping (explicit limits + labels to avoid any mismatch)
  df <- dplyr::mutate(
    df,
    row_id = factor(dplyr::row_number(), levels = rev(dplyr::row_number()))
  )
  y_levels <- levels(df$row_id)
  label_map <- setNames(df$label_clean, as.character(df$row_id))
  y_labels <- unname(label_map[y_levels])

  # ---- significance (only if user supplies sig* colors) ----
  enable_sig <- nzchar(sig_color) || nzchar(sig_errorbar_color)

  is_finite_num <- function(x) is.numeric(x) & is.finite(x)
  finite_ci  <- with(df, is_finite_num(conf.low) & is_finite_num(conf.high) & (conf.high >= conf.low))
  finite_est <- is_finite_num(df$estimate)

  # Value rows = not headers and not reference rows
  is_value_row <- !df$is_header & !ref_flag

  # Use ref_line as the cutoff (0 for linear, 1 otherwise)
  cutoff <- ref_line

  is_sig <- rep(FALSE, nrow(df))
  if (enable_sig) {
    ready <- is_value_row & finite_ci
    if (any(ready)) {
      # significant if CI does NOT cross the cutoff
      is_sig[ready] <- (df$conf.low[ready] > cutoff) | (df$conf.high[ready] < cutoff)
    }
    # never highlight headers or reference levels
    is_sig[df$is_header | ref_flag] <- FALSE
  }

  # Index sets for layers
  sig_idx_est <- which(finite_est &  is_sig)
  ns_idx_est  <- which(finite_est & !is_sig)
  sig_idx_ci  <- which(finite_ci  &  is_sig)
  ns_idx_ci   <- which(finite_ci  & !is_sig)

  # Colors to use (defaults if sig_* not provided)
  sig_point_fill <- if (enable_sig && nzchar(sig_color)) sig_color else point_color
  sig_bar_color  <- if (enable_sig && nzchar(sig_errorbar_color)) sig_errorbar_color else errorbar_color

  # ---- plot ----
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row_id)) +
    # CI lines
    ggplot2::geom_errorbarh(
      data = df[ns_idx_ci, ],
      ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high),
      height = 0.2, color = errorbar_color
    ) +
    ggplot2::geom_errorbarh(
      data = df[sig_idx_ci, ],
      ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high),
      height = 0.2, color = sig_bar_color
    ) +
    # Points
    ggplot2::geom_point(
      data = df[ns_idx_est, ],
      shape = 21, fill = point_color, size = 3, stroke = 0.6, color = "black"
    ) +
    ggplot2::geom_point(
      data = df[sig_idx_est, ],
      shape = 21, fill = sig_point_fill, size = 3.4, stroke = 0.6, color = "black"
    ) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray60") +
    ggplot2::scale_y_discrete(limits = y_levels, labels = y_labels, drop = FALSE) +
    ggplot2::labs(title = title, x = x_axis_label, y = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = if (requireNamespace("ggtext", quietly = TRUE)) {
        ggtext::element_markdown(hjust = 0)
      } else {
        ggplot2::element_text(hjust = 0)
      },
      plot.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 40, 10, 10)
    )

  # Axes (keep your original behavior)
  if (!is.null(xlim))   p <- p + ggplot2::coord_cartesian(xlim = xlim)
  if (!is.null(breaks)) p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  if (isTRUE(log_x))    p <- p + ggplot2::scale_x_log10()

  p
}
