#' Visualize Univariate and Multivariable Regression Side-by-Side
#'
#' Generates side-by-side forest plots to compare univariate & multivariable results.
#'
#' @param tbl_uni A `gtsummary` object from `uni_reg()`, etc.
#' @param tbl_multi A `gtsummary` object from `multi_reg()`, etc.
#' @param title_uni Optional plot title for the univariate panel.
#' @param title_multi Optional plot title for the multivariable panel.
#' @param ref_line Reference line for both panels; if NULL, uses 0 for linear and 1 otherwise.
#' @param order_y Optional character vector to customize y-axis group order.
#' @param log_x Logical. If TRUE, x-axes are log-transformed (ignored for linear).
#' @param point_color Fill color for non-significant points (default "#1F77B4").
#' @param errorbar_color Color for non-significant error bars (default "#4C4C4C").
#' @param base_size Base font size (default 14).
#' @param show_ref Logical. If TRUE, includes reference rows in both panels.
#' @param sig_color Optional fill color for significant points; if "" (default), disable highlighting.
#' @param sig_errorbar_color Optional color for significant error bars; if "" (default), disable highlighting.
#' @param xlim_uni Optional c(min, max) for univariate x-axis.
#' @param breaks_uni Optional numeric vector of breaks for univariate x-axis.
#' @param xlim_multi Optional c(min, max) for multivariable x-axis.
#' @param breaks_multi Optional numeric vector of breaks for multivariable x-axis.
#'
#' @return A `patchwork` object with two `ggplot2` forest plots side-by-side.
#' @export
#'
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr mutate case_when filter row_number arrange if_else
#' @importFrom tidyr fill
#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_point geom_vline
#'   scale_y_discrete labs theme_minimal element_blank element_text margin
#'   coord_cartesian scale_x_continuous scale_x_log10
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
#
plot_reg_combine <- function(tbl_uni,
                             tbl_multi,
                             title_uni = NULL,
                             title_multi = NULL,
                             ref_line = NULL,
                             order_y = NULL,
                             log_x = FALSE,
                             point_color = "#1F77B4",
                             errorbar_color = "#4C4C4C",
                             base_size = 14,
                             show_ref = TRUE,
                             sig_color = "",
                             sig_errorbar_color = "",
                             xlim_uni = NULL, breaks_uni = NULL,
                             xlim_multi = NULL, breaks_multi = NULL) {

  # ---- metadata & axis labels (match plot_reg) ----
  get_axis_label <- function(approach, adjusted = FALSE) {
    base <- dplyr::case_when(
      approach == "logit"        ~ "Odds Ratio",
      approach == "log-binomial" ~ "Risk Ratio",
      approach == "poisson"      ~ "Incidence Rate Ratio",
      approach == "robpoisson"   ~ "Risk Ratio",
      approach == "negbin"       ~ "Incidence Rate Ratio",
      approach == "linear"       ~ "Beta Coefficient",
      TRUE                       ~ "Effect Size"
    )
    if (adjusted) paste("Adjusted", base) else base
  }

  approach_uni   <- attr(tbl_uni, "approach")
  approach_multi <- attr(tbl_multi, "approach")

  xlab_uni   <- get_axis_label(approach_uni,   adjusted = FALSE)
  xlab_multi <- get_axis_label(approach_multi, adjusted = TRUE)

  log_x_uni   <- isTRUE(log_x) && !identical(approach_uni, "linear")
  log_x_multi <- isTRUE(log_x) && !identical(approach_multi, "linear")
  if (log_x_uni)   xlab_uni   <- paste0(xlab_uni, " (log scale)")
  if (log_x_multi) xlab_multi <- paste0(xlab_multi, " (log scale)")

  ref_uni   <- if (is.null(ref_line)) if (identical(approach_uni, "linear")) 0 else 1 else ref_line
  ref_multi <- if (is.null(ref_line)) if (identical(approach_multi, "linear")) 0 else 1 else ref_line

  # ---- prep tables (exact header/ref rules from plot_reg) ----
  prep_df <- function(tbl, order_y = NULL, show_ref = TRUE) {
    df <- tbl$table_body
    ref_flag <- df$reference_row %in% TRUE

    df <- dplyr::mutate(
      df,
      is_header = is.na(.data$reference_row) & is.na(.data$estimate),
      label_clean = dplyr::case_when(
        is_header ~ paste0("**", .data$label, "**"),
        ref_flag & show_ref ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label,
                                     " <span style='color:gray'>(ref)</span>"),
        !ref_flag ~ paste0("&nbsp;&nbsp;&nbsp;", .data$label),
        TRUE ~ .data$label
      )
    )

    df <- dplyr::filter(df, .data$is_header | !is.na(.data$estimate) | (ref_flag & show_ref))

    if (!is.null(order_y)) {
      df <- dplyr::mutate(
        df,
        header_order = dplyr::case_when(.data$is_header ~ match(.data$label, order_y), TRUE ~ NA_real_)
      )
      df <- tidyr::fill(df, header_order, .direction = "down")
      df <- dplyr::arrange(df, header_order, dplyr::row_number())
    }

    # Stable Y mapping: reverse order, carry labels
    df <- dplyr::mutate(df, row_id = factor(dplyr::row_number(), levels = rev(dplyr::row_number())))
    y_levels <- levels(df$row_id)
    label_map <- setNames(df$label_clean, as.character(df$row_id))

    list(df = df, y_levels = y_levels, label_map = label_map)
  }

  pre_uni   <- prep_df(tbl_uni,   order_y, show_ref)
  pre_multi <- prep_df(tbl_multi, order_y, show_ref)

  # Align multivariable panel's y to univariate panel's sequence
  pre_multi$df$row_id <- factor(as.character(seq_len(nrow(pre_multi$df))),
                                levels = as.character(seq_len(nrow(pre_uni$df))))

  # ---- helpers: limits + significance + default breaks ----
  is_finite_num <- function(x) is.numeric(x) & is.finite(x)

  auto_limits <- function(df, use_log = FALSE) {
    finite_ci <- with(df, is_finite_num(conf.low) & is_finite_num(conf.high) & (conf.high >= conf.low))
    if (any(finite_ci)) {
      lo <- min(df$conf.low[finite_ci]); hi <- max(df$conf.high[finite_ci])
    } else {
      finite_est <- is_finite_num(df$estimate)
      if (any(finite_est)) { lo <- min(df$estimate[finite_est]); hi <- max(df$estimate[finite_est]) }
      else { lo <- 0; hi <- 1 }
    }
    if (use_log) {
      lo <- max(lo, .Machine$double.eps * 10)
      hi <- hi * 1.05
      c(lo, hi)
    } else {
      span <- hi - lo
      if (!is.finite(span) || span <= 0) { lo <- lo - 0.5; hi <- hi + 0.5; span <- hi - lo }
      pad <- max(1e-6, 0.05 * span)
      c(lo - pad, hi + pad)
    }
  }

  make_linear_breaks <- function(xlim) {
    if (requireNamespace("scales", quietly = TRUE)) {
      return(scales::pretty_breaks(n = 5)(xlim))
    }
    rng <- range(xlim)
    seq(rng[1], rng[2], length.out = 5)
  }

  make_log_breaks <- function(xlim) {
    if (requireNamespace("scales", quietly = TRUE)) {
      return(scales::log_breaks(n = 5)(xlim))
    }
    lo <- floor(log10(xlim[1])); hi <- ceiling(log10(xlim[2]))
    10^(lo:hi)
  }

  build_panel <- function(df, label_map, y_levels, panel_title, x_label,
                          ref_line_val, use_log, xlim = NULL, breaks = NULL,
                          point_color, errorbar_color, sig_color, sig_errorbar_color) {

    enable_sig <- nzchar(sig_color) || nzchar(sig_errorbar_color)

    finite_ci  <- with(df, is_finite_num(conf.low) & is_finite_num(conf.high) & (conf.high >= conf.low))
    finite_est <- is_finite_num(df$estimate)

    ref_flag <- df$reference_row %in% TRUE
    is_value_row <- !df$is_header & !ref_flag

    is_sig <- rep(FALSE, nrow(df))
    if (enable_sig) {
      ready <- is_value_row & finite_ci
      if (any(ready)) {
        is_sig[ready] <- (df$conf.low[ready] > ref_line_val) | (df$conf.high[ready] < ref_line_val)
      }
      is_sig[df$is_header | ref_flag] <- FALSE
    }

    sig_idx_est <- which(finite_est &  is_sig)
    ns_idx_est  <- which(finite_est & !is_sig)
    sig_idx_ci  <- which(finite_ci  &  is_sig)
    ns_idx_ci   <- which(finite_ci  & !is_sig)

    sig_point_fill <- if (enable_sig && nzchar(sig_color)) sig_color else point_color
    sig_bar_color  <- if (enable_sig && nzchar(sig_errorbar_color)) sig_errorbar_color else errorbar_color

    # limits & breaks
    if (is.null(xlim)) xlim <- auto_limits(df, use_log)
    if (use_log) xlim[1] <- max(xlim[1], .Machine$double.eps * 10)
    if (is.null(breaks)) {
      breaks <- if (use_log) make_log_breaks(xlim) else make_linear_breaks(xlim)
      breaks <- breaks[breaks >= xlim[1] & breaks <= xlim[2]]
      if (length(breaks) < 2) {
        breaks <- if (use_log) c(xlim[1], xlim[2]) else seq(xlim[1], xlim[2], length.out = 3)
      }
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row_id)) +
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
      ggplot2::geom_point(
        data = df[ns_idx_est, ],
        shape = 21, fill = point_color, size = 3, stroke = 0.6, color = "black"
      ) +
      ggplot2::geom_point(
        data = df[sig_idx_est, ],
        shape = 21, fill = sig_point_fill, size = 3.4, stroke = 0.6, color = "black"
      ) +
      ggplot2::geom_vline(xintercept = ref_line_val, linetype = "dashed", color = "gray60") +
      ggplot2::scale_y_discrete(limits = y_levels,
                                labels = unname(label_map[y_levels]),
                                drop = FALSE) +
      ggplot2::labs(title = panel_title, x = x_label, y = NULL) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y = if (requireNamespace("ggtext", quietly = TRUE)) {
          ggtext::element_markdown(hjust = 0)
        } else {
          ggplot2::element_text(hjust = 0)
        },
        plot.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(10, 40, 10, 10),
        axis.line.x  = ggplot2::element_line(color = "grey40"),
        axis.ticks.x = ggplot2::element_line(color = "grey40"),
        axis.text.x  = ggplot2::element_text(),
        axis.title.x = ggplot2::element_text()
      )

    if (isTRUE(use_log)) {
      p <- p + ggplot2::scale_x_log10(limits = xlim, breaks = breaks)
    } else {
      p <- p + ggplot2::scale_x_continuous(limits = xlim, breaks = breaks)
    }

    p
  }

  # ---- build panels ----
  p1 <- build_panel(
    df = pre_uni$df,
    label_map = pre_uni$label_map,
    y_levels = pre_uni$y_levels,
    panel_title = title_uni,
    x_label = xlab_uni,
    ref_line_val = ref_uni,
    use_log = log_x_uni,
    xlim = xlim_uni, breaks = breaks_uni,
    point_color = point_color, errorbar_color = errorbar_color,
    sig_color = sig_color, sig_errorbar_color = sig_errorbar_color
  )

  p2 <- build_panel(
    df = pre_multi$df,
    label_map = pre_uni$label_map,           # share labels to align rows
    y_levels = pre_uni$y_levels,             # enforce same order
    panel_title = title_multi,
    x_label = xlab_multi,
    ref_line_val = ref_multi,
    use_log = log_x_multi,
    xlim = xlim_multi, breaks = breaks_multi,
    point_color = point_color, errorbar_color = errorbar_color,
    sig_color = sig_color, sig_errorbar_color = sig_errorbar_color
  ) +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  patchwork::wrap_plots(p1, p2, ncol = 2, widths = c(1.2, 1))
}
