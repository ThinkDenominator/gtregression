#' Side-by-Side Forest Plots: Univariate vs Multivariable
#'
#' Creates two aligned forest plots (univariate and multivariable) from
#' `gtsummary`-style objects returned by `gtregression` functions
#' (e.g., `uni_reg()`, `multi_reg()`).
#'
#' The y-axis rows are aligned by a unique `(variable, level)` key so each
#' estimate appears exactly once per panel. Label styling is plain text by
#' default (CRAN-safe). To render bold headers / grey refs in vignettes, pair
#'
#' @param tbl_uni Univariate `gtsummary`-like table.
#' @param tbl_multi Multivariable `gtsummary`-like table.
#' @param title_uni,title_multi Optional panel titles.
#' @param ref_line Optional numeric reference line (defaults to 0 for linear,
#'   1 otherwise, inferred per panel).
#' @param order_y Optional character vector to customize header ordering.
#' @param log_x Logical. If `TRUE`, use log x-axis (ignored for linear models).
#' @param point_color,errorbar_color Base colors for non-significant rows.
#' @param base_size Base font size for `theme_minimal()`.
#' @param show_ref Logical; if `TRUE`, include and tag reference levels `(Ref.)`.
#' @param sig_color,sig_errorbar_color Optional colors for significant rows; if
#'   `NULL`, they reuse the base colors.
#' @param xlim_uni,breaks_uni Optional x-limits and breaks for the univariate panel.
#' @param xlim_multi,breaks_multi Optional x-limits and breaks for the multivariable panel.
#' @param alpha Significance level for linear models when `p.value` is available.
#'
#' @return A `patchwork` object with two `ggplot2` panels.
#' @importFrom rlang .data
#' @examples
#' \donttest{
#' if (requireNamespace("mlbench", quietly = TRUE) &&
#'     requireNamespace("gtregression", quietly = TRUE)) {
#'   data("PimaIndiansDiabetes2", package = "mlbench")
#'   d <- PimaIndiansDiabetes2
#'   d$diabetes <- ifelse(d$diabetes == "pos", 1, 0)
#'
#'   tbl_u <- gtregression::uni_reg(d, outcome = "diabetes",
#'                                  exposures = c("age","glucose"), approach = "logit")
#'   tbl_m <- gtregression::multi_reg(d, outcome = "diabetes",
#'                                    exposures = c("age","glucose"), approach = "logit")
#'   plot_reg_combine(tbl_u, tbl_m,
#'                    title_uni = "Univariate", title_multi = "Adjusted")
#' }
#' }
#' @export
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
                             sig_color = NULL,
                             sig_errorbar_color = NULL,
                             xlim_uni = NULL, breaks_uni = NULL,
                             xlim_multi = NULL, breaks_multi = NULL,
                             alpha = 0.05) {

  # ---- axis labels (match plot_reg) ----
  get_axis_label <- function(approach, adjusted = FALSE) {
    base <- dplyr::case_when(
      approach == "logit"        ~ "Odds Ratio",
      approach == "log-binomial" ~ "Risk Ratio",
      approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
      approach == "robpoisson"   ~ "Risk Ratio",
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

  # ---- prep (robust header/data/ref detection; CRAN-safe labels) ----
  prep_df <- function(tbl, order_y = NULL, show_ref = TRUE) {
    df <- tbl$table_body |>
      dplyr::mutate(
        ref_flag  = (.data$reference_row %in% TRUE),
        is_header = (.data$row_type == "label") &
          ( (.data$header_row %in% TRUE) |
              (is.na(.data$conf.low) & is.na(.data$conf.high))) |
          (.data$var_type == "continuous"),
        is_data   = (.data$row_type == "level") | ((.data$row_type == "label") & !.data$is_header),
        label_clean = dplyr::case_when(
          is_header ~ .data$label,                                   # header (no indent)
          ref_flag  & show_ref ~ paste0("  ", .data$label, " (Ref.)"),
          is_data   ~ paste0("  ", .data$label),
          TRUE ~ NA_character_
        ),
        # unique alignment key avoids overlapping rows ("Yes"/"No" across variables)
        row_key = dplyr::case_when(
          .data$is_header ~ paste0(.data$variable, "::__HDR__"),
          TRUE            ~ paste0(.data$variable, "::", .data$label)
        )
      ) |>
      dplyr::filter(.data$is_header | .data$is_data | (.data$ref_flag & show_ref))

    if (!is.null(order_y)) {
      df <- df |>
        dplyr::mutate(
          header_order = dplyr::case_when(
            is_header ~ match(.data$label, order_y),
            TRUE ~ NA_real_
          )
        ) |>
        tidyr::fill(header_order, .direction = "down") |>
        dplyr::arrange(header_order, dplyr::row_number())
    }

    df$row_id <- factor(seq_len(nrow(df)), levels = rev(seq_len(nrow(df))))
    label_map <- setNames(df$label_clean, as.character(df$row_id))
    list(df = df, label_map = label_map, y_levels = levels(df$row_id))
  }

  pre_uni   <- prep_df(tbl_uni,   order_y, show_ref)
  pre_multi <- prep_df(tbl_multi, order_y, show_ref)

  # align multi panel to uni panel by unique key
  key2id <- setNames(as.character(pre_uni$df$row_id), pre_uni$df$row_key)
  pre_multi$df <- pre_multi$df |>
    dplyr::mutate(
      row_id = factor(unname(key2id[.data$row_key]),
                      levels = levels(pre_uni$df$row_id))
    ) |>
    dplyr::filter(!is.na(.data$row_id))

  # ---- significance (mirror plot_reg) ----
  compute_is_sig <- function(df, approach, alpha, ref_line_val) {
    has_p <- "p.value" %in% names(df)
    num_cols <- intersect(c("estimate","conf.low","conf.high"), names(df))
    df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))

    if (identical(approach, "linear") && has_p) {
      dplyr::mutate(
        df,
        is_sig = dplyr::case_when(
          !.data$is_data | .data$ref_flag ~ FALSE,
          !is.na(.data$p.value) & (.data$p.value < alpha) ~ TRUE,
          TRUE ~ FALSE
        )
      )
    } else {
      dplyr::mutate(
        df,
        is_sig = dplyr::case_when(
          !.data$is_data | .data$ref_flag ~ FALSE,
          is.finite(.data$conf.low) & is.finite(.data$conf.high) & (.data$conf.high >= .data$conf.low) &
            ((.data$conf.low > ref_line_val) | (.data$conf.high < ref_line_val)) ~ TRUE,
          TRUE ~ FALSE
        )
      )
    }
  }

  pre_uni$df   <- compute_is_sig(pre_uni$df,   approach_uni,   alpha, ref_uni)
  pre_multi$df <- compute_is_sig(pre_multi$df, approach_multi, alpha, ref_multi)

  # ---- colors (same semantics as plot_reg) ----
  fill_vals <- c("FALSE" = point_color,
                 "TRUE"  = if (!is.null(sig_color)) sig_color else point_color)
  line_vals <- c("FALSE" = errorbar_color,
                 "TRUE"  = if (!is.null(sig_errorbar_color)) sig_errorbar_color else errorbar_color)

  # ---- panel builder (auto xlim identical to plot_reg) ----
  build_panel <- function(df, label_map, y_levels, panel_title, x_label,
                          ref_line_val, use_log, xlim = NULL, breaks = NULL) {

    # auto xlim (include reference line; log-safe)
    if (is.null(xlim)) {
      vals <- c(df$conf.low, df$conf.high, df$estimate, ref_line_val)
      vals <- vals[is.finite(vals)]
      if (length(vals) >= 2L) {
        rng <- range(vals, na.rm = TRUE)
        if (use_log) {
          lower <- max(min(rng[1], ref_line_val, na.rm = TRUE), .Machine$double.eps * 10)
          upper <- max(rng[2], ref_line_val, na.rm = TRUE)
        } else {
          span  <- diff(rng)
          pad   <- if (is.finite(span) && span > 0) 0.05 * span else 0.1
          lower <- min(rng[1], ref_line_val, na.rm = TRUE) - pad
          upper <- max(rng[2], ref_line_val, na.rm = TRUE) + pad
        }
        xlim <- c(lower, upper)
      }
    }

    # label function (character keys)
    lab_fun <- {
      lm <- label_map
      function(x) unname(lm[as.character(x)])
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row_id)) +
      .add_h_ci(df) +
      ggplot2::geom_point(
        ggplot2::aes(fill = .data$is_sig),
        shape = 21, size = 3, stroke = 0.6, show.legend = FALSE, na.rm = TRUE
      ) +
      ggplot2::geom_vline(xintercept = ref_line_val, linetype = "dashed", colour = "gray60") +
      ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
      ggplot2::scale_color_manual(values = line_vals, guide = "none") +
      ggplot2::scale_y_discrete(limits = y_levels, labels = lab_fun, drop = FALSE) +
      ggplot2::labs(title = panel_title, x = x_label, y = NULL) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(hjust = 0),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(10, 40, 10, 10)
      )

    if (!is.null(breaks) && !use_log) p <- p + ggplot2::scale_x_continuous(breaks = breaks)
    if (isTRUE(use_log))            p <- p + ggplot2::scale_x_log10()
    if (!is.null(xlim))             p <- p + ggplot2::coord_cartesian(xlim = xlim)

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
    xlim = xlim_uni, breaks = breaks_uni
  )

  p2 <- build_panel(
    df = pre_multi$df,
    label_map = pre_uni$label_map,   # share uni labels to enforce same y text
    y_levels = pre_uni$y_levels,     # enforce same row order
    panel_title = title_multi,
    x_label = xlab_multi,
    ref_line_val = ref_multi,
    use_log = log_x_multi,
    xlim = xlim_multi, breaks = breaks_multi
  ) +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  patchwork::wrap_plots(p1, p2, ncol = 2, widths = c(1.2, 1))
}
