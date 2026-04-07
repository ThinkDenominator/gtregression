#' Side-by-side forest plots: univariate vs multivariable
#'
#' Creates two aligned forest plots from \code{gtregression} objects returned by
#' \code{uni_reg()} and \code{multi_reg()}.
#'
#' @param tbl_uni A univariate \code{gtregression} object.
#' @param tbl_multi A multivariable \code{gtregression} object.
#' @param title_uni,title_multi Optional panel titles.
#' @param ref_line Optional numeric reference line. If \code{NULL}, uses 0 for
#'   linear models and 1 otherwise.
#' @param order_y Optional character vector to customize exposure ordering.
#' @param log_x Logical; if \code{TRUE}, uses log x-axis for non-linear models.
#' @param point_color,errorbar_color Base colors for non-significant rows.
#' @param base_size Base font size for \code{theme_minimal()}.
#' @param show_ref Logical; if \code{TRUE}, include reference levels as
#'   \code{(ref)}.
#' @param sig_color,sig_errorbar_color Optional colors for significant rows.
#'   If \code{NULL}, base colors are reused.
#' @param xlim_uni,breaks_uni Optional x-axis limits and breaks for the
#'   univariate panel.
#' @param xlim_multi,breaks_multi Optional x-axis limits and breaks for the
#'   multivariable panel.
#' @param alpha Significance level for linear models when \code{p.value} is
#'   available.
#'
#' @return A \code{patchwork} object with two \code{ggplot2} panels.
#' @importFrom rlang .data
#' @importFrom stats setNames
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
                             xlim_uni = NULL,
                             breaks_uni = NULL,
                             xlim_multi = NULL,
                             breaks_multi = NULL,
                             alpha = 0.05) {

  if (!inherits(tbl_uni, "gtregression")) {
    stop("`tbl_uni` must be a gtregression object.", call. = FALSE)
  }
  if (!inherits(tbl_multi, "gtregression")) {
    stop("`tbl_multi` must be a gtregression object.", call. = FALSE)
  }

  if (!identical(tbl_uni$source, "uni_reg")) {
    stop("`tbl_uni` must come from uni_reg().", call. = FALSE)
  }
  if (!identical(tbl_multi$source, "multi_reg")) {
    stop("`tbl_multi` must come from multi_reg().", call. = FALSE)
  }

  req_body <- c("exposure", "level", "estimate", "conf.low", "conf.high", "p.value", "ref")
  req_disp <- c("Characteristic", "is_header")

  if (!all(req_body %in% names(tbl_uni$table_body))) {
    stop("`tbl_uni$table_body` does not have the expected columns.", call. = FALSE)
  }
  if (!all(req_body %in% names(tbl_multi$table_body))) {
    stop("`tbl_multi$table_body` does not have the expected columns.", call. = FALSE)
  }
  if (!all(req_disp %in% names(tbl_uni$table_display))) {
    stop("`tbl_uni$table_display` does not have the expected columns.", call. = FALSE)
  }
  if (!all(req_disp %in% names(tbl_multi$table_display))) {
    stop("`tbl_multi$table_display` does not have the expected columns.", call. = FALSE)
  }

  get_axis_label <- function(approach, adjusted = FALSE) {
    base <- dplyr::case_when(
      approach == "logit" ~ "Odds Ratio",
      approach == "log-binomial" ~ "Risk Ratio",
      approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
      approach == "robpoisson" ~ "Risk Ratio",
      approach == "linear" ~ "Beta Coefficient",
      TRUE ~ "Effect Size"
    )
    if (adjusted) paste("Adjusted", base) else base
  }

  build_plot_df <- function(tbl, show_ref = TRUE, order_y = NULL) {
    df_body <- tbl$table_body
    df_disp <- tbl$table_display

    df_body$estimate <- suppressWarnings(as.numeric(df_body$estimate))
    df_body$conf.low <- suppressWarnings(as.numeric(df_body$conf.low))
    df_body$conf.high <- suppressWarnings(as.numeric(df_body$conf.high))
    df_body$p.value <- suppressWarnings(as.numeric(df_body$p.value))

    header_rows <- df_disp[df_disp$is_header, , drop = FALSE]
    exposure_order <- trimws(header_rows$Characteristic)

    if (!is.null(order_y)) {
      exposure_order <- c(
        intersect(order_y, exposure_order),
        setdiff(exposure_order, order_y)
      )
    }

    plot_rows <- list()

    for (ex in exposure_order) {
      dfx <- df_body[df_body$exposure == ex, , drop = FALSE]
      if (!nrow(dfx)) next

      is_factor_exp <- any(dfx$ref)

      disp_idx <- which(df_disp$is_header & trimws(df_disp$Characteristic) == ex)
      if (!length(disp_idx)) next

      start_i <- disp_idx[1]
      end_i <- if (start_i < nrow(df_disp)) {
        next_headers <- which(df_disp$is_header & seq_len(nrow(df_disp)) > start_i)
        if (length(next_headers)) min(next_headers) - 1 else nrow(df_disp)
      } else {
        nrow(df_disp)
      }

      disp_block <- df_disp[start_i:end_i, , drop = FALSE]

      main_row <- dfx[!dfx$ref & dfx$level == ex, , drop = FALSE]

      header_plot <- data.frame(
        exposure = ex,
        label = ex,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_,
        ref = FALSE,
        is_header = TRUE,
        is_data = FALSE,
        stringsAsFactors = FALSE
      )

      if (!is_factor_exp && nrow(main_row)) {
        header_plot$estimate <- main_row$estimate[1]
        header_plot$conf.low <- main_row$conf.low[1]
        header_plot$conf.high <- main_row$conf.high[1]
        header_plot$p.value <- main_row$p.value[1]
        header_plot$is_data <- TRUE
      }

      plot_rows[[length(plot_rows) + 1]] <- header_plot

      if (nrow(disp_block) > 1) {
        sub_block <- disp_block[-1, , drop = FALSE]

        for (i in seq_len(nrow(sub_block))) {
          char_i <- sub_block$Characteristic[i]
          lab_i <- trimws(char_i)

          row_match <- dfx[dfx$level == lab_i, , drop = FALSE]
          if (!nrow(row_match)) next

          if (isTRUE(row_match$ref[1]) && !show_ref) next

          plot_rows[[length(plot_rows) + 1]] <- data.frame(
            exposure = ex,
            label = lab_i,
            estimate = row_match$estimate[1],
            conf.low = row_match$conf.low[1],
            conf.high = row_match$conf.high[1],
            p.value = row_match$p.value[1],
            ref = isTRUE(row_match$ref[1]),
            is_header = FALSE,
            is_data = !isTRUE(row_match$ref[1]),
            stringsAsFactors = FALSE
          )
        }
      } else {
        extra_rows <- dfx[!dfx$ref & dfx$level != ex, , drop = FALSE]
        if (nrow(extra_rows)) {
          for (i in seq_len(nrow(extra_rows))) {
            plot_rows[[length(plot_rows) + 1]] <- data.frame(
              exposure = ex,
              label = extra_rows$level[i],
              estimate = extra_rows$estimate[i],
              conf.low = extra_rows$conf.low[i],
              conf.high = extra_rows$conf.high[i],
              p.value = extra_rows$p.value[i],
              ref = FALSE,
              is_header = FALSE,
              is_data = TRUE,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    out <- do.call(rbind, plot_rows)

    indent_html <- "<span style='color:white;'>..</span>"

    out$label_clean <- dplyr::case_when(
      out$is_header ~ paste0("<b>", out$label, "</b>"),
      out$ref & show_ref ~ paste0(
        indent_html,
        out$label,
        " <span style='color:gray;'>(ref)</span>"
      ),
      !out$is_header ~ paste0(indent_html, out$label),
      TRUE ~ NA_character_
    )

    out$row_key <- paste0(out$exposure, "::", out$label)
    out
  }

  approach_uni <- tbl_uni$approach
  approach_multi <- tbl_multi$approach

  xlab_uni <- get_axis_label(approach_uni, adjusted = FALSE)
  xlab_multi <- get_axis_label(approach_multi, adjusted = TRUE)

  log_x_uni <- isTRUE(log_x) && !identical(approach_uni, "linear")
  log_x_multi <- isTRUE(log_x) && !identical(approach_multi, "linear")

  if (log_x_uni) xlab_uni <- paste0(xlab_uni, " (log scale)")
  if (log_x_multi) xlab_multi <- paste0(xlab_multi, " (log scale)")

  ref_uni <- if (is.null(ref_line)) {
    if (identical(approach_uni, "linear")) 0 else 1
  } else {
    ref_line
  }

  ref_multi <- if (is.null(ref_line)) {
    if (identical(approach_multi, "linear")) 0 else 1
  } else {
    ref_line
  }

  df_uni <- build_plot_df(tbl_uni, show_ref = show_ref, order_y = order_y)
  df_multi <- build_plot_df(tbl_multi, show_ref = show_ref, order_y = order_y)

  row_keys <- unique(c(df_uni$row_key, df_multi$row_key))

  skeleton <- data.frame(
    row_key = row_keys,
    stringsAsFactors = FALSE
  )

  label_from_key <- function(x) sub("^[^:]+::", "", x)
  skeleton$label <- label_from_key(skeleton$row_key)

  df_uni <- merge(skeleton, df_uni, by = c("row_key", "label"), all.x = TRUE, sort = FALSE)
  df_multi <- merge(skeleton, df_multi, by = c("row_key", "label"), all.x = TRUE, sort = FALSE)

  df_uni$is_header[is.na(df_uni$is_header)] <- FALSE
  df_uni$is_data[is.na(df_uni$is_data)] <- FALSE
  df_multi$is_header[is.na(df_multi$is_header)] <- FALSE
  df_multi$is_data[is.na(df_multi$is_data)] <- FALSE

  # preserve label markdown from uni panel
  label_map_raw <- df_uni$label_clean
  label_map_raw[is.na(label_map_raw)] <- paste0(
    "<span style='color:white;'>..</span>",
    df_uni$label[is.na(label_map_raw)]
  )

  compute_is_sig <- function(df, approach, alpha, ref_line_val) {
    if (identical(approach, "linear")) {
      df$is_sig <- dplyr::case_when(
        !df$is_data ~ FALSE,
        !is.na(df$p.value) & df$p.value < alpha ~ TRUE,
        TRUE ~ FALSE
      )
    } else {
      df$is_sig <- dplyr::case_when(
        !df$is_data ~ FALSE,
        is.finite(df$conf.low) & is.finite(df$conf.high) &
          ((df$conf.low > ref_line_val) | (df$conf.high < ref_line_val)) ~ TRUE,
        TRUE ~ FALSE
      )
    }
    df
  }

  df_uni <- compute_is_sig(df_uni, approach_uni, alpha, ref_uni)
  df_multi <- compute_is_sig(df_multi, approach_multi, alpha, ref_multi)

  df_uni$row_id <- factor(seq_len(nrow(df_uni)), levels = rev(seq_len(nrow(df_uni))))
  df_multi$row_id <- factor(seq_len(nrow(df_multi)), levels = rev(seq_len(nrow(df_multi))))

  label_map <- stats::setNames(label_map_raw, as.character(df_uni$row_id))

  fill_vals <- c(
    "FALSE" = point_color,
    "TRUE" = if (!is.null(sig_color)) sig_color else point_color
  )
  line_vals <- c(
    "FALSE" = errorbar_color,
    "TRUE" = if (!is.null(sig_errorbar_color)) sig_errorbar_color else errorbar_color
  )

  build_panel <- function(df, panel_title, x_label, ref_line_val, use_log,
                          xlim = NULL, breaks = NULL, show_y = TRUE) {

    if (is.null(xlim)) {
      vals <- c(df$conf.low, df$conf.high, df$estimate, ref_line_val)
      vals <- vals[is.finite(vals)]
      if (length(vals) >= 2L) {
        rng <- range(vals, na.rm = TRUE)
        if (use_log) {
          lower <- max(min(rng[1], ref_line_val, na.rm = TRUE), .Machine$double.eps * 10)
          upper <- max(rng[2], ref_line_val, na.rm = TRUE)
        } else {
          span <- diff(rng)
          pad <- if (is.finite(span) && span > 0) 0.05 * span else 0.1
          lower <- min(rng[1], ref_line_val, na.rm = TRUE) - pad
          upper <- max(rng[2], ref_line_val, na.rm = TRUE) + pad
        }
        xlim <- c(lower, upper)
      }
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row_id)) +
      .add_h_ci(df[df$is_data, , drop = FALSE]) +
      ggplot2::geom_point(
        data = df[df$is_data, , drop = FALSE],
        ggplot2::aes(fill = .data$is_sig),
        shape = 21,
        size = 3,
        stroke = 0.6,
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      ggplot2::geom_vline(
        xintercept = ref_line_val,
        linetype = "dashed",
        colour = "gray60"
      ) +
      ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
      ggplot2::scale_color_manual(values = line_vals, guide = "none") +
      ggplot2::scale_y_discrete(
        labels = label_map,
        limits = levels(df$row_id),
        drop = FALSE
      ) +
      ggplot2::labs(title = panel_title, x = x_label, y = NULL) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        axis.text.y = if (show_y) ggtext::element_markdown(family = "mono", hjust = 0) else ggplot2::element_blank(),
        axis.text.y.left = if (show_y) ggtext::element_markdown(family = "mono", hjust = 0) else ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.margin = ggplot2::margin(10, 40, 10, 10)
      )

    if (!is.null(breaks) && !use_log) {
      p <- p + ggplot2::scale_x_continuous(breaks = breaks)
    }
    if (use_log) {
      p <- p + ggplot2::scale_x_log10()
    }
    if (!is.null(xlim)) {
      p <- p + ggplot2::coord_cartesian(xlim = xlim)
    }

    p
  }

  p1 <- build_panel(
    df = df_uni,
    panel_title = title_uni,
    x_label = xlab_uni,
    ref_line_val = ref_uni,
    use_log = log_x_uni,
    xlim = xlim_uni,
    breaks = breaks_uni,
    show_y = TRUE
  )

  p2 <- build_panel(
    df = df_multi,
    panel_title = title_multi,
    x_label = xlab_multi,
    ref_line_val = ref_multi,
    use_log = log_x_multi,
    xlim = xlim_multi,
    breaks = breaks_multi,
    show_y = FALSE
  )

  patchwork::wrap_plots(p1, p2, ncol = 2, widths = c(1.2, 1))
}
