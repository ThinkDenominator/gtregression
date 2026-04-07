#' Visualize a regression model as a forest plot
#'
#' Creates a forest plot from a fitted \code{gtregression} object produced by
#' functions such as \code{uni_reg()} or \code{multi_reg()}.
#'
#' @param tbl A fitted \code{gtregression} object.
#' @param title Optional plot title.
#' @param ref_line Optional numeric value for the reference line.
#'   Defaults to 0 for linear models and 1 otherwise.
#' @param order_y Optional character vector specifying exposure order.
#' @param log_x Logical; if \code{TRUE}, use a log-scaled x-axis for non-linear models.
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits.
#' @param breaks Optional numeric vector of x-axis tick breaks.
#' @param point_color Fill color for points.
#' @param errorbar_color Color for error bars.
#' @param base_size Base font size.
#' @param show_ref Logical; if \code{TRUE}, reference rows are shown.
#' @param sig_color Optional fill color for significant points.
#' @param sig_errorbar_color Optional color for significant error bars.
#' @param alpha Significance level for linear models when \code{p.value} is available.
#'
#' @return A \code{ggplot2} object.
#' @importFrom rlang .data
#' @importFrom stats setNames
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
                     sig_color = NULL,
                     sig_errorbar_color = NULL,
                     alpha = 0.05) {

  if (!inherits(tbl, "gtregression")) {
    stop("`tbl` must be a gtregression object.", call. = FALSE)
  }

  if (is.null(tbl$table_body) || !nrow(tbl$table_body)) {
    stop("`tbl$table_body` is missing or empty.", call. = FALSE)
  }

  if (is.null(tbl$table_display) || !nrow(tbl$table_display)) {
    stop("`tbl$table_display` is missing or empty.", call. = FALSE)
  }

  df_body <- tbl$table_body
  df_disp <- tbl$table_display
  source_type <- tbl$source
  approach <- tbl$approach

  if (is.null(source_type) || is.null(approach)) {
    stop("`tbl` must contain `source` and `approach`.", call. = FALSE)
  }

  if (identical(source_type, "stratified_multi_reg") ||
      identical(source_type, "stratified_uni_reg")) {
    stop("plot_reg() does not support stratified objects.", call. = FALSE)
  }

  req_body <- c("exposure", "level", "estimate", "conf.low", "conf.high", "p.value", "ref")
  if (!all(req_body %in% names(df_body))) {
    stop(
      "plot_reg() requires `table_body` to contain: ",
      paste(req_body, collapse = ", "),
      call. = FALSE
    )
  }

  if (!all(c("Characteristic", "is_header") %in% names(df_disp))) {
    stop(
      "plot_reg() requires `table_display` to contain `Characteristic` and `is_header`.",
      call. = FALSE
    )
  }

  df_body$estimate  <- suppressWarnings(as.numeric(df_body$estimate))
  df_body$conf.low  <- suppressWarnings(as.numeric(df_body$conf.low))
  df_body$conf.high <- suppressWarnings(as.numeric(df_body$conf.high))
  df_body$p.value   <- suppressWarnings(as.numeric(df_body$p.value))

  if (is.null(ref_line)) {
    ref_line <- if (identical(approach, "linear")) 0 else 1
  }

  is_multi <- source_type %in% c("multi_reg", "multi_reg_nbin")

  base_label <- dplyr::case_when(
    approach == "logit" ~ "Odds Ratio",
    approach == "log-binomial" ~ "Risk Ratio",
    approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
    approach == "robpoisson" ~ "Risk Ratio",
    approach == "linear" ~ "Beta Coefficient",
    TRUE ~ "Effect Size"
  )

  x_axis_label <- if (isTRUE(is_multi)) paste("Adjusted", base_label) else base_label

  if (identical(approach, "linear")) {
    log_x <- FALSE
  }
  if (log_x) {
    x_axis_label <- paste0(x_axis_label, " (log scale)")
  }

  # ---- build header order from table_display ----
  header_rows <- df_disp[df_disp$is_header, , drop = FALSE]
  exposure_order <- trimws(header_rows$Characteristic)

  if (!is.null(order_y)) {
    exposure_order <- c(
      intersect(order_y, exposure_order),
      setdiff(exposure_order, order_y)
    )
  }

  # ---- build plotting rows using table_display skeleton + table_body values ----
  plot_rows <- list()

  for (ex in exposure_order) {
    dfx <- df_body[df_body$exposure == ex, , drop = FALSE]
    if (!nrow(dfx)) next

    is_factor_exp <- any(dfx$ref)

    # display rows for this exposure
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

    # header row
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

    # continuous exposure: plot estimate on header row
    if (!is_factor_exp && nrow(main_row)) {
      header_plot$estimate <- main_row$estimate[1]
      header_plot$conf.low <- main_row$conf.low[1]
      header_plot$conf.high <- main_row$conf.high[1]
      header_plot$p.value <- main_row$p.value[1]
      header_plot$is_data <- TRUE
    }

    plot_rows[[length(plot_rows) + 1]] <- header_plot

    # categorical levels OR extra rows (e.g. interaction rows)
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
      # handle non-factor extras in table_body not represented in table_display
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

  plot_df <- do.call(rbind, plot_rows)

  # ---- OG-style label rendering ----
  indent_html <- "<span style='color:white;'>..</span>"

  plot_df$label_clean <- dplyr::case_when(
    plot_df$is_header ~ paste0("<b>", plot_df$label, "</b>"),
    plot_df$ref & show_ref ~ paste0(
      indent_html,
      plot_df$label,
      " <span style='color:gray;'>(ref)</span>"
    ),
    !plot_df$is_header ~ paste0(indent_html, plot_df$label),
    TRUE ~ NA_character_
  )

  # ---- significance ----
  if (identical(approach, "linear")) {
    plot_df$is_sig <- dplyr::case_when(
      !plot_df$is_data ~ FALSE,
      !is.na(plot_df$p.value) & plot_df$p.value < alpha ~ TRUE,
      TRUE ~ FALSE
    )
  } else {
    plot_df$is_sig <- dplyr::case_when(
      !plot_df$is_data ~ FALSE,
      is.finite(plot_df$conf.low) & is.finite(plot_df$conf.high) &
        ((plot_df$conf.low > ref_line) | (plot_df$conf.high < ref_line)) ~ TRUE,
      TRUE ~ FALSE
    )
  }

  plot_df$row_id <- factor(seq_len(nrow(plot_df)), levels = rev(seq_len(nrow(plot_df))))
  label_map <- stats::setNames(plot_df$label_clean, as.character(plot_df$row_id))

  fill_vals <- c(
    "FALSE" = point_color,
    "TRUE" = if (!is.null(sig_color)) sig_color else point_color
  )
  line_vals <- c(
    "FALSE" = errorbar_color,
    "TRUE" = if (!is.null(sig_errorbar_color)) sig_errorbar_color else errorbar_color
  )

  if (is.null(xlim)) {
    vals <- c(plot_df$conf.low, plot_df$conf.high, plot_df$estimate, ref_line)
    vals <- vals[is.finite(vals)]
    if (length(vals) >= 2L) {
      rng <- range(vals, na.rm = TRUE)
      if (log_x) {
        lower <- max(min(rng[1], ref_line, na.rm = TRUE), .Machine$double.eps * 10)
        upper <- max(rng[2], ref_line, na.rm = TRUE)
      } else {
        span <- diff(rng)
        pad <- if (is.finite(span) && span > 0) 0.05 * span else 0.1
        lower <- min(rng[1], ref_line, na.rm = TRUE) - pad
        upper <- max(rng[2], ref_line, na.rm = TRUE) + pad
      }
      xlim <- c(lower, upper)
    }
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$estimate, y = .data$row_id)
  ) +
    .add_h_ci(plot_df[plot_df$is_data, , drop = FALSE]) +
    ggplot2::geom_point(
      data = plot_df[plot_df$is_data, , drop = FALSE],
      ggplot2::aes(fill = .data$is_sig),
      shape = 21,
      size = 3,
      stroke = 0.6,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_vline(
      xintercept = ref_line,
      linetype = "dashed",
      colour = "gray60"
    ) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::scale_color_manual(values = line_vals, guide = "none") +
    ggplot2::scale_y_discrete(
      labels = label_map,
      limits = levels(plot_df$row_id),
      drop = FALSE
    ) +
    ggplot2::labs(title = title, x = x_axis_label, y = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(hjust = 0),
      axis.text.y.left = ggtext::element_markdown(hjust = 0),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 40, 10, 10)
    )

  if (!is.null(breaks) && !log_x) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  }
  if (log_x) {
    p <- p + ggplot2::scale_x_log10()
  }
  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }

  p
}

#' Horizontal CI helper (internal)
#' @keywords internal
#' @noRd
.add_h_ci <- function(data) {
  v <- utils::packageVersion("ggplot2")

  if (v >= "4.0.0") {
    ggplot2::geom_errorbar(
      data = data,
      ggplot2::aes(
        xmin = .data$conf.low,
        xmax = .data$conf.high,
        colour = .data$is_sig
      ),
      orientation = "y",
      width = 0.2,
      na.rm = TRUE
    )
  } else if (v >= "3.5.0") {
    ggplot2::geom_errorbarh(
      data = data,
      ggplot2::aes(
        xmin = .data$conf.low,
        xmax = .data$conf.high,
        colour = .data$is_sig
      ),
      width = 0.2,
      na.rm = TRUE
    )
  } else {
    ggplot2::geom_errorbarh(
      data = data,
      ggplot2::aes(
        xmin = .data$conf.low,
        xmax = .data$conf.high,
        colour = .data$is_sig
      ),
      height = 0.2,
      na.rm = TRUE
    )
  }
}
