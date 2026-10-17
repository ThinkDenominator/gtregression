#' Plot regression estimates
#'
#' Draw a forest-style plot from a fitted \code{gtregression} object produced by
#' functions such as \code{uni_reg()}, \code{multi_reg()}, \code{cox_reg()}, or
#' \code{surv_reg()}. Stratified regression outputs are displayed as one
#' faceted quick-view plot; use \code{forest_df()} and \code{forest_reg()} for
#' publication-style stratified forest tables.
#'
#' @param tbl A fitted \code{gtregression} object.
#' @param title Optional plot title.
#' @param caption Optional plot caption. If \code{NULL}, notes are added
#'   automatically for adjusted \code{multi_reg()} objects and displayed
#'   reference categories.
#' @param ref_line Optional numeric value for the reference line.
#'   Defaults to 0 for linear models and 1 otherwise.
#' @param order_y Optional character vector specifying exposure order.
#' @param log_x Logical. If \code{TRUE}, use a log-scaled x-axis for ratio
#'   measures. The default \code{FALSE} uses a linear x-axis.
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits.
#' @param breaks Optional numeric vector of x-axis tick breaks.
#' @param point_color Fill color for non-significant estimate points.
#' @param errorbar_color Color for non-significant confidence intervals.
#' @param point_size Diameter of estimate points in millimetres.
#' @param point_stroke Outline width of estimate points.
#' @param ci_linewidth Line width of confidence intervals.
#' @param base_size Base font size. The default is designed for a clear report
#'   or slide figure; increase it for a single large plot.
#' @param show_ref Logical or \code{NULL}. The default \code{NULL} inherits the
#'   reference-row choice used to create \code{tbl}. If \code{TRUE}, reference
#'   rows are shown. If \code{FALSE}, binary exposures are shown as compact rows
#'   for the estimated non-reference category; affirmative levels such as
#'   \code{Yes} or \code{1} are displayed using the variable name.
#' @param sig_color Optional fill color for significant points.
#' @param sig_errorbar_color Optional color for significant error bars.
#' @param alpha Significance level for linear models when \code{p.value} is available.
#' @param show_adjustment_note Logical; if \code{TRUE}, add a default caption
#'   describing \code{adjust_for} variables when available.
#'
#' @return A \code{ggplot2} object.
#' @importFrom rlang .data
#' @importFrom stats ave setNames
#' @export
plot_reg <- function(tbl,
                     title = NULL,
                     caption = NULL,
                     ref_line = NULL,
                     order_y = NULL,
                     log_x = FALSE,
                     xlim = NULL,
                     breaks = NULL,
                     point_color = "#6B7280",
                     errorbar_color = "#6B7280",
                     point_size = 2.8,
                     point_stroke = 0.55,
                     ci_linewidth = 0.55,
                     base_size = 12,
                     show_ref = NULL,
                     sig_color = "#0072B2",
                     sig_errorbar_color = "#0072B2",
                     alpha = 0.05,
                     show_adjustment_note = TRUE) {

  .rd_reject(list(tbl), "plot_reg")
  if (!inherits(tbl, "gtregression")) {
    stop("`tbl` must be a gtregression object.", call. = FALSE)
  }

  if (inherits(tbl, "multinomial_reg")) {
    stop(
      "A multinomial model has multiple outcome comparisons. Use ",
      "`plot_reg(x$per_outcome[[\"category\"]])` for one comparison.",
      call. = FALSE
    )
  }

  source_type <- tbl$source
  approach <- .normalize_approach(tbl$approach)

  if (is.null(source_type) || is.null(approach)) {
    stop("`tbl` must contain `source` and `approach`.", call. = FALSE)
  }

  log_x <- .plot_resolve_log_x(log_x, approach)

  show_ref <- .plot_resolve_show_ref(show_ref, tbl)

  if (.plot_is_stratified(tbl, source_type)) {
    return(.plot_reg_stratified(
      tbl = tbl,
      title = title,
      caption = caption,
      ref_line = ref_line,
      order_y = order_y,
      log_x = log_x,
      xlim = xlim,
      breaks = breaks,
      point_color = point_color,
      errorbar_color = errorbar_color,
      point_size = point_size,
      point_stroke = point_stroke,
      ci_linewidth = ci_linewidth,
      base_size = base_size,
      show_ref = show_ref,
      sig_color = sig_color,
      sig_errorbar_color = sig_errorbar_color,
      alpha = alpha,
      show_adjustment_note = show_adjustment_note
    ))
  }

  if (is.null(tbl$table_body) || !nrow(tbl$table_body)) {
    stop("`tbl$table_body` is missing or empty.", call. = FALSE)
  }

  if (is.null(tbl$table_display) || !nrow(tbl$table_display)) {
    stop("`tbl$table_display` is missing or empty.", call. = FALSE)
  }

  df_body <- tbl$table_body
  df_disp <- tbl$table_display

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

  is_multi <- source_type %in% c("multi_reg", "multi_reg_nbin") ||
    .is_adjusted_reg_output(tbl)

  base_label <- dplyr::case_when(
    approach %in% c("logit", "firth") ~ "Odds Ratio",
    approach == "conditional_logit" ~ "Matched Odds Ratio",
    approach == "ordinal_logit" ~ "Common Odds Ratio",
    approach == "multinomial_logit" ~ "Odds Ratio",
    approach == "logbinomial" ~ "Risk Ratio",
    approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
    approach == "robpoisson" ~ "Risk Ratio",
    approach == "cox" ~ "Hazard Ratio",
    approach == "fine_gray" ~ "Subdistribution Hazard Ratio",
    approach == "survreg" ~ "Time Ratio",
    approach == "linear" ~ "Beta Coefficient",
    TRUE ~ "Effect Size"
  )

  x_axis_label <- if (isTRUE(is_multi)) paste("Adjusted", base_label) else base_label

  if (log_x) {
    x_axis_label <- paste0(x_axis_label, " (log scale)")
  }

  # ---- plotting rows keyed on (exposure, level) from table_body ----
  plot_df <- .plot_rows_from_table(tbl, show_ref = show_ref, order_y = order_y)
  plot_df$label_clean <- .plot_axis_labels(plot_df$label_clean)

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

  caption <- .plot_caption(
    tbl = tbl,
    caption = caption,
    show_adjustment_note = show_adjustment_note,
    show_ref = show_ref,
    has_ref = any(plot_df$ref %in% TRUE)
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
    .add_h_ci(plot_df[plot_df$is_data, , drop = FALSE], linewidth = ci_linewidth) +
    ggplot2::geom_point(
      data = plot_df[plot_df$is_data, , drop = FALSE],
      ggplot2::aes(fill = .data$is_sig),
      shape = 21,
      size = point_size,
      stroke = point_stroke,
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
    ggplot2::labs(title = title, x = x_axis_label, y = NULL, caption = caption) +
    .plot_reg_theme(base_size = base_size)

  if (log_x) {
    if (is.null(breaks)) {
      breaks <- .plot_log_breaks(xlim)
    }
    p <- p + ggplot2::scale_x_log10(breaks = breaks)
  } else if (!is.null(breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  }
  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }

  p
}

#' Display labels of a regression table, keyed on table_body names
#'
#' Rows are identified by the raw exposure name and factor level stored in
#' `table_body`; the text printed in `table_display` is used only as the
#' label. A level relabelled with `modify_table(level_labels = )` therefore
#' keeps its estimate.
#' @keywords internal
#' @noRd
.plot_display_labels <- function(tbl, exposure, body_rows) {
  display <- tbl$table_display
  default_header <- .label_var(exposure, tbl$variable_labels)
  is_factor <- any(body_rows$ref %in% TRUE)
  body_levels <- as.character(body_rows$level)
  if (!is_factor) body_levels <- body_levels[body_levels != exposure]
  level_labels <- stats::setNames(body_levels, body_levels)

  row_exposure <- attr(display, "row_exposure", exact = TRUE)
  if (length(row_exposure) == nrow(display)) {
    block <- which(as.character(row_exposure) == exposure)
  } else {
    start <- which(display$is_header %in% TRUE &
                     trimws(display$Characteristic) == default_header)
    if (length(start)) {
      later <- which(display$is_header %in% TRUE &
                       seq_len(nrow(display)) > start[1L])
      end <- if (length(later)) min(later) - 1L else nrow(display)
      block <- start[1L]:end
    } else {
      block <- integer()
    }
  }
  if (!length(block)) return(NULL)

  header_rows <- block[display$is_header[block] %in% TRUE]
  header_label <- if (length(header_rows)) {
    trimws(display$Characteristic[header_rows[1L]])
  } else {
    default_header
  }

  level_rows <- setdiff(block, header_rows)
  shown <- trimws(as.character(display$Characteristic[level_rows]))
  if (length(shown) && length(shown) == length(body_levels)) {
    # Same rows in the same order: the display text is the label.
    level_labels[] <- shown
  } else if (length(shown)) {
    hit <- match(body_levels, shown)
    level_labels[!is.na(hit)] <- shown[hit[!is.na(hit)]]
  }

  list(header = header_label, levels = level_labels)
}

#' Build the plotting rows of one regression table
#'
#' One row per axis line with `exposure`, `level` (raw `table_body` names),
#' `row_key`, the display `label`, the estimate and its interval.
#' @keywords internal
#' @noRd
.plot_rows_from_table <- function(tbl, show_ref = TRUE, order_y = NULL) {
  df_body <- tbl$table_body
  df_body$estimate  <- suppressWarnings(as.numeric(df_body$estimate))
  df_body$conf.low  <- suppressWarnings(as.numeric(df_body$conf.low))
  df_body$conf.high <- suppressWarnings(as.numeric(df_body$conf.high))
  df_body$p.value   <- suppressWarnings(as.numeric(df_body$p.value))

  exposure_order <- unique(as.character(df_body$exposure))
  if (!is.null(order_y)) {
    exposure_order <- c(
      intersect(order_y, exposure_order),
      setdiff(exposure_order, order_y)
    )
  }

  one_row <- function(exposure, level, label, values = NULL, ref = FALSE,
                      is_header = FALSE, is_data = FALSE) {
    data.frame(
      exposure = exposure,
      level = level,
      label = label,
      estimate = if (is.null(values)) NA_real_ else values$estimate[1L],
      conf.low = if (is.null(values)) NA_real_ else values$conf.low[1L],
      conf.high = if (is.null(values)) NA_real_ else values$conf.high[1L],
      p.value = if (is.null(values)) NA_real_ else values$p.value[1L],
      ref = ref,
      is_header = is_header,
      is_data = is_data,
      stringsAsFactors = FALSE
    )
  }

  plot_rows <- list()
  add <- function(row) plot_rows[[length(plot_rows) + 1L]] <<- row

  for (ex in exposure_order) {
    dfx <- df_body[df_body$exposure == ex, , drop = FALSE]
    if (!nrow(dfx)) next

    labels <- .plot_display_labels(tbl, ex, dfx)
    if (is.null(labels)) next

    is_factor_exp <- any(dfx$ref %in% TRUE)
    is_binary_exp <- is_factor_exp && nrow(dfx) == 2L &&
      sum(dfx$ref %in% TRUE) == 1L

    if (is_binary_exp && !isTRUE(show_ref)) {
      binary_row <- dfx[!(dfx$ref %in% TRUE), , drop = FALSE]
      # A display that already shows the variable on one line keeps its own
      # label; otherwise name the plotted level.
      add(one_row(
        ex, as.character(binary_row$level[1L]),
        .plot_binary_label(labels$header, binary_row$level[1L]),
        values = binary_row, is_header = TRUE, is_data = TRUE
      ))
      next
    }

    if (!is_factor_exp) {
      main_row <- dfx[as.character(dfx$level) == ex, , drop = FALSE]
      if (nrow(main_row)) {
        add(one_row(ex, ex, labels$header, values = main_row,
                    is_header = TRUE, is_data = TRUE))
      } else {
        add(one_row(ex, "__header__", labels$header, is_header = TRUE))
      }
      extra <- dfx[as.character(dfx$level) != ex, , drop = FALSE]
      for (i in seq_len(nrow(extra))) {
        level <- as.character(extra$level[i])
        add(one_row(ex, level, unname(labels$levels[level]),
                    values = extra[i, , drop = FALSE], is_data = TRUE))
      }
      next
    }

    add(one_row(ex, "__header__", labels$header, is_header = TRUE))
    for (i in seq_len(nrow(dfx))) {
      is_ref <- isTRUE(dfx$ref[i])
      if (is_ref && !isTRUE(show_ref)) next
      level <- as.character(dfx$level[i])
      add(one_row(ex, level, unname(labels$levels[level]),
                  values = dfx[i, , drop = FALSE],
                  ref = is_ref, is_data = !is_ref))
    }
  }

  if (!length(plot_rows)) {
    stop("No rows of `table_body` could be matched to `table_display`.",
         call. = FALSE)
  }
  out <- do.call(rbind, plot_rows)
  rownames(out) <- NULL
  out$row_key <- paste0(out$exposure, "::", out$level)
  out$label_clean <- .plot_label_markup(out, show_ref = show_ref)
  out
}

#' Axis-label markup for plotting rows (bold headers, indented levels)
#' @keywords internal
#' @noRd
.plot_label_markup <- function(rows, show_ref = TRUE) {
  indent_html <- "<span style='color:white;'>..</span>"
  ifelse(
    rows$is_header %in% TRUE,
    paste0("<b>", rows$label, "</b>"),
    ifelse(
      rows$ref %in% TRUE & isTRUE(show_ref),
      paste0(indent_html, rows$label, " <span style='color:gray;'>(Ref.)</span>"),
      paste0(indent_html, rows$label)
    )
  )
}

#' Is ggtext available to render label markup?
#' @keywords internal
#' @noRd
.plot_has_ggtext <- function() {
  requireNamespace("ggtext", quietly = TRUE)
}

#' Axis labels as drawn: markup with ggtext, plain text without it
#'
#' Without ggtext the axis uses `element_text()`, which would print the HTML
#' tags literally. The markup is therefore removed and the level indent is
#' written with spaces.
#' @keywords internal
#' @noRd
.plot_axis_labels <- function(labels, markdown = .plot_has_ggtext()) {
  if (isTRUE(markdown)) return(labels)
  out <- gsub("<span style='color:white;'>..</span>", "    ", labels, fixed = TRUE)
  out <- gsub("<[^>]+>", "", out)
  out <- gsub("&lt;", "<", out, fixed = TRUE)
  out <- gsub("&gt;", ">", out, fixed = TRUE)
  gsub("&amp;", "&", out, fixed = TRUE)
}

#' Resolve reference-row display for regression plots
#' @keywords internal
#' @noRd
.plot_resolve_show_ref <- function(show_ref, ...) {
  objects <- list(...)

  if (!is.null(show_ref)) {
    if (!is.logical(show_ref) || length(show_ref) != 1L || is.na(show_ref)) {
      stop("`show_ref` must be TRUE, FALSE, or NULL.", call. = FALSE)
    }
    return(isTRUE(show_ref))
  }

  stored <- vapply(objects, function(x) {
    if (is.null(x$show_ref)) NA else isTRUE(x$show_ref)
  }, logical(1))
  stored <- stored[!is.na(stored)]

  if (!length(stored)) return(TRUE)
  if (length(unique(stored)) == 1L) return(stored[[1]])

  warning(
    paste0(
      "The supplied tables use different `show_ref` settings; reference rows are shown to keep ",
      "panels aligned. Set `show_ref` explicitly to override this."
    ),
    call. = FALSE
  )
  TRUE
}

#' Plot stratified regression outputs (internal)
#' @keywords internal
#' @noRd
.plot_reg_stratified <- function(tbl,
                                 title = NULL,
                                 caption = NULL,
                                 ref_line = NULL,
                                 order_y = NULL,
                                 log_x = FALSE,
                                 xlim = NULL,
                                 breaks = NULL,
                                 point_color = "#6B7280",
                                 errorbar_color = "#6B7280",
                                 point_size = 2.8,
                                 point_stroke = 0.55,
                                 ci_linewidth = 0.55,
                                 base_size = 12,
                                 show_ref = TRUE,
                                 sig_color = "#0072B2",
                                 sig_errorbar_color = "#0072B2",
                                 alpha = 0.05,
                                 show_adjustment_note = TRUE) {
  if (is.null(tbl$per_stratum) || !length(tbl$per_stratum)) {
    stop("Stratified regression objects must contain `per_stratum` results.", call. = FALSE)
  }

  strata <- names(tbl$per_stratum)
  if (is.null(strata) || any(!nzchar(strata))) {
    strata <- paste0("Stratum ", seq_along(tbl$per_stratum))
  }

  parts <- Map(function(item, stratum) {
    stratum_tbl <- .plot_stratum_object(tbl, item)
    p <- plot_reg(
      stratum_tbl,
      title = NULL,
      caption = NULL,
      ref_line = ref_line,
      order_y = order_y,
      log_x = log_x,
      xlim = NULL,
      breaks = breaks,
      point_color = point_color,
      errorbar_color = errorbar_color,
      point_size = point_size,
      point_stroke = point_stroke,
      ci_linewidth = ci_linewidth,
      base_size = base_size,
      show_ref = show_ref,
      sig_color = sig_color,
      sig_errorbar_color = sig_errorbar_color,
      alpha = alpha,
      show_adjustment_note = show_adjustment_note
    )
    out <- p$data
    out$stratum <- stratum
    out$row_key <- paste(stratum, seq_len(nrow(out)), sep = "__")
    out
  }, tbl$per_stratum, strata)

  plot_df <- do.call(rbind, parts)
  plot_df$stratum <- factor(plot_df$stratum, levels = strata)
  row_count <- nrow(parts[[1]])
  if (any(vapply(parts, nrow, integer(1)) != row_count)) {
    stop("Each stratum must contain the same display rows for plot_reg().", call. = FALSE)
  }
  plot_df$row_index <- ave(
    seq_len(nrow(plot_df)),
    plot_df$stratum,
    FUN = seq_along
  )
  plot_df$row_key <- factor(plot_df$row_index, levels = rev(seq_len(row_count)))
  first_stratum <- plot_df[plot_df$stratum == strata[[1]], , drop = FALSE]
  label_map <- stats::setNames(first_stratum$label_clean, as.character(first_stratum$row_key))

  approach <- .normalize_approach(tbl$approach)
  if (is.null(ref_line)) {
    ref_line <- if (identical(approach, "linear")) 0 else 1
  }
  log_x <- .plot_resolve_log_x(log_x, approach)

  x_axis_label <- .plot_axis_label(
    approach = approach,
    adjusted = .plot_stratified_is_adjusted(tbl)
  )
  if (log_x) {
    x_axis_label <- paste0(x_axis_label, " (log scale)")
  }

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

  fill_vals <- c(
    "FALSE" = point_color,
    "TRUE" = if (!is.null(sig_color)) sig_color else point_color
  )
  line_vals <- c(
    "FALSE" = errorbar_color,
    "TRUE" = if (!is.null(sig_errorbar_color)) sig_errorbar_color else errorbar_color
  )
  caption <- .plot_caption(
    tbl = tbl,
    caption = caption,
    show_adjustment_note = show_adjustment_note,
    show_ref = show_ref,
    has_ref = any(plot_df$ref %in% TRUE)
  )
  # Facets otherwise drop structural header rows because their estimate is NA.
  plot_df$axis_anchor <- ref_line

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$estimate, y = .data$row_key)
  ) +
    ggplot2::geom_blank(ggplot2::aes(x = .data$axis_anchor, y = .data$row_key)) +
    .add_h_ci(plot_df[plot_df$is_data, , drop = FALSE], linewidth = ci_linewidth) +
    ggplot2::geom_point(
      data = plot_df[plot_df$is_data, , drop = FALSE],
      ggplot2::aes(fill = .data$is_sig),
      shape = 21,
      size = point_size,
      stroke = point_stroke,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_vline(
      xintercept = ref_line,
      linetype = "dashed",
      colour = "gray60"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$stratum), scales = "fixed") +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::scale_color_manual(values = line_vals, guide = "none") +
    ggplot2::scale_y_discrete(
      labels = label_map
    ) +
    ggplot2::labs(title = title, x = x_axis_label, y = NULL, caption = caption) +
    .plot_reg_theme(base_size = base_size, stratified = TRUE)

  if (log_x) {
    if (is.null(breaks)) {
      breaks <- .plot_log_breaks(xlim)
    }
    p <- p + ggplot2::scale_x_log10(breaks = breaks)
  } else if (!is.null(breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = breaks)
  }
  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }

  p
}

#' Stratified plot input detector (internal)
#' @keywords internal
#' @noRd
.plot_is_stratified <- function(tbl, source_type = tbl$source) {
  isTRUE(tbl$stratified) ||
    identical(source_type, "stratified_multi_reg") ||
    identical(source_type, "stratified_uni_reg") ||
    identical(source_type, "stratified_cox_reg") ||
    identical(source_type, "stratified_surv_reg")
}

#' Recreate a plottable per-stratum regression object (internal)
#' @keywords internal
#' @noRd
.plot_stratum_object <- function(parent, x) {
  if (inherits(x, "gtregression")) {
    # Per-stratum uni_reg objects retain their own display data. Rebuild it
    # with the parent label map so faceted plots match the stratified table.
    label_map <- parent$variable_labels
    if (is.null(label_map)) {
      label_map <- x$variable_labels
    }
    x$table_display <- .plot_display_from_body(
      x$table_body,
      label_map
    )
    x$variable_labels <- label_map
    x$stratified <- FALSE
    return(x)
  }

  out <- x
  out$table_display <- .plot_display_from_body(x$table_body, parent$variable_labels)
  out$approach <- parent$approach
  out$variable_labels <- parent$variable_labels
  out$stratified <- FALSE
  out$adjusted_mode <- .forest_or(parent$adjusted_mode, x$adjusted_mode)
  out$multivariable <- .forest_or(parent$multivariable, x$multivariable)
  out$adjust_for <- parent$adjust_for

  if (inherits(parent, "stratified_multi_reg")) {
    out$source <- "multi_reg"
    class(out) <- c("gtregression", "multi_reg", "gt_multi", class(out))
  } else if (inherits(parent, "stratified_cox_reg")) {
    out$source <- "cox_reg"
    class(out) <- c("gtregression", "cox_reg", "gt_cox", class(out))
  } else if (inherits(parent, "stratified_surv_reg")) {
    out$source <- "surv_reg"
    class(out) <- c("gtregression", "surv_reg", "gt_surv", class(out))
  } else {
    out$source <- parent$source
    class(out) <- c("gtregression", class(out))
  }

  out
}

#' Build a minimal table display from table_body rows (internal)
#' @keywords internal
#' @noRd
.plot_display_from_body <- function(table_body, variable_labels = NULL) {
  rows <- list()
  for (ex in unique(table_body$exposure)) {
    dfx <- table_body[table_body$exposure == ex, , drop = FALSE]
    rows[[length(rows) + 1L]] <- data.frame(
      Characteristic = .label_var(ex, variable_labels),
      is_header = TRUE,
      stringsAsFactors = FALSE
    )
    if (any(dfx$ref %in% TRUE)) {
      for (lv in dfx$level) {
        rows[[length(rows) + 1L]] <- data.frame(
          Characteristic = as.character(lv),
          is_header = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot axis label helper (internal)
#' @keywords internal
#' @noRd
.plot_axis_label <- function(approach, adjusted = FALSE) {
  base <- dplyr::case_when(
    approach %in% c("logit", "firth") ~ "Odds Ratio",
    approach == "conditional_logit" ~ "Matched Odds Ratio",
    approach == "ordinal_logit" ~ "Common Odds Ratio",
    approach == "multinomial_logit" ~ "Odds Ratio",
    approach == "logbinomial" ~ "Risk Ratio",
    approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
    approach == "robpoisson" ~ "Risk Ratio",
    approach == "cox" ~ "Hazard Ratio",
    approach == "survreg" ~ "Time Ratio",
    approach == "linear" ~ "Beta Coefficient",
    TRUE ~ "Effect Size"
  )
  if (isTRUE(adjusted)) paste("Adjusted", base) else base
}

#' Resolve automatic log-axis choice for regression plots (internal)
#' @keywords internal
#' @noRd
.plot_resolve_log_x <- function(log_x, approach) {
  if (is.null(log_x)) {
    return(FALSE)
  }
  if (!is.logical(log_x) || length(log_x) != 1L || is.na(log_x)) {
    stop("`log_x` must be TRUE, FALSE, or NULL.", call. = FALSE)
  }
  if (identical(approach, "linear")) return(FALSE)
  isTRUE(log_x)
}

#' Stratified adjusted plot detector (internal)
#' @keywords internal
#' @noRd
.plot_stratified_is_adjusted <- function(tbl) {
  inherits(tbl, "stratified_multi_reg") ||
    isTRUE(tbl$adjusted_mode) ||
    isTRUE(tbl$multivariable) ||
    any(vapply(tbl$per_stratum, function(x) {
      isTRUE(x$adjusted_mode) || isTRUE(x$multivariable)
    }, logical(1)))
}

#' Plot adjustment caption helper (internal)
#' @keywords internal
#' @noRd
.plot_adjustment_caption <- function(tbl, caption = NULL, show_adjustment_note = TRUE) {
  if (!is.null(caption)) {
    return(caption)
  }
  if (!isTRUE(show_adjustment_note)) {
    return(NULL)
  }

  adjust_for <- tbl$adjust_for
  if (!is.null(adjust_for) && length(adjust_for)) {
    return(.adjustment_note(adjust_for))
  }

  if (isTRUE(tbl$multivariable)) {
    return("Adjusted for the other variables in the model")
  }

  NULL
}

#' Plot caption helper (internal)
#' @keywords internal
#' @noRd
.plot_caption <- function(tbl,
                          caption = NULL,
                          show_adjustment_note = TRUE,
                          show_ref = TRUE,
                          has_ref = FALSE) {
  if (!is.null(caption)) {
    return(caption)
  }

  notes <- character(0)
  adjustment_note <- .plot_adjustment_caption(
    tbl,
    caption = NULL,
    show_adjustment_note = show_adjustment_note
  )
  if (!is.null(adjustment_note)) {
    notes <- c(notes, adjustment_note)
  }
  if (isTRUE(show_ref) && isTRUE(has_ref)) {
    notes <- c(notes, .ref_note())
  }

  if (!length(notes)) {
    return(NULL)
  }
  paste(notes, collapse = ". ")
}

#' Compact binary label helper (internal)
#' @keywords internal
#' @noRd
.plot_binary_label <- function(exposure, level) {
  level_chr <- tolower(trimws(as.character(level)))
  affirmative <- level_chr %in% c(
    "yes", "y", "1", "true", "t", "positive", "pos", "present", "exposed"
  )
  if (isTRUE(affirmative)) {
    return(exposure)
  }
  paste0(exposure, ": ", as.character(level))
}

#' Log-scale breaks helper (internal)
#' @keywords internal
#' @noRd
.plot_log_breaks <- function(xlim = NULL) {
  if (is.null(xlim) || length(xlim) != 2L || any(!is.finite(xlim))) {
    return(c(0.25, 0.5, 1, 2, 4, 8, 16))
  }
  # A doubling sequence stays legible in narrow panels. Users can supply
  # `breaks` when clinically meaningful non-standard tick marks are needed.
  candidates <- 2^seq(-4, 7)
  out <- candidates[candidates >= min(xlim) & candidates <= max(xlim)]
  if (length(out) > 5L) {
    targets <- exp(seq(log(min(out)), log(max(out)), length.out = 5L))
    keep <- vapply(targets, function(target) {
      which.min(abs(log(out) - log(target)))
    }, integer(1))
    keep <- unique(keep)

    one_idx <- which(out == 1)
    if (length(one_idx) && !one_idx %in% keep) {
      replace_idx <- which.min(abs(log(out[keep])))
      keep[replace_idx] <- one_idx[1]
    }
    out <- sort(unique(out[keep]))
  }
  if (!length(out)) {
    out <- signif(exp(seq(log(min(xlim)), log(max(xlim)), length.out = 5)), 2)
  }
  out
}

#' Horizontal CI helper (internal)
#' @keywords internal
#' @noRd
.add_h_ci <- function(data, linewidth = 0.55) {
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
      linewidth = linewidth,
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
      linewidth = linewidth,
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
      size = linewidth,
      na.rm = TRUE
    )
  }
}

#' Regression-plot theme (internal)
#' @keywords internal
#' @noRd
.plot_reg_theme <- function(base_size = 12, stratified = FALSE) {
  markdown_text <- function(...) {
    if (.plot_has_ggtext()) {
      ggtext::element_markdown(...)
    } else {
      ggplot2::element_text(...)
    }
  }

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#D9E1E5", linewidth = 0.35),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "#6B7280", linewidth = 0.35),
      axis.ticks.x = ggplot2::element_line(colour = "#6B7280", linewidth = 0.35),
      axis.text.x = ggplot2::element_text(colour = "#374151", size = ggplot2::rel(0.9)),
      axis.title.x = ggplot2::element_text(face = "bold", colour = "#1F2937",
                                           margin = ggplot2::margin(t = 8)),
      axis.text.y = markdown_text(hjust = 0, colour = "#1F2937"),
      axis.text.y.left = markdown_text(hjust = 0, colour = "#1F2937"),
      strip.text = if (stratified) ggplot2::element_text(face = "bold",
                                                         colour = "#1F2937")
      else ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", colour = "#111827",
                                         margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(colour = "#6B7280", hjust = 0,
                                           size = ggplot2::rel(0.85)),
      plot.margin = ggplot2::margin(12, 40, 12, 10)
    )
}
