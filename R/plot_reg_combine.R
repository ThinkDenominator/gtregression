#' Plot crude and adjusted estimates side by side
#'
#' Creates two aligned forest plots from compatible \code{gtregression}
#' regression objects such as crude and adjusted outputs from \code{uni_reg()},
#' \code{multi_reg()}, survival regression, or other supported model families.
#'
#' @param tbl_uni The first \code{gtregression} object: crude results (for
#'   example \code{uni_reg()}, or \code{cox_reg()} without adjustment), or
#'   exposure-specific adjusted results (\code{adjust_for} without
#'   \code{multivariable = TRUE}).
#' @param tbl_multi The second \code{gtregression} object from the same model
#'   family: the adjusted or single multivariable model. When both objects are
#'   adjusted, the second panel is labelled "Mutually Adjusted".
#' @param title_uni,title_multi Optional panel titles.
#' @param caption Optional combined plot caption. If \code{NULL}, notes are
#'   added automatically for adjusted \code{multi_reg()} objects and displayed
#'   reference categories.
#' @param ref_line Optional numeric reference line. If \code{NULL}, uses 0 for
#'   linear models and 1 otherwise.
#' @param order_y Optional character vector to customize exposure ordering.
#' @param log_x Logical. If \code{TRUE}, use a log-scaled x-axis for ratio
#'   measures. The default \code{FALSE} uses a linear x-axis.
#' @param point_color,errorbar_color Base colors for non-significant rows.
#' @param point_size Diameter of estimate points in millimetres.
#' @param point_stroke Outline width of estimate points.
#' @param ci_linewidth Line width of confidence intervals.
#' @param base_size Base font size for the plot theme.
#' @param show_ref Logical or \code{NULL}. The default \code{NULL} inherits the
#'   setting used to create the supplied tables. If \code{TRUE}, include
#'   reference levels as \code{(Ref.)}. If \code{FALSE}, binary exposures are
#'   shown as compact rows for the estimated non-reference category;
#'   affirmative levels such as \code{Yes} or \code{1} are displayed using the
#'   variable name.
#' @param sig_color,sig_errorbar_color Optional colors for significant rows.
#'   If \code{NULL}, base colors are reused.
#' @param xlim_uni,breaks_uni Optional x-axis limits and breaks for the
#'   univariate panel.
#' @param xlim_multi,breaks_multi Optional x-axis limits and breaks for the
#'   multivariable panel.
#' @param alpha Significance level for linear models when \code{p.value} is
#'   available.
#' @param show_adjustment_note Logical; if \code{TRUE}, add a default caption
#'   describing \code{adjust_for} variables from \code{tbl_multi} when available.
#'
#' @return A \code{patchwork} object with two \code{ggplot2} panels.
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @export
plot_reg_combine <- function(tbl_uni,
                             tbl_multi,
                             title_uni = NULL,
                             title_multi = NULL,
                             caption = NULL,
                             ref_line = NULL,
                             order_y = NULL,
                             log_x = FALSE,
                             point_color = "#6B7280",
                             errorbar_color = "#6B7280",
                             point_size = 2.8,
                             point_stroke = 0.55,
                             ci_linewidth = 0.55,
                             base_size = 12,
                             show_ref = NULL,
                             sig_color = "#0072B2",
                             sig_errorbar_color = "#0072B2",
                             xlim_uni = NULL,
                             breaks_uni = NULL,
                             xlim_multi = NULL,
                             breaks_multi = NULL,
                             alpha = 0.05,
                             show_adjustment_note = TRUE) {

  .rd_reject(list(tbl_uni, tbl_multi), "plot_reg_combine")
  if (!inherits(tbl_uni, "gtregression")) {
    stop("`tbl_uni` must be a gtregression object.", call. = FALSE)
  }
  if (!inherits(tbl_multi, "gtregression")) {
    stop("`tbl_multi` must be a gtregression object.", call. = FALSE)
  }
  if (.plot_is_stratified(tbl_uni) || .plot_is_stratified(tbl_multi)) {
    stop("plot_reg_combine() does not support stratified objects.", call. = FALSE)
  }

  show_ref <- .plot_resolve_show_ref(show_ref, tbl_uni, tbl_multi)

  valid_standard_pair <- identical(tbl_uni$source, "uni_reg") &&
    identical(tbl_multi$source, "multi_reg")
  paired_sources <- c(
    "cox_reg", "surv_reg", "conditional_logit_reg",
    "competing_risk_reg", "ordinal_reg", "multinomial_outcome"
  )
  valid_model_pair <- tbl_uni$source %in% paired_sources &&
    identical(tbl_uni$source, tbl_multi$source) &&
    !.is_adjusted_reg_output(tbl_uni) &&
    .is_adjusted_reg_output(tbl_multi)
  # Exposure-specific adjusted models next to the single multivariable model
  # of the same family (for example adjust_for = followed by
  # multivariable = TRUE).
  adjusted_pair <- tbl_uni$source %in% c("multi_reg", paired_sources) &&
    identical(tbl_uni$source, tbl_multi$source) &&
    .is_adjusted_reg_output(tbl_uni) &&
    !.forest_is_single_model(tbl_uni) &&
    .forest_is_single_model(tbl_multi)
  if (adjusted_pair) valid_model_pair <- TRUE

  if (!valid_standard_pair && !valid_model_pair) {
    if (identical(tbl_uni$source, "multi_reg")) {
      stop(
        "`tbl_uni` must come from uni_reg() and `tbl_multi` from multi_reg().",
        call. = FALSE
      )
    }
    if (identical(tbl_multi$source, "uni_reg")) {
      stop("`tbl_multi` must come from multi_reg().", call. = FALSE)
    }
    if (!identical(tbl_uni$source, "uni_reg") &&
        !tbl_uni$source %in% paired_sources) {
      stop(
        "`tbl_uni` must be a supported crude regression object.",
        call. = FALSE
      )
    }
    if (!identical(tbl_multi$source, "multi_reg") &&
        !tbl_multi$source %in% paired_sources) {
      stop(
        "`tbl_multi` must be a supported adjusted regression object.",
        call. = FALSE
      )
    }
    if (tbl_uni$source %in% paired_sources ||
        tbl_multi$source %in% paired_sources) {
      stop(
        paste0(
          "Use crude and adjusted outputs from the same model family, with ",
          "the crude object first and its adjusted counterpart second."
        ),
        call. = FALSE
      )
    }
    stop("`tbl_uni` must come from uni_reg() and `tbl_multi` must come from multi_reg().", call. = FALSE)
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
    approach <- .normalize_approach(approach)
    base <- dplyr::case_when(
      approach %in% c("logit", "firth") ~ "Odds Ratio",
      approach == "logbinomial" ~ "Risk Ratio",
      approach %in% c("poisson", "negbin") ~ "Incidence Rate Ratio",
      approach == "robpoisson" ~ "Risk Ratio",
      approach == "cox" ~ "Hazard Ratio",
      approach == "fine_gray" ~ "Subdistribution Hazard Ratio",
      approach == "survreg" ~ "Time Ratio",
      approach == "conditional_logit" ~ "Matched Odds Ratio",
      approach == "ordinal_logit" ~ "Common Odds Ratio",
      approach == "multinomial_logit" ~ "Odds Ratio",
      approach == "linear" ~ "Beta Coefficient",
      TRUE ~ "Effect Size"
    )
    if (adjusted) paste("Adjusted", base) else base
  }

  approach_uni <- tbl_uni$approach
  approach_multi <- tbl_multi$approach

  xlab_uni <- get_axis_label(approach_uni, adjusted = adjusted_pair)
  xlab_multi <- get_axis_label(approach_multi, adjusted = TRUE)
  if (adjusted_pair) xlab_multi <- sub("^Adjusted ", "Mutually Adjusted ", xlab_multi)

  log_x_uni <- .plot_resolve_log_x(log_x, .normalize_approach(approach_uni))
  log_x_multi <- .plot_resolve_log_x(log_x, .normalize_approach(approach_multi))

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

  df_uni <- .plot_rows_from_table(tbl_uni, show_ref = show_ref, order_y = order_y)
  df_multi <- .plot_rows_from_table(tbl_multi, show_ref = show_ref, order_y = order_y)

  # One axis for both panels: rows are keyed on (exposure, level) from
  # `table_body`. A level, interaction row or variable present only in the
  # adjusted table is placed inside its own variable block, not at the bottom.
  header_key <- function(ex) paste0(ex, "::__header__")
  variables <- unique(c(df_uni$exposure, df_multi$exposure))
  row_keys <- unlist(lapply(variables, function(ex) {
    keys <- df_uni$row_key[df_uni$exposure == ex]
    extra <- df_multi$row_key[df_multi$exposure == ex]
    for (i in seq_along(extra)) {
      if (extra[i] %in% keys) next
      anchor <- if (i > 1L) match(extra[i - 1L], keys) else NA_integer_
      if (is.na(anchor)) {
        anchor <- if (identical(extra[i], header_key(ex))) {
          0L
        } else {
          max(0L, match(header_key(ex), keys), na.rm = TRUE)
        }
      }
      keys <- append(keys, extra[i], after = anchor)
    }
    hdr <- keys[keys == header_key(ex)]
    c(hdr, setdiff(keys, hdr))
  }), use.names = FALSE)

  skeleton <- data.frame(
    row_key = row_keys,
    stringsAsFactors = FALSE
  )

  # Labels and row formatting come from the crude table when it has the row,
  # otherwise from the adjusted table.
  from_uni <- match(row_keys, df_uni$row_key)
  from_multi <- match(row_keys, df_multi$row_key)
  pick <- function(column) {
    ifelse(is.na(from_uni), df_multi[[column]][from_multi], df_uni[[column]][from_uni])
  }
  skeleton$exposure <- pick("exposure")
  skeleton$level <- pick("level")
  skeleton$label <- pick("label")
  skeleton$label_clean <- pick("label_clean")
  skeleton$axis_header <- pick("is_header")

  align_to_skeleton <- function(df) {
    idx <- match(skeleton$row_key, df$row_key)
    out <- df[idx, , drop = FALSE]
    out$row_key <- skeleton$row_key
    out$exposure <- skeleton$exposure
    out$level <- skeleton$level
    out$label <- skeleton$label
    rownames(out) <- NULL
    out
  }
  df_uni <- align_to_skeleton(df_uni)
  df_multi <- align_to_skeleton(df_multi)

  df_uni$is_header <- skeleton$axis_header %in% TRUE
  df_multi$is_header <- skeleton$axis_header %in% TRUE
  df_uni$is_data[is.na(df_uni$is_data)] <- FALSE
  df_multi$is_data[is.na(df_multi$is_data)] <- FALSE
  df_uni$ref[is.na(df_uni$ref)] <- FALSE
  df_multi$ref[is.na(df_multi$ref)] <- FALSE

  # label markup (plain text when ggtext is not installed)
  label_map_raw <- .plot_axis_labels(skeleton$label_clean)
  df_uni$label_clean <- label_map_raw
  df_multi$label_clean <- label_map_raw

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
      .add_h_ci(df[df$is_data, , drop = FALSE], linewidth = ci_linewidth) +
      ggplot2::geom_point(
        data = df[df$is_data, , drop = FALSE],
        ggplot2::aes(fill = .data$is_sig),
        shape = 21,
        size = point_size,
        stroke = point_stroke,
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
      .plot_reg_theme(base_size = base_size) +
      ggplot2::theme(
        axis.text.y = if (show_y && .plot_has_ggtext()) {
          ggtext::element_markdown(hjust = 0, colour = "#1F2937")
        }
        else if (show_y) ggplot2::element_text(hjust = 0, colour = "#1F2937")
        else ggplot2::element_blank(),
        axis.text.y.left = if (show_y && .plot_has_ggtext()) {
          ggtext::element_markdown(hjust = 0, colour = "#1F2937")
        }
        else if (show_y) ggplot2::element_text(hjust = 0, colour = "#1F2937")
        else ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )

    if (use_log) {
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

  out <- patchwork::wrap_plots(p1, p2, ncol = 2, widths = c(1.2, 1))

  caption <- .plot_caption(
    tbl = tbl_multi,
    caption = caption,
    show_adjustment_note = show_adjustment_note,
    show_ref = show_ref,
    has_ref = any(df_uni$ref %in% TRUE) || any(df_multi$ref %in% TRUE)
  )
  if (!is.null(caption)) {
    out <- out + patchwork::plot_annotation(caption = caption)
  }

  out
}
