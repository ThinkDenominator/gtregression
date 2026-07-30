#' Kaplan-Meier survival plot
#'
#' Plot observed survival over time, with optional confidence intervals,
#' censoring marks, a log-rank p-value, and a number-at-risk table.
#'
#' @param data A \code{data.frame} containing survival time and event status.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for separate Kaplan-Meier curves. Quoted
#'   and bare names are accepted.
#' @param conf.int Logical; if \code{TRUE}, show confidence limits.
#' @param risk_table Logical; if \code{TRUE}, add a number-at-risk table below
#'   the curve.
#' @param p_value Logical; if \code{TRUE}, show the log-rank p-value when
#'   \code{by} is supplied.
#' @param p_value_position Optional numeric vector of length 2 giving the
#'   \code{x} and \code{y} coordinates for the log-rank p-value inside the
#'   plotting panel. If \code{NULL}, a lower-left position is chosen
#'   automatically.
#' @param censor Logical; if \code{TRUE}, show censoring marks.
#' @param break_time_by Optional numeric interval for x-axis and risk-table time
#'   breaks. If \code{NULL}, breaks are chosen automatically.
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits.
#' @param xlab,ylab Axis labels.
#' @param title Optional plot title.
#' @param legend_title Optional legend title. If \code{NULL}, the labelled
#'   \code{by} variable name is used.
#' @param palette Optional character vector of colors for grouped curves.
#' @param base_size Base font size.
#'
#' @return A \code{ggplot2} object when \code{risk_table = FALSE}; otherwise a
#'   \code{patchwork} object combining the survival curve and risk table. The
#'   returned object has attributes \code{fit}, \code{plot_data},
#'   \code{risk_table}, and \code{logrank_p}.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' km_plot(
#'   data = lung_data,
#'   time = time,
#'   event = status
#' )
#'
#' km_plot(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt,
#'   break_time_by = 200
#' )
#'
#' @importFrom survival Surv survfit survdiff
#' @importFrom stats as.formula pchisq complete.cases
#' @importFrom ggplot2 ggplot aes geom_step geom_point geom_text labs theme_minimal theme element_blank element_text scale_y_continuous scale_x_continuous coord_cartesian scale_color_manual guides guide_legend
#' @importFrom patchwork wrap_plots
#' @export
km_plot <- function(data,
                    time,
                    event,
                    by = NULL,
                    conf.int = TRUE,
                    risk_table = TRUE,
                    p_value = TRUE,
                    p_value_position = NULL,
                    censor = TRUE,
                    break_time_by = NULL,
                    xlim = NULL,
                    xlab = "Time",
                    ylab = "Survival probability",
                    title = NULL,
                    legend_title = NULL,
                    palette = NULL,
                    base_size = 13) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  by <- .vars_arg(substitute(by), env = parent.frame(), allow_null = TRUE)

  .validate_km_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    conf.int = conf.int,
    risk_table = risk_table,
    p_value = p_value,
    p_value_position = p_value_position,
    censor = censor,
    break_time_by = break_time_by,
    xlim = xlim,
    base_size = base_size
  )

  vars_needed <- unique(c(time, event, by))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  data_clean[[event]] <- .cox_event01(data_clean[[event]])

  if (!is.null(by)) {
    data_clean[[by]] <- factor(data_clean[[by]])
  }

  fml <- .km_formula(time, event, by)
  fit <- survival::survfit(fml, data = data_clean)

  plot_data <- .km_tidy_survfit(fit)
  risk_breaks <- .km_time_breaks(data_clean[[time]], break_time_by, xlim)
  risk_data <- .km_risk_table(fit, risk_breaks)
  logrank_p <- .km_logrank_p(data_clean, time, event, by)

  strata_count <- length(unique(plot_data$strata))
  if (is.null(palette)) {
    palette <- .km_default_palette(strata_count)
  }

  by_label <- if (is.null(by)) NULL else .label_var(by, .var_label_map(data, by))
  if (is.null(legend_title)) {
    legend_title <- by_label
  }

  main_plot <- .km_main_plot(
    plot_data = plot_data,
    conf.int = conf.int,
    censor = censor,
    xlim = xlim,
    breaks = risk_breaks,
    xlab = xlab,
    ylab = ylab,
    title = title,
    legend_title = legend_title,
    palette = palette,
    base_size = base_size,
    logrank_p = if (isTRUE(p_value)) logrank_p else NA_real_,
    p_value_position = p_value_position
  )

  out <- main_plot
  if (isTRUE(risk_table)) {
    risk_plot <- .km_risk_plot(
      risk_data = risk_data,
      breaks = risk_breaks,
      xlim = xlim,
      xlab = xlab,
      palette = palette,
      base_size = base_size
    )
    out <- patchwork::wrap_plots(main_plot, risk_plot, ncol = 1, heights = c(3, 1))
  }

  attr(out, "fit") <- fit
  attr(out, "plot_data") <- plot_data
  attr(out, "risk_table") <- risk_data
  attr(out, "logrank_p") <- logrank_p
  out
}

#' @keywords internal
#' @noRd
.validate_km_inputs <- function(data, time, event, by, conf.int, risk_table,
                                p_value, p_value_position, censor,
                                break_time_by, xlim, base_size) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1L || !time %in% names(data)) {
    stop("`time` must be a single survival time variable in `data`.", call. = FALSE)
  }
  if (!is.character(event) || length(event) != 1L || !event %in% names(data)) {
    stop("`event` must be a single event indicator variable in `data`.", call. = FALSE)
  }
  if (!is.null(by) && (!is.character(by) || length(by) != 1L || !by %in% names(data))) {
    stop("`by` must be NULL or a single grouping variable in `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[time]])) {
    stop("`time` must be numeric.", call. = FALSE)
  }
  if (any(data[[time]] < 0, na.rm = TRUE)) {
    stop("`time` must contain non-negative follow-up times.", call. = FALSE)
  }
  for (arg in c("conf.int", "risk_table", "p_value", "censor")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.null(break_time_by) &&
      (!is.numeric(break_time_by) || length(break_time_by) != 1L ||
       is.na(break_time_by) || break_time_by <= 0)) {
    stop("`break_time_by` must be NULL or a positive number.", call. = FALSE)
  }
  if (!is.null(xlim) &&
      (!is.numeric(xlim) || length(xlim) != 2L || anyNA(xlim) || xlim[1] >= xlim[2])) {
    stop("`xlim` must be NULL or a numeric vector of length 2.", call. = FALSE)
  }
  if (!is.null(p_value_position) &&
      (!is.numeric(p_value_position) || length(p_value_position) != 2L ||
       anyNA(p_value_position) || any(!is.finite(p_value_position)) ||
       p_value_position[2] < 0 || p_value_position[2] > 1)) {
    stop("`p_value_position` must be NULL or a numeric vector `c(x, y)` with y between 0 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(base_size) || length(base_size) != 1L || is.na(base_size) || base_size <= 0) {
    stop("`base_size` must be a positive number.", call. = FALSE)
  }

  vars_needed <- unique(c(time, event, by))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  if (nrow(data_clean) == 0) {
    stop("No complete cases available for Kaplan-Meier estimation.", call. = FALSE)
  }
  event01 <- .cox_event01(data_clean[[event]])
  if (sum(event01 == 1) == 0) {
    stop("`event` must include at least one event.", call. = FALSE)
  }
  if (!is.null(by) && length(unique(data_clean[[by]])) < 2L) {
    stop("`by` must contain at least two non-missing groups.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.km_formula <- function(time, event, by = NULL) {
  bt <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  rhs <- if (is.null(by)) "1" else bt(by)
  stats::as.formula(paste0("survival::Surv(", bt(time), ", ", bt(event), ") ~ ", rhs))
}

#' @keywords internal
#' @noRd
.km_tidy_survfit <- function(fit) {
  sm <- summary(fit, censored = TRUE)
  strata <- if (is.null(sm$strata)) {
    rep("Overall", length(sm$time))
  } else {
    sub("^.*=", "", as.character(sm$strata))
  }

  out <- data.frame(
    time = sm$time,
    n.risk = sm$n.risk,
    n.event = sm$n.event,
    n.censor = sm$n.censor,
    survival = sm$surv,
    conf.low = sm$lower,
    conf.high = sm$upper,
    strata = strata,
    stringsAsFactors = FALSE
  )

  starts <- lapply(unique(out$strata), function(s) {
    data.frame(
      time = 0,
      n.risk = max(out$n.risk[out$strata == s], na.rm = TRUE),
      n.event = 0,
      n.censor = 0,
      survival = 1,
      conf.low = 1,
      conf.high = 1,
      strata = s,
      stringsAsFactors = FALSE
    )
  })
  out <- rbind(do.call(rbind, starts), out)
  out$strata <- factor(out$strata, levels = unique(out$strata))
  out[order(out$strata, out$time), , drop = FALSE]
}

#' @keywords internal
#' @noRd
.km_time_breaks <- function(time, break_time_by = NULL, xlim = NULL) {
  rng <- if (is.null(xlim)) c(0, max(time, na.rm = TRUE)) else xlim
  if (!is.null(break_time_by)) {
    return(seq(rng[1], rng[2], by = break_time_by))
  }
  pretty(rng, n = 5)
}

#' @keywords internal
#' @noRd
.km_risk_table <- function(fit, times) {
  sm <- summary(fit, times = times, extend = TRUE)
  strata <- if (is.null(sm$strata)) {
    rep("Overall", length(sm$time))
  } else {
    sub("^.*=", "", as.character(sm$strata))
  }
  out <- data.frame(
    time = sm$time,
    n.risk = sm$n.risk,
    strata = strata,
    stringsAsFactors = FALSE
  )
  out$strata <- factor(out$strata, levels = unique(out$strata))
  out
}

#' @keywords internal
#' @noRd
.km_logrank_p <- function(data, time, event, by = NULL) {
  if (is.null(by)) {
    return(NA_real_)
  }
  fit <- survival::survdiff(.km_formula(time, event, by), data = data)
  stats::pchisq(fit$chisq, df = length(fit$n) - 1L, lower.tail = FALSE)
}

#' @keywords internal
#' @noRd
.km_fmt_p <- function(p) {
  if (is.na(p)) {
    return(NULL)
  }
  paste0("Log-rank p = ", if (p < 0.001) "<0.001" else formatC(p, format = "f", digits = 3))
}

#' @keywords internal
#' @noRd
.km_p_value_position <- function(plot_data, xlim = NULL, position = NULL) {
  if (!is.null(position)) {
    return(position)
  }
  xrng <- if (is.null(xlim)) range(plot_data$time, na.rm = TRUE) else xlim
  c(xrng[1] + diff(xrng) * 0.05, 0.08)
}

#' @keywords internal
#' @noRd
.km_default_palette <- function(n) {
  cols <- c("#1F77B4", "#D55E00", "#009E73", "#CC79A7", "#0072B2", "#E69F00")
  rep(cols, length.out = n)
}

#' @keywords internal
#' @noRd
.km_main_plot <- function(plot_data, conf.int, censor, xlim, breaks, xlab, ylab,
                          title, legend_title, palette, base_size, logrank_p,
                          p_value_position) {
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$time, y = .data$survival, color = .data$strata)
  )

  if (isTRUE(conf.int)) {
    ci_data <- plot_data[
      is.finite(plot_data$conf.low) & is.finite(plot_data$conf.high),
      ,
      drop = FALSE
    ]
    p <- p +
      ggplot2::geom_step(data = ci_data, ggplot2::aes(y = .data$conf.low), linewidth = 0.4,
                         linetype = "dashed", alpha = 0.45) +
      ggplot2::geom_step(data = ci_data, ggplot2::aes(y = .data$conf.high), linewidth = 0.4,
                         linetype = "dashed", alpha = 0.45)
  }

  p <- p +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = legend_title
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = if (length(unique(plot_data$strata)) > 1L) "bottom" else "none",
      plot.title = ggplot2::element_text(face = "bold")
    )

  if (isTRUE(censor)) {
    cens <- plot_data[plot_data$n.censor > 0, , drop = FALSE]
    if (nrow(cens)) {
      p <- p + ggplot2::geom_point(data = cens, shape = 3, size = 2, stroke = 0.8)
    }
  }

  p_label <- .km_fmt_p(logrank_p)
  if (!is.null(p_label)) {
    p_pos <- .km_p_value_position(plot_data, xlim = xlim, position = p_value_position)
    p <- p +
      ggplot2::annotate(
        "text",
        x = p_pos[1],
        y = p_pos[2],
        label = p_label,
        hjust = 0,
        vjust = 0,
        size = base_size / 3.4,
        fontface = "plain",
        color = "black"
      )
  }

  if (!is.null(palette)) {
    levs <- levels(plot_data$strata)
    p <- p + ggplot2::scale_color_manual(values = stats::setNames(palette[seq_along(levs)], levs))
  }

  p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = c(0, 1))

  p
}

#' @keywords internal
#' @noRd
.km_risk_plot <- function(risk_data, breaks, xlim, xlab, palette, base_size) {
  p <- ggplot2::ggplot(
    risk_data,
    ggplot2::aes(x = .data$time, y = .data$strata, label = .data$n.risk,
                 color = .data$strata)
  ) +
    ggplot2::geom_text(size = base_size / 4) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::labs(x = xlab, y = "Number at risk") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(face = "bold")
    )

  if (!is.null(palette)) {
    levs <- levels(risk_data$strata)
    p <- p + ggplot2::scale_color_manual(values = stats::setNames(palette[seq_along(levs)], levs))
  }
  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }
  p
}
