#' Cumulative incidence plot for competing risks
#'
#' Estimate and plot the cumulative incidence of one event in the presence of
#' competing events, with optional group comparison using Gray's test.
#'
#' @param data A data frame containing follow-up time and event status.
#' @param time Follow-up time. Quoted and bare column names are accepted.
#' @param event Event-status variable with one censoring code and distinct codes
#'   for each event type. Quoted and bare column names are accepted.
#' @param event_of_interest Code or label for the event to plot. This must be an
#'   observed value of `event` and must differ from `censor_code`.
#' @param censor_code Code or label identifying censored observations. Defaults
#'   to `0`.
#' @param by Optional single grouping variable. When supplied, separate curves
#'   are drawn and Gray's test compares groups for the event of interest.
#'   Groups are ordered by factor level (as in \code{km_plot()}).
#' @param conf.int Logical; show pointwise 95 percent confidence intervals.
#' @param p_value Logical; display Gray's test p-value when `by` is supplied.
#' @param p_value_position Optional numeric vector of length two giving the x
#'   and y coordinates of the p-value annotation.
#' @param break_time_by Optional positive interval for x-axis breaks.
#' @param xlim,ylim Optional numeric vectors of length two, given as
#'   \code{c(lower, upper)}, defining plot limits. The plot is zoomed to these
#'   limits; no data are removed.
#' @param xlab,ylab Axis labels.
#' @param title,subtitle,caption Optional plot text.
#' @param legend_title Optional legend title. The display label of `by` is used
#'   by default.
#' @param legend_position One of `"bottom"`, `"top"`, `"right"`, `"left"`,
#'   or `"none"`.
#' @param palette Optional character vector of curve colors.
#' @param y_percent Logical; display cumulative incidence as percentages.
#' @param theme One of `"classic"`, `"minimal"`, `"bw"`, or `"light"`.
#' @param grid Logical; show major grid lines.
#' @param base_size Base font size.
#'
#' @return A `ggplot2` object with attributes `fit`, `plot_data`, `gray_test`,
#'   `event_of_interest`, and `censor_code`.
#'
#' @details
#' Cumulative incidence estimates the absolute probability of the selected
#' event over time while correctly retaining competing events in the risk
#' process. Treating competing events as ordinary censoring with a
#' Kaplan-Meier curve generally overestimates event probability.
#'
#' Gray's test compares cumulative-incidence functions between groups. It is a
#' group comparison and not an adjusted effect estimate; use
#' `competing_risk_reg()` when a subdistribution hazard ratio is required.
#'
#' @examples
#' cuminc_plot(
#'   data_competing_risk,
#'   time = follow_up_months,
#'   event = outcome,
#'   event_of_interest = Relapse,
#'   censor_code = Censored,
#'   by = treatment
#' )
#'
#' @export
cuminc_plot <- function(data,
                        time,
                        event,
                        event_of_interest,
                        censor_code = 0,
                        by = NULL,
                        conf.int = TRUE,
                        p_value = TRUE,
                        p_value_position = NULL,
                        break_time_by = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        xlab = "Time",
                        ylab = "Cumulative incidence",
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        legend_title = NULL,
                        legend_position = NULL,
                        palette = NULL,
                        y_percent = TRUE,
                        theme = "classic",
                        grid = FALSE,
                        base_size = 13) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    stop("Package 'cmprsk' is required for `cuminc_plot()`.", call. = FALSE)
  }

  time <- .cox_single_var_arg(substitute(time), data, parent.frame())
  event <- .cox_single_var_arg(substitute(event), data, parent.frame())
  by <- .survival_optional_single_var_arg(substitute(by), data, parent.frame())
  event_of_interest <- .competing_code_arg(
    substitute(event_of_interest), parent.frame()
  )
  censor_code <- .competing_code_arg(substitute(censor_code), parent.frame())
  theme <- .km_theme_arg(substitute(theme), parent.frame())
  legend_position <- .km_legend_position_arg(
    substitute(legend_position), parent.frame()
  )

  clean <- .validate_cuminc_inputs(
    data, time, event, event_of_interest, censor_code, by,
    conf.int, p_value, p_value_position, break_time_by, xlim, ylim,
    y_percent, theme, legend_position, grid, base_size
  )
  event_of_interest <- as.character(event_of_interest)
  censor_code <- as.character(censor_code)
  status <- as.character(clean[[event]])
  group <- if (is.null(by)) NULL else factor(clean[[by]])

  fit_args <- list(
    ftime = clean[[time]],
    fstatus = status,
    cencode = censor_code
  )
  if (!is.null(group)) fit_args$group <- group
  fit <- do.call(cmprsk::cuminc, fit_args)
  plot_data <- .tidy_cuminc(fit, event_of_interest, grouped = !is.null(by))
  if (!is.null(group)) {
    # Follow the factor level order (as km_plot() does), not the alphabetical
    # order of the cuminc() component names.
    group_levels <- intersect(levels(droplevels(group)), unique(plot_data$group))
    group_levels <- c(group_levels, setdiff(unique(plot_data$group), group_levels))
    plot_data$group <- factor(plot_data$group, levels = group_levels)
    plot_data <- plot_data[order(plot_data$group), , drop = FALSE]
    rownames(plot_data) <- NULL
  }
  gray_test <- .cuminc_gray_test(fit, event_of_interest, by)

  if (is.null(palette)) {
    palette <- .km_default_palette(length(unique(plot_data$group)))
  }
  if (is.null(legend_title) && !is.null(by)) {
    legend_title <- .label_var(by, .var_label_map(data, by))
  }
  if (is.null(legend_position)) {
    legend_position <- if (is.null(by)) "none" else "bottom"
  }

  p <- .build_cuminc_plot(
    plot_data, gray_test, conf.int, p_value, p_value_position,
    break_time_by, xlim, ylim, xlab, ylab, title, subtitle, caption,
    legend_title, legend_position, palette, y_percent, theme, grid,
    base_size
  )
  attr(p, "fit") <- fit
  attr(p, "plot_data") <- plot_data
  attr(p, "gray_test") <- gray_test
  attr(p, "event_of_interest") <- event_of_interest
  attr(p, "censor_code") <- censor_code
  p
}

#' @keywords internal
#' @noRd
.competing_code_arg <- function(expr, env) {
  if (missing(expr) || (is.symbol(expr) && !nzchar(as.character(expr)))) {
    stop("`event_of_interest` must be supplied.", call. = FALSE)
  }
  if (is.symbol(expr)) {
    value <- as.character(expr)
    if (exists(value, envir = env, inherits = TRUE)) {
      return(get(value, envir = env, inherits = TRUE))
    }
    return(value)
  }
  eval(expr, envir = env)
}

#' @keywords internal
#' @noRd
.validate_cuminc_inputs <- function(data, time, event, event_of_interest,
                                    censor_code, by, conf.int, p_value,
                                    p_value_position, break_time_by, xlim, ylim,
                                    y_percent, theme, legend_position, grid,
                                    base_size) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!time %in% names(data) || !is.numeric(data[[time]])) {
    stop("`time` must be a numeric variable in `data`.", call. = FALSE)
  }
  if (!event %in% names(data)) {
    stop("`event` must be a variable in `data`.", call. = FALSE)
  }
  if (any(data[[time]] < 0, na.rm = TRUE)) {
    stop("`time` must contain non-negative follow-up times.", call. = FALSE)
  }
  if (!is.null(by) && (!by %in% names(data) || identical(by, time) ||
                       identical(by, event))) {
    stop("`by` must be a separate grouping variable in `data`.", call. = FALSE)
  }
  if (length(event_of_interest) != 1L || is.na(event_of_interest) ||
      length(censor_code) != 1L || is.na(censor_code)) {
    stop("`event_of_interest` and `censor_code` must each be one non-missing value.", call. = FALSE)
  }
  if (identical(as.character(event_of_interest), as.character(censor_code))) {
    stop("`event_of_interest` must differ from `censor_code`.", call. = FALSE)
  }
  logical_args <- list(conf.int = conf.int, p_value = p_value,
                       y_percent = y_percent, grid = grid)
  valid_logical <- vapply(
    logical_args,
    function(x) is.logical(x) && length(x) == 1L && !is.na(x),
    logical(1)
  )
  if (!all(valid_logical)) {
    stop("Plot switches must each be TRUE or FALSE.", call. = FALSE)
  }
  .validate_cuminc_numeric_options(
    p_value_position, break_time_by, xlim, ylim, base_size
  )

  vars <- unique(c(time, event, by))
  clean <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  if (!nrow(clean)) stop("No complete cases are available.", call. = FALSE)
  observed <- unique(as.character(clean[[event]]))
  if (!as.character(censor_code) %in% observed) {
    stop("`censor_code` was not observed in `event`.", call. = FALSE)
  }
  if (!as.character(event_of_interest) %in% observed) {
    stop("`event_of_interest` was not observed in `event`.", call. = FALSE)
  }
  competing <- setdiff(observed, c(as.character(censor_code),
                                   as.character(event_of_interest)))
  if (!length(competing)) {
    stop("At least one competing-event code must be observed.", call. = FALSE)
  }
  if (!is.null(by) && length(unique(clean[[by]])) < 2L) {
    stop("`by` must contain at least two observed groups.", call. = FALSE)
  }
  clean
}

#' @keywords internal
#' @noRd
.validate_cuminc_numeric_options <- function(p_position, break_by, xlim, ylim,
                                             base_size) {
  for (x in list(xlim = xlim, ylim = ylim, p_value_position = p_position)) {
    if (!is.null(x) && (!is.numeric(x) || length(x) != 2L || any(!is.finite(x)))) {
      stop("Plot limits and positions must be finite numeric vectors of length two.", call. = FALSE)
    }
  }
  for (x in list(xlim, ylim)) {
    if (!is.null(x) && x[1] >= x[2]) {
      stop("`xlim` and `ylim` must each be given as c(lower, upper) with lower < upper.",
           call. = FALSE)
    }
  }
  if (!is.null(break_by) && (!is.numeric(break_by) || length(break_by) != 1L ||
                            !is.finite(break_by) || break_by <= 0)) {
    stop("`break_time_by` must be one positive number.", call. = FALSE)
  }
  if (!is.numeric(base_size) || length(base_size) != 1L ||
      !is.finite(base_size) || base_size <= 0) {
    stop("`base_size` must be one positive number.", call. = FALSE)
  }
}

#' @keywords internal
#' @noRd
.tidy_cuminc <- function(fit, event_code, grouped) {
  component_names <- setdiff(names(fit), "Tests")
  suffix <- paste0(" ", event_code)
  selected <- component_names[endsWith(component_names, suffix)]
  if (!length(selected)) {
    stop("No cumulative-incidence curve was returned for the event of interest.", call. = FALSE)
  }
  out <- lapply(selected, function(component_name) {
    curve <- fit[[component_name]]
    group <- if (grouped) {
      substr(component_name, 1L, nchar(component_name) - nchar(suffix))
    } else {
      "Overall"
    }
    se <- sqrt(pmax(curve$var, 0))
    data.frame(
      time = curve$time,
      estimate = curve$est,
      conf.low = pmax(0, curve$est - stats::qnorm(0.975) * se),
      conf.high = pmin(1, curve$est + stats::qnorm(0.975) * se),
      group = group,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' @keywords internal
#' @noRd
.cuminc_gray_test <- function(fit, event_code, by) {
  if (is.null(by) || is.null(fit$Tests) || !event_code %in% rownames(fit$Tests)) {
    return(NULL)
  }
  data.frame(
    event = event_code,
    statistic = unname(fit$Tests[event_code, "stat"]),
    df = unname(fit$Tests[event_code, "df"]),
    p.value = unname(fit$Tests[event_code, "pv"]),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_cuminc_plot <- function(data, gray_test, conf.int, p_value, p_position,
                               break_by, xlim, ylim, xlab, ylab, title,
                               subtitle, caption, legend_title, legend_position,
                               palette, y_percent, theme_name, grid, base_size) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$time,
      y = .data$estimate,
      color = .data$group,
      fill = .data$group
    )
  )
  if (isTRUE(conf.int)) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
      alpha = 0.14, colour = NA
    )
  }
  p <- p +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      x = xlab, y = ylab, title = title, subtitle = subtitle,
      caption = caption, color = legend_title, fill = legend_title
    )
  if (!is.null(break_by)) {
    upper <- if (is.null(xlim)) max(data$time, na.rm = TRUE) else xlim[2]
    p <- p + ggplot2::scale_x_continuous(breaks = seq(0, upper, by = break_by))
  }
  y_limits <- if (is.null(ylim)) c(0, 1) else if (isTRUE(y_percent) &&
      max(ylim) > 1) ylim / 100 else ylim
  # Zoom with coord_cartesian(): limits on the scale would drop the parts of
  # the curve, ribbon, and annotation that fall outside `ylim`.
  p <- p + ggplot2::scale_y_continuous(
    labels = if (isTRUE(y_percent)) scales::label_percent() else scales::label_number()
  ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = y_limits)
  p <- p + .km_plot_theme(theme_name, base_size, grid = grid) +
    ggplot2::theme(legend.position = legend_position)
  if (isTRUE(p_value) && !is.null(gray_test)) {
    position <- p_position %||% c(
      max(data$time) * 0.05,
      min(max(data$estimate) * 0.9, y_limits[1] + diff(y_limits) * 0.9)
    )
    p <- p + ggplot2::annotate(
      "text", x = position[1], y = position[2], hjust = 0,
      label = paste0(
        "Gray's test p = ",
        if (gray_test$p.value < 0.001) "<0.001" else {
          formatC(gray_test$p.value, format = "f", digits = 3)
        }
      )
    )
  }
  p
}
