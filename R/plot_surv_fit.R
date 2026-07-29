#' Plot observed and fitted parametric survival curves
#'
#' Compare the observed Kaplan-Meier survival curve with fitted parametric
#' survival curves from \code{survival::survreg()}.
#'
#' @param data A \code{data.frame} containing survival time, event status, and
#'   optional grouping or adjustment variables.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for observed and fitted curves. Quoted
#'   and bare names are accepted.
#' @param adjust_for Optional character vector of adjustment variables included
#'   in the fitted parametric model. Fitted curves are predicted at typical
#'   adjustment values.
#' @param distributions Parametric survival distributions to overlay. One or
#'   more of \code{"weibull"}, \code{"exponential"}, \code{"lognormal"}, or
#'   \code{"loglogistic"}. Quoted and bare values are accepted. Common
#'   spellings such as \code{"log-normal"} and \code{"log-logistic"} are also
#'   accepted.
#' @param break_time_by Optional numeric interval for x-axis breaks. If
#'   \code{NULL}, breaks are chosen automatically.
#' @param xlim Optional numeric vector of length 2 specifying x-axis limits.
#' @param xlab,ylab Axis labels.
#' @param title Optional plot title.
#' @param legend_title Optional legend title. If \code{NULL}, the labelled
#'   \code{by} variable name is used.
#' @param palette Optional character vector of colors for observed groups.
#' @param base_size Base font size.
#' @param n_points Number of points used to draw each fitted curve.
#'
#' @details
#' \code{plot_surv_fit()} is a visual diagnostic for parametric survival
#' modelling. It is useful after \code{surv_model_compare()} and before treating
#' a final \code{surv_reg()} model as the preferred model. It is not a Cox-model
#' diagnostic; use \code{check_ph()} for Cox proportional hazards assumptions.
#'
#' When \code{adjust_for} is supplied, fitted curves are predicted at typical
#' adjustment values: medians for numeric variables and the most common level for
#' categorical variables. Use this as a model-fit screen, not as a replacement
#' for clinical or subject-matter judgement.
#'
#' @return A \code{ggplot2} object with attributes \code{km_fit},
#'   \code{model_fits}, \code{observed_data}, \code{fitted_data}, and
#'   \code{prediction_data}.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' plot_surv_fit(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt,
#'   distributions = c(weibull, lognormal),
#'   break_time_by = 200
#' )
#'
#' plot_surv_fit(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   by = "trt",
#'   adjust_for = c(age, karno),
#'   distributions = "log-logistic"
#' )
#'
#' @importFrom survival Surv survfit survreg
#' @importFrom stats complete.cases median predict
#' @importFrom ggplot2 ggplot aes geom_step geom_line labs theme_minimal theme element_text scale_y_continuous scale_x_continuous coord_cartesian scale_color_manual scale_linetype_manual
#' @export
plot_surv_fit <- function(data,
                          time,
                          event,
                          by = NULL,
                          adjust_for = NULL,
                          distributions = c("weibull", "exponential", "lognormal", "loglogistic"),
                          break_time_by = NULL,
                          xlim = NULL,
                          xlab = "Time",
                          ylab = "Survival probability",
                          title = NULL,
                          legend_title = NULL,
                          palette = NULL,
                          base_size = 13,
                          n_points = 200) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  by <- .vars_arg(substitute(by), env = parent.frame(), allow_null = TRUE)
  adjust_for <- .vars_arg(substitute(adjust_for), env = parent.frame(), allow_null = TRUE)
  distributions <- .surv_distribution_arg(
    substitute(distributions),
    env = parent.frame(),
    multiple = TRUE,
    arg = "distributions"
  )

  data_clean <- .validate_plot_surv_fit_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    adjust_for = adjust_for,
    distributions = distributions,
    break_time_by = break_time_by,
    xlim = xlim,
    base_size = base_size,
    n_points = n_points
  )

  if (!is.null(by)) {
    data_clean[[by]] <- factor(data_clean[[by]])
  }

  km_fit <- survival::survfit(.km_formula(time, event, by), data = data_clean)
  observed <- .km_tidy_survfit(km_fit)
  observed$Curve <- "Observed KM"
  observed$Distribution <- "Kaplan-Meier"

  predictors <- unique(c(by, adjust_for))
  fitted_models <- vector("list", length(distributions))
  names(fitted_models) <- distributions

  for (dist in distributions) {
    fitted_models[[dist]] <- .fit_surv_model(data_clean, time, event, predictors, dist)
    if (is.null(fitted_models[[dist]])) {
      stop("Parametric survival model fitting failed for distribution '", dist, "'.", call. = FALSE)
    }
  }

  prediction_data <- .plot_surv_fit_newdata(data_clean, by = by, adjust_for = adjust_for)
  fitted <- .plot_surv_fit_fitted_curves(
    models = fitted_models,
    newdata = prediction_data,
    by = by,
    n_points = n_points
  )

  breaks <- .km_time_breaks(data_clean[[time]], break_time_by, xlim)
  strata_count <- length(unique(observed$strata))
  if (is.null(palette)) {
    palette <- .km_default_palette(strata_count)
  }
  strata_levels <- unique(as.character(observed$strata))

  if (is.null(legend_title)) {
    legend_title <- if (is.null(by)) "Curve" else .label_var(by, .var_label_map(data, by))
  }
  if (is.null(title)) {
    title <- "Observed and fitted survival curves"
  }

  plot_data <- rbind(
    observed[, c("time", "survival", "strata", "Curve", "Distribution"), drop = FALSE],
    fitted[, c("time", "survival", "strata", "Curve", "Distribution"), drop = FALSE]
  )
  plot_data$strata <- factor(plot_data$strata, levels = strata_levels)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$time,
      y = .data$survival,
      color = .data$strata,
      linetype = .data$Curve
    )
  ) +
    ggplot2::geom_step(
      data = plot_data[plot_data$Curve == "Observed KM", , drop = FALSE],
      linewidth = 0.9
    ) +
    ggplot2::geom_line(
      data = plot_data[plot_data$Curve != "Observed KM", , drop = FALSE],
      linewidth = 0.8
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_linetype_manual(
      values = .plot_surv_fit_linetypes(unique(plot_data$Curve))
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = legend_title,
      linetype = "Curve"
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = c(0, 1))

  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_manual(
      values = stats::setNames(palette[seq_along(strata_levels)], strata_levels)
    )
  }

  attr(p, "km_fit") <- km_fit
  attr(p, "model_fits") <- fitted_models
  attr(p, "observed_data") <- observed
  attr(p, "fitted_data") <- fitted
  attr(p, "prediction_data") <- prediction_data
  p
}

#' @keywords internal
#' @noRd
.validate_plot_surv_fit_inputs <- function(data,
                                           time,
                                           event,
                                           by,
                                           adjust_for,
                                           distributions,
                                           break_time_by,
                                           xlim,
                                           base_size,
                                           n_points) {
  choices <- c("weibull", "exponential", "lognormal", "loglogistic")
  if (!is.character(distributions) || length(distributions) < 1L ||
      anyNA(distributions) || any(!distributions %in% choices)) {
    stop(
      "`distributions` must contain one or more of: ",
      paste(choices, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(n_points) || length(n_points) != 1L || is.na(n_points) ||
      n_points < 25 || n_points != floor(n_points)) {
    stop("`n_points` must be a whole number of at least 25.", call. = FALSE)
  }

  .validate_km_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    conf.int = FALSE,
    risk_table = FALSE,
    p_value = FALSE,
    censor = FALSE,
    break_time_by = break_time_by,
    xlim = xlim,
    base_size = base_size
  )

  if (!is.null(adjust_for)) {
    missing_adjust <- setdiff(adjust_for, names(data))
    if (length(missing_adjust)) {
      stop(
        "The following adjustment variables were not found in `data`: ",
        paste(missing_adjust, collapse = ", "),
        call. = FALSE
      )
    }
  }

  vars_needed <- unique(c(time, event, by, adjust_for))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  data_clean[[event]] <- .cox_event01(data_clean[[event]])
  data_clean
}

#' @keywords internal
#' @noRd
.plot_surv_fit_newdata <- function(data, by = NULL, adjust_for = NULL) {
  typical <- function(x) {
    if (is.numeric(x)) {
      return(stats::median(x, na.rm = TRUE))
    }
    if (is.factor(x)) {
      tab <- table(x)
      val <- names(tab)[which.max(tab)]
      return(factor(val, levels = levels(x), ordered = is.ordered(x)))
    }
    if (is.logical(x)) {
      tab <- table(x)
      return(as.logical(names(tab)[which.max(tab)]))
    }
    tab <- table(x)
    names(tab)[which.max(tab)]
  }

  if (is.null(by)) {
    out <- data.frame(.curve_id = "Overall", stringsAsFactors = FALSE)
  } else {
    levs <- levels(factor(data[[by]]))
    out <- data.frame(.curve_id = levs, stringsAsFactors = FALSE)
    out[[by]] <- factor(levs, levels = levels(factor(data[[by]])))
  }

  if (!is.null(adjust_for)) {
    for (var in adjust_for) {
      out[[var]] <- typical(data[[var]])
    }
  }

  out
}

#' @keywords internal
#' @noRd
.plot_surv_fit_fitted_curves <- function(models, newdata, by = NULL, n_points = 200) {
  p_grid <- seq(0.001, 0.999, length.out = n_points)

  pieces <- list()
  idx <- 1L
  for (dist in names(models)) {
    fit <- models[[dist]]
    pred <- stats::predict(fit, newdata = newdata, type = "quantile", p = p_grid)
    pred <- as.matrix(pred)
    if (nrow(pred) != nrow(newdata) && ncol(pred) == nrow(newdata)) {
      pred <- t(pred)
    }

    for (i in seq_len(nrow(newdata))) {
      times <- as.numeric(pred[i, ])
      ok <- is.finite(times) & times >= 0
      curve <- data.frame(
        time = c(0, times[ok]),
        survival = c(1, 1 - p_grid[ok]),
        strata = newdata$.curve_id[i],
        Curve = paste0(.plot_surv_fit_distribution_label(dist), " fit"),
        Distribution = dist,
        stringsAsFactors = FALSE
      )
      curve <- curve[order(curve$time, decreasing = FALSE), , drop = FALSE]
      pieces[[idx]] <- curve
      idx <- idx + 1L
    }
  }

  do.call(rbind, pieces)
}

#' @keywords internal
#' @noRd
.plot_surv_fit_distribution_label <- function(distribution) {
  stats::setNames(
    c("Weibull", "Exponential", "Lognormal", "Log-logistic"),
    c("weibull", "exponential", "lognormal", "loglogistic")
  )[[distribution]]
}

#' @keywords internal
#' @noRd
.plot_surv_fit_linetypes <- function(curves) {
  vals <- c("solid", "dashed", "dotdash", "twodash", "longdash")
  stats::setNames(rep(vals, length.out = length(curves)), curves)
}
