#' Plot model-fit diagnostics
#'
#' Visualise model fit for fitted regression models and models stored inside
#' \code{uni_reg()} or \code{multi_reg()} results.
#'
#' @param model A fitted \code{lm} or \code{glm} model, or a
#'   \code{uni_reg()} / \code{multi_reg()} result.
#' @param model_name Optional model name to select when \code{model} contains
#'   multiple fitted models. Quoted and bare names are accepted.
#' @param type Plot type. One of \code{"auto"}, \code{"all"},
#'   \code{"residual"}, \code{"qq"}, \code{"scale_location"},
#'   \code{"cooks"}, \code{"observed_predicted"}, or \code{"calibration"}.
#'   Quoted and bare values are accepted.
#' @param bins Number of groups used for binomial calibration plots.
#' @param base_size Base font size for the plot theme.
#'
#' @return A \code{ggplot2} object for a single plot, or a patchwork object when
#'   multiple diagnostics are requested.
#'
#' @details
#' \code{plot_model_fit()} is a visual check of how a fitted model behaves, not
#' a formal model-selection rule. For survival models, use \code{check_ph()} for Cox
#' proportional hazards diagnostics and \code{plot_surv_fit()} for parametric
#' survival model fit.
#'
#' For binomial models, \code{type = "calibration"} compares grouped predicted
#' probabilities with observed event proportions. This is most informative for
#' multivariable models, where predictions vary across many patients. A
#' univariable binary predictor may produce only two calibration points; that is
#' expected and simply reflects the two fitted probabilities in the model.
#' Logistic residual plots often show two bands because the outcome is coded as
#' event/non-event.
#'
#' @examples
#' fit_lm <- lm(mpg ~ wt + hp, data = mtcars)
#' plot_model_fit(fit_lm)
#'
#' fit_glm <- glm(am ~ mpg + wt, data = mtcars, family = binomial())
#' plot_model_fit(fit_glm, type = calibration, bins = 4)
#'
#' uni_fit <- uni_reg(mtcars, am, c(mpg, wt), approach = logit)
#' plot_model_fit(uni_fit, model_name = mpg, type = residual)
#'
#' @export
plot_model_fit <- function(model,
                           model_name = NULL,
                           type = c(
                             "auto",
                             "all",
                             "residual",
                             "qq",
                             "scale_location",
                             "cooks",
                             "observed_predicted",
                             "calibration"
                           ),
                           bins = 10,
                           base_size = 13) {
  choices <- c(
    "auto",
    "all",
    "residual",
    "qq",
    "scale_location",
    "cooks",
    "observed_predicted",
    "calibration"
  )
  type <- .choice_arg(substitute(type), env = parent.frame(), choices = choices)
  type <- match.arg(type, choices)
  model_name <- .plot_model_fit_model_name_arg(
    substitute(model_name),
    env = parent.frame()
  )

  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) || bins < 2) {
    stop("`bins` must be a single number greater than or equal to 2.", call. = FALSE)
  }
  bins <- as.integer(bins)

  if (!is.numeric(base_size) || length(base_size) != 1L || is.na(base_size) ||
      base_size <= 0) {
    stop("`base_size` must be a single positive number.", call. = FALSE)
  }

  selected <- .plot_model_fit_extract_model(model, model_name)
  fit <- selected$fit

  if (.plot_model_fit_is_survival_result(model) ||
      inherits(fit, c("coxph", "survreg"))) {
    stop(
      "Survival model fit is handled by `check_ph()` and `plot_surv_fit()`.",
      call. = FALSE
    )
  }

  if (!.plot_model_fit_supported(fit)) {
    stop(
      "`plot_model_fit()` currently supports `lm` and `glm` compatible models.",
      call. = FALSE
    )
  }

  data <- .plot_model_fit_data(fit)
  model_label <- selected$name

  plot_type <- type
  if (identical(plot_type, "auto")) {
    plot_type <- "all"
  }

  if (identical(plot_type, "all")) {
    plot_types <- .plot_model_fit_default_types(fit)
    plots <- lapply(
      plot_types,
      .plot_model_fit_single,
      data = data,
      fit = fit,
      model_label = model_label,
      bins = bins,
      base_size = base_size
    )
    out <- patchwork::wrap_plots(plots, ncol = 2)
  } else {
    out <- .plot_model_fit_single(
      plot_type,
      data = data,
      fit = fit,
      model_label = model_label,
      bins = bins,
      base_size = base_size
    )
  }

  attr(out, "model_name") <- model_label
  attr(out, "source") <- "plot_model_fit"
  out
}

.plot_model_fit_model_name_arg <- function(expr, env = parent.frame()) {
  if (identical(expr, quote(NULL))) {
    return(NULL)
  }

  if (is.symbol(expr)) {
    name <- as.character(expr)
    if (exists(name, envir = env, inherits = TRUE)) {
      value <- get(name, envir = env, inherits = TRUE)
      if (is.character(value) && length(value) == 1L) {
        return(value)
      }
    }
    return(name)
  }

  value <- eval(expr, envir = env)
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("`model_name` must be a single model name.", call. = FALSE)
  }
  value
}

.plot_model_fit_extract_model <- function(model, model_name = NULL) {
  if (inherits(model, c("glm", "lm"))) {
    return(list(
      fit = model,
      name = .plot_model_fit_formula_label(model)
    ))
  }

  if (!inherits(model, "gtregression")) {
    stop(
      "`model` must be a fitted model or a gtregression result containing fitted models.",
      call. = FALSE
    )
  }

  models <- model[["models"]]
  if (!is.list(models) || !length(models)) {
    stop("No fitted models were found in `model$models`.", call. = FALSE)
  }

  model_names <- names(models)
  if (is.null(model_names)) {
    model_names <- paste0("model_", seq_along(models))
  }

  if (is.null(model_name)) {
    selected <- model_names[[1L]]
    if (length(models) > 1L) {
      message(
        "Multiple fitted models found; plotting `", selected,
        "`. Use `model_name = ...` to choose another model."
      )
    }
  } else {
    selected <- model_name
  }

  idx <- match(selected, model_names)
  if (is.na(idx)) {
    stop(
      "`model_name` must match one of: ",
      paste(model_names, collapse = ", "),
      call. = FALSE
    )
  }

  list(fit = models[[idx]], name = selected)
}

.plot_model_fit_formula_label <- function(fit) {
  f <- stats::formula(fit)
  paste(deparse(f), collapse = " ")
}

.plot_model_fit_is_survival_result <- function(model) {
  inherits(model, c("cox_reg", "surv_reg", "surv_model_compare"))
}

.plot_model_fit_supported <- function(fit) {
  inherits(fit, c("glm", "lm"))
}

.plot_model_fit_default_types <- function(fit) {
  if (inherits(fit, "glm") && .plot_model_fit_is_binomial(fit)) {
    return(c("calibration", "residual", "cooks"))
  }
  if (inherits(fit, "glm")) {
    return(c("observed_predicted", "residual", "cooks"))
  }
  c("residual", "qq", "scale_location", "cooks")
}

.plot_model_fit_data <- function(fit) {
  mf <- stats::model.frame(fit)
  observed <- stats::model.response(mf)
  if (is.factor(observed)) {
    observed <- as.numeric(observed) - 1
  }
  if (is.logical(observed)) {
    observed <- as.numeric(observed)
  }

  residual_type <- if (inherits(fit, "glm")) "deviance" else "response"
  std_resid <- tryCatch(stats::rstandard(fit), error = function(e) NA_real_)

  data.frame(
    observed = as.numeric(observed),
    fitted = as.numeric(stats::fitted(fit)),
    residual = as.numeric(stats::residuals(fit, type = residual_type)),
    std_resid = as.numeric(std_resid),
    cooks = as.numeric(stats::cooks.distance(fit)),
    index = seq_along(stats::fitted(fit))
  )
}

.plot_model_fit_is_binomial <- function(fit) {
  inherits(fit, "glm") &&
    !is.null(fit$family$family) &&
    identical(fit$family$family, "binomial")
}

.plot_model_fit_single <- function(type,
                                   data,
                                   fit,
                                   model_label,
                                   bins,
                                   base_size) {
  switch(
    type,
    residual = .plot_model_fit_residual(data, model_label, base_size),
    qq = .plot_model_fit_qq(data, model_label, base_size),
    scale_location = .plot_model_fit_scale_location(data, model_label, base_size),
    cooks = .plot_model_fit_cooks(data, model_label, base_size),
    observed_predicted = .plot_model_fit_observed_predicted(data, model_label, base_size),
    calibration = .plot_model_fit_calibration(data, fit, model_label, bins, base_size),
    stop("Unsupported plot type.", call. = FALSE)
  )
}

.plot_model_fit_theme <- function(base_size) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

.plot_model_fit_residual <- function(data, model_label, base_size) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data$fitted, y = .data$residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_point(alpha = 0.75, colour = "#1F77B4") +
    ggplot2::labs(
      title = "Residuals vs fitted",
      subtitle = model_label,
      x = "Fitted values",
      y = "Residuals"
    ) +
    .plot_model_fit_theme(base_size)
}

.plot_model_fit_qq <- function(data, model_label, base_size) {
  ggplot2::ggplot(data, ggplot2::aes(sample = .data$residual)) +
    ggplot2::stat_qq(alpha = 0.75, colour = "#1F77B4") +
    ggplot2::stat_qq_line(colour = "grey35") +
    ggplot2::labs(
      title = "Normal Q-Q plot",
      subtitle = model_label,
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    .plot_model_fit_theme(base_size)
}

.plot_model_fit_scale_location <- function(data, model_label, base_size) {
  data$sqrt_abs_std_resid <- sqrt(abs(data$std_resid))
  ggplot2::ggplot(data, ggplot2::aes(x = .data$fitted, y = .data$sqrt_abs_std_resid)) +
    ggplot2::geom_point(alpha = 0.75, colour = "#1F77B4") +
    ggplot2::labs(
      title = "Scale-location",
      subtitle = model_label,
      x = "Fitted values",
      y = "sqrt(|standardised residuals|)"
    ) +
    .plot_model_fit_theme(base_size)
}

.plot_model_fit_cooks <- function(data, model_label, base_size) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data$index, y = .data$cooks)) +
    ggplot2::geom_col(fill = "#4C78A8", alpha = 0.85) +
    ggplot2::labs(
      title = "Cook's distance",
      subtitle = model_label,
      x = "Observation",
      y = "Cook's distance"
    ) +
    .plot_model_fit_theme(base_size)
}

.plot_model_fit_observed_predicted <- function(data, model_label, base_size) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data$fitted, y = .data$observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_point(alpha = 0.75, colour = "#1F77B4") +
    ggplot2::labs(
      title = "Observed vs predicted",
      subtitle = model_label,
      x = "Predicted values",
      y = "Observed values"
    ) +
    .plot_model_fit_theme(base_size)
}

.plot_model_fit_calibration <- function(data, fit, model_label, bins, base_size) {
  if (!.plot_model_fit_is_binomial(fit)) {
    stop("`type = \"calibration\"` is available only for binomial `glm` models.", call. = FALSE)
  }

  unique_fitted <- sort(unique(data$fitted[is.finite(data$fitted)]))
  if (length(unique_fitted) < 2L) {
    stop("Predicted probabilities do not vary enough for a calibration plot.", call. = FALSE)
  }

  if (length(unique_fitted) <= bins) {
    data$bin <- factor(
      match(data$fitted, unique_fitted),
      levels = seq_along(unique_fitted)
    )
  } else {
    probs <- stats::quantile(
      data$fitted,
      probs = seq(0, 1, length.out = bins + 1L),
      na.rm = TRUE,
      names = FALSE
    )
    probs <- unique(probs)
    if (length(probs) < 3L) {
      data$bin <- factor(match(data$fitted, unique_fitted))
    } else {
      data$bin <- cut(data$fitted, breaks = probs, include.lowest = TRUE)
    }
  }
  cal <- stats::aggregate(
    cbind(predicted = data$fitted, observed = data$observed),
    by = list(bin = data$bin),
    FUN = mean,
    na.rm = TRUE
  )

  ggplot2::ggplot(cal, ggplot2::aes(x = .data$predicted, y = .data$observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_line(colour = "#1F77B4") +
    ggplot2::geom_point(size = 2.4, colour = "#1F77B4") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Calibration plot",
      subtitle = model_label,
      x = "Mean predicted probability",
      y = "Observed event proportion"
    ) +
    .plot_model_fit_theme(base_size)
}
