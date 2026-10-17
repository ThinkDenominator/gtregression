#' Proportional-odds ordinal logistic regression
#'
#' Fit crude, exposure-specific adjusted, or joint multivariable
#' proportional-odds models and return publication-ready common odds ratios.
#'
#' @param data A data frame containing the outcome and predictors.
#' @param outcome An ordered-factor outcome. Its level order defines movement
#'   from lower to higher categories.
#' @param exposures Predictor variables to report.
#' @param adjust_for Optional covariates included but not reported.
#' @param multivariable Logical; fit all exposures in one model.
#' @param multivariate Optional alias for `multivariable`.
#' @param assumption_test Logical; perform term-level likelihood-ratio tests of
#'   the proportional-odds assumption.
#' @param model_stats Logical; return model-fit and sample-size statistics.
#' @param format Output table format: `"flextable"` or `"gt"`.
#' @param theme Table styling preset.
#' @param show_ref Logical; display categorical reference rows.
#'
#' @return A `gtregression` object containing the publication table, fitted
#'   models, common odds ratios, proportional-odds tests, and metadata.
#'
#' @details
#' An odds ratio above one indicates greater odds of being in a higher outcome
#' category, assuming the proportional-odds assumption holds. The outcome must
#' be an ordered factor so category direction is never inferred silently.
#'
#' @examples
#' ordinal_data <- data.frame(
#'   recovery = ordered(
#'     c("Poor", "Fair", "Good", "Excellent", "Fair", "Good"),
#'     levels = c("Poor", "Fair", "Good", "Excellent")
#'   ),
#'   treatment = factor(c("Control", "Control", "New", "New", "Control", "New")),
#'   age = c(70, 64, 58, 51, 62, 55)
#' )
#' ordinal_reg(
#'   ordinal_data,
#'   outcome = recovery,
#'   exposures = treatment,
#'   adjust_for = age,
#'   assumption_test = FALSE
#' )
#'
#' @export
ordinal_reg <- function(data,
                        outcome,
                        exposures,
                        adjust_for = NULL,
                        multivariable = FALSE,
                        multivariate = NULL,
                        assumption_test = TRUE,
                        model_stats = FALSE,
                        format = c("flextable", "gt"),
                        theme = "minimal",
                        show_ref = TRUE) {
  outcome <- .cox_single_var_arg(substitute(outcome), data, parent.frame())
  exposures <- .vars_arg(substitute(exposures), parent.frame(),
                         data_names = names(data))
  adjust_for <- .vars_arg(
    substitute(adjust_for), parent.frame(), allow_null = TRUE,
    data_names = names(data)
  )
  format <- .choice_arg(
    substitute(format), parent.frame(), choices = c("flextable", "gt")
  )
  theme <- .choice_arg(substitute(theme), parent.frame())
  multivariable <- .ordinal_multivariable_arg(multivariable, multivariate)
  flags <- list(
    assumption_test = assumption_test,
    model_stats = model_stats,
    show_ref = show_ref
  )
  if (!all(vapply(flags, .ordinal_scalar_logical, logical(1)))) {
    stop(
      "`assumption_test`, `model_stats`, and `show_ref` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  .validate_ordinal_inputs(data, outcome, exposures, adjust_for)

  adjusted <- length(adjust_for) > 0L || isTRUE(multivariable)
  model_terms <- if (isTRUE(multivariable)) {
    list(multivariable_model = exposures)
  } else {
    stats::setNames(as.list(exposures), exposures)
  }
  fits <- tidy_parts <- stats_parts <- assumption_parts <- list()

  for (model_name in names(model_terms)) {
    reported <- model_terms[[model_name]]
    predictors <- unique(c(reported, adjust_for))
    clean <- .prepare_ordinal_data(data, outcome, predictors)
    fit <- .fit_ordinal_model(
      clean, outcome, predictors, assumption_test = assumption_test
    )
    tidy_parts[[model_name]] <- do.call(rbind, lapply(reported, function(x) {
      .tidy_ordinal_fixed(fit, clean, x)
    }))
    fits[[model_name]] <- fit
    stats_parts[[model_name]] <- .ordinal_model_stats(
      fit, clean, outcome, model_name
    )
    if (isTRUE(assumption_test)) {
      assumption_parts[[model_name]] <- .ordinal_assumption_test(fit, model_name)
    }
  }

  table_body <- do.call(rbind, tidy_parts)
  rownames(table_body) <- NULL
  variable_labels <- .var_label_map(data, unique(c(exposures, adjust_for)))
  effect_label <- if (adjusted) {
    "Adjusted common OR (95% CI)"
  } else {
    "Common OR (95% CI)"
  }
  display <- .make_display_multi(
    table_body, data, outcome, effect_label, variable_labels, show_ref
  )
  display <- .add_model_n_column(display, fits)
  .message_hidden_ref_rows("ordinal_reg", table_body, show_ref)
  outcome_levels <- levels(data[[outcome]])
  footnotes <- c(
    "Common OR = common Odds Ratio; CI = Confidence Interval.",
    if (.has_visible_reference(table_body, show_ref)) .ref_note() else NULL,
    paste0(
      "Outcome order (lower to higher): ",
      paste(outcome_levels, collapse = " < "), "."
    ),
    "An OR above 1 indicates greater odds of being in a higher outcome category.",
    if (length(adjust_for)) {
      .display_adjustment_note(adjust_for, variable_labels)
    } else NULL,
    if (isTRUE(multivariable) && length(exposures) > 1L) {
      "Adjusted for the other reported variables in the model."
    } else NULL,
    .model_n_footnote(fits)
  )
  format <- match.arg(format, c("flextable", "gt"))
  .validate_theme_arg(theme)
  theme <- .resolve_theme(theme)
  table <- if (format == "gt") {
    .build_gt_multi(display, effect_label, footnotes, theme)
  } else {
    .build_flextable_multi(display, effect_label, footnotes, theme)
  }
  assumption_tests <- if (length(assumption_parts)) {
    do.call(rbind, assumption_parts)
  } else NULL
  .warn_ordinal_assumption(assumption_tests)

  result <- list(
    table = table,
    table_body = table_body,
    table_display = display,
    models = fits,
    model_summaries = lapply(fits, summary),
    model_stats = if (isTRUE(model_stats)) do.call(rbind, stats_parts) else NULL,
    assumption_tests = assumption_tests,
    variable_labels = variable_labels,
    footnotes = footnotes,
    outcome = outcome,
    outcome_levels = outcome_levels,
    exposures = exposures,
    adjust_for = adjust_for,
    adjusted_mode = adjusted,
    multivariable = isTRUE(multivariable),
    approach = "ordinal_logit",
    format = format,
    source = "ordinal_reg",
    show_ref = isTRUE(show_ref)
  )
  fmt_class <- if (format == "gt") "gt_ordinal" else "ft_ordinal"
  class(result) <- c("gtregression", "ordinal_reg", fmt_class, class(result))
  result
}

#' @keywords internal
#' @noRd
.ordinal_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @keywords internal
#' @noRd
.ordinal_multivariable_arg <- function(multivariable, multivariate) {
  if (!is.null(multivariate)) {
    if (!.ordinal_scalar_logical(multivariate)) {
      stop("`multivariate` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!identical(multivariable, FALSE) &&
        !identical(multivariable, multivariate)) {
      stop("Use only one of `multivariable` or `multivariate`.", call. = FALSE)
    }
    multivariable <- multivariate
  }
  if (!.ordinal_scalar_logical(multivariable)) {
    stop("`multivariable` must be TRUE or FALSE.", call. = FALSE)
  }
  multivariable
}

#' @keywords internal
#' @noRd
.validate_ordinal_inputs <- function(data, outcome, exposures, adjust_for) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  variables <- unique(c(outcome, exposures, adjust_for))
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop("Variables not found in `data`: ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }
  if (!length(exposures)) {
    stop("`exposures` must contain at least one variable.", call. = FALSE)
  }
  if (outcome %in% c(exposures, adjust_for) ||
      length(intersect(exposures, adjust_for))) {
    stop("Outcome, exposures, and adjustment variables must not overlap.",
         call. = FALSE)
  }
  if (!is.ordered(data[[outcome]])) {
    stop(
      "`outcome` must be an ordered factor with levels arranged from lower to higher.",
      call. = FALSE
    )
  }
  if (nlevels(droplevels(data[[outcome]])) < 3L) {
    stop("Ordinal regression requires at least three observed outcome levels.",
         call. = FALSE)
  }
  .validate_exposures(data, unique(c(exposures, adjust_for)))
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.prepare_ordinal_data <- function(data, outcome, predictors) {
  variables <- unique(c(outcome, predictors))
  clean <- data[stats::complete.cases(data[, variables, drop = FALSE]), , drop = FALSE]
  if (!nrow(clean)) stop("No complete observations are available.", call. = FALSE)
  clean[[outcome]] <- droplevels(clean[[outcome]])
  if (nlevels(clean[[outcome]]) < 3L) {
    stop("Complete cases must contain at least three outcome levels.", call. = FALSE)
  }
  for (variable in predictors) {
    if (is.character(clean[[variable]])) clean[[variable]] <- factor(clean[[variable]])
    if (is.ordered(clean[[variable]])) {
      clean[[variable]] <- factor(
        clean[[variable]], levels = levels(clean[[variable]]), ordered = FALSE
      )
    }
    if (is.factor(clean[[variable]])) clean[[variable]] <- droplevels(clean[[variable]])
  }
  clean
}

#' @keywords internal
#' @noRd
.fit_ordinal_model <- function(data, outcome, predictors, assumption_test) {
  formula <- stats::reformulate(predictors, response = outcome)
  fit <- ordinal::clm(
    formula, data = data, link = "logit", Hess = TRUE, model = TRUE
  )
  if (isTRUE(assumption_test)) {
    original_call <- fit$call
    fit$call$formula <- formula
    fit$call$data <- data
    test <- tryCatch(
      ordinal::nominal_test(fit),
      error = function(error) NULL
    )
    fit$call <- original_call
    attr(fit, "gtregression_assumption_test") <- test
  }
  fit
}

#' @keywords internal
#' @noRd
.tidy_ordinal_fixed <- function(fit, data, exposure) {
  coefficients <- stats::coef(fit)[names(fit$beta)]
  covariance <- stats::vcov(fit)[names(fit$beta), names(fit$beta), drop = FALSE]
  fixed_formula <- stats::formula(fit)
  fixed_matrix <- stats::model.matrix(fixed_formula, data = data)
  assignment <- attr(fixed_matrix, "assign")
  names(assignment) <- colnames(fixed_matrix)
  term_labels <- gsub(
    "`", "", attr(stats::terms(fixed_formula), "term.labels"), fixed = TRUE
  )
  term_index <- match(exposure, term_labels)
  selected_names <- intersect(
    names(assignment)[assignment == term_index], names(coefficients)
  )
  if (!length(selected_names)) {
    stop("No estimable coefficient for exposure '", exposure, "'.", call. = FALSE)
  }
  estimate <- coefficients[selected_names]
  se <- sqrt(diag(covariance[selected_names, selected_names, drop = FALSE]))
  z <- stats::qnorm(0.975)
  x <- data[[exposure]]
  nonref <- data.frame(
    exposure = exposure,
    level = if (is.factor(x)) levels(x)[-1L] else exposure,
    estimate = exp(estimate),
    conf.low = exp(estimate - z * se),
    conf.high = exp(estimate + z * se),
    p.value = 2 * stats::pnorm(abs(estimate / se), lower.tail = FALSE),
    ref = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.factor(x)) return(nonref)
  reference <- data.frame(
    exposure = exposure,
    level = levels(x)[1L],
    estimate = 1,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = NA_real_,
    ref = TRUE,
    stringsAsFactors = FALSE
  )
  rbind(reference, nonref)
}

#' @keywords internal
#' @noRd
.ordinal_model_stats <- function(fit, data, outcome, model_name) {
  data.frame(
    model = model_name,
    AIC = stats::AIC(fit),
    BIC = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)),
    n = stats::nobs(fit),
    outcome_levels = nlevels(data[[outcome]]),
    convergence = fit$convergence$code,
    convergence_message = fit$convergence$messages,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.ordinal_assumption_test <- function(fit, model_name) {
  test <- attr(fit, "gtregression_assumption_test")
  if (is.null(test) || nrow(test) < 2L) return(NULL)
  out <- as.data.frame(test[-1L, , drop = FALSE])
  p_value <- out[["Pr(>Chi)"]]
  keep <- !is.na(p_value)
  if (!any(keep)) return(NULL)
  out <- out[keep, , drop = FALSE]
  p_value <- p_value[keep]
  data.frame(
    model = model_name,
    term = rownames(out),
    df = out$Df,
    statistic = out$LRT,
    p.value = p_value,
    proportional_odds = ifelse(p_value < 0.05, "Questionable", "Supported"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @keywords internal
#' @noRd
.warn_ordinal_assumption <- function(tests) {
  if (is.null(tests) || !nrow(tests)) return(invisible(NULL))
  failed <- tests[!is.na(tests$p.value) & tests$p.value < 0.05, , drop = FALSE]
  if (nrow(failed)) {
    warning(
      "The proportional-odds assumption may not hold for: ",
      paste(unique(failed$term), collapse = ", "),
      ". Review `$assumption_tests` before interpreting common odds ratios.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
