#' Fine-Gray competing-risk regression
#'
#' Fit proportional subdistribution hazards models and return publication-ready
#' subdistribution hazard ratios.
#'
#' @param data A data frame containing follow-up time, event status, and model
#'   variables.
#' @param time Numeric follow-up-time variable. Quoted and bare names work.
#' @param event Event-status variable with one censoring code and distinct codes
#'   for the event of interest and competing events.
#' @param event_of_interest Observed code or label for the event being modelled.
#' @param censor_code Observed code or label for censoring. Defaults to `0`.
#' @param exposures Variables whose effects are reported.
#' @param adjust_for Optional adjustment variables. One adjusted model is fitted
#'   per exposure and only that exposure is reported.
#' @param multivariable Logical; fit one model containing and reporting all
#'   variables in `exposures`.
#' @param multivariate Optional alias for `multivariable`.
#' @param format Output table format: `"flextable"` or `"gt"`.
#' @param theme Table styling preset.
#' @param model_stats Logical; return N, event counts, pseudo-log-likelihood,
#'   and convergence status.
#' @param show_ref Logical; display categorical reference rows.
#'
#' @return A `gtregression` object containing the publication table, numeric
#'   estimates, fitted `crr` models, model summaries, and analysis metadata.
#'
#' @details
#' Each model uses the rows that are complete for its own variables. The N
#' column and the footnote report the observations and the events of interest
#' used by the fitted models, with the same wording as \code{cox_reg()} and
#' \code{surv_reg()}.
#'
#' Fine-Gray regression models the subdistribution hazard for the selected
#' event. An SHR above 1 indicates a higher cumulative incidence of that event
#' over follow-up, accounting for the presence of competing events. It is not a
#' cause-specific Cox hazard ratio and should not be interpreted as one.
#'
#' @examples
#' competing_risk_reg(
#'   data_competing_risk,
#'   time = follow_up_months,
#'   event = outcome,
#'   event_of_interest = Relapse,
#'   censor_code = Censored,
#'   exposures = c(treatment, age, stage)
#' )
#'
#' @export
competing_risk_reg <- function(data,
                               time,
                               event,
                               event_of_interest,
                               exposures,
                               censor_code = 0,
                               adjust_for = NULL,
                               multivariable = FALSE,
                               multivariate = NULL,
                               format = c("flextable", "gt"),
                               theme = c("minimal"),
                               model_stats = FALSE,
                               show_ref = TRUE) {
  if (!requireNamespace("cmprsk", quietly = TRUE)) {
    stop("Package 'cmprsk' is required for `competing_risk_reg()`.", call. = FALSE)
  }
  time <- .cox_single_var_arg(substitute(time), data, parent.frame())
  event <- .cox_single_var_arg(substitute(event), data, parent.frame())
  event_of_interest <- .competing_code_arg(
    substitute(event_of_interest), parent.frame()
  )
  censor_code <- .competing_code_arg(substitute(censor_code), parent.frame())
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

  if (!is.null(multivariate)) {
    if (!is.logical(multivariate) || length(multivariate) != 1L ||
        is.na(multivariate)) {
      stop("`multivariate` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!identical(multivariable, FALSE) &&
        !identical(multivariable, multivariate)) {
      stop("Use only one of `multivariable` or `multivariate`.", call. = FALSE)
    }
    multivariable <- multivariate
  }
  flags <- list(multivariable = multivariable, model_stats = model_stats,
                show_ref = show_ref)
  if (!all(vapply(flags, function(x) is.logical(x) && length(x) == 1L &&
                  !is.na(x), logical(1)))) {
    stop("Model switches must each be TRUE or FALSE.", call. = FALSE)
  }
  if (is.character(exposures)) exposures <- unique(exposures)
  if (isTRUE(multivariable) && length(adjust_for)) {
    stop(
      "`adjust_for` is not used with `multivariable = TRUE`; include all variables in `exposures`.",
      call. = FALSE
    )
  }

  clean <- .validate_fg_inputs(
    data, time, event, event_of_interest, censor_code, exposures, adjust_for
  )
  event_of_interest <- as.character(event_of_interest)
  censor_code <- as.character(censor_code)
  adjusted <- length(adjust_for) > 0L

  core <- .run_fg_core(
    clean, time, event, event_of_interest, censor_code,
    exposures, adjust_for, multivariable
  )
  effect_label <- if (adjusted || isTRUE(multivariable)) {
    "Adjusted SHR (95% CI)"
  } else {
    "SHR (95% CI)"
  }
  variable_labels <- .var_label_map(data, unique(c(exposures, adjust_for)))
  if (!adjusted && !isTRUE(multivariable)) {
    display <- .make_display_survival_uni(
      core$table_body, core$data_clean, time, event, effect_label,
      variable_labels, show_ref
    )
  } else {
    display <- .make_display_multi(
      core$table_body, core$data_clean, event, effect_label,
      variable_labels, show_ref
    )
  }
  display <- .add_model_n_column(display, core$models)
  event_label <- .competing_event_label(data[[event]], event_of_interest)
  footnotes <- c(
    .abbrev_note("fine_gray"),
    if (.has_visible_reference(core$table_body, show_ref)) .ref_note() else NULL,
    if (adjusted) .display_adjustment_note(adjust_for, variable_labels) else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other variables in the model." else NULL,
    paste0(
      "Event of interest: ", event_label,
      ". Competing events were retained in the Fine-Gray analysis."
    ),
    .model_n_footnote(core$models),
    .survival_events_note(.fg_model_events(core$models))
  )
  format <- match.arg(format, c("flextable", "gt"))
  theme <- .resolve_theme(theme)
  crude <- !adjusted && !isTRUE(multivariable)
  table <- if (format == "gt") {
    if (crude) .build_gt(display, effect_label, footnotes, theme) else {
      .build_gt_multi(display, effect_label, footnotes, theme)
    }
  } else {
    if (crude) .build_flextable(display, effect_label, footnotes, theme) else {
      .build_flextable_multi(display, effect_label, footnotes, theme)
    }
  }
  .message_hidden_ref_rows("competing_risk_reg", core$table_body, show_ref)

  result <- list(
    table = table,
    table_body = core$table_body,
    table_display = display,
    models = core$models,
    model_summaries = lapply(core$models, summary),
    model_stats = if (isTRUE(model_stats)) .fg_model_stats(core$models) else NULL,
    variable_labels = variable_labels,
    footnotes = footnotes,
    time = time,
    event = event,
    event_of_interest = event_of_interest,
    censor_code = censor_code,
    approach = "fine_gray",
    source = "competing_risk_reg",
    format = format,
    adjusted_mode = adjusted,
    multivariable = isTRUE(multivariable),
    adjust_for = if (adjusted) unique(adjust_for) else NULL,
    exposures = unique(exposures),
    show_ref = isTRUE(show_ref)
  )
  fmt_class <- if (format == "gt") "gt_competing_risk" else "ft_competing_risk"
  class(result) <- c("gtregression", "competing_risk_reg", fmt_class, class(result))
  result
}

#' @keywords internal
#' @noRd
.validate_fg_inputs <- function(data, time, event, failcode, cencode,
                                exposures, adjust_for) {
  .validate_cuminc_inputs(
    data, time, event, failcode, cencode, by = NULL,
    conf.int = TRUE, p_value = FALSE, p_value_position = NULL,
    break_time_by = NULL, xlim = NULL, ylim = NULL, y_percent = TRUE,
    theme = "classic", legend_position = NULL, grid = FALSE, base_size = 13
  )
  exposures <- unique(exposures)
  adjust_for <- unique(adjust_for)
  if (!length(exposures) || !all(exposures %in% names(data))) {
    stop("One or more exposure variables were not found in the dataset.", call. = FALSE)
  }
  if (length(adjust_for) && !all(adjust_for %in% names(data))) {
    stop("One or more adjustment variables were not found in the dataset.", call. = FALSE)
  }
  if (any(exposures %in% adjust_for)) {
    stop("`exposures` and `adjust_for` must not overlap.", call. = FALSE)
  }
  if (any(c(exposures, adjust_for) %in% c(time, event))) {
    stop("Model variables must differ from `time` and `event`.", call. = FALSE)
  }
  .validate_exposures(data, unique(c(exposures, adjust_for)))
  data
}

#' @keywords internal
#' @noRd
.run_fg_core <- function(data, time, event, failcode, cencode, exposures,
                         adjust_for, multivariable) {
  if (isTRUE(multivariable)) {
    fit <- .fit_fg_model(data, time, event, exposures, failcode, cencode)
    bodies <- lapply(exposures, function(x) .tidy_fg(fit, x))
    return(list(
      data_clean = attr(fit, "gtregression_model_frame"),
      table_body = do.call(rbind, bodies),
      models = list(multivariable_model = fit)
    ))
  }
  models <- setNames(vector("list", length(exposures)), exposures)
  bodies <- setNames(vector("list", length(exposures)), exposures)
  clean_frames <- vector("list", length(exposures))
  for (i in seq_along(exposures)) {
    variables <- unique(c(exposures[i], adjust_for))
    models[[i]] <- .fit_fg_model(
      data, time, event, variables, failcode, cencode
    )
    bodies[[i]] <- .tidy_fg(models[[i]], exposures[i])
    clean_frames[[i]] <- attr(models[[i]], "gtregression_model_frame")
  }
  list(
    data_clean = data,
    table_body = do.call(rbind, bodies),
    models = models
  )
}

#' @keywords internal
#' @noRd
.fit_fg_model <- function(data, time, event, variables, failcode, cencode) {
  needed <- unique(c(time, event, variables))
  model_data <- data[stats::complete.cases(data[, needed, drop = FALSE]), , drop = FALSE]
  for (variable in variables) {
    if (is.character(model_data[[variable]])) {
      model_data[[variable]] <- factor(model_data[[variable]])
    } else if (is.factor(model_data[[variable]])) {
      # Ordered factors would otherwise get polynomial contrasts, which are not
      # the category-versus-reference effects labelled in the table.
      model_data[[variable]] <- droplevels(factor(
        model_data[[variable]],
        levels = levels(model_data[[variable]]),
        ordered = FALSE
      ))
    }
  }
  .validate_exposures(model_data, variables)
  formula <- stats::reformulate(variables)
  design_full <- stats::model.matrix(formula, model_data)
  keep <- colnames(design_full) != "(Intercept)"
  assignments <- attr(design_full, "assign")[keep]
  design <- design_full[, keep, drop = FALSE]
  if (!ncol(design)) stop("No estimable model terms were created.", call. = FALSE)

  fit <- tryCatch(
    cmprsk::crr(
      ftime = model_data[[time]],
      fstatus = as.character(model_data[[event]]),
      cov1 = design,
      failcode = as.character(failcode),
      cencode = as.character(cencode)
    ),
    error = function(error) {
      stop("Fine-Gray model fitting failed: ", conditionMessage(error), call. = FALSE)
    }
  )
  if (!isTRUE(fit$converged)) {
    warning("Fine-Gray model did not converge.", call. = FALSE)
  }
  term_labels <- attr(stats::terms(formula), "term.labels")
  attr(fit, "gtregression_model_frame") <- model_data
  attr(fit, "gtregression_columns") <- stats::setNames(
    assignments, colnames(design)
  )
  attr(fit, "gtregression_term_labels") <- term_labels
  attr(fit, "gtregression_failcode") <- failcode
  attr(fit, "gtregression_cencode") <- cencode
  attr(fit, "gtregression_status") <- as.character(model_data[[event]])
  fit
}

#' @keywords internal
#' @noRd
.tidy_fg <- function(fit, exposure) {
  labels <- attr(fit, "gtregression_term_labels")
  assignment <- attr(fit, "gtregression_columns")
  term_index <- match(exposure, gsub("`", "", labels, fixed = TRUE))
  columns <- names(assignment)[assignment == term_index]
  if (!length(columns)) {
    stop("No estimable coefficient for exposure '", exposure, "'.", call. = FALSE)
  }
  smry <- summary(fit)
  coef_table <- smry$coef[columns, , drop = FALSE]
  ci_table <- smry$conf.int[columns, , drop = FALSE]
  model_data <- attr(fit, "gtregression_model_frame")
  x <- model_data[[exposure]]
  if (is.character(x)) x <- factor(x)

  nonref <- data.frame(
    exposure = exposure,
    level = if (is.factor(x)) levels(x)[-1L] else exposure,
    estimate = as.numeric(coef_table[, "exp(coef)"]),
    conf.low = as.numeric(ci_table[, "2.5%"]),
    conf.high = as.numeric(ci_table[, "97.5%"]),
    p.value = as.numeric(coef_table[, "p-value"]),
    ref = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.factor(x)) return(nonref)
  reference <- data.frame(
    exposure = exposure, level = levels(x)[1L], estimate = 1,
    conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_,
    ref = TRUE, stringsAsFactors = FALSE
  )
  rbind(reference, nonref)
}

#' @keywords internal
#' @noRd
.fg_model_stats <- function(models) {
  rows <- lapply(names(models), function(name) {
    fit <- models[[name]]
    status <- attr(fit, "gtregression_status")
    failcode <- as.character(attr(fit, "gtregression_failcode"))
    cencode <- as.character(attr(fit, "gtregression_cencode"))
    data.frame(
      model = name,
      pseudo_logLik = as.numeric(fit$loglik),
      converged = isTRUE(fit$converged),
      events_of_interest = sum(status == failcode),
      competing_events = sum(!status %in% c(failcode, cencode)),
      n = fit$n,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Events of interest in the rows used by each Fine-Gray model
#' @keywords internal
#' @noRd
.fg_model_events <- function(models) {
  vapply(models, function(fit) {
    status <- attr(fit, "gtregression_status")
    failcode <- as.character(attr(fit, "gtregression_failcode"))
    if (is.null(status) || !length(failcode)) return(NA_integer_)
    as.integer(sum(status == failcode))
  }, integer(1))
}

#' @keywords internal
#' @noRd
.competing_event_label <- function(event, code) {
  code <- as.character(code)
  if (is.factor(event) && code %in% levels(event)) return(code)
  code
}
