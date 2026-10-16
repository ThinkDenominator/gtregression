#' Cox proportional hazards regression
#'
#' Fit Cox proportional hazards models and report hazard ratios.
#'
#' @param data A \code{data.frame} containing survival time, event status, and
#'   exposure variables.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param exposures Character vector of exposure variable names. Quoted names
#'   are recommended in scripts, and bare names are also accepted.
#' @param adjust_for Optional character vector of adjustment variables. When
#'   supplied, one adjusted Cox model is fitted per exposure.
#' @param stratifier Optional single stratifying variable. When supplied,
#'   stratum-specific Cox tables are produced using the same crude, adjusted,
#'   or multivariable workflow requested by the other arguments. The
#'   stratifier cannot also be used as the time, event, exposure, adjustment,
#'   or interaction variable.
#' @param baseline_strata Optional character vector of categorical variables
#'   used to stratify the baseline hazard within one Cox model. Common hazard
#'   ratios are estimated while each stratum has a separate baseline hazard.
#'   No hazard ratio is estimated for these variables. This cannot be combined
#'   with \code{stratifier}, which fits separate models.
#' @param time_start Optional numeric variable containing entry or interval-start
#'   times. When supplied, a counting-process response
#'   \code{Surv(time_start, time, event)} is fitted, enabling delayed entry and
#'   start-stop data.
#' @param cluster Optional variable identifying correlated observations. Cox
#'   models use a clustered sandwich variance estimate for this variable.
#' @param weights Optional numeric case-weight variable.
#' @param robust Optional logical passed to \code{survival::coxph()}. The
#'   default \code{NULL} lets \code{coxph()} choose its standard behaviour;
#'   clustered models use robust variance automatically.
#' @param ties Method for handling tied event times: \code{"efron"} (default),
#'   \code{"breslow"}, or \code{"exact"}.
#' @param control Optional named list of arguments for
#'   \code{survival::coxph.control()}, or a completed control list returned by
#'   that function.
#' @param interaction Optional character scalar specifying one interaction term
#'   using standard formula syntax, e.g. \code{"trt*prior"}. Quoted and bare
#'   interaction syntax are accepted. In exposure-by-exposure mode, supply a
#'   single exposure; in \code{multivariable = TRUE} mode, the interaction is
#'   added to the single multivariable model.
#' @param multivariable Logical; if \code{FALSE} (default), the current
#'   exposure-by-exposure workflow is used. If \code{TRUE}, one multivariable
#'   Cox model is fitted using all variables in \code{exposures}, and all
#'   exposure coefficients are reported.
#' @param multivariate Optional logical alias for \code{multivariable}. This is
#'   accepted for convenience; \code{multivariable} is used internally.
#' @param format Output table format; one of \code{"flextable"} (default) or
#'   \code{"gt"}.
#' @param theme Table styling preset.
#' @param show_sample For stratified Cox tables (\code{stratifier} supplied),
#'   controls which sample-size columns are shown in the publication table. One
#'   of \code{"both"} (default; N and events), \code{"n"}, \code{"events"}, or
#'   \code{"none"}. Model statistics, when requested, still retain both N and
#'   event counts. This argument is ignored when \code{stratifier} is not
#'   supplied: non-stratified tables always show the model N column and report
#'   the number of events in a footnote.
#' @param model_stats Logical; if \code{TRUE}, extract model-fit statistics
#'   including AIC, BIC, log-likelihood, concordance, number of events, and N.
#' @param show_ref Logical; if \code{TRUE} (default), display reference-category
#'   rows as \code{"Ref."}. If \code{FALSE}, hide reference rows; a message
#'   reminds users to use \code{show_ref = TRUE} when reference rows are needed.
#'
#' @details
#' By default, \code{cox_reg()} keeps the exposure-by-exposure workflow:
#' without \code{adjust_for}, one crude Cox model is fitted per exposure; with
#' \code{adjust_for}, one adjusted Cox model is fitted per exposure and only the
#' exposure estimate is reported. This is useful for screening or for reporting
#' several adjusted exposure effects.
#'
#' With \code{multivariable = TRUE}, all variables in \code{exposures} are
#' included in a single Cox model and all coefficients are reported. This mirrors
#' the multivariable workflow used by \code{multi_reg()}. The
#' \code{adjust_for} argument is not used in this mode; include every variable
#' that belongs in the model inside \code{exposures}. Since these estimates are
#' adjusted for the other variables in the same model, the table reports
#' \code{Adjusted HR (95\% CI)}.
#'
#' Interaction terms specified via \code{interaction} are included using
#' standard formula expansion (for example, \code{trt*prior}). Interaction
#' effects are displayed as additional rows beneath the corresponding exposure.
#'
#' \code{baseline_strata} fits one Cox model with separate baseline hazards for
#' the observed combinations of the selected categorical variables. It reports
#' common hazard ratios for the predictors and no coefficient for the strata.
#' This differs from \code{stratifier}, which fits separate models and reports
#' stratum-specific results.
#'
#' The proportional hazards assumption should be assessed separately, for
#' example with \code{check_ph()}.
#'
#' Stratified Cox tables show both N and event counts by default
#' (\code{show_sample = "both"}). Use \code{show_sample = "n"},
#' \code{show_sample = "events"}, or \code{show_sample = "none"} to control the
#' displayed sample columns. All counts are taken from the fitted model within
#' each stratum, so rows dropped for missing values in any model variable
#' (including \code{time_start}, \code{cluster}, and \code{weights}) are not
#' counted. When models within a stratum use different numbers of
#' observations, the footnote reports the range. Rows with a missing
#' \code{stratifier} value are excluded and counted in a footnote.
#' \code{show_sample} has no effect without \code{stratifier}.
#'
#' Complete-case analysis is used: each model uses the rows that are complete
#' for its own variables. Unused factor levels are dropped for each fitted
#' model, so the reference category shown as \code{"Ref."} is the first level
#' present in the data used by that model. Ordered factors are analysed with
#' treatment (category-versus-reference) contrasts. Data in which every
#' observation is an event are accepted.
#'
#' If exposure variables have a \code{"label"} attribute, for example from
#' \code{labelled::var_label()}, those labels are used automatically in the
#' displayed table.
#'
#' @return A list of class \code{c("gtregression","cox_reg", ...)} with elements:
#' \describe{
#'   \item{table}{A \code{flextable} or \code{gt_tbl}.}
#'   \item{table_body}{Data frame of hazard ratios and confidence intervals.}
#'   \item{table_display}{Data frame used to render the publication table.}
#'   \item{models}{List of fitted \code{coxph} models.}
#'   \item{model_summaries}{Summary output for the fitted models.}
#'   \item{model_stats}{Model-fit statistics when \code{model_stats = TRUE};
#'   otherwise \code{NULL}.}
#'   \item{variable_labels}{Named character vector of display labels.}
#'   \item{footnotes}{Character vector used by the rendered table. Adjustment
#'   notes use the same display labels as the table.}
#'   \item{time,event,approach,format,source,adjust_for,baseline_strata,
#'   exposures,interaction}{Metadata fields.}
#' }
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#' lung_data$prior <- factor(lung_data$prior, levels = c(0, 10),
#'                           labels = c("No", "Yes"))
#'
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c("trt", "celltype", "karno", "age")
#' )
#'
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, celltype, prior),
#'   adjust_for = c(age, karno)
#' )
#'
#' # Interaction in an adjusted exposure model
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = trt,
#'   adjust_for = c(age, karno),
#'   interaction = trt*prior
#' )
#'
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, celltype, prior, age, karno),
#'   multivariable = TRUE
#' )
#'
#' # One model with separate baseline hazards by cell type
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, age, karno),
#'   baseline_strata = celltype,
#'   multivariable = TRUE
#' )
#'
#' # multivariate is accepted as an alias
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, age, karno),
#'   multivariate = TRUE
#' )
#'
#' @importFrom survival Surv coxph strata
#' @export
cox_reg <- function(data,
                    time,
                    event,
                    exposures,
                    adjust_for = NULL,
                    stratifier = NULL,
                    interaction = NULL,
                    multivariable = FALSE,
                    multivariate = NULL,
                    format = c("flextable", "gt"),
                    theme = c("minimal"),
                    show_sample = "both",
                    model_stats = FALSE,
                    show_ref = TRUE,
                    baseline_strata = NULL,
                    time_start = NULL,
                    cluster = NULL,
                    weights = NULL,
                    robust = NULL,
                    ties = c("efron", "breslow", "exact"),
                    control = NULL) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame(),
                         data_names = names(data))
  adjust_for <- .vars_arg(substitute(adjust_for), env = parent.frame(),
                          allow_null = TRUE, data_names = names(data))
  stratifier <- .survival_optional_single_var_arg(
    substitute(stratifier),
    data = data,
    env = parent.frame()
  )
  baseline_strata <- .vars_arg(
    substitute(baseline_strata), env = parent.frame(), allow_null = TRUE,
    data_names = names(data)
  )
  time_start <- .survival_optional_single_var_arg(
    substitute(time_start), data = data, env = parent.frame()
  )
  cluster <- .survival_optional_single_var_arg(
    substitute(cluster), data = data, env = parent.frame()
  )
  weights <- .survival_optional_single_var_arg(
    substitute(weights), data = data, env = parent.frame()
  )
  interaction <- .interaction_arg(substitute(interaction), env = parent.frame(), allow_null = TRUE)
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("flextable", "gt"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  show_sample <- .choice_arg(
    substitute(show_sample),
    env = parent.frame(),
    choices = c("events", "n", "both", "none")
  )
  ties <- .choice_arg(
    substitute(ties), env = parent.frame(),
    choices = c("efron", "breslow", "exact")
  )

  if (!is.logical(model_stats) || length(model_stats) != 1L || is.na(model_stats)) {
    stop("`model_stats` must be TRUE or FALSE.", call. = FALSE)
  }
  .validate_show_ref(show_ref)
  if (!is.null(multivariate)) {
    if (!is.logical(multivariate) || length(multivariate) != 1L || is.na(multivariate)) {
      stop("`multivariate` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!identical(multivariable, FALSE) && !identical(multivariable, multivariate)) {
      stop("Use only one of `multivariable` or `multivariate`, or give them the same value.", call. = FALSE)
    }
    multivariable <- multivariate
  }
  if (!is.logical(multivariable) || length(multivariable) != 1L || is.na(multivariable)) {
    stop("`multivariable` must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(multivariable) && !is.null(adjust_for) && length(adjust_for) > 0) {
    stop(
      "`adjust_for` is not used when `multivariable = TRUE`; include all model variables in `exposures`.",
      call. = FALSE
    )
  }

  format <- match.arg(format, c("flextable", "gt"))
  show_sample_choices <- c("events", "n", "both", "none")
  if (!is.character(show_sample) || length(show_sample) != 1L || !show_sample %in% show_sample_choices) {
    stop("`show_sample` must be one of 'events', 'n', 'both', or 'none'.", call. = FALSE)
  }
  show_sample <- match.arg(show_sample, show_sample_choices)
  ties <- match.arg(ties, c("efron", "breslow", "exact"))
  if (is.character(exposures)) exposures <- unique(exposures)
  if (identical(ties, "exact") && !is.null(time_start)) {
    stop(
      "`ties = \"exact\"` cannot be combined with `time_start`; ",
      "use \"efron\" or \"breslow\" for counting-process data.",
      call. = FALSE
    )
  }
  if (!is.null(robust) &&
      (!is.logical(robust) || length(robust) != 1L || is.na(robust))) {
    stop("`robust` must be TRUE, FALSE, or NULL.", call. = FALSE)
  }
  if (!is.null(cluster) && identical(robust, FALSE)) {
    stop(
      "`robust = FALSE` cannot be combined with `cluster`; clustered Cox models require robust variance.",
      call. = FALSE
    )
  }
  control <- .cox_control_arg(control)
  theme <- .resolve_theme(theme)
  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  fmt_class <- if (format == "gt") "gt_cox" else "ft_cox"

  if (!is.null(stratifier) && length(baseline_strata)) {
    stop(
      "Use either `stratifier` for separate models or `baseline_strata` for one Cox model, not both.",
      call. = FALSE
    )
  }

  if (!is.null(stratifier)) {
    if (stratifier %in% c(time_start, cluster, weights)) {
      stop(
        "`stratifier` cannot also be used as `time_start`, `cluster`, or `weights`.",
        call. = FALSE
      )
    }
    .validate_survival_stratifier(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      adjust_for = adjust_for,
      interaction = interaction,
      stratifier = stratifier
    )

    return(.run_stratified_cox_reg(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      adjust_for = adjust_for,
      stratifier = stratifier,
      interaction = interaction,
      multivariable = multivariable,
      format = format,
      theme = theme,
      show_sample = show_sample,
      model_stats = model_stats,
      show_ref = show_ref,
      fmt_class = fmt_class,
      event_note = .survival_event_level_note(data, event),
      time_start = time_start,
      cluster = cluster,
      weights = weights,
      robust = robust,
      ties = ties,
      control = control
    ))
  }

  core <- .run_cox_core(
    data = data,
    time = time,
    event = event,
    exposures = exposures,
    adjust_for = adjust_for,
    baseline_strata = baseline_strata,
    interaction = interaction,
    multivariable = multivariable,
    time_start = time_start,
    cluster = cluster,
    weights = weights,
    robust = robust,
    ties = ties,
    control = control
  )

  effect_label <- if (adjusted_mode || isTRUE(multivariable)) "Adjusted HR (95% CI)" else "HR (95% CI)"
  variable_labels <- .var_label_map(
    data,
    unique(c(
      exposures, adjust_for, baseline_strata, time_start, cluster, weights,
      .interaction_vars(interaction)
    ))
  )
  core$table_body <- .format_interaction_levels(
    core$table_body, core$data_clean, variable_labels
  )
  crude_mode <- !adjusted_mode && !isTRUE(multivariable)

  if (crude_mode) {
    display_df <- .make_display_survival_uni(
      core$table_body,
      core$data_clean,
      time = time,
      event = event,
      effect_label = effect_label,
      variable_labels = variable_labels,
      show_ref = show_ref
    )
    .must_be_display_df(display_df)
  } else {
    display_df <- .make_display_multi(
      core$table_body,
      core$data_clean,
      outcome = event,
      effect_label = effect_label,
      variable_labels = variable_labels,
      show_ref = show_ref
    )
    .must_be_display_df_multi(display_df)
  }
  display_df <- .add_model_n_column(display_df, core$models)

  footnotes <- c(
    .abbrev_note("cox"),
    if (.has_visible_reference(core$table_body, show_ref)) .ref_note() else NULL,
    if (adjusted_mode) .display_adjustment_note(adjust_for, variable_labels) else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other variables in the model." else NULL,
    .cox_baseline_strata_note(baseline_strata, variable_labels),
    .cox_analysis_note(time_start, cluster, weights, robust, ties, variable_labels),
    .cox_weights_robust_note(core$models, cluster, weights, robust),
    .survival_event_level_note(data, event),
    if (!is.null(interaction)) .interaction_note(interaction) else NULL,
    .model_n_footnote(core$models),
    .survival_events_note(.survival_model_events(core$models))
  )

  .message_hidden_ref_rows("cox_reg", core$table_body, show_ref)

  tbl <- if (format == "gt") {
    if (crude_mode) {
      .build_gt(display_df, effect_label, footnotes, theme)
    } else {
      .build_gt_multi(display_df, effect_label, footnotes, theme)
    }
  } else {
    if (crude_mode) {
      .build_flextable(display_df, effect_label, footnotes, theme)
    } else {
      .build_flextable_multi(display_df, effect_label, footnotes, theme)
    }
  }

  res <- list(
    table = tbl,
    table_body = core$table_body,
    table_display = display_df,
    models = core$models,
    model_summaries = core$model_summaries,
    model_stats = if (isTRUE(model_stats)) .cox_model_stats_table(core$models) else NULL,
    variable_labels = variable_labels,
    footnotes = footnotes,
    time = time,
    event = event,
    time_start = time_start,
    approach = "cox",
    format = format,
    source = "cox_reg",
    adjusted_mode = adjusted_mode,
    multivariable = isTRUE(multivariable),
    adjust_for = if (adjusted_mode) unique(adjust_for) else NULL,
    baseline_strata = baseline_strata,
    cluster = cluster,
    weights = weights,
    robust = robust,
    ties = ties,
    control = control,
    exposures = unique(exposures),
    interaction = interaction,
    show_ref = isTRUE(show_ref)
  )

  class(res) <- c("gtregression", "cox_reg", fmt_class, class(res))
  res
}

#' @keywords internal
#' @noRd
.run_cox_core <- function(data,
                          time,
                          event,
                          exposures,
                          adjust_for = NULL,
                          baseline_strata = NULL,
                          interaction = NULL,
                          multivariable = FALSE,
                          time_start = NULL,
                          cluster = NULL,
                          weights = NULL,
                          robust = NULL,
                          ties = "efron",
                          control = NULL) {
  data_valid <- .validate_cox_inputs(
    data,
    time,
    event,
    exposures,
    adjust_for,
    baseline_strata = baseline_strata,
    interaction = interaction,
    multivariable = multivariable,
    time_start = time_start,
    cluster = cluster,
    weights = weights
  )
  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  interaction_vars <- .interaction_vars(interaction)

  if (isTRUE(multivariable)) {
    predictors <- unique(c(exposures, interaction_vars))
    data_model <- .cox_model_data(
      data_valid, time, event, predictors, baseline_strata = baseline_strata,
      time_start = time_start, cluster = cluster, weights = weights
    )
    .warn_cox_baseline_strata(data_model, event, baseline_strata)
    fit <- .fit_cox_model(
      data_model, time, event, predictors,
      interaction = interaction,
      baseline_strata = baseline_strata, time_start = time_start,
      cluster = cluster, weights = weights, robust = robust,
      ties = ties, control = control
    )

    if (is.null(fit)) {
      stop("Cox model fitting failed for the multivariable model.", call. = FALSE)
    }

    td_list <- lapply(unique(exposures), function(exposure) .tidy_cox(fit, exposure, interaction = interaction))
    td_list <- Filter(Negate(is.null), td_list)
    if (!length(td_list)) {
      stop("No estimable Cox coefficients for the multivariable model.", call. = FALSE)
    }

    fits <- list(multivariable_model = fit)
    return(list(
      data_clean = data_valid,
      table_body = do.call(rbind, td_list),
      models = fits,
      model_summaries = lapply(fits, summary)
    ))
  }

  fits <- vector("list", length(exposures))
  names(fits) <- exposures
  tds <- vector("list", length(exposures))
  names(tds) <- exposures

  for (i in seq_along(exposures)) {
    exposure <- exposures[i]
    predictors <- unique(c(if (adjusted_mode) c(exposure, adjust_for) else exposure, interaction_vars))
    data_model <- .cox_model_data(
      data_valid, time, event, predictors, baseline_strata = baseline_strata,
      time_start = time_start, cluster = cluster, weights = weights
    )
    .warn_cox_baseline_strata(data_model, event, baseline_strata, exposure)
    fit <- .fit_cox_model(
      data_model, time, event, predictors,
      interaction = interaction,
      baseline_strata = baseline_strata, time_start = time_start,
      cluster = cluster, weights = weights, robust = robust,
      ties = ties, control = control
    )

    if (is.null(fit)) {
      stop("Cox model fitting failed for exposure '", exposure, "'.", call. = FALSE)
    }

    td <- .tidy_cox(fit, exposure, interaction = interaction)
    if (is.null(td) || !nrow(td)) {
      stop("No estimable Cox coefficients for exposure '", exposure, "'.", call. = FALSE)
    }

    fits[[i]] <- fit
    tds[[i]] <- td
  }

  list(
    data_clean = data_valid,
    table_body = do.call(rbind, tds),
    models = fits,
    model_summaries = lapply(fits, summary)
  )
}

#' @keywords internal
#' @noRd
.cox_single_var_arg <- function(expr, data, env = parent.frame()) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% names(data)) {
      # Data column wins, unless a calling (wrapper) function forwards the
      # name of another column through a same-named argument.
      passed <- .wrapper_passed_name(nm, env, names(data))
      if (!is.null(passed) && length(passed) == 1L) {
        return(passed)
      }
      return(nm)
    }
    if (exists(nm, envir = env, inherits = TRUE)) {
      val <- get(nm, envir = env, inherits = TRUE)
      if (is.character(val) && length(val) == 1L) {
        return(val)
      }
    }
    return(nm)
  }

  out <- eval(expr, envir = env)
  if (!is.character(out) || length(out) != 1L) {
    stop("Survival variable arguments must be single column names.", call. = FALSE)
  }
  out
}

#' @keywords internal
#' @noRd
.validate_cox_inputs <- function(data,
                                 time,
                                 event,
                                 exposures,
                                 adjust_for = NULL,
                                 baseline_strata = NULL,
                                 interaction = NULL,
                                 multivariable = FALSE,
                                 time_start = NULL,
                                 cluster = NULL,
                                 weights = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(time) || length(time) != 1L || !time %in% names(data)) {
    stop("`time` must be a single survival time variable in `data`.", call. = FALSE)
  }

  if (!is.character(event) || length(event) != 1L || !event %in% names(data)) {
    stop("`event` must be a single event indicator variable in `data`.", call. = FALSE)
  }

  optional_vars <- c(time_start, cluster, weights)
  if (length(optional_vars) && any(!optional_vars %in% names(data))) {
    stop(
      "One or more `time_start`, `cluster`, or `weights` variables were not found in the dataset.",
      call. = FALSE
    )
  }
  if (anyDuplicated(c(time, event, optional_vars))) {
    stop(
      "`time`, `event`, `time_start`, `cluster`, and `weights` must refer to distinct variables.",
      call. = FALSE
    )
  }

  exposures <- unique(exposures)
  if (!is.character(exposures) || length(exposures) < 1L) {
    stop("`exposures` must contain at least one variable.", call. = FALSE)
  }

  if (!all(exposures %in% names(data))) {
    stop("One or more exposure variables were not found in the dataset.", call. = FALSE)
  }

  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  if (adjusted_mode) {
    adjust_for <- unique(adjust_for)
    if (!is.character(adjust_for) || !all(adjust_for %in% names(data))) {
      stop("One or more adjustment variables were not found in the dataset.", call. = FALSE)
    }
    if (any(exposures %in% adjust_for)) {
      stop("In adjusted mode, `exposures` and `adjust_for` must not overlap.", call. = FALSE)
    }
  } else {
    adjust_for <- NULL
  }

  role_clash <- intersect(c(exposures, adjust_for), c(time, event, time_start))
  if (length(role_clash)) {
    stop(
      "The time and event variables cannot also be exposures or adjustment variables: ",
      paste(role_clash, collapse = ", "), ".",
      call. = FALSE
    )
  }

  interaction_vars <- .validate_interaction_term(
    data = data,
    exposures = exposures,
    interaction = interaction,
    adjusted_mode = adjusted_mode,
    exposure_by_exposure = !isTRUE(multivariable)
  )

  role_overlap <- intersect(
    c(time_start, cluster, weights),
    unique(c(exposures, adjust_for, interaction_vars))
  )
  if (length(role_overlap)) {
    stop(
      "`time_start`, `cluster`, and `weights` must not also be model predictors: ",
      paste(role_overlap, collapse = ", "), ".",
      call. = FALSE
    )
  }

  baseline_strata <- unique(baseline_strata)
  if (length(baseline_strata)) {
    if (!is.character(baseline_strata) ||
        !all(baseline_strata %in% names(data))) {
      stop(
        "One or more `baseline_strata` variables were not found in the dataset.",
        call. = FALSE
      )
    }
    reserved <- unique(c(
      time, event, time_start, cluster, weights,
      exposures, adjust_for, interaction_vars
    ))
    if (any(baseline_strata %in% reserved)) {
      stop(
        "`baseline_strata` must not overlap with time, event, exposures, adjustment, or interaction variables.",
        call. = FALSE
      )
    }
    valid_type <- vapply(
      data[baseline_strata],
      function(x) is.factor(x) || is.character(x) || is.logical(x),
      logical(1)
    )
    if (!all(valid_type)) {
      stop(
        "`baseline_strata` variables must be categorical (factor, character, or logical).",
        call. = FALSE
      )
    }
    observed_levels <- vapply(
      data[baseline_strata],
      function(x) length(unique(stats::na.omit(x))),
      integer(1)
    )
    if (any(observed_levels < 2L)) {
      stop(
        "Each `baseline_strata` variable must have at least two observed levels.",
        call. = FALSE
      )
    }
  }

  if (!is.numeric(data[[time]])) {
    stop("`time` must be numeric.", call. = FALSE)
  }

  if (any(data[[time]] < 0, na.rm = TRUE)) {
    stop("`time` must contain non-negative follow-up times.", call. = FALSE)
  }

  if (!is.null(time_start)) {
    if (!is.numeric(data[[time_start]])) {
      stop("`time_start` must be numeric.", call. = FALSE)
    }
    if (any(data[[time_start]] < 0, na.rm = TRUE)) {
      stop("`time_start` must contain non-negative entry times.", call. = FALSE)
    }
    complete_time <- stats::complete.cases(data[, c(time_start, time), drop = FALSE])
    if (any(data[[time_start]][complete_time] >= data[[time]][complete_time])) {
      stop("Every complete `time_start` value must be less than `time`.", call. = FALSE)
    }
  }

  if (!is.null(cluster) &&
      length(unique(stats::na.omit(data[[cluster]]))) < 2L) {
    stop("`cluster` must contain at least two observed groups.", call. = FALSE)
  }

  if (!is.null(weights)) {
    weight_values <- data[[weights]]
    if (!is.numeric(weight_values) ||
        any(!is.finite(weight_values[!is.na(weight_values)])) ||
        any(weight_values <= 0, na.rm = TRUE)) {
      stop(
        "`weights` must be numeric, finite, and greater than zero ",
        "(remove rows with zero weight before fitting).",
        call. = FALSE
      )
    }
  }

  data_valid <- data

  data_valid[[event]] <- .cox_event01(data_valid[[event]])
  if (sum(data_valid[[event]] == 1, na.rm = TRUE) == 0) {
    stop("`event` must include at least one event.", call. = FALSE)
  }

  .validate_exposures(data_valid, unique(c(exposures, adjust_for, interaction_vars)))
  data_valid
}

#' @keywords internal
#' @noRd
.cox_model_data <- function(data,
                            time,
                            event,
                            predictors,
                            baseline_strata = NULL,
                            time_start = NULL,
                            cluster = NULL,
                            weights = NULL) {
  vars_needed <- unique(c(
    time_start, time, event, predictors, baseline_strata, cluster, weights
  ))
  cc_idx <- stats::complete.cases(data[, vars_needed, drop = FALSE])
  data_model <- data[cc_idx, , drop = FALSE]

  if (nrow(data_model) == 0) {
    stop("No complete cases available for this Cox model.", call. = FALSE)
  }

  if (sum(data_model[[event]] == 1, na.rm = TRUE) == 0) {
    stop("`event` must include at least one event for this Cox model.", call. = FALSE)
  }

  .validate_exposures(data_model, predictors)
  .survival_prepare_predictors(data_model, predictors)
}

#' Prepare categorical predictors for one fitted survival model
#'
#' Ordered factors otherwise receive polynomial contrasts (`.L`, `.Q`, ...),
#' which are not the category-versus-reference effects shown in the tables, and
#' unused factor levels would make the first declared level look like the
#' reference although the model used the first level present in the data.
#' Applied to the complete-case data of each model, so the reference row always
#' matches the fitted contrasts.
#'
#' @keywords internal
#' @noRd
.survival_prepare_predictors <- function(data, predictors) {
  for (variable in intersect(unique(predictors), names(data))) {
    x <- data[[variable]]
    if (!is.factor(x)) next
    label <- attr(x, "label", exact = TRUE)
    if (is.ordered(x)) {
      x <- factor(x, levels = levels(x), ordered = FALSE)
    }
    x <- droplevels(x)
    if (!is.null(label)) attr(x, "label") <- label
    data[[variable]] <- x
  }
  data
}

#' Event counts used by fitted survival models
#' @keywords internal
#' @noRd
.survival_model_events <- function(models) {
  vapply(models, function(fit) {
    value <- suppressWarnings(as.integer(.strata_survival_event_count(fit)))
    if (!length(value)) NA_integer_ else value
  }, integer(1))
}

#' Format one count or a range of counts across models
#' @keywords internal
#' @noRd
.survival_count_range <- function(values) {
  values <- suppressWarnings(as.integer(values))
  values <- values[!is.na(values)]
  if (!length(values)) return(NA_character_)
  if (length(unique(values)) == 1L) return(as.character(values[1L]))
  paste0(min(values), "\u2013", max(values))
}

#' Footnote reporting the events behind the model N values
#'
#' Shared by `cox_reg()`, `surv_reg()`, and `competing_risk_reg()` so that the
#' sentence is identical in the three functions.
#'
#' @keywords internal
#' @noRd
.survival_events_note <- function(events) {
  txt <- .survival_count_range(events)
  if (is.na(txt)) return(NULL)
  events <- unique(stats::na.omit(suppressWarnings(as.integer(events))))
  if (length(events) > 1L) {
    return(paste0("Number of events: ", txt, " across models."))
  }
  paste0("Number of events: ", txt, ".")
}

#' Footnote naming the level treated as the event for text-coded events
#' @keywords internal
#' @noRd
.survival_event_level_note <- function(data, event) {
  if (!is.data.frame(data) || !is.character(event) || length(event) != 1L ||
      !event %in% names(data)) {
    return(NULL)
  }
  x <- data[[event]]
  if (!is.factor(x) && !is.character(x)) return(NULL)
  levs <- levels(factor(x))
  if (length(levs) != 2L) return(NULL)
  paste0("Event: ", levs[2L], " (", levs[1L], " treated as censored).")
}

#' Footnote for the robust variance that coxph() selects for case weights
#' @keywords internal
#' @noRd
.cox_weights_robust_note <- function(models, cluster, weights, robust) {
  if (is.null(weights) || !is.null(cluster) || isTRUE(robust)) {
    return(NULL)
  }
  used <- any(vapply(models, function(fit) {
    inherits(fit, "coxph") && !is.null(fit$naive.var)
  }, logical(1)))
  if (used) "Robust sandwich variance used with case weights." else NULL
}

#' @keywords internal
#' @noRd
.cox_event01 <- function(x) {
  if (is.logical(x)) {
    return(as.integer(x))
  }

  if (is.factor(x) || is.character(x)) {
    xf <- factor(x)
    if (nlevels(xf) != 2L) {
      stop("`event` must have exactly two levels.", call. = FALSE)
    }
    return(as.integer(xf == levels(xf)[2L]))
  }

  if (is.numeric(x)) {
    vals <- sort(unique(stats::na.omit(x)))
    if (length(vals) > 0 && all(vals %in% c(0, 1))) {
      return(as.integer(x))
    }
    if (length(vals) > 0 && all(vals %in% c(1, 2))) {
      return(as.integer(x == 2))
    }
  }

  stop("`event` must be coded as 0/1, 1/2, logical, or a two-level factor/character variable.", call. = FALSE)
}

#' @keywords internal
#' @noRd
.fit_cox_model <- function(data,
                           time,
                           event,
                           predictors,
                           interaction = NULL,
                           baseline_strata = NULL,
                           time_start = NULL,
                           cluster = NULL,
                           weights = NULL,
                           robust = NULL,
                           ties = "efron",
                           control = NULL) {
  bt <- .surv_bt
  rhs <- .survival_rhs(predictors, interaction)
  if (length(baseline_strata)) {
    strata_term <- paste0(
      "strata(",
      paste(bt(baseline_strata), collapse = ", "),
      ")"
    )
    rhs <- paste(rhs, strata_term, sep = " + ")
  }
  response <- if (is.null(time_start)) {
    paste0("survival::Surv(", bt(time), ", ", bt(event), ")")
  } else {
    paste0(
      "survival::Surv(", bt(time_start), ", ", bt(time), ", ", bt(event), ")"
    )
  }
  fml <- stats::as.formula(paste0(response, " ~ ", rhs))

  args <- list(
    formula = fml,
    data = data,
    model = TRUE,
    x = TRUE,
    y = TRUE,
    ties = ties
  )
  if (!is.null(cluster)) args$cluster <- data[[cluster]]
  if (!is.null(weights)) args$weights <- data[[weights]]
  if (!is.null(robust)) args$robust <- robust
  if (!is.null(control)) args$control <- control

  tryCatch(
    do.call(survival::coxph, args),
    error = function(e) {
      warning("Cox model failed: ", e$message, call. = FALSE)
      NULL
    }
  )
}

#' @keywords internal
#' @noRd
.cox_control_arg <- function(control) {
  if (is.null(control)) {
    return(NULL)
  }
  if (!is.list(control) || is.null(names(control)) || any(!nzchar(names(control)))) {
    stop("`control` must be a named list of coxph.control() arguments.", call. = FALSE)
  }
  tryCatch(
    do.call(survival::coxph.control, control),
    error = function(e) {
      stop("Invalid `control`: ", conditionMessage(e), call. = FALSE)
    }
  )
}

#' @keywords internal
#' @noRd
.cox_analysis_note <- function(time_start,
                               cluster,
                               weights,
                               robust,
                               ties,
                               variable_labels) {
  label <- function(x) {
    value <- unname(variable_labels[x])
    if (!length(value) || is.na(value)) x else value
  }
  c(
    if (!is.null(time_start)) {
      paste0("Counting-process Cox model with entry time: ", label(time_start), ".")
    } else NULL,
    if (!is.null(cluster)) {
      paste0("Cluster-robust variance by ", label(cluster), ".")
    } else if (isTRUE(robust)) {
      "Robust sandwich variance used."
    } else NULL,
    if (!is.null(weights)) paste0("Case weights: ", label(weights), ".") else NULL,
    if (!identical(ties, "efron")) paste0("Ties method: ", ties, ".") else NULL
  )
}

#' @keywords internal
#' @noRd
.warn_cox_baseline_strata <- function(data,
                                      event,
                                      baseline_strata,
                                      model_name = NULL) {
  if (!length(baseline_strata)) {
    return(invisible(NULL))
  }

  strata <- interaction(data[baseline_strata], drop = TRUE, lex.order = TRUE)
  n_by_stratum <- table(strata)
  events_by_stratum <- tapply(data[[event]], strata, sum, na.rm = TRUE)
  sparse <- names(n_by_stratum)[n_by_stratum < 10L]
  no_events <- names(events_by_stratum)[events_by_stratum == 0]
  context <- if (is.null(model_name)) "" else {
    paste0(" for model '", model_name, "'")
  }

  details <- character(0)
  if (length(sparse)) {
    details <- c(
      details,
      paste0("fewer than 10 observations: ", paste(sparse, collapse = ", "))
    )
  }
  if (length(no_events)) {
    details <- c(
      details,
      paste0("no observed events: ", paste(no_events, collapse = ", "))
    )
  }
  if (length(details)) {
    warning(
      "Sparse baseline-hazard strata", context, " (",
      paste(details, collapse = "; "), "). Estimates may be unstable.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.cox_baseline_strata_note <- function(baseline_strata, variable_labels) {
  if (!length(baseline_strata)) {
    return(NULL)
  }
  labels <- unname(variable_labels[baseline_strata])
  labels[is.na(labels)] <- baseline_strata[is.na(labels)]
  paste0(
    "Baseline hazard stratified by ", paste(labels, collapse = ", "),
    ". No hazard ratio is estimated for ",
    if (length(labels) == 1L) "this variable." else "these variables."
  )
}

#' @keywords internal
#' @noRd
.tidy_cox <- function(fit, exposure, interaction = NULL) {
  smry <- summary(fit)
  coefs <- smry$coefficients
  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }

  rn <- rownames(coefs)
  term_map <- .coefficient_term_map(fit, rn)
  idx <- which(vapply(term_map, .term_contains_variable, logical(1),
                      variable = exposure))
  df_nonref <- NULL

  if (length(idx)) {
    est_log <- coefs[idx, "coef"]
    se_col <- if ("robust se" %in% colnames(coefs)) "robust se" else "se(coef)"
    se <- coefs[idx, se_col]
    p_col <- grep("^Pr\\(", colnames(coefs), value = TRUE)
    p <- coefs[idx, p_col[1]]
    z <- stats::qnorm(0.975)

    df_nonref <- data.frame(
      exposure = exposure,
      level = mapply(
        .survival_coefficient_display_level,
        coefficient = rn[idx],
        term = term_map[idx],
        MoreArgs = list(exposure = exposure, fit = fit),
        USE.NAMES = FALSE
      ),
      estimate = exp(est_log),
      conf.low = exp(est_log - z * se),
      conf.high = exp(est_log + z * se),
      p.value = p,
      ref = FALSE,
      stringsAsFactors = FALSE
    )
  }

  ref_row <- NULL
  exposure_data <- .model_data_col(fit, exposure)
  if (!is.null(exposure_data) && is.factor(exposure_data)) {
    levs <- levels(exposure_data)
    ref_row <- data.frame(
      exposure = exposure,
      level = levs[1],
      estimate = 1,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      ref = TRUE,
      stringsAsFactors = FALSE
    )

    if (!is.null(df_nonref)) {
      df_nonref$..ord <- match(df_nonref$level, levs)
      df_nonref <- df_nonref[order(df_nonref$..ord), , drop = FALSE]
      df_nonref$..ord <- NULL
    }
  }

  if (is.null(ref_row) && is.null(df_nonref)) {
    return(NULL)
  }
  if (!is.null(ref_row) && !is.null(df_nonref)) {
    return(rbind(ref_row, df_nonref))
  }
  if (!is.null(ref_row)) ref_row else df_nonref
}

#' Display label for a survival-model coefficient
#'
#' Main effects are delegated to the shared helper. Interaction coefficients
#' are split with the fitted factor levels instead of on every colon, so a
#' level such as `c:d` stays intact (`grpc:d x trtTest`).
#'
#' @keywords internal
#' @noRd
.survival_coefficient_display_level <- function(coefficient, term, exposure,
                                                fit = NULL) {
  fallback <- .coefficient_display_level(coefficient, term, exposure)
  if (is.null(fit) || is.na(term) || !grepl(":", coefficient, fixed = TRUE)) {
    return(fallback)
  }
  term_vars <- tryCatch(
    all.vars(stats::as.formula(paste("~", term))),
    error = function(e) character()
  )
  if (length(term_vars) < 2L) {
    return(fallback)
  }

  pieces <- lapply(term_vars, function(variable) {
    x <- .model_data_col(fit, variable)
    if (is.factor(x)) paste0(variable, levels(x)) else variable
  })
  target <- gsub("`", "", coefficient, fixed = TRUE)
  combos <- expand.grid(pieces, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  joined <- do.call(paste, c(unname(as.list(combos)), sep = ":"))
  hit <- which(joined == target)
  if (length(hit) != 1L) {
    return(fallback)
  }
  paste(unlist(combos[hit, , drop = TRUE], use.names = FALSE), collapse = " x ")
}

#' @keywords internal
#' @noRd
.surv_bt <- function(x) {
  paste0("`", gsub("`", "", x, fixed = TRUE), "`")
}

#' @keywords internal
#' @noRd
.interaction_vars <- function(interaction) {
  if (is.null(interaction) || !length(interaction)) {
    return(character(0))
  }
  vars <- trimws(unlist(strsplit(interaction, "\\*")))
  vars[nzchar(vars)]
}

#' @keywords internal
#' @noRd
.interaction_formula_term <- function(interaction) {
  vars <- .interaction_vars(interaction)
  if (!length(vars)) {
    return(NULL)
  }
  paste(.surv_bt(vars), collapse = " * ")
}

#' @keywords internal
#' @noRd
.survival_rhs <- function(predictors, interaction = NULL) {
  terms <- if (length(predictors)) .surv_bt(predictors) else character(0)
  if (!is.null(interaction)) {
    terms <- c(terms, .interaction_formula_term(interaction))
  }
  if (!length(terms)) {
    return("1")
  }
  paste(unique(terms), collapse = " + ")
}

#' @keywords internal
#' @noRd
.cox_model_stats_table <- function(models) {
  out <- lapply(names(models), function(model_name) {
    fit <- models[[model_name]]
    smry <- tryCatch(summary(fit), error = function(e) NULL)
    concordance <- if (!is.null(smry) && !is.null(smry$concordance)) {
      suppressWarnings(as.numeric(smry$concordance[1]))
    } else {
      NA_real_
    }

    data.frame(
      model = model_name,
      AIC = .safe_numeric(stats::AIC(fit)),
      BIC = .safe_numeric(stats::BIC(fit)),
      logLik = .safe_numeric(as.numeric(stats::logLik(fit))),
      concordance = .safe_numeric(concordance),
      events = .safe_numeric(fit$nevent),
      n = .safe_numeric(fit$n),
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}
