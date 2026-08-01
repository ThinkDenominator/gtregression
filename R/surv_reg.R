#' Parametric survival regression
#'
#' Fit parametric survival models and report time ratios.
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
#'   supplied, one adjusted model is fitted per exposure.
#' @param stratifier Optional single stratifying variable. When supplied,
#'   stratum-specific parametric survival tables are produced using the same
#'   crude, adjusted, or multivariable workflow requested by the other
#'   arguments. The stratifier cannot also be used as the time, event,
#'   exposure, adjustment, or interaction variable.
#' @param interaction Optional character scalar specifying one interaction term
#'   using standard formula syntax, e.g. \code{"trt*prior"}. Quoted and bare
#'   interaction syntax are accepted. In exposure-by-exposure mode, supply a
#'   single exposure; in \code{multivariable = TRUE} mode, the interaction is
#'   added to the single multivariable model.
#' @param multivariable Logical; if \code{FALSE} (default), the current
#'   exposure-by-exposure workflow is used. If \code{TRUE}, one multivariable
#'   parametric survival model is fitted using all variables in
#'   \code{exposures}, and all exposure coefficients are reported.
#' @param multivariate Optional logical alias for \code{multivariable}. This is
#'   accepted for convenience; \code{multivariable} is used internally.
#' @param distribution Parametric survival distribution. One of
#'   \code{"weibull"}, \code{"exponential"}, \code{"lognormal"}, or
#'   \code{"loglogistic"}. Quoted and bare values are accepted. Common
#'   spellings such as \code{"log-normal"} and \code{"log-logistic"} are also
#'   accepted.
#' @param format Output table format; one of \code{"flextable"} (default) or
#'   \code{"gt"}.
#' @param theme Table styling preset.
#' @param show_sample For stratified parametric survival tables, controls which
#'   sample-size columns are shown in the publication table. One of
#'   \code{"events"} (default), \code{"n"}, \code{"both"}, or \code{"none"}.
#'   Model statistics, when requested, still retain both N and event counts.
#' @param model_stats Logical; if \code{TRUE}, extract model-fit statistics
#'   including AIC, BIC, log-likelihood, scale, number of events, and N.
#'
#' @details
#' \code{surv_reg()} fits accelerated failure time style parametric survival
#' models using \code{survival::survreg()}. The exponentiated coefficient is
#' displayed as a time ratio. A time ratio above 1 suggests longer survival time;
#' a time ratio below 1 suggests shorter survival time, conditional on the chosen
#' distribution.
#'
#' By default, \code{surv_reg()} keeps the exposure-by-exposure workflow:
#' without \code{adjust_for}, one crude model is fitted per exposure; with
#' \code{adjust_for}, one adjusted model is fitted per exposure and only the
#' exposure estimate is reported.
#'
#' With \code{multivariable = TRUE}, all variables in \code{exposures} are
#' included in one parametric survival model and all coefficients are reported.
#' Since these estimates are adjusted for the other variables in the same model,
#' the table reports \code{Adjusted Time Ratio (95\% CI)}. The
#' \code{adjust_for} argument is not used in this mode; include every variable
#' that belongs in the model inside \code{exposures}.
#'
#' Interaction terms specified via \code{interaction} are included using
#' standard formula expansion (for example, \code{trt*prior}). Interaction
#' effects are displayed as additional rows beneath the corresponding exposure.
#'
#' Stratified parametric survival tables show event counts by default. Use
#' \code{show_sample = "n"}, \code{show_sample = "both"}, or
#' \code{show_sample = "none"} to control the displayed sample columns. Crude
#' stratified tables calculate these counts for each exposure-specific model;
#' adjusted and multivariable stratified tables use the corresponding fitted
#' model within each stratum.
#'
#' If exposure variables have a \code{"label"} attribute, for example from
#' \code{labelled::var_label()}, those labels are used automatically in the
#' displayed table.
#'
#' @return A list of class \code{c("gtregression","surv_reg", ...)} with elements:
#' \describe{
#'   \item{table}{A \code{flextable} or \code{gt_tbl}.}
#'   \item{table_body}{Data frame of time ratios and confidence intervals.}
#'   \item{table_display}{Data frame used to render the publication table.}
#'   \item{models}{List of fitted \code{survreg} models.}
#'   \item{model_summaries}{Summary output for the fitted models.}
#'   \item{model_stats}{Model-fit statistics when \code{model_stats = TRUE};
#'   otherwise \code{NULL}.}
#'   \item{variable_labels}{Named character vector of display labels.}
#'   \item{time,event,distribution,approach,format,source,adjust_for,exposures,interaction}{Metadata fields.}
#' }
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#' lung_data$prior <- factor(lung_data$prior, levels = c(0, 10),
#'                           labels = c("No", "Yes"))
#'
#' surv_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c("trt", "celltype", "karno", "age")
#' )
#'
#' surv_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, celltype, prior),
#'   adjust_for = c(age, karno),
#'   distribution = lognormal
#' )
#'
#' # Interaction in an adjusted exposure model
#' surv_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = trt,
#'   adjust_for = c(age, karno),
#'   interaction = trt*prior,
#'   distribution = weibull
#' )
#'
#' surv_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, celltype, prior, age, karno),
#'   distribution = weibull,
#'   multivariable = TRUE
#' )
#'
#' # multivariate is accepted as an alias
#' surv_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, age, karno),
#'   multivariate = TRUE
#' )
#'
#' @importFrom survival Surv survreg
#' @export
surv_reg <- function(data,
                     time,
                     event,
                     exposures,
                     adjust_for = NULL,
                     stratifier = NULL,
                     interaction = NULL,
                     multivariable = FALSE,
                     multivariate = NULL,
                     distribution = "weibull",
                     format = c("flextable", "gt"),
                     theme = c("minimal"),
                     show_sample = "events",
                     model_stats = FALSE) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  adjust_for <- .vars_arg(substitute(adjust_for), env = parent.frame(), allow_null = TRUE)
  stratifier <- .survival_optional_single_var_arg(
    substitute(stratifier),
    data = data,
    env = parent.frame()
  )
  interaction <- .interaction_arg(substitute(interaction), env = parent.frame(), allow_null = TRUE)
  distribution <- .surv_distribution_arg(
    substitute(distribution),
    env = parent.frame(),
    multiple = FALSE,
    arg = "distribution"
  )
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("flextable", "gt"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  show_sample <- .choice_arg(
    substitute(show_sample),
    env = parent.frame(),
    choices = c("events", "n", "both", "none")
  )

  if (!is.logical(model_stats) || length(model_stats) != 1L || is.na(model_stats)) {
    stop("`model_stats` must be TRUE or FALSE.", call. = FALSE)
  }
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
  theme <- .resolve_theme(theme)
  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  fmt_class <- if (format == "gt") "gt_surv" else "ft_surv"

  if (!is.null(stratifier)) {
    .validate_survival_stratifier(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      adjust_for = adjust_for,
      interaction = interaction,
      stratifier = stratifier
    )

    return(.run_stratified_surv_reg(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      adjust_for = adjust_for,
      stratifier = stratifier,
      interaction = interaction,
      multivariable = multivariable,
      distribution = distribution,
      format = format,
      theme = theme,
      show_sample = show_sample,
      model_stats = model_stats,
      fmt_class = fmt_class
    ))
  }

  core <- .run_surv_core(
    data = data,
    time = time,
    event = event,
    exposures = exposures,
    adjust_for = adjust_for,
    interaction = interaction,
    multivariable = multivariable,
    distribution = distribution
  )

  effect_label <- if (adjusted_mode || isTRUE(multivariable)) {
    "Adjusted Time Ratio (95% CI)"
  } else {
    "Time Ratio (95% CI)"
  }

  variable_labels <- .var_label_map(data, unique(exposures))
  crude_mode <- !adjusted_mode && !isTRUE(multivariable)

  if (crude_mode) {
    display_df <- .make_display_survival_uni(
      core$table_body,
      core$data_clean,
      time = time,
      event = event,
      effect_label = effect_label,
      variable_labels = variable_labels
    )
    .must_be_display_df(display_df)
  } else {
    display_df <- .make_display_multi(
      core$table_body,
      core$data_clean,
      outcome = event,
      effect_label = effect_label,
      variable_labels = variable_labels
    )
    .must_be_display_df_multi(display_df)
  }

  footnotes <- c(
    .abbrev_note("survreg"),
    paste0("Distribution: ", distribution, "."),
    if (any(core$table_body$ref %in% TRUE)) .ref_note() else NULL,
    if (adjusted_mode) .adjustment_note(adjust_for) else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other variables in the model." else NULL,
    if (!is.null(interaction)) .interaction_note(interaction) else NULL,
    paste0(
      "Event variable: ", event,
      " (1 = event, 0 = censored after internal coding)."
    )
  )

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
    model_stats = if (isTRUE(model_stats)) .surv_model_stats_table(core$models) else NULL,
    variable_labels = variable_labels,
    time = time,
    event = event,
    distribution = distribution,
    approach = "survreg",
    format = format,
    source = "surv_reg",
    adjusted_mode = adjusted_mode,
    multivariable = isTRUE(multivariable),
    adjust_for = if (adjusted_mode) unique(adjust_for) else NULL,
    exposures = unique(exposures),
    interaction = interaction
  )

  class(res) <- c("gtregression", "surv_reg", fmt_class, class(res))
  res
}

#' @keywords internal
#' @noRd
.run_surv_core <- function(data,
                           time,
                           event,
                           exposures,
                           adjust_for = NULL,
                           interaction = NULL,
                           multivariable = FALSE,
                           distribution = "weibull") {
  data_valid <- .validate_cox_inputs(
    data,
    time,
    event,
    exposures,
    adjust_for,
    interaction = interaction,
    multivariable = multivariable
  )
  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  interaction_vars <- .interaction_vars(interaction)

  if (isTRUE(multivariable)) {
    predictors <- unique(c(exposures, interaction_vars))
    data_model <- .surv_model_data(data_valid, time, event, predictors)
    fit <- .fit_surv_model(data_model, time, event, predictors, distribution, interaction = interaction)

    if (is.null(fit)) {
      stop("Parametric survival model fitting failed for the multivariable model.", call. = FALSE)
    }

    td_list <- lapply(unique(exposures), function(exposure) .tidy_survreg(fit, exposure, interaction = interaction))
    td_list <- Filter(Negate(is.null), td_list)
    if (!length(td_list)) {
      stop("No estimable survival coefficients for the multivariable model.", call. = FALSE)
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
    data_model <- .surv_model_data(data_valid, time, event, predictors)
    fit <- .fit_surv_model(data_model, time, event, predictors, distribution, interaction = interaction)

    if (is.null(fit)) {
      stop("Parametric survival model fitting failed for exposure '", exposure, "'.", call. = FALSE)
    }

    td <- .tidy_survreg(fit, exposure, interaction = interaction)
    if (is.null(td) || !nrow(td)) {
      stop("No estimable survival coefficients for exposure '", exposure, "'.", call. = FALSE)
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
.surv_model_data <- function(data, time, event, predictors) {
  vars_needed <- unique(c(time, event, predictors))
  cc_idx <- stats::complete.cases(data[, vars_needed, drop = FALSE])
  data_model <- data[cc_idx, , drop = FALSE]

  if (nrow(data_model) == 0) {
    stop("No complete cases available for this parametric survival model.", call. = FALSE)
  }

  if (sum(data_model[[event]] == 1, na.rm = TRUE) == 0) {
    stop("`event` must include at least one event for this parametric survival model.", call. = FALSE)
  }
  if (sum(data_model[[event]] == 0, na.rm = TRUE) == 0) {
    stop("`event` must include at least one censored observation for this parametric survival model.", call. = FALSE)
  }

  .validate_exposures(data_model, predictors)
  data_model
}

#' @keywords internal
#' @noRd
.fit_surv_model <- function(data, time, event, predictors, distribution, interaction = NULL) {
  bt <- .surv_bt
  rhs <- .survival_rhs(predictors, interaction)
  fml <- stats::as.formula(paste0("survival::Surv(", bt(time), ", ", bt(event), ") ~ ", rhs))

  tryCatch(
    {
      fit <- survival::survreg(fml, data = data, dist = distribution, model = TRUE)
      attr(fit, "gtregression_events") <- sum(data[[event]] == 1, na.rm = TRUE)
      attr(fit, "gtregression_distribution") <- distribution
      fit
    },
    error = function(e) {
      warning("Parametric survival model failed: ", e$message, call. = FALSE)
      NULL
    }
  )
}

#' @keywords internal
#' @noRd
.tidy_survreg <- function(fit, exposure, interaction = NULL) {
  smry <- summary(fit)
  coefs <- smry$table
  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }

  rn <- rownames(coefs)
  rn_clean <- gsub("`", "", rn, fixed = TRUE)
  exposure_clean <- gsub("`", "", exposure, fixed = TRUE)

  keep <- rn_clean != "(Intercept)" &
    rn_clean != "Log(scale)" &
    (rn_clean == exposure_clean |
       (startsWith(rn_clean, exposure_clean) & !grepl(":", rn_clean, fixed = TRUE)) |
       (!is.null(interaction) &
          grepl(":", rn_clean, fixed = TRUE) &
          vapply(strsplit(rn_clean, ":", fixed = TRUE),
                 function(parts) any(startsWith(parts, exposure_clean) | parts == exposure_clean),
                 logical(1))))
  idx <- which(keep)

  df_nonref <- NULL
  if (length(idx)) {
    est_log <- coefs[idx, "Value"]
    se <- coefs[idx, "Std. Error"]
    p_col <- grep("^p$|^Pr\\(", colnames(coefs), value = TRUE)
    p <- if (length(p_col)) coefs[idx, p_col[1]] else NA_real_
    z <- stats::qnorm(0.975)

    levels_out <- vapply(rn_clean[idx], function(term) {
      if (grepl(":", term, fixed = TRUE)) {
        return(paste(strsplit(term, ":", fixed = TRUE)[[1]], collapse = " x "))
      }
      lvl <- sub(paste0("^", exposure_clean), "", term)
      if (identical(lvl, "")) exposure else lvl
    }, character(1))

    df_nonref <- data.frame(
      exposure = exposure,
      level = levels_out,
      estimate = exp(est_log),
      conf.low = exp(est_log - z * se),
      conf.high = exp(est_log + z * se),
      p.value = p,
      ref = FALSE,
      stringsAsFactors = FALSE
    )
  }

  ref_row <- NULL
  if (!is.null(fit$model[[exposure]]) && is.factor(fit$model[[exposure]])) {
    levs <- levels(fit$model[[exposure]])
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

#' @keywords internal
#' @noRd
.surv_model_stats_table <- function(models) {
  out <- lapply(names(models), function(model_name) {
    fit <- models[[model_name]]
    data.frame(
      model = model_name,
      distribution = attr(fit, "gtregression_distribution", exact = TRUE),
      AIC = .safe_numeric(stats::AIC(fit)),
      BIC = .safe_numeric(stats::BIC(fit)),
      logLik = .safe_numeric(as.numeric(stats::logLik(fit))),
      scale = .safe_numeric(fit$scale),
      events = .safe_numeric(attr(fit, "gtregression_events", exact = TRUE)),
      n = .safe_numeric(stats::nobs(fit)),
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}
