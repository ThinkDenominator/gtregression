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
#' @param show_sample For stratified Cox tables, controls which sample-size
#'   columns are shown in the publication table. One of \code{"events"}
#'   (default), \code{"n"}, \code{"both"}, or \code{"none"}. Model statistics,
#'   when requested, still retain both N and event counts.
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
#' The proportional hazards assumption should be assessed separately, for
#' example with \code{check_ph()}.
#'
#' Stratified Cox tables show event counts by default. Use
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
#'   \item{time,event,approach,format,source,adjust_for,exposures,interaction}{Metadata fields.}
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
#' # multivariate is accepted as an alias
#' cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, age, karno),
#'   multivariate = TRUE
#' )
#'
#' @importFrom survival Surv coxph
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
                    show_sample = "events",
                    model_stats = FALSE,
                    show_ref = TRUE) {

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
  theme <- .resolve_theme(theme)
  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  fmt_class <- if (format == "gt") "gt_cox" else "ft_cox"

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
      fmt_class = fmt_class
    ))
  }

  core <- .run_cox_core(
    data = data,
    time = time,
    event = event,
    exposures = exposures,
    adjust_for = adjust_for,
    interaction = interaction,
    multivariable = multivariable
  )

  effect_label <- if (adjusted_mode || isTRUE(multivariable)) "Adjusted HR (95% CI)" else "HR (95% CI)"
  variable_labels <- .var_label_map(data, unique(exposures))
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

  footnotes <- c(
    .abbrev_note("cox"),
    if (isTRUE(show_ref) && any(core$table_body$ref %in% TRUE)) .ref_note() else NULL,
    if (adjusted_mode) .adjustment_note(adjust_for) else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other variables in the model." else NULL,
    if (!is.null(interaction)) .interaction_note(interaction) else NULL,
    paste0(
      "Event variable: ", event,
      " (1 = event, 0 = censored after internal coding)."
    )
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
    time = time,
    event = event,
    approach = "cox",
    format = format,
    source = "cox_reg",
    adjusted_mode = adjusted_mode,
    multivariable = isTRUE(multivariable),
    adjust_for = if (adjusted_mode) unique(adjust_for) else NULL,
    exposures = unique(exposures),
    interaction = interaction
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
                          interaction = NULL,
                          multivariable = FALSE) {
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
    data_model <- .cox_model_data(data_valid, time, event, predictors)
    fit <- .fit_cox_model(data_model, time, event, predictors, interaction = interaction)

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
    data_model <- .cox_model_data(data_valid, time, event, predictors)
    fit <- .fit_cox_model(data_model, time, event, predictors, interaction = interaction)

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
                                 interaction = NULL,
                                 multivariable = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(time) || length(time) != 1L || !time %in% names(data)) {
    stop("`time` must be a single survival time variable in `data`.", call. = FALSE)
  }

  if (!is.character(event) || length(event) != 1L || !event %in% names(data)) {
    stop("`event` must be a single event indicator variable in `data`.", call. = FALSE)
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

  interaction_vars <- .validate_interaction_term(
    data = data,
    exposures = exposures,
    interaction = interaction,
    adjusted_mode = adjusted_mode,
    exposure_by_exposure = !isTRUE(multivariable)
  )

  if (!is.numeric(data[[time]])) {
    stop("`time` must be numeric.", call. = FALSE)
  }

  if (any(data[[time]] < 0, na.rm = TRUE)) {
    stop("`time` must contain non-negative follow-up times.", call. = FALSE)
  }

  data_valid <- data

  data_valid[[event]] <- .cox_event01(data_valid[[event]])
  if (sum(data_valid[[event]] == 1, na.rm = TRUE) == 0) {
    stop("`event` must include at least one event.", call. = FALSE)
  }
  if (sum(data_valid[[event]] == 0, na.rm = TRUE) == 0) {
    stop("`event` must include at least one censored observation.", call. = FALSE)
  }

  .validate_exposures(data_valid, unique(c(exposures, adjust_for, interaction_vars)))
  data_valid
}

#' @keywords internal
#' @noRd
.cox_model_data <- function(data, time, event, predictors) {
  vars_needed <- unique(c(time, event, predictors))
  cc_idx <- stats::complete.cases(data[, vars_needed, drop = FALSE])
  data_model <- data[cc_idx, , drop = FALSE]

  if (nrow(data_model) == 0) {
    stop("No complete cases available for this Cox model.", call. = FALSE)
  }

  if (sum(data_model[[event]] == 1, na.rm = TRUE) == 0) {
    stop("`event` must include at least one event for this Cox model.", call. = FALSE)
  }
  if (sum(data_model[[event]] == 0, na.rm = TRUE) == 0) {
    stop("`event` must include at least one censored observation for this Cox model.", call. = FALSE)
  }

  .validate_exposures(data_model, predictors)
  data_model
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
.fit_cox_model <- function(data, time, event, predictors, interaction = NULL) {
  bt <- .surv_bt
  rhs <- .survival_rhs(predictors, interaction)
  fml <- stats::as.formula(paste0("survival::Surv(", bt(time), ", ", bt(event), ") ~ ", rhs))

  tryCatch(
    survival::coxph(fml, data = data, model = TRUE),
    error = function(e) {
      warning("Cox model failed: ", e$message, call. = FALSE)
      NULL
    }
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

  is_exposure_term <- function(term, exposure) {
    term_clean <- gsub("`", "", term, fixed = TRUE)
    exp_clean <- gsub("`", "", exposure, fixed = TRUE)
    if (identical(term_clean, exp_clean) ||
        (startsWith(term_clean, exp_clean) && !grepl(":", term_clean, fixed = TRUE))) {
      return(TRUE)
    }
    if (grepl(":", term_clean, fixed = TRUE) && !is.null(interaction)) {
      parts <- strsplit(term_clean, ":", fixed = TRUE)[[1]]
      return(any(startsWith(parts, exp_clean) | parts == exp_clean))
    }
    FALSE
  }

  clean_level <- function(term, exposure) {
    term_clean <- gsub("`", "", term, fixed = TRUE)
    exp_clean <- gsub("`", "", exposure, fixed = TRUE)
    if (identical(term_clean, exp_clean)) {
      return(exp_clean)
    }
    if (grepl(":", term_clean, fixed = TRUE)) {
      return(paste(strsplit(term_clean, ":", fixed = TRUE)[[1]], collapse = " x "))
    }
    lvl <- sub(paste0("^", exp_clean), "", term_clean)
    if (identical(lvl, "")) exp_clean else lvl
  }

  idx <- which(vapply(rn, is_exposure_term, logical(1), exposure = exposure))
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
      level = vapply(rn[idx], clean_level, character(1), exposure = exposure),
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
