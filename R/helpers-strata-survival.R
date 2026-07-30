#' Validate a survival stratifier argument
#' @keywords internal
#' @noRd
.survival_optional_single_var_arg <- function(expr, data, env = parent.frame()) {
  if (is.null(expr) || identical(expr, quote(NULL))) {
    return(NULL)
  }

  .cox_single_var_arg(expr, data = data, env = env)
}

#' Validate a survival stratifier argument
#' @keywords internal
#' @noRd
.validate_survival_stratifier <- function(data,
                                          time,
                                          event,
                                          exposures,
                                          adjust_for = NULL,
                                          interaction = NULL,
                                          stratifier = NULL) {
  if (is.null(stratifier)) {
    return(invisible(NULL))
  }

  if (!is.character(stratifier) || length(stratifier) != 1L || !nzchar(stratifier)) {
    stop("`stratifier` must be a single variable.", call. = FALSE)
  }

  if (!stratifier %in% names(data)) {
    stop("`stratifier` was not found in `data`.", call. = FALSE)
  }

  interaction_vars <- .interaction_vars(interaction)
  blocked <- unique(c(time, event, exposures, adjust_for, interaction_vars))
  if (stratifier %in% blocked) {
    stop(
      "`stratifier` cannot also be used as `time`, `event`, an exposure, ",
      "an adjustment variable, or an interaction variable.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Run stratified Cox models and build a wide stratified table
#' @keywords internal
#' @noRd
.run_stratified_cox_reg <- function(data,
                                    time,
                                    event,
                                    exposures,
                                    adjust_for = NULL,
                                    stratifier,
                                    interaction = NULL,
                                    multivariable = FALSE,
                                    format = "flextable",
                                    theme = "minimal",
                                    model_stats = FALSE,
                                    fmt_class = "ft_cox") {
  .run_stratified_survival_reg(
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
    model_stats = model_stats,
    fmt_class = fmt_class,
    approach = "cox",
    source = "stratified_cox_reg",
    object_class = "cox_reg",
    run_core = .run_cox_core,
    stats_fun = .cox_model_stats_table,
    effect_label = if (!is.null(adjust_for) && length(adjust_for) > 0 || isTRUE(multivariable)) {
      "Adjusted HR (95% CI)"
    } else {
      "HR (95% CI)"
    },
    extra_footnotes = NULL
  )
}

#' Run stratified parametric survival models and build a wide stratified table
#' @keywords internal
#' @noRd
.run_stratified_surv_reg <- function(data,
                                     time,
                                     event,
                                     exposures,
                                     adjust_for = NULL,
                                     stratifier,
                                     interaction = NULL,
                                     multivariable = FALSE,
                                     distribution = "weibull",
                                     format = "flextable",
                                     theme = "minimal",
                                     model_stats = FALSE,
                                     fmt_class = "ft_surv") {
  run_core <- function(data, time, event, exposures, adjust_for, interaction, multivariable) {
    .run_surv_core(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      adjust_for = adjust_for,
      interaction = interaction,
      multivariable = multivariable,
      distribution = distribution
    )
  }

  .run_stratified_survival_reg(
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
    model_stats = model_stats,
    fmt_class = fmt_class,
    approach = "survreg",
    source = "stratified_surv_reg",
    object_class = "surv_reg",
    run_core = run_core,
    stats_fun = .surv_model_stats_table,
    effect_label = if (!is.null(adjust_for) && length(adjust_for) > 0 || isTRUE(multivariable)) {
      "Adjusted Time Ratio (95% CI)"
    } else {
      "Time Ratio (95% CI)"
    },
    extra_footnotes = paste0("Distribution: ", distribution, "."),
    distribution = distribution
  )
}

#' Shared stratified survival model runner
#' @keywords internal
#' @noRd
.run_stratified_survival_reg <- function(data,
                                         time,
                                         event,
                                         exposures,
                                         adjust_for = NULL,
                                         stratifier,
                                         interaction = NULL,
                                         multivariable = FALSE,
                                         format = "flextable",
                                         theme = "minimal",
                                         model_stats = FALSE,
                                         fmt_class,
                                         approach,
                                         source,
                                         object_class,
                                         run_core,
                                         stats_fun,
                                         effect_label,
                                         extra_footnotes = NULL,
                                         distribution = NULL) {
  message("Running stratified survival regression by: ", stratifier)

  levs <- .strata_levels(data, stratifier)
  if (!length(levs)) {
    stop("`stratifier` has no non-missing strata.", call. = FALSE)
  }

  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  variable_labels <- .var_label_map(data, unique(exposures))
  per_stratum <- list()
  tds <- list()
  models <- list()
  sums <- list()
  stats_by_stratum <- list()
  n_by_stratum <- list()

  for (lv in levs) {
    message("  > Stratum: ", stratifier, " = ", lv)

    dlev <- data[data[[stratifier]] == lv, , drop = FALSE]
    key <- as.character(lv)

    res_i <- tryCatch(
      run_core(
        data = dlev,
        time = time,
        event = event,
        exposures = exposures,
        adjust_for = adjust_for,
        interaction = interaction,
        multivariable = multivariable
      ),
      error = function(e) {
        warning("Skipping stratum ", lv, ": ", e$message, call. = FALSE)
        NULL
      }
    )

    if (is.null(res_i)) {
      next
    }

    tds[[key]] <- res_i$table_body
    models[[key]] <- res_i$models
    sums[[key]] <- res_i$model_summaries
    stats_by_stratum[[key]] <- if (isTRUE(model_stats)) stats_fun(res_i$models) else NULL
    n_by_stratum[[key]] <- .stratified_survival_n_used(res_i$models)

    per_stratum[[key]] <- list(
      table_body = res_i$table_body,
      models = res_i$models,
      model_summaries = res_i$model_summaries,
      model_stats = stats_by_stratum[[key]],
      adjusted_mode = adjusted_mode,
      multivariable = isTRUE(multivariable)
    )
  }

  if (!length(tds)) {
    stop("No valid models across strata.", call. = FALSE)
  }

  if (adjusted_mode || isTRUE(multivariable)) {
    built <- .strata_build_wide_multi(
      data = data,
      exposures = exposures,
      stratifier = stratifier,
      td_by_stratum = tds,
      variable_labels = variable_labels
    )
    wide <- .strata_add_survival_counts(built$wide, models)
    spanners <- built$spanners
    tbl <- if (format == "gt") {
      .build_gt_strata_wide_multi(wide, spanners, effect_label, theme, .stratified_survival_footnotes(
        approach = approach,
        tds = tds,
        event = event,
        adjusted_mode = adjusted_mode,
        adjust_for = adjust_for,
        multivariable = multivariable,
        interaction = interaction,
        stratifier = stratifier,
        n_by_stratum = n_by_stratum,
        extra_footnotes = extra_footnotes
      ))
    } else {
      .build_flex_strata_wide_multi(wide, spanners, effect_label, theme, .stratified_survival_footnotes(
        approach = approach,
        tds = tds,
        event = event,
        adjusted_mode = adjusted_mode,
        adjust_for = adjust_for,
        multivariable = multivariable,
        interaction = interaction,
        stratifier = stratifier,
        n_by_stratum = n_by_stratum,
        extra_footnotes = extra_footnotes
      ))
    }
  } else {
    per_for_uni <- lapply(per_stratum, function(x) list(table_body = x$table_body))
    built <- .strata_build_wide_survival_uni(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      stratifier = stratifier,
      per_stratum = per_for_uni,
      variable_labels = variable_labels
    )
    wide <- built$wide
    spanners <- built$spanners
    tbl <- if (format == "gt") {
      .build_gt_strata_wide_uni(wide, spanners, effect_label, theme, .stratified_survival_footnotes(
        approach = approach,
        tds = tds,
        event = event,
        adjusted_mode = adjusted_mode,
        adjust_for = adjust_for,
        multivariable = multivariable,
        interaction = interaction,
        stratifier = stratifier,
        n_by_stratum = n_by_stratum,
        extra_footnotes = extra_footnotes
      ))
    } else {
      .build_flex_strata_wide_uni(wide, spanners, effect_label, theme, .stratified_survival_footnotes(
        approach = approach,
        tds = tds,
        event = event,
        adjusted_mode = adjusted_mode,
        adjust_for = adjust_for,
        multivariable = multivariable,
        interaction = interaction,
        stratifier = stratifier,
        n_by_stratum = n_by_stratum,
        extra_footnotes = extra_footnotes
      ))
    }
  }

  model_stats_out <- if (isTRUE(model_stats)) {
    .bind_stratified_model_stats(stats_by_stratum)
  } else {
    NULL
  }

  out <- list(
    table = tbl,
    table_display = wide,
    per_stratum = per_stratum,
    models = models,
    model_summaries = sums,
    model_stats = model_stats_out,
    variable_labels = variable_labels,
    time = time,
    event = event,
    distribution = distribution,
    by = stratifier,
    levels = names(tds),
    approach = approach,
    format = format,
    source = source,
    stratified = TRUE,
    adjusted_mode = adjusted_mode,
    multivariable = isTRUE(multivariable),
    adjust_for = if (adjusted_mode) unique(adjust_for) else NULL,
    exposures = unique(exposures),
    interaction = interaction
  )

  class(out) <- c("gtregression", source, object_class, fmt_class, class(out))
  out
}

#' Pull crude stratified survival columns aligned to a skeleton
#' @keywords internal
#' @noRd
.strata_pull_cols_survival_uni <- function(dlev,
                                           uni_res,
                                           skeleton,
                                           is_factor,
                                           exposures,
                                           time,
                                           event) {
  td <- uni_res$table_body

  N_map <- vapply(exposures, function(x) {
    sum(stats::complete.cases(dlev[, c(time, event, x), drop = FALSE]))
  }, integer(1))
  names(N_map) <- exposures

  Events_map <- vapply(exposures, function(x) {
    complete <- stats::complete.cases(dlev[, c(time, event, x), drop = FALSE])
    if (!any(complete)) {
      return(0L)
    }
    sum(.cox_event01(dlev[[event]][complete]) == 1L, na.rm = TRUE)
  }, integer(1))
  names(Events_map) <- exposures

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(z) formatC(z, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
  }

  N_vec <- character(nrow(skeleton))
  events_vec <- character(nrow(skeleton))
  eff_vec <- character(nrow(skeleton))
  p_vec <- character(nrow(skeleton))

  for (i in seq_len(nrow(skeleton))) {
    ex <- skeleton$exposure[i]
    lv <- skeleton$level[i]
    hdr <- skeleton$is_header[i]

    if (hdr) {
      N_vec[i] <- as.character(N_map[[ex]])
      events_vec[i] <- as.character(Events_map[[ex]])
      if (!is_factor[[ex]]) {
        row <- td[td$exposure == ex & td$level == ex, , drop = FALSE]
        if (nrow(row)) {
          eff_vec[i] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
          p_vec[i] <- .fmt_p(row$p.value)
        }
      }
    } else {
      row <- td[td$exposure == ex & td$level == lv, , drop = FALSE]
      if (nrow(row)) {
        if (isTRUE(row$ref[1])) {
          eff_vec[i] <- "Ref."
          p_vec[i] <- ""
        } else {
          eff_vec[i] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
          p_vec[i] <- .fmt_p(row$p.value)
        }
      }
    }
  }

  list(N = N_vec, Events = events_vec, effect = eff_vec, pval = p_vec)
}

#' Assemble wide display data for crude stratified survival models
#' @keywords internal
#' @noRd
.strata_build_wide_survival_uni <- function(data,
                                            time,
                                            event,
                                            exposures,
                                            stratifier,
                                            per_stratum,
                                            variable_labels = NULL) {
  sk <- .strata_build_skeleton(data, exposures, variable_labels = variable_labels)
  skeleton <- sk$skeleton
  is_factor <- sk$is_factor

  wide <- skeleton[, c("Characteristic", "is_header"), drop = FALSE]
  wide <- .attach_display_metadata(
    wide,
    row_exposure = skeleton$exposure,
    variable_labels = attr(skeleton, "variable_labels", exact = TRUE)
  )

  for (lev in names(per_stratum)) {
    dlev <- data[data[[stratifier]] == lev, , drop = FALSE]
    cols <- .strata_pull_cols_survival_uni(
      dlev = dlev,
      uni_res = per_stratum[[lev]],
      skeleton = skeleton,
      is_factor = is_factor,
      exposures = exposures,
      time = time,
      event = event
    )
    wide[[paste0("..N__", lev)]] <- cols$N
    wide[[paste0("..Events__", lev)]] <- cols$Events
    wide[[paste0("..eff__", lev)]] <- cols$effect
    wide[[paste0("..p__", lev)]] <- cols$pval
  }

  list(wide = wide, spanners = paste0(stratifier, " = ", names(per_stratum)))
}

#' Survival footnotes for stratified Cox and survreg output
#' @keywords internal
#' @noRd
.stratified_survival_footnotes <- function(approach,
                                           tds,
                                           event,
                                           adjusted_mode,
                                           adjust_for = NULL,
                                           multivariable = FALSE,
                                           interaction = NULL,
                                           stratifier,
                                           n_by_stratum,
                                           extra_footnotes = NULL) {
  c(
    .abbrev_note(approach),
    extra_footnotes,
    if (any(unlist(lapply(tds, function(x) x$ref %in% TRUE), use.names = FALSE))) .ref_note() else NULL,
    if (isTRUE(adjusted_mode)) .adjustment_note(adjust_for) else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other variables in the model." else NULL,
    if (!is.null(interaction)) .interaction_note(interaction) else NULL,
    .n_note_multi_strata(stratifier, n_by_stratum),
    paste0("Event variable: ", event, " (1 = event, 0 = censored after internal coding).")
  )
}

#' Extract a compact model N summary for stratified survival footnotes
#' @keywords internal
#' @noRd
.stratified_survival_n_used <- function(models) {
  n_vals <- vapply(models, function(fit) {
    tryCatch(as.numeric(stats::nobs(fit)), error = function(e) NA_real_)
  }, numeric(1))

  n_vals <- stats::na.omit(n_vals)
  if (!length(n_vals)) {
    return(NA_integer_)
  }

  if (length(unique(n_vals)) == 1L) {
    return(as.integer(n_vals[1]))
  }

  paste0(min(n_vals), "-", max(n_vals))
}

#' Add model N and event count columns to a stratified survival display table
#' @keywords internal
#' @noRd
.strata_add_survival_counts <- function(wide, models_by_stratum) {
  wide <- .strata_add_model_n(wide, models_by_stratum)

  if (!length(models_by_stratum) || !"is_header" %in% names(wide)) {
    return(wide)
  }

  row_exposure <- attr(wide, "row_exposure", exact = TRUE)
  if (is.null(row_exposure)) {
    return(wide)
  }

  is_header <- wide$is_header %in% TRUE

  for (lev in names(models_by_stratum)) {
    models <- models_by_stratum[[lev]]
    if (!length(models)) {
      next
    }

    events_vec <- character(nrow(wide))
    model_names <- names(models)
    has_single_model <- identical(model_names, "multivariable_model")

    for (i in seq_len(nrow(wide))) {
      if (!isTRUE(is_header[i])) {
        next
      }

      fit <- if (has_single_model) {
        models[["multivariable_model"]]
      } else {
        models[[row_exposure[i]]]
      }

      if (!is.null(fit)) {
        events_vec[i] <- .strata_survival_event_count(fit)
      }
    }

    wide[[paste0("..Events__", lev)]] <- events_vec
  }

  wide
}

#' Extract event counts from Cox or parametric survival fits
#' @keywords internal
#' @noRd
.strata_survival_event_count <- function(fit) {
  events <- NULL

  if (inherits(fit, "coxph") && !is.null(fit$nevent)) {
    events <- fit$nevent
  }

  if (is.null(events)) {
    events <- attr(fit, "gtregression_events", exact = TRUE)
  }

  if (is.null(events)) {
    y <- tryCatch(stats::model.response(stats::model.frame(fit)), error = function(e) NULL)
    if (inherits(y, "Surv")) {
      events <- sum(y[, ncol(y)] == 1L, na.rm = TRUE)
    }
  }

  if (is.null(events) || !is.finite(as.numeric(events))) {
    return("")
  }

  as.character(as.integer(events))
}

#' Bind model statistics with a stratum column
#' @keywords internal
#' @noRd
.bind_stratified_model_stats <- function(stats_by_stratum) {
  stats_by_stratum <- Filter(Negate(is.null), stats_by_stratum)
  if (!length(stats_by_stratum)) {
    return(NULL)
  }

  out <- Map(function(x, nm) {
    cbind(stratum = nm, x, stringsAsFactors = FALSE)
  }, stats_by_stratum, names(stats_by_stratum))

  do.call(rbind, out)
}
