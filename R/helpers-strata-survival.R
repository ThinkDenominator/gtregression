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
                                    show_sample = "both",
                                    model_stats = FALSE,
                                    show_ref = TRUE,
                                    fmt_class = "ft_cox",
                                    event_note = NULL,
                                    time_start = NULL,
                                    cluster = NULL,
                                    weights = NULL,
                                    robust = NULL,
                                    ties = "efron",
                                    control = NULL) {
  run_core <- function(data, time, event, exposures, adjust_for,
                       interaction, multivariable) {
    .run_cox_core(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      adjust_for = adjust_for,
      interaction = interaction,
      multivariable = multivariable,
      time_start = time_start,
      cluster = cluster,
      weights = weights,
      robust = robust,
      ties = ties,
      control = control
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
    show_sample = show_sample,
    model_stats = model_stats,
    show_ref = show_ref,
    fmt_class = fmt_class,
    approach = "cox",
    source = "stratified_cox_reg",
    object_class = "cox_reg",
    run_core = run_core,
    stats_fun = .cox_model_stats_table,
    effect_label = if (!is.null(adjust_for) && length(adjust_for) > 0 || isTRUE(multivariable)) {
      "Adjusted HR (95% CI)"
    } else {
      "HR (95% CI)"
    },
    extra_footnotes = c(
      .cox_analysis_note(
        time_start,
        cluster,
        weights,
        robust,
        ties,
        .var_label_map(data, unique(c(time_start, cluster, weights)))
      ),
      event_note
    ),
    time_start = time_start,
    cluster = cluster,
    weights = weights,
    robust = robust,
    ties = ties,
    control = control
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
                                     show_sample = "both",
                                     model_stats = FALSE,
                                     show_ref = TRUE,
                                     fmt_class = "ft_surv",
                                     event_note = NULL) {
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
    show_sample = show_sample,
    model_stats = model_stats,
    show_ref = show_ref,
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
    extra_footnotes = c(paste0("Distribution: ", distribution, "."), event_note),
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
                                         show_sample = "both",
                                         model_stats = FALSE,
                                         show_ref = TRUE,
                                         fmt_class,
                                         approach,
                                         source,
                                         object_class,
                                         run_core,
                                         stats_fun,
                                         effect_label,
                                         extra_footnotes = NULL,
                                         distribution = NULL,
                                         time_start = NULL,
                                         cluster = NULL,
                                         weights = NULL,
                                         robust = NULL,
                                         ties = NULL,
                                         control = NULL) {
  message("Running stratified survival regression by: ", stratifier)

  levs <- .strata_levels(data, stratifier)
  if (!length(levs)) {
    stop("`stratifier` has no non-missing strata.", call. = FALSE)
  }
  n_missing_stratifier <- sum(is.na(data[[stratifier]]))
  stratifier_label <- .stratifier_label(data, stratifier)

  # Recode the event once on the full data. Re-deriving the coding inside each
  # stratum would misread a 1/2-coded stratum that happens to be all censored
  # (all 1) as "all events".
  if (is.data.frame(data) && is.character(event) && length(event) == 1L &&
      event %in% names(data)) {
    data[[event]] <- .cox_event01(data[[event]])
  }

  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0
  variable_labels <- .var_label_map(
    data,
    unique(c(exposures, adjust_for, .interaction_vars(interaction)))
  )
  per_stratum <- list()
  tds <- list()
  models <- list()
  sums <- list()
  stats_by_stratum <- list()
  n_by_stratum <- list()

  for (lv in levs) {
    message("  > Stratum: ", stratifier, " = ", lv)

    dlev <- data[which(data[[stratifier]] == lv), , drop = FALSE]
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

    res_i$table_body <- .format_interaction_levels(
      res_i$table_body, dlev, variable_labels
    )

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

  all_models <- unlist(models, recursive = FALSE)
  footnotes <- .stratified_survival_footnotes(
    approach = approach,
    tds = tds,
    event = event,
    adjusted_mode = adjusted_mode,
    adjust_for = adjust_for,
    multivariable = multivariable,
    interaction = interaction,
    stratifier = stratifier,
    stratifier_label = stratifier_label,
    n_by_stratum = n_by_stratum,
    show_sample = show_sample,
    show_ref = show_ref,
    extra_footnotes = c(
      extra_footnotes,
      if (identical(approach, "cox")) {
        .cox_weights_robust_note(all_models, cluster, weights, robust)
      } else NULL
    ),
    variable_labels = variable_labels,
    counting = !is.null(time_start),
    n_missing_stratifier = n_missing_stratifier
  )

  if (adjusted_mode || isTRUE(multivariable)) {
    built <- .strata_build_wide_multi(
      data = data,
      exposures = exposures,
      stratifier = stratifier,
      td_by_stratum = tds,
      variable_labels = variable_labels,
      show_ref = show_ref
    )
    wide <- .strata_filter_survival_sample_cols(
      .strata_add_survival_counts(built$wide, models),
      show_sample = show_sample
    )
    spanners <- built$spanners
    tbl <- if (format == "gt") {
      .build_gt_strata_wide_multi(wide, spanners, effect_label, theme, footnotes)
    } else {
      .build_flex_strata_wide_multi(wide, spanners, effect_label, theme, footnotes)
    }
  } else {
    per_for_uni <- lapply(per_stratum, function(x) {
      list(table_body = x$table_body, models = x$models)
    })
    built <- .strata_build_wide_survival_uni(
      data = data,
      time = time,
      event = event,
      exposures = exposures,
      stratifier = stratifier,
      per_stratum = per_for_uni,
      variable_labels = variable_labels,
      show_ref = show_ref
    )
    wide <- .strata_filter_survival_sample_cols(built$wide, show_sample = show_sample)
    spanners <- built$spanners
    tbl <- if (format == "gt") {
      .build_gt_strata_wide_uni(wide, spanners, effect_label, theme, footnotes)
    } else {
      .build_flex_strata_wide_uni(wide, spanners, effect_label, theme, footnotes)
    }
  }

  .message_hidden_ref_rows(
    source,
    do.call(rbind, tds),
    show_ref
  )

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
    footnotes = footnotes,
    time = time,
    event = event,
    time_start = time_start,
    distribution = distribution,
    by = stratifier,
    levels = names(tds),
    approach = approach,
    format = format,
    source = source,
    stratified = TRUE,
    show_sample = show_sample,
    adjusted_mode = adjusted_mode,
    multivariable = isTRUE(multivariable),
    adjust_for = if (adjusted_mode) unique(adjust_for) else NULL,
    exposures = unique(exposures),
    interaction = interaction,
    cluster = cluster,
    weights = weights,
    robust = robust,
    ties = ties,
    control = control,
    show_ref = isTRUE(show_ref)
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

  # N and Events come from the fitted models, so rows dropped for a missing
  # entry time, weight, or cluster value are reflected in the table.
  models <- uni_res$models
  N_map <- vapply(exposures, function(x) {
    fit <- models[[x]]
    if (is.null(fit)) return("")
    value <- tryCatch(.model_observation_count(fit), error = function(e) NA_integer_)
    if (is.na(value)) "" else as.character(value)
  }, character(1))
  names(N_map) <- exposures

  Events_map <- vapply(exposures, function(x) {
    fit <- models[[x]]
    if (is.null(fit)) "" else .strata_survival_event_count(fit)
  }, character(1))
  names(Events_map) <- exposures

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(z) formatC(z, digits = digits, format = "f", big.mark = ",")
    out <- paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
    # non-estimable coefficient: blank cell, as in the non-stratified tables
    out[is.na(est)] <- ""
    out
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
      N_vec[i] <- N_map[[ex]]
      events_vec[i] <- Events_map[[ex]]
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
                                            variable_labels = NULL,
                                            show_ref = TRUE) {
  sk <- .strata_build_skeleton(
    data,
    exposures,
    variable_labels = variable_labels,
    show_ref = show_ref
  )
  skeleton <- sk$skeleton
  is_factor <- sk$is_factor
  skeleton_labels <- attr(skeleton, "variable_labels", exact = TRUE)

  # Keep only level rows that at least one stratum-specific model used. Unused
  # factor levels are dropped before fitting, so they have no estimate anywhere.
  fitted_levels <- do.call(rbind, lapply(per_stratum, function(x) {
    x$table_body[, c("exposure", "level"), drop = FALSE]
  }))
  used <- skeleton$is_header | paste(skeleton$exposure, skeleton$level, sep = "\r") %in%
    paste(fitted_levels$exposure, fitted_levels$level, sep = "\r")
  skeleton <- skeleton[used, , drop = FALSE]
  rownames(skeleton) <- NULL

  wide <- skeleton[, c("Characteristic", "is_header"), drop = FALSE]
  wide <- .attach_display_metadata(
    wide,
    row_exposure = skeleton$exposure,
    variable_labels = skeleton_labels
  )

  for (lev in names(per_stratum)) {
    dlev <- data[which(data[[stratifier]] == lev), , drop = FALSE]
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

  list(wide = wide, spanners = names(per_stratum))
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
                                           show_sample = "both",
                                           show_ref = TRUE,
                                           extra_footnotes = NULL,
                                           variable_labels = NULL,
                                           counting = FALSE,
                                           n_missing_stratifier = 0L,
                                           stratifier_label = NULL) {
  c(
    .stratified_by_note(stratifier, stratifier_label),
    .abbrev_note(approach),
    extra_footnotes,
    if (.has_visible_reference(do.call(rbind, tds), show_ref)) .ref_note() else NULL,
    if (isTRUE(adjusted_mode)) {
      .display_adjustment_note(adjust_for, variable_labels)
    } else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other variables in the model." else NULL,
    if (!is.null(interaction)) .interaction_note(interaction) else NULL,
    .stratified_survival_sample_note(
      stratifier, n_by_stratum, show_sample, counting = counting
    ),
    .stratified_reference_note(tds, variable_labels),
    .stratified_missing_stratifier_note(stratifier, n_missing_stratifier)
  )
}

#' Footnote for exposures whose reference category is not the same in all strata
#'
#' Happens when the first level of a factor is absent from the rows used in a
#' stratum; that stratum then uses its own first observed level.
#'
#' @keywords internal
#' @noRd
.stratified_reference_note <- function(tds, variable_labels = NULL) {
  refs <- do.call(rbind, lapply(tds, function(td) {
    td[td$ref %in% TRUE, c("exposure", "level"), drop = FALSE]
  }))
  if (is.null(refs) || !nrow(refs)) {
    return(NULL)
  }
  n_refs <- tapply(refs$level, refs$exposure, function(x) length(unique(x)))
  differing <- names(n_refs)[n_refs > 1L]
  differing <- unique(refs$exposure)[unique(refs$exposure) %in% differing]
  if (!length(differing)) {
    return(NULL)
  }
  labels <- vapply(differing, .label_var, character(1), label_map = variable_labels)
  paste0(
    "The reference category differs between strata for ",
    paste(labels, collapse = ", "),
    " because its first level is not observed in every stratum."
  )
}

#' Footnote for rows dropped because the stratifier is missing
#' @keywords internal
#' @noRd
.stratified_missing_stratifier_note <- function(stratifier, n_missing) {
  if (is.null(n_missing) || is.na(n_missing) || n_missing < 1L) {
    return(NULL)
  }
  paste0(
    n_missing,
    if (n_missing == 1L) " row" else " rows",
    " with a missing value of ", stratifier,
    if (n_missing == 1L) " was" else " were",
    " excluded."
  )
}

#' Keep only requested sample columns in stratified survival display tables
#' @keywords internal
#' @noRd
.strata_filter_survival_sample_cols <- function(wide, show_sample = "events") {
  if (!show_sample %in% c("events", "n", "both", "none")) {
    stop("`show_sample` must be one of 'events', 'n', 'both', or 'none'.", call. = FALSE)
  }

  drop_prefix <- switch(
    show_sample,
    events = "..N__",
    n = "..Events__",
    both = character(0),
    none = c("..N__", "..Events__")
  )

  if (!length(drop_prefix)) {
    return(wide)
  }

  keep <- !Reduce(`|`, lapply(drop_prefix, function(prefix) startsWith(names(wide), prefix)))
  out <- wide[, keep, drop = FALSE]
  attr(out, "row_exposure") <- attr(wide, "row_exposure", exact = TRUE)
  attr(out, "variable_labels") <- attr(wide, "variable_labels", exact = TRUE)
  out
}

#' Footnote matching the displayed sample columns in stratified survival tables
#' @keywords internal
#' @noRd
.stratified_survival_sample_note <- function(stratifier,
                                             n_by_stratum,
                                             show_sample,
                                             counting = FALSE) {
  if (identical(show_sample, "none")) {
    return(NULL)
  }

  event_note <- "Events are event counts in each stratum-specific model."
  n_note <- .stratified_survival_n_note(stratifier, n_by_stratum, counting)

  switch(
    show_sample,
    events = event_note,
    n = n_note,
    both = c(n_note, event_note),
    NULL
  )
}

#' Per-stratum N footnote for stratified survival tables
#'
#' Each element of `n_by_stratum` holds the N of every model fitted in that
#' stratum. One value is printed when the models agree, otherwise the range
#' (for example `N = 79` or `N = 79<en dash>97`).
#'
#' @keywords internal
#' @noRd
.stratified_survival_n_note <- function(stratifier, n_by_stratum, counting = FALSE) {
  unit <- if (isTRUE(counting)) "analysis intervals" else "observations"
  if (!length(n_by_stratum)) {
    return(paste0("N reflects complete ", unit, " used in each stratum-specific model"))
  }

  parts <- vapply(names(n_by_stratum), function(stratum) {
    txt <- .survival_count_range(n_by_stratum[[stratum]])
    if (is.na(txt)) {
      return(paste0(stratum, ": N unavailable"))
    }
    paste0(stratum, ": N = ", txt)
  }, character(1))

  paste0(
    "Complete ", unit, " included by ",
    stratifier,
    " stratum: ",
    paste(parts, collapse = "; ")
  )
}

#' Model N values used in one stratum (one integer per fitted model)
#' @keywords internal
#' @noRd
.stratified_survival_n_used <- function(models) {
  n_vals <- vapply(models, function(fit) {
    tryCatch(as.integer(.model_observation_count(fit)),
             error = function(e) NA_integer_)
  }, integer(1))

  n_vals <- unname(n_vals[!is.na(n_vals)])
  if (!length(n_vals)) {
    return(NA_integer_)
  }
  n_vals
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
