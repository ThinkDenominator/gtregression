#' Get ordered levels for a stratifier
#' @keywords internal
.strata_levels <- function(data, stratifier) {
  v <- data[[stratifier]]
  if (is.factor(v)) {
    levels(stats::na.omit(v))
  } else {
    unique(stats::na.omit(v))
  }
}
#' Build canonical row skeleton (exposure headers + factor levels)
#' @keywords internal
#' @noRd
.strata_build_skeleton <- function(data, exposures, variable_labels = NULL, show_ref = TRUE) {
  is_factor <- vapply(exposures, function(x) is.factor(data[[x]]), logical(1))
  label_map <- variable_labels
  if (is.null(label_map)) {
    label_map <- .var_label_map(data, exposures)
  }

  levels_map <- lapply(
    exposures,
    function(x) if (is.factor(data[[x]])) levels(data[[x]]) else NULL
  )
  names(levels_map) <- exposures

  rows <- list()

  for (x in exposures) {
    rows[[length(rows) + 1]] <- data.frame(
      exposure = x,
      level = NA_character_,
      Characteristic = .label_var(x, label_map),
      is_header = TRUE,
      stringsAsFactors = FALSE
    )

    if (is_factor[[x]]) {
      levels_to_show <- levels_map[[x]]
      if (!isTRUE(show_ref) && length(levels_to_show) > 0L) {
        levels_to_show <- levels_to_show[-1]
      }

      for (lv in levels_to_show) {
        rows[[length(rows) + 1]] <- data.frame(
          exposure = x,
          level = lv,
          Characteristic = paste0("  ", lv),
          is_header = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  skeleton <- do.call(rbind, rows)
  skeleton <- .attach_display_metadata(
    skeleton,
    row_exposure = skeleton$exposure,
    variable_labels = label_map
  )

  list(
    skeleton = skeleton,
    is_factor = is_factor
  )
}
#' Build canonical row skeleton for stratified multivariable tables
#'
#' Uses the union of rows observed across per-stratum multivariable outputs,
#' preserving exposure headers, factor levels, and interaction rows.
#'
#' @keywords internal
#' @noRd
.strata_build_skeleton_multi <- function(exposures,
                                         td_by_stratum,
                                         variable_labels = NULL,
                                         show_ref = TRUE) {
  rows <- list()

  for (x in exposures) {
    rows[[length(rows) + 1]] <- data.frame(
      exposure = x,
      level = NA_character_,
      Characteristic = .label_var(x, variable_labels),
      is_header = TRUE,
      stringsAsFactors = FALSE
    )

    td_exp_list <- lapply(td_by_stratum, function(td) {
      td[td$exposure == x, , drop = FALSE]
    })
    td_exp_list <- Filter(function(z) nrow(z) > 0, td_exp_list)

    if (!length(td_exp_list)) {
      next
    }

    td_exp <- do.call(rbind, td_exp_list)

    # factor reference/main rows first, then non-reference rows
    seen_levels <- character(0)

    # factor levels if present
    if (any(td_exp$ref, na.rm = TRUE)) {
      factor_levels <- unique(td_exp$level)
      factor_levels <- factor_levels[!is.na(factor_levels)]
      factor_levels <- factor_levels[factor_levels != x]
      if (!isTRUE(show_ref)) {
        ref_levels <- unique(td_exp$level[td_exp$ref %in% TRUE])
        factor_levels <- setdiff(factor_levels, ref_levels)
      }

      for (lv in factor_levels) {
        if (!(lv %in% seen_levels)) {
          rows[[length(rows) + 1]] <- data.frame(
            exposure = x,
            level = lv,
            Characteristic = paste0("  ", lv),
            is_header = FALSE,
            stringsAsFactors = FALSE
          )
          seen_levels <- c(seen_levels, lv)
        }
      }
    } else {
      # continuous exposure: include non-main rows such as interactions
      extra_levels <- unique(td_exp$level)
      extra_levels <- extra_levels[!is.na(extra_levels)]
      extra_levels <- extra_levels[extra_levels != x]

      for (lv in extra_levels) {
        if (!(lv %in% seen_levels)) {
          rows[[length(rows) + 1]] <- data.frame(
            exposure = x,
            level = lv,
            Characteristic = paste0("  ", lv),
            is_header = FALSE,
            stringsAsFactors = FALSE
          )
          seen_levels <- c(seen_levels, lv)
        }
      }
    }
  }

  skeleton <- do.call(rbind, rows)
  .attach_display_metadata(
    skeleton,
    row_exposure = skeleton$exposure,
    variable_labels = variable_labels
  )
}
# ----- UNIVARIATE: pull N/effect/p per stratum aligned to skeleton -----------

#' Pull (N, Effect, p) for a univariate stratum, aligned to a skeleton
#' @keywords internal
.strata_pull_cols_uni <- function(dlev, uni_res, skeleton, is_factor, exposures, outcome) {
  td <- uni_res$table_body

  # pairwise complete N for (outcome + exposure) within this stratum
  N_map <- vapply(exposures, function(x) {
    sum(stats::complete.cases(dlev[, c(outcome, x), drop = FALSE]))
  }, integer(1))
  names(N_map) <- exposures

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(z) formatC(z, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
  }

  N_vec   <- character(nrow(skeleton))
  eff_vec <- character(nrow(skeleton))
  p_vec   <- character(nrow(skeleton))

  for (i in seq_len(nrow(skeleton))) {
    ex  <- skeleton$exposure[i]
    lv  <- skeleton$level[i]
    hdr <- skeleton$is_header[i]

    if (hdr) {
      N_vec[i] <- as.character(N_map[[ex]])
      if (!is_factor[[ex]]) {
        row <- td[td$exposure == ex & td$level == ex, , drop = FALSE]
        if (nrow(row)) {
          eff_vec[i] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
          p_vec[i]   <- .fmt_p(row$p.value)
        }
      }
    } else {
      row <- td[td$exposure == ex & td$level == lv, , drop = FALSE]
      if (nrow(row)) {
        if (isTRUE(row$ref[1])) {
          eff_vec[i] <- "Ref."; p_vec[i] <- ""
        } else {
          eff_vec[i] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
          p_vec[i]   <- .fmt_p(row$p.value)
        }
      }
    }
  }

  list(N = N_vec, effect = eff_vec, pval = p_vec)
}

#' Assemble wide display DF for all strata (univariate)
#' @keywords internal
.strata_build_wide_uni <- function(data,
                                   outcome,
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
  skeleton  <- sk$skeleton
  is_factor <- sk$is_factor

  wide <- skeleton[, c("Characteristic","is_header"), drop = FALSE]
  wide <- .attach_display_metadata(
    wide,
    row_exposure = skeleton$exposure,
    variable_labels = attr(skeleton, "variable_labels", exact = TRUE)
  )

  for (lev in names(per_stratum)) {
    dlev <- data[data[[stratifier]] == lev, , drop = FALSE]
    cols <- .strata_pull_cols_uni(dlev, per_stratum[[lev]], skeleton, is_factor,
                                  exposures, outcome)
    wide[[paste0("..N__",   lev)]] <- cols$N
    wide[[paste0("..eff__", lev)]] <- cols$effect
    wide[[paste0("..p__",   lev)]] <- cols$pval
  }

  list(wide = wide, spanners = paste0(stratifier, " = ", names(per_stratum)))
}

# ----- MULTIVARIABLE: pull Adjusted effect/p per stratum ---------------------
#' Pull (Adjusted Effect, p) for a multivariable stratum, aligned to skeleton
#' @keywords internal
#' @noRd
.strata_pull_cols_multi <- function(td, skeleton) {
  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(z) formatC(z, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
  }

  eff <- character(nrow(skeleton))
  pv  <- character(nrow(skeleton))

  for (i in seq_len(nrow(skeleton))) {
    ex  <- skeleton$exposure[i]
    lv  <- skeleton$level[i]
    hdr <- skeleton$is_header[i]

    if (hdr) {
      row <- td[td$exposure == ex & td$level == ex, , drop = FALSE]

      if (nrow(row)) {
        eff[i] <- fmt_est_ci(row$estimate[1], row$conf.low[1], row$conf.high[1])
        pv[i]  <- .fmt_p(row$p.value[1])
      } else {
        # factor headers or missing main row
        eff[i] <- ""
        pv[i]  <- ""
      }
    } else {
      row <- td[td$exposure == ex & td$level == lv, , drop = FALSE]

      if (nrow(row)) {
        if (isTRUE(row$ref[1])) {
          eff[i] <- "Ref."
          pv[i]  <- ""
        } else {
          eff[i] <- fmt_est_ci(row$estimate[1], row$conf.low[1], row$conf.high[1])
          pv[i]  <- .fmt_p(row$p.value[1])
        }
      }
    }
  }

  list(effect = eff, pval = pv)
}
#' Assemble wide display DF for all strata (multivariable)
#' @keywords internal
#' @noRd
.strata_build_wide_multi <- function(data,
                                     exposures,
                                     stratifier,
                                     td_by_stratum,
                                     variable_labels = NULL,
                                     show_ref = TRUE) {
  if (is.null(variable_labels)) {
    variable_labels <- .var_label_map(data, exposures)
  }
  skeleton <- .strata_build_skeleton_multi(
    exposures = exposures,
    td_by_stratum = td_by_stratum,
    variable_labels = variable_labels,
    show_ref = show_ref
  )

  wide <- skeleton[, c("Characteristic", "is_header"), drop = FALSE]
  wide <- .attach_display_metadata(
    wide,
    row_exposure = skeleton$exposure,
    variable_labels = variable_labels
  )
  strata_names <- names(td_by_stratum)

  for (lev in strata_names) {
    td <- td_by_stratum[[lev]]
    cols <- .strata_pull_cols_multi(td, skeleton)
    wide[[paste0("..eff__", lev)]] <- cols$effect
    wide[[paste0("..p__", lev)]] <- cols$pval
  }

  list(
    wide = wide,
    spanners = paste0(stratifier, " = ", strata_names)
  )
}

#' Add model N columns to a stratified multivariable display table
#' @keywords internal
#' @noRd
.strata_add_model_n <- function(wide, models_by_stratum) {
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

    n_vec <- character(nrow(wide))
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
        n_vec[i] <- tryCatch(
          as.character(as.integer(stats::nobs(fit))),
          error = function(e) ""
        )
      }
    }

    wide[[paste0("..N__", lev)]] <- n_vec
  }

  wide
}

#' Describe visible columns for stratified table builders
#' @keywords internal
#' @noRd
.strata_display_spec <- function(df, spanners, effect_label) {
  block_ids <- character(0)
  block_labels <- character(0)
  widths <- integer(length(spanners))

  for (i in seq_along(spanners)) {
    nm <- sub("^.*=\\s*", "", spanners[i])
    ids <- c(
      paste0("..N__", nm),
      paste0("..Events__", nm),
      paste0("..eff__", nm),
      paste0("..p__", nm)
    )
    labels <- c("N", "Events", effect_label, "p-value")
    keep <- ids %in% names(df)

    ids <- ids[keep]
    labels <- labels[keep]
    widths[i] <- length(ids)

    block_ids <- c(block_ids, ids)
    block_labels <- c(block_labels, labels)
  }

  list(
    ids = block_ids,
    labels = c("Characteristic", block_labels),
    widths = widths
  )
}
