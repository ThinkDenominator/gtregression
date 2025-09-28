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
.strata_build_skeleton <- function(data, exposures) {
  is_factor  <- vapply(exposures, function(x) is.factor(data[[x]]), logical(1))
  levels_map <- lapply(exposures, function(x) if (is.factor(data[[x]])) levels(data[[x]]) else NULL)
  names(levels_map) <- exposures

  rows <- list()
  for (x in exposures) {
    # exposure header
    rows[[length(rows)+1]] <- data.frame(
      exposure = x, level = NA_character_, Characteristic = x, is_header = TRUE,
      stringsAsFactors = FALSE
    )
    # factor levels (indented)
    if (is_factor[[x]]) {
      for (lv in levels_map[[x]]) {
        rows[[length(rows)+1]] <- data.frame(
          exposure = x, level = lv, Characteristic = paste0("  ", lv),
          is_header = FALSE, stringsAsFactors = FALSE
        )
      }
    }
  }

  list(skeleton = do.call(rbind, rows), is_factor = is_factor)
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
          eff_vec[i] <- "\u2014"; p_vec[i] <- ""
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
.strata_build_wide_uni <- function(data, outcome, exposures, stratifier, per_stratum) {
  sk <- .strata_build_skeleton(data, exposures)
  skeleton  <- sk$skeleton
  is_factor <- sk$is_factor

  wide <- skeleton[, c("Characteristic","is_header"), drop = FALSE]

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
.strata_pull_cols_multi <- function(td, skeleton, is_factor) {
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
      if (!is_factor[[ex]]) {
        row <- td[td$exposure == ex & td$level == ex, , drop = FALSE]
        if (nrow(row)) {
          eff[i] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
          pv[i]  <- .fmt_p(row$p.value)
        }
      } else {
        eff[i] <- ""; pv[i] <- ""
      }
    } else {
      row <- td[td$exposure == ex & td$level == lv, , drop = FALSE]
      if (nrow(row)) {
        if (isTRUE(row$ref[1])) {
          eff[i] <- "\u2014"; pv[i] <- ""
        } else {
          eff[i] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
          pv[i]  <- .fmt_p(row$p.value)
        }
      }
    }
  }
  list(effect = eff, pval = pv)
}

#' Assemble wide display DF for all strata (multivariable)
#' @keywords internal
.strata_build_wide_multi <- function(data, exposures, stratifier, td_by_stratum) {
  sk <- .strata_build_skeleton(data, exposures)
  skeleton  <- sk$skeleton
  is_factor <- sk$is_factor

  wide <- skeleton[, c("Characteristic","is_header"), drop = FALSE]

  for (lev in names(td_by_stratum)) {
    td   <- td_by_stratum[[lev]]
    cols <- .strata_pull_cols_multi(td, skeleton, is_factor)
    wide[[paste0("..eff__", lev)]] <- cols$effect
    wide[[paste0("..p__",   lev)]] <- cols$pval
  }

  list(wide = wide, spanners = paste0(stratifier, " = ", names(td_by_stratum)))
}
