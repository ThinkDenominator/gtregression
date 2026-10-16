#' @keywords internal
#' @noRd
.model_data_col <- function(fit, variable) {
  mf <- fit$model
  if (is.null(mf)) {
    mf <- attr(fit, "gtregression_model_frame", exact = TRUE)
  }
  if (is.null(mf) || !variable %in% names(mf)) {
    return(NULL)
  }
  x <- mf[[variable]]

  # R's modelling machinery expands character predictors as treatment-coded
  # categorical variables, but model.frame() may retain their character class.
  # Normalise only for table reconstruction so every fitted level is displayed.
  if (is.character(x)) {
    x <- factor(x)
  }

  x
}

#' Map fitted coefficient names to their originating formula terms
#'
#' Model-matrix assignments are authoritative. String-prefix matching is not:
#' variables such as `age` and `age_group` otherwise contaminate one another,
#' and a colon inside a factor level can be mistaken for an interaction.
#'
#' @keywords internal
#' @noRd
.coefficient_term_map <- function(fit, coefficient_names) {
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  tt <- tryCatch(stats::terms(fit), error = function(e) NULL)
  if (is.null(mm) && !is.null(tt) && !is.null(fit$model)) {
    mm <- tryCatch(
      stats::model.matrix(tt, data = fit$model),
      error = function(e) NULL
    )
  }
  if (is.null(mm) || is.null(tt)) {
    return(stats::setNames(rep(NA_character_, length(coefficient_names)),
                           coefficient_names))
  }

  assignment <- attr(mm, "assign")
  term_labels <- attr(tt, "term.labels")
  mapped <- rep(NA_character_, ncol(mm))
  keep <- assignment > 0L & assignment <= length(term_labels)
  mapped[keep] <- term_labels[assignment[keep]]

  unname(mapped[.match_coefficient_columns(coefficient_names, colnames(mm))])
}

#' Match coefficient names to model-matrix columns by position
#'
#' Coefficient names are not unique: a factor `x` with level `b` and a
#' variable called `xb` both produce the name "xb". Coefficients are reported
#' in model-matrix order (aliased columns may be missing), so each name is
#' matched to the first not-yet-used column of that name at or after the
#' previous match.
#'
#' @keywords internal
#' @noRd
.match_coefficient_columns <- function(coefficient_names, column_names) {
  if (identical(coefficient_names, column_names)) {
    return(seq_along(column_names))
  }
  out <- rep(NA_integer_, length(coefficient_names))
  used <- rep(FALSE, length(column_names))
  last <- 0L
  for (i in seq_along(coefficient_names)) {
    hits <- which(column_names == coefficient_names[i] & !used)
    if (!length(hits)) next
    ahead <- hits[hits > last]
    pick <- if (length(ahead)) ahead[1L] else hits[1L]
    out[i] <- pick
    used[pick] <- TRUE
    last <- pick
  }
  out
}

#' Split interaction coefficients into their per-variable pieces
#'
#' Uses the model's term structure (`terms` factors and `xlevels`) instead of
#' splitting the pasted coefficient name on ":" so that factor levels
#' containing ":" stay intact.
#'
#' @return A list parallel to `coefficient_names`; `NULL` for main effects and
#'   otherwise a list with `vars` and `levels` (`""` for numeric variables).
#' @keywords internal
#' @noRd
.coefficient_interaction_pieces <- function(fit, coefficient_names) {
  empty <- vector("list", length(coefficient_names))
  tt <- tryCatch(stats::terms(fit), error = function(e) NULL)
  term_map <- .coefficient_term_map(fit, coefficient_names)
  if (is.null(tt)) return(empty)
  factors <- attr(tt, "factors")
  if (is.null(factors) || !length(dim(factors))) return(empty)
  xlevels <- fit$xlevels
  if (is.null(xlevels)) xlevels <- list()
  mf <- fit$model
  if (is.null(mf)) mf <- attr(fit, "gtregression_model_frame", exact = TRUE)

  var_levels <- function(variable) {
    bare <- gsub("`", "", variable, fixed = TRUE)
    lev <- xlevels[[variable]]
    if (is.null(lev)) lev <- xlevels[[bare]]
    if (is.null(lev) && !is.null(mf) && bare %in% names(mf)) {
      value <- mf[[bare]]
      if (is.factor(value)) lev <- levels(value)
      if (is.character(value)) lev <- levels(factor(value))
      if (is.logical(value)) lev <- c("FALSE", "TRUE")
    }
    lev
  }

  split_one <- function(coefficient, vars) {
    rest <- coefficient
    levels_out <- character(length(vars))
    for (j in seq_along(vars)) {
      variable <- vars[j]
      last <- j == length(vars)
      candidates <- c("", var_levels(variable))
      pieces <- paste0(variable, candidates)
      # longest candidate first so that a level that is a prefix of another
      # level cannot win
      ord <- order(nchar(pieces), decreasing = TRUE)
      found <- FALSE
      for (k in ord) {
        piece <- pieces[k]
        if (last) {
          if (identical(rest, piece)) {
            levels_out[j] <- candidates[k]; found <- TRUE; break
          }
        } else if (startsWith(rest, paste0(piece, ":"))) {
          tail <- substring(rest, nchar(piece) + 2L)
          if (startsWith(tail, vars[j + 1L])) {
            levels_out[j] <- candidates[k]; rest <- tail; found <- TRUE; break
          }
        }
      }
      if (!found) return(NULL)
    }
    list(vars = gsub("`", "", vars, fixed = TRUE), levels = levels_out)
  }

  for (i in seq_along(coefficient_names)) {
    term <- term_map[i]
    if (is.na(term) || !term %in% colnames(factors)) next
    vars <- rownames(factors)[factors[, term] > 0]
    if (length(vars) < 2L) next
    pieces <- split_one(coefficient_names[i], vars)
    if (is.null(pieces)) {
      raw <- strsplit(gsub("`", "", coefficient_names[i], fixed = TRUE), ":",
                      fixed = TRUE)[[1]]
      pieces <- list(vars = NULL, levels = NULL, raw = raw)
    } else {
      pieces$raw <- paste0(pieces$vars, pieces$levels)
    }
    empty[[i]] <- pieces
  }
  empty
}

#' @keywords internal
#' @noRd
.safe_interaction_pieces <- function(fit, coefficient_names) {
  tryCatch(
    .coefficient_interaction_pieces(fit, coefficient_names),
    error = function(e) vector("list", length(coefficient_names))
  )
}

#' Readable labels for interaction rows, derived from the fitted models
#'
#' Replaces the raw interaction label produced by `.tidy_multi()`
#' (`"raceBlack x smokeYes"`) by `"race: Black x smoke: Yes"` using variable
#' labels. The mapping is built from the model structure, so levels containing
#' ":" or " x " are handled correctly.
#'
#' @keywords internal
#' @noRd
.label_interaction_rows <- function(table_body, models, variable_labels = NULL,
                                    data = NULL) {
  if (!is.data.frame(table_body) || !"level" %in% names(table_body) ||
      !length(models)) {
    return(table_body)
  }
  if (!is.null(models) && !is.null(models$coefficients)) models <- list(models)
  raw <- character(0)
  pretty <- character(0)
  for (fit in models) {
    if (is.null(fit)) next
    names_i <- tryCatch(rownames(summary(fit)$coefficients),
                        error = function(e) NULL)
    if (is.null(names_i)) names_i <- names(stats::coef(fit))
    pieces <- .safe_interaction_pieces(fit, names_i)
    for (piece in pieces) {
      if (is.null(piece)) next
      raw <- c(raw, paste(piece$raw, collapse = " x "))
      if (is.null(piece$vars)) {
        pretty <- c(pretty, paste(piece$raw, collapse = " x "))
      } else {
        labels <- vapply(piece$vars, .label_var, character(1),
                         label_map = variable_labels)
        parts <- ifelse(nzchar(piece$levels),
                        paste0(labels, ": ", piece$levels), labels)
        pretty <- c(pretty, paste(parts, collapse = " x "))
      }
    }
  }
  if (!length(raw)) {
    # No structural information (for example a model class without terms):
    # fall back to the text-based formatter.
    if (!is.null(data)) {
      return(.format_interaction_levels(table_body, data, variable_labels))
    }
    return(table_body)
  }
  flag <- attr(table_body, "interaction_row", exact = TRUE)
  idx <- match(table_body$level, raw)
  change <- !is.na(idx) & !(table_body$ref %in% TRUE)
  if (!is.null(flag) && length(flag) == nrow(table_body)) {
    change <- change & flag
  }
  table_body$level[change] <- pretty[idx[change]]
  table_body
}

#' Test whether a formula term contains a requested variable
#' @keywords internal
#' @noRd
.term_contains_variable <- function(term, variable) {
  if (is.na(term) || !nzchar(term)) {
    return(FALSE)
  }
  vars <- tryCatch(
    all.vars(stats::as.formula(paste("~", term))),
    error = function(e) character()
  )
  variable %in% vars
}

#' Create a display label from a coefficient and its formula term
#' @keywords internal
#' @noRd
.coefficient_display_level <- function(coefficient, term, exposure,
                                       pieces = NULL) {
  if (!is.null(pieces) && !is.null(pieces$raw)) {
    return(paste(pieces$raw, collapse = " x "))
  }
  coefficient <- gsub("`", "", coefficient, fixed = TRUE)
  term_vars <- tryCatch(
    all.vars(stats::as.formula(paste("~", term))),
    error = function(e) character()
  )

  if (length(term_vars) > 1L) {
    return(paste(strsplit(coefficient, ":", fixed = TRUE)[[1]],
                 collapse = " x "))
  }
  if (identical(coefficient, exposure)) {
    return(exposure)
  }
  if (startsWith(coefficient, exposure)) {
    level <- substring(coefficient, nchar(exposure) + 1L)
    if (nzchar(level)) return(level)
  }
  coefficient
}

#' @keywords internal
#' @noRd
.tidy_coefficients <- function(fit, approach) {
  approach <- .normalize_approach(approach)

  if (identical(approach, "firth")) {
    est <- tryCatch(stats::coef(fit), error = function(e) fit$coefficients)
    if (is.null(est)) return(NULL)
    term <- names(est)
    if (is.null(term)) term <- names(fit$coefficients)
    if (is.null(term)) return(NULL)

    lo <- fit$ci.lower
    hi <- fit$ci.upper
    p  <- fit$prob

    if (is.null(lo) || is.null(hi)) {
      se <- tryCatch(sqrt(diag(fit$var)), error = function(e) rep(NA_real_, length(est)))
      z <- stats::qnorm(0.975)
      lo <- est - z * se
      hi <- est + z * se
    }

    if (is.null(p)) {
      se <- tryCatch(sqrt(diag(fit$var)), error = function(e) rep(NA_real_, length(est)))
      p <- 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)
    }

    out <- data.frame(
      term = term,
      estimate = as.numeric(est),
      conf.low = as.numeric(lo[term]),
      conf.high = as.numeric(hi[term]),
      p.value = as.numeric(p[term]),
      stringsAsFactors = FALSE
    )

    missing_ci <- is.na(out$conf.low) | is.na(out$conf.high)
    if (any(missing_ci)) {
      out$conf.low[missing_ci] <- as.numeric(lo)[missing_ci]
      out$conf.high[missing_ci] <- as.numeric(hi)[missing_ci]
    }

    missing_p <- is.na(out$p.value)
    if (any(missing_p)) {
      out$p.value[missing_p] <- as.numeric(p)[missing_p]
    }

    return(out)
  }

  smry  <- summary(fit)
  coefs <- smry$coefficients
  if (is.null(coefs) || nrow(coefs) == 0) return(NULL)

  rn <- rownames(coefs)
  est <- coefs[, 1]
  se <- coefs[, 2]
  p <- coefs[, 4]
  # Linear models: t quantile (as confint.lm), consistent with the t-based
  # p-values. All other approaches: Wald (normal) intervals.
  z <- stats::qnorm(0.975)
  if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    df_resid <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)
    if (length(df_resid) == 1L && is.finite(df_resid) && df_resid > 0) {
      z <- stats::qt(0.975, df = df_resid)
    }
  }

  data.frame(
    term = rn,
    estimate = as.numeric(est),
    conf.low = as.numeric(est - z * se),
    conf.high = as.numeric(est + z * se),
    p.value = as.numeric(p),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.tidy_uni <- function(fit, exposure, approach) {
  coefs <- .tidy_coefficients(fit, approach)
  if (is.null(coefs) || nrow(coefs) == 0) return(NULL)

  rn <- coefs$term
  term_map <- .coefficient_term_map(fit, rn)
  keep <- vapply(term_map, .term_contains_variable, logical(1),
                 variable = exposure)
  idx <- which(keep)

  df_nonref <- NULL
  if (length(idx)) {
    est <- coefs$estimate[idx]
    lo  <- coefs$conf.low[idx]
    hi  <- coefs$conf.high[idx]
    p   <- coefs$p.value[idx]
    if (.is_ratio(approach)) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
    term <- rn[idx]
    pieces <- .safe_interaction_pieces(fit, rn)[idx]
    lvl <- vapply(seq_along(idx), function(k) {
      .coefficient_display_level(term[k], term_map[idx][k], exposure,
                                 pieces = pieces[[k]])
    }, character(1))

    df_nonref <- data.frame(
      exposure  = exposure,
      level     = lvl,
      estimate  = est,
      conf.low  = lo,
      conf.high = hi,
      p.value   = p,
      ref       = FALSE,
      stringsAsFactors = FALSE
    )
  }

  ref_row <- NULL
  x <- .model_data_col(fit, exposure)
  if (!is.null(x) && is.factor(x)) {
    levs <- levels(x)
    ref_level <- levs[1]
    ref_row <- data.frame(
      exposure  = exposure,
      level     = ref_level,
      estimate  = if (.is_ratio(approach)) 1 else 0,
      conf.low  = NA_real_,
      conf.high = NA_real_,
      p.value   = NA_real_,
      ref       = TRUE,
      stringsAsFactors = FALSE
    )
    if (!is.null(df_nonref)) {
      df_nonref$..ord <- match(df_nonref$level, levs)
      df_nonref <- df_nonref[order(df_nonref$..ord), , drop = FALSE]
      df_nonref$..ord <- NULL
    }
  }

  if (is.null(ref_row) && is.null(df_nonref)) return(NULL)
  if (!is.null(ref_row) && !is.null(df_nonref)) rbind(ref_row, df_nonref) else
    if (!is.null(ref_row)) ref_row else df_nonref
}

#' @keywords internal
#' @noRd
.tidy_multi <- function(fit, exposures, approach) {
  coefs <- .tidy_coefficients(fit, approach)

  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }

  rn <- coefs$term
  term_map <- .coefficient_term_map(fit, rn)
  all_pieces <- .safe_interaction_pieces(fit, rn)

  out_list <- lapply(exposures, function(exposure) {
    idx <- which(vapply(term_map, .term_contains_variable, logical(1),
                        variable = exposure))

    df_nonref <- NULL

    if (length(idx) > 0) {
      est <- coefs$estimate[idx]
      lo  <- coefs$conf.low[idx]
      hi  <- coefs$conf.high[idx]
      p   <- coefs$p.value[idx]

      if (.is_ratio(approach)) {
        est <- exp(est)
        lo  <- exp(lo)
        hi  <- exp(hi)
      }

      term <- rn[idx]
      pieces <- all_pieces[idx]
      lvl <- vapply(seq_along(idx), function(k) {
        .coefficient_display_level(term[k], term_map[idx][k], exposure,
                                   pieces = pieces[[k]])
      }, character(1))
      is_interaction_row <- !vapply(pieces, is.null, logical(1))

      df_nonref <- data.frame(
        exposure  = exposure,
        level     = lvl,
        estimate  = est,
        conf.low  = lo,
        conf.high = hi,
        p.value   = p,
        ref       = FALSE,
        stringsAsFactors = FALSE
      )
      df_nonref$..interaction <- is_interaction_row

      df_nonref <- df_nonref[
        !duplicated(df_nonref[, c("level", "..interaction")]), , drop = FALSE
      ]
    }

    ref_row <- NULL

    x <- .model_data_col(fit, exposure)
    if (!is.null(x) && is.factor(x)) {
      levs <- levels(x)
      ref_level <- levs[1]

      ref_row <- data.frame(
        exposure  = exposure,
        level     = ref_level,
        estimate  = if (.is_ratio(approach)) 1 else 0,
        conf.low  = NA_real_,
        conf.high = NA_real_,
        p.value   = NA_real_,
        ref       = TRUE,
        stringsAsFactors = FALSE
      )

      if (!is.null(df_nonref)) {
        # Interaction rows are identified from the model structure, not from
        # the text of the label (a level may itself contain " x ").
        ord_main <- match(df_nonref$level, levs)
        ord_main[is.na(ord_main) | df_nonref$..interaction] <- Inf

        df_nonref$..ord <- ord_main

        df_nonref <- df_nonref[
          order(df_nonref$..interaction, df_nonref$..ord, df_nonref$level),
          ,
          drop = FALSE
        ]

        df_nonref$..ord <- NULL
      }
      ref_row$..interaction <- FALSE
    }

    if (is.null(ref_row) && is.null(df_nonref)) {
      return(NULL)
    }

    if (!is.null(ref_row) && !is.null(df_nonref)) {
      return(rbind(ref_row, df_nonref))
    }

    if (!is.null(ref_row)) {
      return(ref_row)
    }

    df_nonref
  })

  out_list <- Filter(Negate(is.null), out_list)

  if (!length(out_list)) {
    return(NULL)
  }

  out <- do.call(rbind, out_list)
  # Keep the public table_body columns unchanged; the structural interaction
  # flag travels as an attribute.
  flag <- out$..interaction %in% TRUE
  out$..interaction <- NULL
  rownames(out) <- NULL
  attr(out, "interaction_row") <- flag
  out
}
