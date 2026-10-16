#' @keywords internal
#' @noRd
.must_be_display_df <- function(df) {
  need <- c("Characteristic", "is_header", "N")
  if (!all(need %in% names(df))) {
    stop("Internal: display frame missing required columns (",
         paste(setdiff(need, names(df)), collapse = ", "),
         "). Did you call .make_display_uni() first?", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.make_display_survival_uni <- function(td,
                                       data,
                                       time,
                                       event,
                                       effect_label,
                                       variable_labels = NULL,
                                       show_ref = TRUE) {
  stopifnot(all(c(
    "exposure", "level", "estimate", "conf.low", "conf.high", "p.value", "ref"
  ) %in% names(td)))

  label_map <- variable_labels
  if (is.null(label_map)) {
    label_map <- .var_label_map(data, unique(td$exposure))
  }

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    out <- paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
    out[is.na(est)] <- ""
    out
  }

  complete_n <- function(d, time, event, x) {
    sum(stats::complete.cases(d[, c(time, event, x), drop = FALSE]))
  }

  exposure_order <- unique(td$exposure)
  blocks <- lapply(split(td, factor(td$exposure, levels = exposure_order)), function(df) {
    exp_nm <- unique(df$exposure)[1]
    exp_lab <- .label_var(exp_nm, label_map)
    N_here <- complete_n(data, time, event, exp_nm)
    is_fact <- any(df$ref)

    if (!is_fact && nrow(df) == 1) {
      header <- data.frame(Characteristic = exp_lab, stringsAsFactors = FALSE)
      header[[effect_label]] <- fmt_est_ci(df$estimate[1], df$conf.low[1], df$conf.high[1])
      header[["p-value"]] <- .fmt_p(df$p.value[1])
      header$N <- N_here
      header$is_header <- TRUE
      return(header[, c("Characteristic", effect_label, "p-value", "N", "is_header"),
                    drop = FALSE])
    }

    header <- data.frame(Characteristic = exp_lab, stringsAsFactors = FALSE)
    header[[effect_label]] <- ""
    header[["p-value"]] <- ""
    header$N <- N_here
    header$is_header <- TRUE

    lev <- df
    if (!isTRUE(show_ref) && nrow(lev) == 2L && sum(lev$ref %in% TRUE) == 1L) {
      row <- lev[!(lev$ref %in% TRUE), , drop = FALSE][1, ]
      header[[effect_label]] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
      header[["p-value"]] <- .fmt_p(row$p.value)
      return(header[, c("Characteristic", effect_label, "p-value", "N", "is_header"),
                    drop = FALSE])
    }

    lev$Characteristic <- paste0("  ", lev$level)
    lev[[effect_label]] <- ifelse(
      lev$ref,
      "Ref.",
      fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high)
    )
    lev[["p-value"]] <- ifelse(lev$ref, "", .fmt_p(lev$p.value))
    lev$N <- NA_integer_
    lev$is_header <- FALSE
    lev <- lev[, c("Characteristic", effect_label, "p-value", "N", "is_header"),
               drop = FALSE]

    rbind(header[, names(lev), drop = FALSE], lev, make.row.names = FALSE)
  })

  out <- do.call(rbind, blocks)
  row_exposure <- rep(names(blocks), vapply(blocks, nrow, integer(1)))
  .attach_display_metadata(out, row_exposure = row_exposure, variable_labels = label_map)
}

#' @keywords internal
#' @noRd
.make_display_multi <- function(td,
                                data,
                                outcome,
                                effect_label,
                                variable_labels = NULL,
                                show_ref = TRUE) {
  stopifnot(all(c(
    "exposure", "level", "estimate", "conf.low", "conf.high", "p.value", "ref"
  ) %in% names(td)))

  label_map <- variable_labels
  if (is.null(label_map)) {
    label_map <- .var_label_map(data, unique(td$exposure))
  }

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    out <- paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
    out[is.na(est)] <- ""
    out
  }

  fmt_p <- .fmt_p

  exposure_order <- unique(td$exposure)
  blocks <- lapply(split(td, factor(td$exposure, levels = exposure_order)), function(df) {
    exp_nm <- unique(df$exposure)[1]
    exp_lab <- .label_var(exp_nm, label_map)
    is_fact <- any(df$ref)

    # Case 1: continuous exposure with a single row
    if (!is_fact && nrow(df) == 1) {
      est <- df$estimate[1]
      lo  <- df$conf.low[1]
      hi  <- df$conf.high[1]
      p   <- df$p.value[1]

      header <- data.frame(Characteristic = exp_lab, stringsAsFactors = FALSE)
      header[[effect_label]] <- fmt_est_ci(est, lo, hi)
      header[["p-value"]] <- fmt_p(p)
      header$is_header <- TRUE

      return(header[, c("Characteristic", effect_label, "p-value", "is_header"),
                    drop = FALSE])
    }

    # Case 2: factor exposure OR continuous exposure with interaction rows
    header <- data.frame(Characteristic = exp_lab, stringsAsFactors = FALSE)
    header[[effect_label]] <- if (is_fact) "" else fmt_est_ci(
      df$estimate[df$level == exp_nm][1],
      df$conf.low[df$level == exp_nm][1],
      df$conf.high[df$level == exp_nm][1]
    )
    header[["p-value"]] <- if (is_fact) "" else fmt_p(
      df$p.value[df$level == exp_nm][1]
    )
    header$is_header <- TRUE

    lev <- df

    # For factor exposures: show reference and levels underneath
    if (is_fact) {
      if (!isTRUE(show_ref) && nrow(lev) == 2L && sum(lev$ref %in% TRUE) == 1L) {
        row <- lev[!(lev$ref %in% TRUE), , drop = FALSE][1, ]
        header[[effect_label]] <- fmt_est_ci(row$estimate, row$conf.low, row$conf.high)
        header[["p-value"]] <- fmt_p(row$p.value)
        return(header[, c("Characteristic", effect_label, "p-value", "is_header"),
                      drop = FALSE])
      }

      lev$Characteristic <- paste0("  ", lev$level)
      lev[[effect_label]] <- ifelse(
        lev$ref,
        "Ref.",
        fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high)
      )
      lev[["p-value"]] <- ifelse(lev$ref, "", fmt_p(lev$p.value))
    } else {
      # For continuous exposures with interaction rows:
      # keep only non-main rows under the header
      lev <- lev[lev$level != exp_nm, , drop = FALSE]

      if (nrow(lev) > 0) {
        lev$Characteristic <- paste0("  ", lev$level)
        lev[[effect_label]] <- fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high)
        lev[["p-value"]] <- fmt_p(lev$p.value)
      }
    }

    lev$is_header <- FALSE

    lev <- lev[, c("Characteristic", effect_label, "p-value", "is_header"),
               drop = FALSE]

    if (nrow(lev) == 0) {
      return(header[, names(lev), drop = FALSE])
    }

    rbind(header[, names(lev), drop = FALSE], lev, make.row.names = FALSE)
  })

  out <- do.call(rbind, blocks)
  row_exposure <- rep(names(blocks), vapply(blocks, nrow, integer(1)))
  .attach_display_metadata(out, row_exposure = row_exposure, variable_labels = label_map)
}

#' @keywords internal
#' @noRd
.must_be_display_df_multi <- function(df) {
  need <- c("Characteristic", "is_header")
  if (!all(need %in% names(df))) {
    stop("Internal: display frame missing required columns (",
         paste(setdiff(need, names(df)), collapse = ", "),
         ").", call. = FALSE)
  }
  invisible(TRUE)
}

#' Number of observations represented by a fitted model
#'
#' `nobs.coxph()` can report the number of events, so Cox models must use the
#' model's stored row count. Fine-Gray models also retain their analysis N.
#'
#' @keywords internal
#' @noRd
.model_observation_count <- function(fit) {
  if (inherits(fit, "coxph") && !is.null(fit$n)) {
    return(as.integer(fit$n))
  }
  if (inherits(fit, "crr") && !is.null(fit$n)) {
    return(as.integer(fit$n))
  }
  value <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)
  if (length(value) != 1L || is.na(value)) {
    frame <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
    value <- if (is.null(frame)) NA_integer_ else nrow(frame)
  }
  as.integer(value)
}

#' Add fitted-model N values to a multivariable display table
#' @keywords internal
#' @noRd
.add_model_n_column <- function(display, models) {
  row_exposure <- attr(display, "row_exposure", exact = TRUE)
  variable_labels <- attr(display, "variable_labels", exact = TRUE)
  if (is.null(row_exposure) || !length(models)) return(display)

  n_values <- rep(NA_integer_, nrow(display))
  header_rows <- which(display$is_header %in% TRUE)
  for (i in header_rows) {
    fit <- models[[row_exposure[i]]]
    if (is.null(fit) && length(models) == 1L) fit <- models[[1L]]
    if (!is.null(fit)) n_values[i] <- .model_observation_count(fit)
  }

  if ("N" %in% names(display)) {
    display$N <- n_values
  } else {
    insert_after <- match("Characteristic", names(display))
    display <- append(display, list(N = n_values), after = insert_after)
    display <- as.data.frame(display, stringsAsFactors = FALSE,
                             check.names = FALSE)
  }
  .attach_display_metadata(
    display,
    row_exposure = row_exposure,
    variable_labels = variable_labels
  )
}

#' Explain model-derived N values
#' @keywords internal
#' @noRd
.model_n_footnote <- function(models) {
  is_counting <- any(vapply(models, function(fit) {
    inherits(fit, "coxph") && !is.null(fit$y) &&
      identical(attr(fit$y, "type"), "counting")
  }, logical(1)))
  if (is_counting) {
    return("N is the number of complete analysis intervals used in each model.")
  }
  "N is the number of complete observations used in each model."
}

#' Wrap an estimate formatter so non-estimable coefficients print as blank
#'
#' A missing estimate is displayed as an empty cell, the same convention used
#' for missing p-values, instead of the literal text "NA (NA-NA)".
#' @keywords internal
#' @noRd
.blank_missing_estimate <- function(formatter) {
  force(formatter)
  function(est, lo, hi, ...) {
    out <- formatter(est, lo, hi, ...)
    out[is.na(est)] <- ""
    out
  }
}

#' Tidy formatted effect cells from any package table
#'
#' Removes the literal "NA (NA-NA)" text of a non-estimable coefficient and
#' writes the confidence-interval separator as an en dash, so that tables
#' created with a hyphen separator read consistently beside the others.
#' @keywords internal
#' @noRd
.tidy_effect_cells <- function(x) {
  if (!is.character(x)) return(x)
  missing_cell <- grepl("^\\s*NA\\s*\\(\\s*NA\\s*.\\s*NA\\s*\\)\\s*$", x)
  x[missing_cell] <- ""
  number <- "-?[0-9][0-9.,]*"
  pattern <- paste0("^(\\s*", number, "\\s*\\(\\s*", number, ")-(", number, "\\)\\s*)$")
  hit <- !is.na(x) & grepl(pattern, x)
  x[hit] <- sub(pattern, "\\1\u2013\\2", x[hit])
  x
}
