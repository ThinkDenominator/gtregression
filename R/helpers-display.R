#' @keywords internal
#' @noRd
.make_display <- function(td, data, outcome, approach, effect_label) {
  stopifnot(all(c("exposure","level","estimate","conf.low","conf.high","p.value","ref") %in% names(td)))
  label_map <- .var_label_map(data, unique(td$exposure))
  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "-", f(hi), ")")
  }
  fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
  complete_n <- function(d, y, x) sum(stats::complete.cases(d[, c(y, x), drop = FALSE]))

  exposure_order <- unique(td$exposure)
  blocks <- lapply(split(td, factor(td$exposure, levels = exposure_order)), function(df) {
    exp_nm  <- unique(df$exposure)[1]
    exp_lab <- .label_var(exp_nm, label_map)
    N_here  <- complete_n(data, outcome, exp_nm)
    is_fact <- any(df$ref)

    if (!is_fact) {
      est <- df$estimate[1]; lo <- df$conf.low[1]; hi <- df$conf.high[1]; p <- df$p.value[1]
      header <- data.frame(Characteristic = exp_lab, stringsAsFactors = FALSE)
      header[[effect_label]] <- fmt_est_ci(est, lo, hi)
      header[["p-value"]]    <- fmt_p(p)
      header$N               <- N_here
      header$is_header       <- TRUE
      header[, c("Characteristic", effect_label, "p-value", "N", "is_header"), drop = FALSE]
    } else {
      header <- data.frame(Characteristic = exp_lab, stringsAsFactors = FALSE)
      header[[effect_label]] <- ""
      header[["p-value"]]    <- ""
      header$N               <- N_here
      header$is_header       <- TRUE

      lev <- df
      lev$Characteristic <- ifelse(lev$ref, lev$level, paste0("  ", lev$level))
      lev[[effect_label]] <- ifelse(lev$ref, "Ref.",
                                    fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high))
      lev[["p-value"]] <- ifelse(lev$ref, "", fmt_p(lev$p.value))
      lev$N <- NA_integer_
      lev$is_header <- FALSE
      lev <- lev[, c("Characteristic", effect_label, "p-value", "N", "is_header"), drop = FALSE]

      rbind(header[, names(lev)], lev, make.row.names = FALSE)
    }
  })
  out <- do.call(rbind, blocks)
  row_exposure <- rep(names(blocks), vapply(blocks, nrow, integer(1)))
  .attach_display_metadata(out, row_exposure = row_exposure, variable_labels = label_map)
}

#' @keywords internal
#' @noRd
.must_be_display_df <- function(df) {
  need <- c("Characteristic", "is_header", "N")
  if (!all(need %in% names(df))) {
    stop("Internal: display frame missing required columns (",
         paste(setdiff(need, names(df)), collapse = ", "),
         "). Did you call .make_display() first?", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.make_display_survival_uni <- function(td, data, time, event, effect_label, variable_labels = NULL) {
  stopifnot(all(c(
    "exposure", "level", "estimate", "conf.low", "conf.high", "p.value", "ref"
  ) %in% names(td)))

  label_map <- variable_labels
  if (is.null(label_map)) {
    label_map <- .var_label_map(data, unique(td$exposure))
  }

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
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
    lev$Characteristic <- ifelse(lev$ref, lev$level, paste0("  ", lev$level))
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
.make_display_multi <- function(td, data, outcome, effect_label, variable_labels = NULL) {
  stopifnot(all(c(
    "exposure", "level", "estimate", "conf.low", "conf.high", "p.value", "ref"
  ) %in% names(td)))

  label_map <- variable_labels
  if (is.null(label_map)) {
    label_map <- .var_label_map(data, unique(td$exposure))
  }

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
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
      lev$Characteristic <- ifelse(lev$ref, lev$level, paste0("  ", lev$level))
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
