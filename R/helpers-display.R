#' @keywords internal
#' @noRd
.make_display <- function(td, data, outcome, approach, effect_label) {
  stopifnot(all(c("exposure","level","estimate","conf.low","conf.high","p.value","ref") %in% names(td)))
  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "–", f(hi), ")")
  }
  fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
  complete_n <- function(d, y, x) sum(stats::complete.cases(d[, c(y, x), drop = FALSE]))

  blocks <- lapply(split(td, td$exposure), function(df) {
    exp_nm  <- unique(df$exposure)
    N_here  <- complete_n(data, outcome, exp_nm)
    is_fact <- any(df$ref)

    if (!is_fact) {
      est <- df$estimate[1]; lo <- df$conf.low[1]; hi <- df$conf.high[1]; p <- df$p.value[1]
      header <- data.frame(Characteristic = exp_nm, stringsAsFactors = FALSE)
      header[[effect_label]] <- fmt_est_ci(est, lo, hi)
      header[["p-value"]]    <- fmt_p(p)
      header$N               <- N_here
      header$is_header       <- TRUE
      header[, c("Characteristic", effect_label, "p-value", "N", "is_header"), drop = FALSE]
    } else {
      header <- data.frame(Characteristic = exp_nm, stringsAsFactors = FALSE)
      header[[effect_label]] <- ""
      header[["p-value"]]    <- ""
      header$N               <- N_here
      header$is_header       <- TRUE

      lev <- df
      lev$Characteristic <- ifelse(lev$ref, lev$level, paste0("  ", lev$level))
      lev[[effect_label]] <- ifelse(lev$ref, "—",
                                    fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high))
      lev[["p-value"]] <- ifelse(lev$ref, "", fmt_p(lev$p.value))
      lev$N <- NA_integer_
      lev$is_header <- FALSE
      lev <- lev[, c("Characteristic", effect_label, "p-value", "N", "is_header"), drop = FALSE]

      rbind(header[, names(lev)], lev, make.row.names = FALSE)
    }
  })
  do.call(rbind, blocks)
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
.make_display_multi <- function(td, data, outcome, effect_label) {
  stopifnot(all(c("exposure","level","estimate","conf.low","conf.high","p.value","ref") %in% names(td)))

  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")  # en dash
  }
  fmt_p <- .fmt_p

  blocks <- lapply(split(td, td$exposure), function(df) {
    exp_nm  <- unique(df$exposure)
    is_fact <- any(df$ref)

    if (!is_fact) {
      est <- df$estimate[1]; lo <- df$conf.low[1]; hi <- df$conf.high[1]; p <- df$p.value[1]
      header <- data.frame(Characteristic = exp_nm, stringsAsFactors = FALSE)
      header[[effect_label]] <- fmt_est_ci(est, lo, hi)
      header[["p-value"]]    <- fmt_p(p)
      header$is_header       <- TRUE
      header[, c("Characteristic", effect_label, "p-value", "is_header"), drop = FALSE]
    } else {
      header <- data.frame(Characteristic = exp_nm, stringsAsFactors = FALSE)
      header[[effect_label]] <- ""
      header[["p-value"]]    <- ""
      header$is_header       <- TRUE

      lev <- df
      lev$Characteristic <- ifelse(lev$ref, lev$level, paste0("  ", lev$level))
      lev[[effect_label]] <- ifelse(lev$ref, "\u2014",  # em dash for ref
                                    fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high))
      lev[["p-value"]] <- ifelse(lev$ref, "", fmt_p(lev$p.value))
      lev$is_header <- FALSE
      lev <- lev[, c("Characteristic", effect_label, "p-value", "is_header"), drop = FALSE]

      rbind(header[, names(lev)], lev, make.row.names = FALSE)
    }
  })
  do.call(rbind, blocks)
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
