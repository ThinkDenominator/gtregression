#' @keywords internal
#' @noRd
.tidy_uni <- function(fit, exposure, approach) {
  smry  <- summary(fit)
  coefs <- smry$coefficients
  if (is.null(coefs) || nrow(coefs) == 0) return(NULL)

  rn <- rownames(coefs)
  keep <- grepl(paste0("^", exposure), rn) |
    grepl(paste0("^`", exposure, "`"), rn) |
    rn %in% exposure | rn %in% paste0("`", exposure, "`")
  idx <- which(keep)

  df_nonref <- NULL
  if (length(idx)) {
    est <- coefs[idx, 1]; se <- coefs[idx, 2]; p <- coefs[idx, 4]
    z   <- stats::qnorm(0.975)
    lo  <- est - z * se; hi <- est + z * se
    if (.is_ratio(approach)) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
    term <- rn[idx]
    lvl  <- sub(paste0("^`?", exposure, "`?"), "", term)
    lvl[lvl == ""] <- exposure  # continuous → single row at header
    lvl  <- sub("^[:\\.]?", "", lvl)

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
  if (!is.null(fit$model[[exposure]]) && is.factor(fit$model[[exposure]])) {
    levs <- levels(fit$model[[exposure]])
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
  smry  <- summary(fit)
  coefs <- smry$coefficients
  if (is.null(coefs) || nrow(coefs) == 0) return(NULL)

  out_list <- lapply(exposures, function(exposure) {
    rn <- rownames(coefs)
    keep <- grepl(paste0("^", exposure), rn) |
      grepl(paste0("^`", exposure, "`"), rn) |
      rn %in% exposure | rn %in% paste0("`", exposure, "`")
    idx <- which(keep)

    df_nonref <- NULL
    if (length(idx)) {
      est <- coefs[idx, 1]; se <- coefs[idx, 2]; p <- coefs[idx, 4]
      z   <- stats::qnorm(0.975)
      lo  <- est - z * se; hi <- est + z * se
      if (.is_ratio(approach)) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
      term <- rn[idx]
      lvl  <- sub(paste0("^`?", exposure, "`?"), "", term)
      lvl[lvl == ""] <- exposure           # continuous → single row under header
      lvl  <- sub("^[:\\.]?", "", lvl)

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
    if (!is.null(fit$model[[exposure]]) && is.factor(fit$model[[exposure]])) {
      levs <- levels(fit$model[[exposure]])
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
  })

  out_list <- Filter(Negate(is.null), out_list)
  if (!length(out_list)) return(NULL)
  do.call(rbind, out_list)
}
