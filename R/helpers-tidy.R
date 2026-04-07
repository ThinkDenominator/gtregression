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
  smry <- summary(fit)
  coefs <- smry$coefficients

  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }

  rn <- rownames(coefs)

  .is_exposure_term <- function(term, exposure) {
    term_clean <- gsub("`", "", term, fixed = TRUE)
    exp_clean  <- gsub("`", "", exposure, fixed = TRUE)

    # 1. Exact match (continuous)
    if (identical(term_clean, exp_clean)) {
      return(TRUE)
    }

    # 2. Factor main effects (e.g. sexMale)
    if (startsWith(term_clean, exp_clean) && !grepl(":", term_clean, fixed = TRUE)) {
      return(TRUE)
    }

    # 3. Interaction term: must explicitly contain exposure as a separate part
    if (grepl(":", term_clean, fixed = TRUE)) {
      parts <- strsplit(term_clean, ":", fixed = TRUE)[[1]]
      parts <- gsub("`", "", parts, fixed = TRUE)

      # KEY: exact match, not partial
      return(any(parts == exp_clean))
    }

    FALSE
  }

  .clean_term_label <- function(term, exposure) {
    term_clean <- gsub("`", "", term, fixed = TRUE)
    exp_clean  <- gsub("`", "", exposure, fixed = TRUE)

    # Main effect
    if (identical(term_clean, exp_clean)) {
      return(exp_clean)
    }

    # Factor level (e.g. sexMale -> Male)
    if (startsWith(term_clean, exp_clean) && !grepl(":", term_clean, fixed = TRUE)) {
      lvl <- sub(paste0("^", exp_clean), "", term_clean)
      if (identical(lvl, "")) return(exp_clean)
      return(lvl)
    }

    # Interaction term (KEY FIX)
    if (grepl(":", term_clean, fixed = TRUE)) {
      parts <- strsplit(term_clean, ":", fixed = TRUE)[[1]]
      parts <- gsub("`", "", parts, fixed = TRUE)

      # Always return full readable interaction
      return(paste(parts, collapse = " × "))
    }

    term_clean
  }

  out_list <- lapply(exposures, function(exposure) {
    idx <- which(vapply(rn, .is_exposure_term, logical(1), exposure = exposure))

    df_nonref <- NULL

    if (length(idx) > 0) {
      est <- coefs[idx, 1]
      se  <- coefs[idx, 2]
      p   <- coefs[idx, 4]

      z  <- stats::qnorm(0.975)
      lo <- est - z * se
      hi <- est + z * se

      if (.is_ratio(approach)) {
        est <- exp(est)
        lo  <- exp(lo)
        hi  <- exp(hi)
      }

      term <- rn[idx]
      lvl <- vapply(term, .clean_term_label, character(1), exposure = exposure)

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

      df_nonref <- df_nonref[!duplicated(df_nonref$level), , drop = FALSE]
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
        is_interaction <- grepl("×", df_nonref$level, fixed = TRUE)

        ord_main <- match(df_nonref$level, levs)
        ord_main[is.na(ord_main)] <- Inf

        df_nonref$..interaction <- is_interaction
        df_nonref$..ord <- ord_main

        df_nonref <- df_nonref[
          order(df_nonref$..interaction, df_nonref$..ord, df_nonref$level),
          ,
          drop = FALSE
        ]

        df_nonref$..interaction <- NULL
        df_nonref$..ord <- NULL
      }
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

  do.call(rbind, out_list)
}
