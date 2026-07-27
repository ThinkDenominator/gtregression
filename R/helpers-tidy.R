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
  mf[[variable]]
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
  z <- stats::qnorm(0.975)

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
  keep <- grepl(paste0("^", exposure), rn) |
    grepl(paste0("^`", exposure, "`"), rn) |
    rn %in% exposure | rn %in% paste0("`", exposure, "`")
  idx <- which(keep)

  df_nonref <- NULL
  if (length(idx)) {
    est <- coefs$estimate[idx]
    lo  <- coefs$conf.low[idx]
    hi  <- coefs$conf.high[idx]
    p   <- coefs$p.value[idx]
    if (.is_ratio(approach)) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
    term <- rn[idx]
    lvl  <- sub(paste0("^`?", exposure, "`?"), "", term)
    lvl[lvl == ""] <- exposure  # continuous -> single row at header
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
      return(paste(parts, collapse = " x "))
    }

    term_clean
  }

  out_list <- lapply(exposures, function(exposure) {
    idx <- which(vapply(rn, .is_exposure_term, logical(1), exposure = exposure))

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
        is_interaction <- grepl(" x ", df_nonref$level, fixed = TRUE)

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
