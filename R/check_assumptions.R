#' @keywords internal
#' @noRd
.resolve_weights <- function(weights, data) {
  if (is.null(weights)) return(NULL)

  # Accept either a column name (length-1 character) or a numeric vector
  v <- NULL
  if (is.character(weights) && length(weights) == 1L) {
    if (!weights %in% names(data)) stop("`weights` column not found in `data`.")
    v <- data[[weights]]
  } else if (is.numeric(weights)) {
    v <- weights
  } else {
    stop("`weights` must be a column name (character) or a numeric vector.")
  }

  v <- as.numeric(v)
  if (anyNA(v)) {
    # Safer default for beginners: set NA weights to 0 and warn.
    warning("`weights` contains NA; setting NA weights to 0.")
    v[is.na(v)] <- 0
  }
  v
}

#' Check model assumptions (beginner-friendly)
#'
#' @description
#' Fits a model under the hood (like `uni_reg()`/`multi_reg()`) and runs
#' assumption checks appropriate to the approach. Returns a layered object with
#' a tidy summary table and plots for quick visual diagnosis.
#'
#' @param data A data frame.
#' @param outcome Character. Outcome variable name.
#' @param exposures Character vector of predictors. If `NULL`, uses all columns
#'   except `outcome`.
#' @param approach One of `"auto","linear","logit","log-binomial","poisson",
#'   "robpoisson","negbin"`. Default `"auto"`.
#' @param multivariate Logical. If `TRUE`, fits one adjusted model with all
#'   `exposures` (and `confounders` if supplied). If `FALSE`, screens each
#'   exposure with `outcome ~ exposure` and stacks the diagnostics.
#' @param confounders Optional character vector (used when `multivariate = TRUE`).
#' @param weights Optional weights vector name (character).
#' @param cluster Optional cluster id variable name for robust notes (placeholder).
#' @param groups Integer. Number of bins for calibration curve (binary models).
#' @param top_n Integer. How many influential points to list/mark.
#' @param explain Logical. If `TRUE`, adds plain-English suggestions to notes.
#' @param output One of `"both","summary","plots"`.
#' @param quiet Logical. Suppress messages.
#' @param ... Reserved for future options.
#'
#' @return An object of class `gt_assumption_check` with:
#'  - `$summary`: tibble of assumption results
#'  - `$plots`: named list of ggplot objects (may be empty)
#'  - `$details`: raw test objects to aid reproducibility (always includes `$fit`)
#'  - `$meta`: list with `approach`, `formula`, `multivariate`, `n`, `weights_used`
#'
#' @examples
#' # Logistic example
#' df <- mtcars; df$am <- as.integer(df$am)
#' ac <- check_assumptions(
#'   data = df, outcome = "am", exposures = c("wt","hp"),
#'   approach = "auto", multivariate = TRUE, explain = TRUE
#' )
#' ac$summary
#' if (interactive()) plot(ac)
#'
#' # Poisson example
#' ac2 <- check_assumptions(
#'   data = warpbreaks, outcome = "breaks",
#'   exposures = c("wool","tension"), approach = "auto", multivariate = TRUE
#' )
#' ac2$summary
#'
#' @export
check_assumptions <- function(
    data,
    outcome,
    exposures = NULL,
    approach = c("auto","linear","logit","log-binomial","poisson","robpoisson","negbin"),
    multivariate = TRUE,
    confounders = NULL,
    weights = NULL,
    cluster = NULL,
    groups = 10,
    top_n = 5,
    explain = TRUE,
    output = c("both","summary","plots"),
    quiet = TRUE,
    ...
) {
  output   <- match.arg(output)
  approach <- match.arg(approach)

  stopifnot(is.data.frame(data))
  if (!outcome %in% names(data)) stop("Outcome not found in data.")
  if (is.null(exposures)) exposures <- setdiff(names(data), outcome)
  if (!all(exposures %in% names(data))) stop("One or more exposures not in data.")

  # Minimal validators for outcome type
  y <- data[[outcome]]
  is_binary  <- function(x) (is.factor(x) && length(levels(x)) == 2) ||
    (is.numeric(x) && all(x %in% c(0,1), na.rm = TRUE)) ||
    is.logical(x)
  is_count   <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  is_continu <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  # Auto approach detection
  if (approach == "auto") {
    if (is_binary(y)) {
      approach <- "logit"
      if (!quiet) message("Auto-detected binary outcome → logit.")
    } else if (is_count(y)) {
      approach <- "poisson"
      if (!quiet) message("Auto-detected count outcome → poisson (will test dispersion).")
    } else if (is_continu(y)) {
      approach <- "linear"
      if (!quiet) message("Auto-detected continuous outcome → linear.")
    } else {
      stop("Could not auto-detect a suitable approach for the outcome.")
    }
  }

  # weights
  # weights
  wts <- .resolve_weights(weights, data)


  # Helper: dispatch one formula/data run
  .run_once <- function(fmla, appr) {
    switch(
      appr,
      "logit"        = .assump_logit(data, fmla, groups = groups, top_n = top_n, explain = explain, quiet = quiet, weights = wts),
      "poisson"      = .assump_poisson(data, fmla, top_n = top_n, explain = explain, quiet = quiet, weights = wts),
      "linear"       = .assump_linear(data, fmla, top_n = top_n, explain = explain, quiet = quiet, weights = wts),
      "log-binomial" = .assump_logbin(data, fmla, groups = groups, top_n = top_n, explain = explain, quiet = quiet, weights = wts),
      "robpoisson"   = .assump_robpoisson(data, fmla, top_n = top_n, explain = explain, quiet = quiet, weights = wts),
      "negbin"       = .assump_negbin(data, fmla, top_n = top_n, explain = explain, quiet = quiet, weights = wts),
      stop("Approach not supported.")
    )
  }

  # Build formulas
  results <- NULL
  plots   <- list()
  details <- list()
  meta    <- list(approach = approach, multivariate = multivariate, n = nrow(data),
                  weights_used = !is.null(wts), outcome = outcome)

  if (isTRUE(multivariate)) {
    rhs <- exposures
    if (!is.null(confounders)) rhs <- unique(c(exposures, confounders))
    fmla <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
    out  <- .run_once(fmla, approach)
    # annotate
    out$summary$model <- deparse(fmla)
    results <- out$summary
    plots   <- out$plots
    details <- out$details
    meta$formula <- deparse(fmla)
  } else {
    # screen: one exposure at a time
    results <- tibble::tibble()
    for (x in exposures) {
      fmla <- stats::as.formula(paste(outcome, "~", x))
      out  <- .run_once(fmla, approach)
      s    <- out$summary
      if (!is.null(s) && nrow(s)) {
        s$model <- deparse(fmla)
        s$exposure <- x
      }
      results <- rbind(results, s)
      # Keep the *last* run's plots and fit for convenience; stash others in details
      plots <- out$plots
      details[[x]] <- out$details
    }
    meta$formula <- "screen: outcome ~ each exposure"
  }

  # Assemble
  res <- list(
    summary = results,
    plots   = plots,
    details = details,
    meta    = meta
  )
  class(res) <- c("gt_assumption_check", class(res))

  if (output == "summary") res$plots <- list()
  if (output == "plots")   res$summary <- res$summary[0, ]
  res
}

# ---------------- helpers: summaries/plots ----------------

.pad_range <- function(x, pad = 0.04) {
  r <- range(x, na.rm = TRUE); w <- diff(r)
  if (!is.finite(w) || w == 0) r + c(-1, 1) else r + c(-pad, pad) * w
}

.safe_theme <- function() ggplot2::theme_minimal(base_size = 11)

.add_note <- function(explain, text) if (isTRUE(explain)) text else ""

.safe_calibration_bins <- function(p, groups = 10) {
  # Try quantile breaks; if duplicates, fall back to pretty breaks over range
  qs <- stats::quantile(p, probs = seq(0, 1, length.out = groups + 1), na.rm = TRUE)
  if (anyDuplicated(signif(qs, 10))) {
    br <- seq(min(p, na.rm = TRUE), max(p, na.rm = TRUE), length.out = groups + 1)
  } else {
    br <- qs
  }
  cut(p, breaks = br, include.lowest = TRUE)
}

# ---------------- engines ----------------

.assump_logit <- function(data, fmla, groups = 10, top_n = 5, explain = TRUE, quiet = TRUE, weights = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  fit <- stats::glm(fmla, data = data, family = stats::binomial("logit"),
                    weights = weights,
                    na.action = stats::na.exclude)

  pr <- stats::predict(fit, type = "response")
  pe <- stats::residuals(fit, type = "pearson")

  # Separation check (heuristic)
  sep_flag <- any(pr < 1e-6 | pr > 1 - 1e-6, na.rm = TRUE)
  sep_note <- if (sep_flag)
    paste0("Possible separation (predicted probabilities near 0/1). ",
           .add_note(explain, "Consider robust Poisson or penalized logistic."))
  else "OK"

  # Calibration
  g <- .safe_calibration_bins(pr, groups)
  obs <- tapply(data[[all.vars(fmla)[1]]], g, mean, na.rm = TRUE)
  exp <- tapply(pr, g, mean, na.rm = TRUE)
  calib_df <- data.frame(group = seq_along(obs), observed = as.numeric(obs), expected = as.numeric(exp))

  p_cal <- ggplot2::ggplot(calib_df, ggplot2::aes(expected, observed)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = "Calibration (grouped)", x = "Predicted mean", y = "Observed mean") +
    .safe_theme()

  # Influence
  h   <- stats::hatvalues(fit)
  ck  <- stats::cooks.distance(fit)
  ord <- order(ck, decreasing = TRUE)[seq_len(min(top_n, length(ck)))]
  infl_df <- data.frame(index = seq_along(ck), cook = ck, leverage = h, .flag = FALSE)
  infl_df$.flag[infl_df$index %in% ord] <- TRUE

  p_inf <- ggplot2::ggplot(infl_df, ggplot2::aes(leverage, cook)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_point(data = infl_df[infl_df$.flag, ], color = "red") +
    ggplot2::labs(title = "Influence (Cook's vs leverage)", x = "Leverage", y = "Cook's distance") +
    .safe_theme()

  # Residuals vs fitted
  p_rvf <- ggplot2::ggplot(data.frame(fitted = pr, resid = pe), ggplot2::aes(fitted, resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = "Residuals vs fitted (Pearson)", x = "Fitted (p)", y = "Pearson residuals") +
    .safe_theme()

  tib <- tibble::tibble(
    assumption = c("Separation", "Calibration (grouped)", "Influence (Cook's)", "Residual pattern"),
    test       = c("Pred prob near 0/1", paste0("Grouped bins = ", groups), "Top Cook's indices", "LOESS trend"),
    variable   = c(NA, NA, paste0(utils::head(ord, top_n), collapse = ", "), NA),
    statistic  = c(NA, NA, sprintf("max Cook's = %.3f", max(ck, na.rm = TRUE)), NA),
    df         = c(NA, NA, NA, NA),
    p_value    = c(NA, NA, NA, NA),
    decision   = c(ifelse(sep_flag, "Flag", "OK"), "Inspect", "Inspect", "Inspect"),
    note       = c(sep_note,
                   .add_note(explain, "Deviation from 45° suggests mis-specification or missing interactions."),
                   .add_note(explain, "High influence points: verify data and model form."),
                   .add_note(explain, "Curvature/funnel patterns suggest re-specification."))
  )

  list(
    summary = tib,
    plots   = list(calibration = p_cal, influence = p_inf, resid_fitted = p_rvf),
    details = list(fit = fit, calib = calib_df, influence = infl_df)
  )
}

.assump_poisson <- function(data, fmla, top_n = 5, explain = TRUE, quiet = TRUE, weights = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  fit <- stats::glm(fmla, data = data, family = stats::poisson("log"),
                    weights = weights,
                    na.action = stats::na.exclude)

  pe <- stats::residuals(fit, type = "pearson")
  mu <- stats::fitted(fit)

  # Dispersion (Pearson phi)
  disp <- sum(pe^2, na.rm = TRUE) / fit$df.residual
  over <- is.finite(disp) && disp > 1.5
  disp_note <- if (over)
    paste0(sprintf("Overdispersion detected (phi = %.2f). ", disp),
           .add_note(explain, "Consider robust Poisson (sandwich SE) or negative binomial."))
  else sprintf("OK (phi = %.2f)", disp)

  # Influence
  h   <- stats::hatvalues(fit)
  ck  <- stats::cooks.distance(fit)
  ord <- order(ck, decreasing = TRUE)[seq_len(min(top_n, length(ck)))]
  infl_df <- data.frame(index = seq_along(ck), cook = ck, leverage = h, .flag = FALSE)
  infl_df$.flag[infl_df$index %in% ord] <- TRUE

  p_inf <- ggplot2::ggplot(infl_df, ggplot2::aes(leverage, cook)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_point(data = infl_df[infl_df$.flag, ], color = "red") +
    ggplot2::labs(title = "Influence (Cook's vs leverage)", x = "Leverage", y = "Cook's distance") +
    .safe_theme()

  # Residuals vs fitted
  p_rvf <- ggplot2::ggplot(data.frame(fitted = mu, resid = pe), ggplot2::aes(fitted, resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = "Residuals vs fitted (Pearson)", x = "Fitted (mu)", y = "Pearson residuals") +
    .safe_theme()

  # Rootogram-lite
  df_root <- data.frame(obs = data[[all.vars(fmla)[1]]], fitted = round(mu))
  tab_obs <- table(factor(df_root$obs, levels = 0:max(df_root$obs, na.rm = TRUE)))
  tab_fit <- table(factor(df_root$fitted, levels = 0:max(df_root$obs, na.rm = TRUE)))
  root_df <- data.frame(k = as.integer(names(tab_obs)), obs = as.numeric(tab_obs), fit = as.numeric(tab_fit))
  root_df$root_diff <- sqrt(pmax(root_df$obs, 0)) - sqrt(pmax(root_df$fit, 0))

  p_root <- ggplot2::ggplot(root_df, ggplot2::aes(k, root_diff)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(title = "Rootogram (approx.)", x = "Count", y = "sqrt(obs) - sqrt(fit)") +
    .safe_theme()

  tib <- tibble::tibble(
    assumption = c("Dispersion", "Influence (Cook's)", "Residual pattern"),
    test       = c("Pearson phi = sum(pearson^2)/df", "Top Cook's indices", "LOESS trend"),
    variable   = c(NA, paste0(utils::head(ord, top_n), collapse = ", "), NA),
    statistic  = c(sprintf("phi = %.2f", disp), sprintf("max Cook's = %.3f", max(ck, na.rm = TRUE)), NA),
    df         = c(fit$df.residual, NA, NA),
    p_value    = c(NA, NA, NA),
    decision   = c(ifelse(over, "Flag", "OK"), "Inspect", "Inspect"),
    note       = c(disp_note,
                   .add_note(explain, "High influence points: verify data quality."),
                   .add_note(explain, "Curvature/funnel shapes suggest mis-specification."))
  )

  list(
    summary = tib,
    plots   = list(influence = p_inf, resid_fitted = p_rvf, rootogram = p_root),
    details = list(fit = fit, root = root_df, influence = infl_df, dispersion = disp)
  )
}

.assump_linear <- function(data, fmla, top_n = 5, explain = TRUE, quiet = TRUE, weights = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  fit <- stats::lm(fmla, data = data, weights = weights)

  r  <- stats::residuals(fit)
  f  <- stats::fitted(fit)
  n  <- length(r)

  # Heteroscedasticity proxy: Spearman(|resid| ~ fitted) + BP-like R^2*n
  spe <- suppressWarnings(stats::cor.test(abs(r), f, method = "spearman"))
  spe_rho <- if (!is.null(spe$estimate)) unname(spe$estimate) else NA_real_
  spe_p   <- if (!is.null(spe$p.value))  spe$p.value           else NA_real_

  bp_mod  <- stats::lm(I(r^2) ~ f)
  bp_stat <- max(0, summary(bp_mod)$r.squared) * (n - 1)
  bp_p    <- stats::pchisq(bp_stat, df = 1, lower.tail = FALSE)

  # Normality (skip Shapiro for very large n)
  sw <- if (n >= 5 && n <= 5000) stats::shapiro.test(r) else NULL
  sw_W  <- if (!is.null(sw)) as.numeric(sw$statistic) else NA_real_
  sw_df <- if (!is.null(sw)) as.numeric(sw$parameter) else NA_real_
  sw_p  <- if (!is.null(sw)) sw$p.value              else NA_real_

  # Influence
  h   <- stats::hatvalues(fit)
  ck  <- stats::cooks.distance(fit)
  ord <- order(ck, decreasing = TRUE)[seq_len(min(top_n, length(ck)))]
  infl_df <- data.frame(index = seq_along(ck), cook = ck, leverage = h, .flag = FALSE)
  infl_df$.flag[infl_df$index %in% ord] <- TRUE

  # Plots
  p_rvf <- ggplot2::ggplot(data.frame(fitted = f, resid = r), ggplot2::aes(fitted, resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = "Residuals vs fitted", x = "Fitted", y = "Residuals") +
    .safe_theme()

  p_sl <- ggplot2::ggplot(data.frame(fitted = f, sres = sqrt(abs(r))), ggplot2::aes(fitted, sres)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = "Scale–Location", x = "Fitted", y = "sqrt(|Residual|)") +
    .safe_theme()

  qq <- ggplot2::ggplot(data.frame(sample = r), ggplot2::aes(sample = sample)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    ggplot2::labs(title = "QQ plot of residuals") + .safe_theme()

  # Safe min that returns NA if all inputs are NA
  .min_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) min(x) else NA_real_ }

  tib <- tibble::tibble(
    assumption = c("Heteroscedasticity (proxy)", "Normality (Shapiro)", "Influence (Cook's)"),
    test       = c("Spearman |resid|~fitted + BP proxy", "Shapiro–Wilk", "Top Cook's indices"),
    variable   = c(NA, NA, paste0(utils::head(ord, top_n), collapse = ", ")),
    statistic  = c(sprintf("rho=%.2f; BP=%.2f", spe_rho, bp_stat),
                   if (is.null(sw)) "n>5000 (skipped)" else sprintf("W=%.3f", sw_W),
                   sprintf("max Cook's=%.3f", max(ck, na.rm = TRUE))),
    # Shapiro has no df; use scalar NA to keep column length = 3
    df         = c(1, NA_real_, NA_real_),
    p_value    = c(.min_na(c(spe_p, bp_p)), sw_p, NA),
    decision   = c("Inspect", "Inspect", "Inspect"),
    note       = c(.add_note(explain, "Non-constant variance or trend suggests re-specification or transformations."),
                   .add_note(explain, "Large deviations in QQ plot indicate non-normal residuals."),
                   .add_note(explain, "High influence points: check leverage and data quality."))
  )


  list(
    summary = tib,
    plots   = list(resid_fitted = p_rvf, scale_location = p_sl, qq = qq),
    details = list(fit = fit, influence = infl_df, bp_stat = bp_stat, bp_p = bp_p)
  )
}


.assump_logbin <- function(data, fmla, groups = 10, top_n = 5, explain = TRUE, quiet = TRUE, weights = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  # Attempt log-binomial; catch non-convergence gracefully
  warn <- NULL
  fit <- withCallingHandlers(
    expr = stats::glm(fmla, data = data, family = stats::binomial(link = "log"),
                      weights = weights,
                      na.action = stats::na.exclude),
    warning = function(w) { warn <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )

  pr <- stats::predict(fit, type = "response")
  bad <- any(pr > 1, na.rm = TRUE) || !is.null(warn)
  lb_note <- if (bad)
    paste0("Potential convergence/boundary issues", if (!is.null(warn)) paste0(" (", warn, ")"), ". ",
           .add_note(explain, "Consider robust Poisson as a stable alternative."))
  else "OK"

  # Reuse logistic-style diagnostics
  out <- .assump_logit(data, fmla, groups = groups, top_n = top_n, explain = explain, quiet = quiet, weights = weights)
  out$summary <- rbind(
    tibble::tibble(
      assumption = "Log-binomial fit",
      test       = "Convergence & predictions ≤ 1",
      variable   = NA_character_,
      statistic  = NA_character_,
      df         = NA_real_,
      p_value    = NA_real_,
      decision   = ifelse(bad, "Flag", "OK"),
      note       = lb_note
    ),
    out$summary
  )
  out$details$fit <- fit
  out
}

.assump_robpoisson <- function(data, fmla, top_n = 5, explain = TRUE, quiet = TRUE, weights = NULL) {
  # We fit a standard Poisson for diagnostics; robust SEs are about inference, not assumptions
  out <- .assump_poisson(data, fmla, top_n = top_n, explain = explain, quiet = quiet, weights = weights)
  out$summary <- rbind(
    tibble::tibble(
      assumption = "Robust inference note",
      test       = "Sandwich SEs (not computed here)",
      variable   = NA_character_,
      statistic  = NA_character_,
      df         = NA_real_,
      p_value    = NA_real_,
      decision   = "Info",
      note       = .add_note(explain, "Use sandwich::vcovHC + lmtest::coeftest for robust SEs if needed.")
    ),
    out$summary
  )
  out
}

.assump_negbin <- function(data, fmla, top_n = 5, explain = TRUE, quiet = TRUE, weights = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  if (!requireNamespace("MASS", quietly = TRUE)) {
    tib <- tibble::tibble(
      assumption = "Negative binomial",
      test       = "MASS::glm.nb availability",
      variable   = NA_character_,
      statistic  = NA_character_,
      df         = NA_real_,
      p_value    = NA_real_,
      decision   = "Flag",
      note       = "Package MASS not available; cannot fit negbin."
    )
    return(list(summary = tib, plots = list(), details = list(note = "MASS missing")))
  }
  fit <- MASS::glm.nb(fmla, data = data, weights = weights,
                      na.action = stats::na.exclude)
  # Reuse Poisson-style diagnostics on NB fit (Pearson residuals exist)
  pe <- stats::residuals(fit, type = "pearson")
  mu <- stats::fitted(fit)

  # Influence (approx via glm POI measures; MASS::cooks.distance.glm already works)
  ck  <- stats::cooks.distance(fit)
  h   <- stats::hatvalues(fit)
  ord <- order(ck, decreasing = TRUE)[seq_len(min(top_n, length(ck)))]
  infl_df <- data.frame(index = seq_along(ck), cook = ck, leverage = h, .flag = FALSE)
  infl_df$.flag[infl_df$index %in% ord] <- TRUE

  p_inf <- ggplot2::ggplot(infl_df, ggplot2::aes(leverage, cook)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_point(data = infl_df[infl_df$.flag, ], color = "red") +
    ggplot2::labs(title = "Influence (Cook's vs leverage, NB)", x = "Leverage", y = "Cook's distance") +
    .safe_theme()

  p_rvf <- ggplot2::ggplot(data.frame(fitted = mu, resid = pe), ggplot2::aes(fitted, resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = "Residuals vs fitted (Pearson, NB)", x = "Fitted (mu)", y = "Pearson residuals") +
    .safe_theme()

  tib <- tibble::tibble(
    assumption = c("Overdispersion handled", "Influence (Cook's)", "Residual pattern"),
    test       = c("Fitted NB (theta>0)", "Top Cook's indices", "LOESS trend"),
    variable   = c(NA, paste0(utils::head(ord, top_n), collapse = ", "), NA),
    statistic  = c(sprintf("theta = %.3f", fit$theta), sprintf("max Cook's = %.3f", max(ck, na.rm = TRUE)), NA),
    df         = c(fit$df.residual, NA, NA),
    p_value    = c(NA, NA, NA),
    decision   = c("OK", "Inspect", "Inspect"),
    note       = c(.add_note(explain, "Negative binomial addresses overdispersion."),
                   .add_note(explain, "Check leverage and influential observations."),
                   .add_note(explain, "Curvature/funnel suggests mis-specification."))
  )

  list(
    summary = tib,
    plots   = list(influence = p_inf, resid_fitted = p_rvf),
    details = list(fit = fit, theta = fit$theta, influence = infl_df)
  )
}

# ---------------- S3 methods ----------------

#' @export
print.gt_assumption_check <- function(x, ...) {
  if (!is.null(x$summary) && nrow(x$summary)) {
    print(x$summary)
  } else {
    cat("<gt_assumption_check: no summary rows>\n")
  }
  invisible(x)
}

#' @export
plot.gt_assumption_check <- function(x, ...) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  if (length(x$plots) == 0) {
    message("No plots available.")
    return(invisible(NULL))
  }
  for (nm in names(x$plots)) print(x$plots[[nm]])
  invisible(NULL)
}
