#' Linear Regression Diagnostic Checks (Internal)
#'
#' Performs diagnostic checks for fitted linear regression models.
#'
#' The returned rows cover heteroskedasticity, residual normality, model
#' specification, and influential observations. If a diagnostic cannot be
#' computed for a given model, the function returns a clear "not available"
#' result for that row instead of failing.
#'
#' @param model A fitted linear model (`lm` object).
#' @param exposure Character string giving the name of the exposure variable
#'   for labeling.
#'
#' @return A data frame with one row per diagnostic test, including:
#' \describe{
#'   \item{Exposure}{Name of the exposure variable.}
#'   \item{Test}{Diagnostic test name.}
#'   \item{Statistic}{Test statistic or summary (e.g., p-values).}
#'   \item{Interpretation}{Plain-language result interpretation.}
#' }
#'
#' @keywords internal
.reg_check_linear <- function(model, exposure) {
  if (!inherits(model, "lm")) {
    stop("`model` must be a fitted linear model created by lm().", call. = FALSE)
  }
  if (!is.character(exposure) || length(exposure) != 1L ||
      is.na(exposure) || !nzchar(exposure)) {
    stop("`exposure` must be a single non-empty character string.",
         call. = FALSE)
  }

  safe_p_value <- function(expr) {
    result <- tryCatch(
      suppressWarnings(expr),
      error = function(e) NULL
    )
    if (is.null(result) || is.null(result$p.value) ||
        length(result$p.value) != 1L || is.na(result$p.value)) {
      return(NA_real_)
    }
    as.numeric(result$p.value)
  }

  format_p <- function(p) {
    if (is.na(p)) {
      return("not available")
    }
    paste0("p = ", signif(p, 4))
  }

  bp_p <- safe_p_value(lmtest::bptest(model))

  resid <- stats::residuals(model)
  resid <- resid[is.finite(resid)]
  sw_p <- if (length(resid) < 3L) {
    NA_real_
  } else {
    safe_p_value(stats::shapiro.test(utils::head(resid, 5000L)))
  }

  reset_p <- safe_p_value(lmtest::resettest(model, power = 2:3, type = "fitted"))

  n <- tryCatch(stats::nobs(model), error = function(e) NA_integer_)
  cooks <- tryCatch(
    suppressWarnings(stats::cooks.distance(model)),
    error = function(e) numeric(0)
  )
  cooks <- cooks[is.finite(cooks)]
  threshold <- if (!is.na(n) && n > 0) 4 / n else NA_real_
  high_infl <- if (length(cooks) && !is.na(threshold)) {
    sum(cooks > threshold, na.rm = TRUE)
  } else {
    NA_integer_
  }

  interp <- c(
    if (is.na(bp_p)) {
      "Heteroskedasticity test could not be calculated for this model."
    } else if (bp_p < 0.05) {
      "Heteroskedasticity detected: residual variance may not be constant."
    } else {
      "No evidence of heteroskedasticity."
    },
    if (is.na(sw_p)) {
      "Residual normality test could not be calculated for this model."
    } else if (sw_p < 0.05) {
      "Residuals may not be normally distributed; use caution with small samples."
    } else {
      "Residuals appear normally distributed."
    },
    if (is.na(reset_p)) {
      "Model specification test could not be calculated for this model."
    } else if (reset_p < 0.05) {
      "Model may be mis-specified; consider non-linear terms or interactions."
    } else {
      "Functional form appears adequate."
    },
    if (is.na(high_infl)) {
      "Cook's distance could not be calculated for this model."
    } else if (high_infl > 0) {
      "Influential points detected; review outliers or high-leverage observations."
    } else {
      "No strong influential observations detected."
    }
  )

  cook_stat <- if (is.na(high_infl) || is.na(threshold)) {
    "not available"
  } else {
    paste0(high_infl, " obs > 4/n (", round(threshold, 4), ")")
  }

  data.frame(
    Exposure = rep(exposure, 4),
    Test = c("Breusch-Pagan", "Shapiro-Wilk", "RESET", "Cook's Distance"),
    Statistic = c(
      format_p(bp_p),
      format_p(sw_p),
      format_p(reset_p),
      cook_stat
    ),
    Interpretation = interp,
    stringsAsFactors = FALSE
  )
}
