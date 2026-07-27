#' Extract model-fit statistics from fitted models
#'
#' @keywords internal
#' @noRd
.model_stats_table <- function(models, approach) {
  if (is.null(models) || !length(models)) {
    return(NULL)
  }

  out <- lapply(names(models), function(model_name) {
    .model_stats_one(models[[model_name]], model_name = model_name, approach = approach)
  })

  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    return(NULL)
  }

  do.call(rbind, out)
}

#' Extract model-fit statistics from one fitted model
#'
#' @keywords internal
#' @noRd
.model_stats_one <- function(model, model_name, approach) {
  data.frame(
    model = model_name,
    AIC = .safe_numeric(stats::AIC(model)),
    BIC = .safe_numeric(stats::BIC(model)),
    logLik = .safe_numeric(as.numeric(stats::logLik(model))),
    deviance = .safe_numeric(stats::deviance(model)),
    null_deviance = .safe_null_deviance(model),
    pseudo_r2 = .safe_pseudo_r2(model, approach),
    r_squared = .safe_r_squared(model),
    adj_r_squared = .safe_adj_r_squared(model),
    n = .safe_numeric(stats::nobs(model)),
    stringsAsFactors = FALSE
  )
}

#' Safely coerce a scalar statistic to numeric
#'
#' @keywords internal
#' @noRd
.safe_numeric <- function(expr) {
  out <- tryCatch(expr, error = function(e) NA_real_)
  out <- suppressWarnings(as.numeric(out))
  if (!length(out) || is.na(out[1])) {
    return(NA_real_)
  }
  out[1]
}

#' Safely extract null deviance
#'
#' @keywords internal
#' @noRd
.safe_null_deviance <- function(model) {
  out <- tryCatch(model$null.deviance, error = function(e) NA_real_)
  out <- suppressWarnings(as.numeric(out))
  if (!length(out) || is.na(out[1])) {
    return(NA_real_)
  }
  out[1]
}

#' Safely calculate McFadden pseudo R-squared for non-linear models
#'
#' @keywords internal
#' @noRd
.safe_pseudo_r2 <- function(model, approach) {
  if (identical(approach, "linear")) {
    return(NA_real_)
  }

  ll_model <- .safe_numeric(as.numeric(stats::logLik(model)))
  if (!is.finite(ll_model)) {
    return(NA_real_)
  }

  ll_null <- tryCatch({
    null_model <- NULL
    utils::capture.output(
      null_model <- stats::update(model, stats::as.formula(". ~ 1"))
    )
    as.numeric(stats::logLik(null_model))
  }, error = function(e) NA_real_)
  ll_null <- .safe_numeric(ll_null)

  if (!is.finite(ll_null) || isTRUE(all.equal(ll_null, 0))) {
    return(.safe_deviance_pseudo_r2(model))
  }

  out <- 1 - (ll_model / ll_null)
  if (!is.finite(out)) {
    return(.safe_deviance_pseudo_r2(model))
  }
  out
}

#' Safely calculate deviance-based pseudo R-squared
#'
#' @keywords internal
#' @noRd
.safe_deviance_pseudo_r2 <- function(model) {
  dev <- .safe_numeric(stats::deviance(model))
  null_dev <- .safe_null_deviance(model)

  if (!is.finite(dev) || !is.finite(null_dev) || isTRUE(all.equal(null_dev, 0))) {
    return(NA_real_)
  }

  out <- 1 - (dev / null_dev)
  if (!is.finite(out)) {
    return(NA_real_)
  }
  out
}

#' Safely extract R-squared for linear models
#'
#' @keywords internal
#' @noRd
.safe_r_squared <- function(model) {
  if (!inherits(model, "lm")) {
    return(NA_real_)
  }
  out <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)
  out <- suppressWarnings(as.numeric(out))
  if (!length(out) || is.na(out[1])) {
    return(NA_real_)
  }
  out[1]
}

#' Safely extract adjusted R-squared for linear models
#'
#' @keywords internal
#' @noRd
.safe_adj_r_squared <- function(model) {
  if (!inherits(model, "lm")) {
    return(NA_real_)
  }
  out <- tryCatch(summary(model)$adj.r.squared, error = function(e) NA_real_)
  out <- suppressWarnings(as.numeric(out))
  if (!length(out) || is.na(out[1])) {
    return(NA_real_)
  }
  out[1]
}
