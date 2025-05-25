#' Check Convergence for a Specific Regression Approach
#'
#' This function checks convergence status and maximum predicted probability
#' for each exposure (univariate) or combined exposures (multivariate)
#' using a specified regression approach.
#'
#' @param data A data frame.
#' @param exposures A character vector of predictor variables.
#' @param outcome The name of the outcome variable.
#' @param approach Regression approach: one of "logit", "log-binomial", "poisson",
#' "robpoisson" or "negbin".
#' @param multivariate Logical. If TRUE, checks convergence for a multivariate model.
#'
#' @return A data frame with columns: Exposure, Model, Converged, Max.prob.
#' @details For `robpoisson`, predicted probabilities may occasionally exceed 1. These are valid for estimating risk ratios but should not be interpreted as actual probabilities.
#' @export
check_convergence <- function(data, exposures, outcome, approach = "logit", multivariate = FALSE) {
  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "negbin")
  if (!approach %in% valid_approaches) {
    stop("Invalid approach. Choose from: ", paste(valid_approaches, collapse = ", "))
  }

  outcome_vec <- data[[outcome]]
  is_binary <- function(x) {
    is.factor(x) && length(levels(x)) == 2 ||
      is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE) ||
      is.logical(x)
  }
  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) && length(unique(x[!is.na(x)])) > 2
  }

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("The outcome must be binary for the selected approach: ", approach)
  }
  if (approach %in% c("poisson", "negbin") && !is_count(outcome_vec)) {
    stop("The outcome must be a count (non-negative integers) for the selected approach: ", approach)
  }

  if (nrow(data) == 0) {
    return(data.frame(
      Exposure = if (multivariate) NA else character(0),
      Model = character(0),
      Converged = logical(0),
      Max.prob. = numeric(0)
    ))
  }

  results_list <- list()

  if (!multivariate) {
    for (exposure in exposures) {
      fmla <- stats::as.formula(paste(outcome, "~", exposure))
      result <- tryCatch({
        fit <- switch(
          approach,
          "logit" = glm(fmla, data = data, family = binomial("logit")),
          "log-binomial" = glm(fmla, data = data, family = binomial("log")),
          "poisson" = glm(fmla, data = data, family = poisson("log")),
          "negbin" = MASS::glm.nb(fmla, data = data),
          risks::riskratio(formula = fmla, data = data, approach = approach)
        )
        converged <- if ("converged" %in% names(fit)) fit$converged else if (!is.null(fit$conv)) fit$conv == 0 else NA
        max_prob <- if ("maxprob" %in% names(fit)) fit$maxprob else max(predict(fit, type = "response"), na.rm = TRUE)
        if (approach == "robpoisson" && max_prob > 1) {
          warning("robpoisson: Predicted probability exceeds 1. Interpret RR estimates with caution.")
        }
        data.frame(Exposure = exposure, Model = approach, Converged = converged, Max.prob. = max_prob)
      }, error = function(e) {
        data.frame(Exposure = exposure, Model = approach, Converged = FALSE, Max.prob. = NA)
      })
      results_list[[exposure]] <- result
    }
  } else {
    fmla <- stats::as.formula(paste(outcome, "~", paste(exposures, collapse = " + ")))
    result <- tryCatch({
      fit <- switch(
        approach,
        "logit" = glm(fmla, data = data, family = binomial("logit")),
        "log-binomial" = glm(fmla, data = data, family = binomial("log")),
        "poisson" = glm(fmla, data = data, family = poisson("log")),
        "negbin" = MASS::glm.nb(fmla, data = data),
        risks::riskratio(formula = fmla, data = data, approach = approach)
      )
      converged <- if ("converged" %in% names(fit)) fit$converged else if (!is.null(fit$conv)) fit$conv == 0 else NA
      max_prob <- if ("maxprob" %in% names(fit)) fit$maxprob else max(predict(fit, type = "response"), na.rm = TRUE)
      if (approach == "robpoisson" && max_prob > 1) {
        warning("robpoisson: Predicted probability exceeds 1. Interpret RR estimates with caution.")
      }
      data.frame(Exposure = paste(exposures, collapse = " + "), Model = approach, Converged = converged, Max.prob. = max_prob)
    }, error = function(e) {
      data.frame(Exposure = NA, Model = approach, Converged = FALSE, Max.prob. = NA)
    })
    results_list[["multivariable"]] <- result
  }

  return(do.call(rbind, results_list))
}
