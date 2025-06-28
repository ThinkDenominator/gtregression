#' Check Convergence for a Regression Model
#'
#' Assesses model convergence and provides diagnostics
#' for each exposure (in univariate mode) or
#' for the full model (in multivariable mode),
#' depending on the regression approach used.
#'
#' @param data A data frame containing the dataset.
#' @param exposures A character vector of predictor variable names.
#' If \code{multivariate = FALSE},
#' each exposure is assessed separately; otherwise, combined model is evaluated.
#' @param outcome A character string specifying the outcome variable.
#' @param approach A character string specifying the regression approach.
#' One of:
#'   \code{"logit"}, \code{"log-binomial"}, \code{"poisson"},
#'   \code{"robpoisson"}, or \code{"negbin"}.
#' @param multivariate Logical. If \code{TRUE},
#' checks convergence for a multivariable model;
#'   otherwise, performs checks for each univariate model.
#'
#' @return A data frame summarizing convergence diagnostics, including:
#' \describe{
#'   \item{\code{Exposure}}{Name of the exposure variable.}
#'   \item{\code{Model}}{The regression approach used.}
#'   \item{\code{Converged}}{\code{TRUE} if the model converged successfully;
#'    \code{FALSE} otherwise.}
#'   \item{\code{Max.prob}}{Maximum predicted probability or
#'   fitted value in the dataset.}
#' }
#'
#' @details
#' For \code{robpoisson}, predicted probabilities (fitted values) may exceed 1,
#' which is acceptable when estimating risk ratios but should not be interpreted
#' as actual probabilities.
#'
#' This function is useful for identifying convergence issues, especially for
#' \code{"log-binomial"} models, which often fail to converge .
#'
#' @seealso [identify_confounder()],  [interaction_models()]
#'
#' @examples
#' if (requireNamespace("gtregression", quietly = TRUE)) {
#'   data(data_PimaIndiansDiabetes, package = "gtregression")
#'
#'   check_convergence(
#'     data = data_PimaIndiansDiabetes,
#'     exposures = c("age", "bmi"),
#'     outcome = "diabetes",
#'     approach = "logit"
#'   )
#'
#'   check_convergence(
#'     data = data_PimaIndiansDiabetes,
#'     exposures = c("age", "bmi"),
#'     outcome = "diabetes",
#'     approach = "logit",
#'     multivariate = TRUE
#'   )
#' }
#' @export
check_convergence <- function(data,
                              exposures,
                              outcome,
                              approach = "logit",
                              multivariate = FALSE) {

  .validate_approach(approach, context = "uni_reg")

  .validate_outcome_by_approach(data[[outcome]], approach)

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
          fit <- switch(approach,
            "logit" = glm(fmla, data = data, family = binomial("logit")),
            "log-binomial" = glm(fmla, data = data, family = binomial("log")),
            "poisson" = glm(fmla, data = data, family = poisson("log")),
            "negbin" = MASS::glm.nb(fmla, data = data),
            risks::riskratio(formula = fmla, data = data, approach = approach)
          )
          converged <- if ("converged" %in% names(fit)) fit$converged
          else if (!is.null(fit$conv)) fit$conv == 0
          else NA
          max_prob <- if ("maxprob" %in% names(fit)) fit$maxprob
          else max(predict(fit, type = "response"), na.rm = TRUE)
          if (approach == "robpoisson" && max_prob > 1) {
            warning("robpoisson: Predicted probability exceeds 1.
              Interpret estimates with caution.", call. = FALSE)
          }
          data.frame(Exposure = exposure, Model = approach,
                     Converged = converged, Max.prob. = max_prob)
        },
        error = function(e) {
          data.frame(Exposure = exposure, Model = approach,
                     Converged = FALSE, Max.prob. = NA)
        }
      )
      results_list[[exposure]] <- result
    }
  } else {
    fmla <- stats::as.formula(paste(outcome, "~",
                                    paste(exposures, collapse = " + ")))
    result <- tryCatch({
        fit <- switch(approach,
          "logit" = glm(fmla, data = data, family = binomial("logit")),
          "log-binomial" = glm(fmla, data = data, family = binomial("log")),
          "poisson" = glm(fmla, data = data, family = poisson("log")),
          "negbin" = MASS::glm.nb(fmla, data = data),
          risks::riskratio(formula = fmla, data = data, approach = approach)
        )
        converged <- if ("converged" %in% names(fit)) fit$converged
        else if (!is.null(fit$conv)) fit$conv == 0 else NA
        max_prob <- if ("maxprob" %in% names(fit)) fit$maxprob
        else max(predict(fit, type = "response"), na.rm = TRUE)

        # Custom warning if predicted prob > 1
        if (approach == "robpoisson" && max_prob > 1) {
          warning("robpoisson: Predicted probability exceeds 1.
              Interpret estimates with caution.", call. = FALSE)
        }
        data.frame(Exposure = paste(exposures, collapse = " + "),
                   Model = approach, Converged = converged,
                   Max.prob. = max_prob)
      },
      error = function(e) {
        warning(glue::glue("{approach}: Model fitting failed — {e$message}"))
        data.frame(Exposure = NA,
                   Model = approach,
                   Converged = FALSE,
                   Max.prob. = NA)
      }
    )


    results_list[["multivariable"]] <- result
  }

  return(do.call(rbind, results_list))
}
