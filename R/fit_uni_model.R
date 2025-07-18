#' Fit Regression Model with One or More Predictors (Internal)
#'
#' Fits a regression model based on the selected approach. Can handle a single exposure or a vector of exposures.
#'
#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposures A string or character vector of predictor(s).
#' @param approach A string specifying the regression approach. One of
#' `"logit"`, `"log-binomial"`, `"poisson"`, `"linear"`, `"robpoisson"`, or `"negbin"`.
#'
#' @return A fitted model object (`glm`, `lm`, `riskratio`, or `negbin`) or `NULL` if fitting fails.
#' @keywords internal
.fit_uni_model <- function(data, outcome, exposures, approach) {
  formula <- as.formula(paste(outcome, "~", exposures))

  tryCatch({
    # Fit model by approach
    fit <- switch(approach,
                  "logit" = glm(formula,
                                data = data,
                                family = binomial("logit")),
                  "log-binomial" = glm(formula,
                                       data = data,
                                       family = binomial("log")),
                  "poisson" = glm(formula,
                                  data = data,
                                  family = poisson("log")),
                  "linear" = lm(formula,
                                data = data),
                  "robpoisson" = risks::riskratio(formula,
                                                  data = data,
                                                  approach = "robpoisson"),
                  "negbin" = MASS::glm.nb(formula,
                                          data = data,
                                          control = glm.control(maxit = 200)),
                  stop(
                    paste0("Invalid approach: '", approach,
                           "'. Choose from: logit, log-binomial, poisson, ",
                           "robpoisson, linear, negbin.")
                  )
    )
    return(fit)
  }, error = function(e) {
    warning("Model failed for exposure(s) '", paste(exposures, collapse = ", "),
            "' using '", approach, "': ", e$message, call. = FALSE)
    return(NULL)
  })
}
