#' Fit Multivariable Regression Model (Internal)
#'
#' Fits a regression model based on the selected approach and multiple exposures.
#'
#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposures A character vector of predictor (exposure) variable names.
#' @param approach A string specifying the regression approach. One of `"logit"`, `"log-binomial"`, `"poisson"`, `"linear"`, or `"robpoisson"`.
#'
#' @return A fitted model object (`glm`, `lm`, or `riskratio`) or `NULL` if fitting fails.
#' @keywords internal

.fit_multi_model <- function(data, outcome, exposures, approach) {
  formula_str <- paste(outcome, "~", paste(exposures, collapse = " + "))
  model_formula <- as.formula(formula_str)

  tryCatch(
    {
      if (approach == "logit") {
        glm(model_formula, data = data, family = binomial("logit"))
      } else if (approach == "log-binomial") {
        glm(model_formula, data = data, family = binomial("log"))
      } else if (approach == "poisson") {
        glm(model_formula, data = data, family = poisson("log"))
      } else if (approach == "linear") {
        lm(model_formula, data = data)
      } else {
        risks::riskratio(formula = model_formula, data = data,
                         approach = "robpoisson")
      }
    },
    error = function(e) {
      warning("Model fitting failed: ", e$message)
      NULL
    }
  )
}
