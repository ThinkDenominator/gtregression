#' Fit Univariate Model (Internal)
#'
#' Fits a univariate regression model based on the selected approach.
#'
#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposure A string. Name of a single predictor (exposure) variable.
#' @param approach A string specifying the regression approach. One of `"logit"`, `"log-binomial"`, `"poisson"`, `"linear"`, or `"robpoisson"`.
#'
#' @return A fitted model object (`glm`, `lm`, or `riskratio`) or `NULL` if fitting fails.
#' @keywords internal


.fit_uni_model <- function(data, outcome, exposure, approach) {
  formula <- as.formula(paste(outcome, "~", exposure))
  tryCatch(
    {
      if (approach == "logit") {
        glm(formula, data = data, family = binomial("logit"))
      } else if (approach == "log-binomial") {
        glm(formula, data = data, family = binomial("log"))
      } else if (approach == "poisson") {
        glm(formula, data = data, family = poisson("log"))
      } else if (approach == "linear") {
        lm(formula, data = data)
      } else {
        risks::riskratio(formula = formula,
                         data = data,
                         approach = "robpoisson")
      }
    },
    error = function(e) {
      warning("Model failed for '", exposure, "': ", e$message)
      NULL
    }
  )
}
