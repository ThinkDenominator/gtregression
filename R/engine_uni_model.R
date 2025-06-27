#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposures A character vector of predictor (exposure) variable names.
#' @return A fitted model object (e.g., `glm`, `lm`, or `glm.nb`) or `NULL` if model fitting fails.


.fit_uni_model <- function(data, outcome, exposure, approach) {
  formula <- as.formula(paste(outcome, "~", exposure))
  tryCatch({
    if (approach == "logit") {
      glm(formula, data = data, family = binomial("logit"))
    } else if (approach == "log-binomial") {
      glm(formula, data = data, family = binomial("log"))
    } else if (approach == "poisson") {
      glm(formula, data = data, family = poisson("log"))
    } else if (approach == "linear") {
      lm(formula, data = data)
    } else {
      risks::riskratio(formula = formula, data = data, approach = "robpoisson")
    }
  }, error = function(e) {
    warning("Model failed for '", exposure, "': ", e$message)
    NULL
  })
}
