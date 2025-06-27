#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposures A character vector of predictor (exposure) variable names.
#' @return A fitted model object (e.g., `glm`, `lm`, or `glm.nb`) or `NULL` if model fitting fails.

.fit_multi_model_nbin <- function(data, outcome, exposures) {
  formula_str <- paste(outcome, "~", paste(exposures, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  tryCatch({
    MASS::glm.nb(model_formula, data = data, control = glm.control(maxit = 200))
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    NULL
  })
}
