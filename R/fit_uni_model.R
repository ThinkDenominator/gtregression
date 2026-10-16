#' Fit Regression Model with One or More Predictors (Internal)
#'
#' Fits a regression model based on the selected approach. Can handle a single exposure or a vector of exposures.
#'
#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposures A string or character vector of predictor(s).
#' @param approach A string specifying the regression approach. One of
#' `"logit"`, `"firth"`, `"logbinomial"`, `"poisson"`, `"linear"`, `"robpoisson"`, or `"negbin"`.
#'
#' @return A fitted model object (`glm`, `logistf`, `lm`, `riskratio`, or `negbin`) or `NULL` if fitting fails.
#' @keywords internal
.fit_uni_model <- function(data, outcome, exposures, approach) {
  approach <- .normalize_approach(approach)
  data <- .prepare_regression_model_data(data, outcome, exposures, approach)
  formula <- as.formula(paste(
    .formula_name(outcome), "~", paste(.formula_name(exposures), collapse = " + ")
  ))

  tryCatch({
    # Fit model by approach (always with treatment contrasts: the tables show
    # level-versus-reference rows)
    fit <- .with_treatment_contrasts(switch(approach,
                  "logit" = glm(formula,
                                data = data,
                                family = binomial("logit")),
                  "firth" = logistf::logistf(formula = formula,
                                             data = data),
                  "logbinomial" = .fit_logbinomial(formula, data),
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
                           "'. Choose from: logit, firth, logbinomial, poisson, ",
                           "robpoisson, linear, negbin."),
                    call. = FALSE
                  )
    ))
    attr(fit, "gtregression_model_frame") <- .with_treatment_contrasts(
      stats::model.frame(formula, data = data)
    )
    return(fit)
  }, error = function(e) {
    warning("Model failed for exposure(s) '", paste(exposures, collapse = ", "),
            "' using '", approach, "': ", sub("\\.+$", "", e$message), ".",
            .fit_failure_hint(approach), call. = FALSE)
    return(NULL)
  })
}
