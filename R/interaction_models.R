#' Compare Models With and Without Interaction Term
#'
#' This function fits two models—one with and one without an interaction term between an exposure and a potential effect modifier—
#' and compares them using either a likelihood ratio test (LRT) or Wald test.
#' It is useful for assessing whether there is statistical evidence of interaction (effect modification).
#'
#' @param data A data frame containing all required variables.
#' @param outcome The name of the outcome variable (binary, count, or continuous depending on the approach).
#' @param exposure The name of the main exposure variable. Must be a single variable.
#' @param covariates Optional character vector of additional covariates to adjust for (default = \code{NULL}).
#' @param effect_modifier The name of the variable to test for interaction with the exposure.
#' @param approach The regression modeling approach to use. One of:
#'   \code{"logit"}, \code{"log-binomial"}, \code{"poisson"},
#'   \code{"robpoisson"}, \code{"negbin"}, or \code{"linear"}.
#' @param test Type of statistical test for model comparison. Either:
#'   \code{"LRT"} (likelihood ratio test, default) or \code{"Wald"} (test for interaction term coefficient).
#' @param verbose Logical; if \code{TRUE}, prints a basic interpretation of whether interaction is likely present (default = \code{FALSE}).
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{model_no_interaction}: The model without the interaction term.
#'   \item \code{model_with_interaction}: The model with the interaction term.
#'   \item \code{p_value}: The p-value for interaction (based on selected test).
#'   \item \code{interpretation}: A brief text interpretation if \code{verbose = TRUE}.
#' }
#'
#' @importFrom stats glm anova coef as.formula
#' @importFrom MASS glm.nb
#' @examples
#' data <- data_PimaIndiansDiabetes
#` interaction_models(
#`  data = data,
#`  outcome = "glucose",
#`  exposure = "insulin",
#`  effect_modifier = "age_cat",
#`  covariates = "pregnant",
#`  approach = "linear"
#`)
#'
#' @export

interaction_models <- function(data, outcome, exposure, covariates = NULL,
                               effect_modifier, approach = "logit", test = c("LRT", "Wald"),
                               verbose = TRUE) {
  .validate_approach(approach)
  .validate_outcome_by_approach(data[[outcome]], approach)

  test <- match.arg(test)
  rhs <- c(exposure, effect_modifier, covariates)
  base_formula <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  int_formula <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + "), "+", paste0(exposure, ":", effect_modifier)))

  fit_model <- function(formula) {
    switch(approach,
      "logit" = glm(formula, data = data, family = binomial("logit")),
      "log-binomial" = glm(formula, data = data, family = binomial("log")),
      "poisson" = glm(formula, data = data, family = poisson(link = "log")),
      "robpoisson" = glm(formula, data = data, family = poisson(link = "log")),
      "negbin" = MASS::glm.nb(formula, data = data),
      "linear" = lm(formula, data = data),
      stop("Unsupported regression approach.")
    )
  }

  model1 <- tryCatch(fit_model(base_formula), error = function(e) NULL)
  model2 <- tryCatch(fit_model(int_formula), error = function(e) NULL)

  if (is.null(model1) || is.null(model2)) stop("Model fitting failed for one or both models.")

  if (test == "LRT") {
    if (approach == "negbin") {
      comp <- anova(model1, model2)
      p_val_col <- grep("P\\(>Chi\\)", names(comp), value = TRUE)
      p_value <- if (length(p_val_col) > 0 && !is.null(comp[[p_val_col]][2])) comp[[p_val_col]][2] else NA_real_
    } else {
      comp <- anova(model1, model2, test = "LRT")
      p_value <- comp$"Pr(>Chi)"[2]
    }
    test_label <- "Likelihood Ratio Test"
  } else {
    comp <- lmtest::waldtest(model1, model2)
    p_value <- comp[2, "Pr(>Chisq)"]
    test_label <- "Wald Test"
  }

  if (approach == "robpoisson") {
    model1 <- lmtest::coeftest(model1, vcov. = sandwich::vcovHC)
    model2 <- lmtest::coeftest(model2, vcov. = sandwich::vcovHC)
  }

  if (verbose) {
    cat("---------------------------------------------------\n")
    cat("Interaction Term Assessment using", test_label, "\n")
    cat("Model without interaction:\n   ", format(base_formula), "\n")
    cat("Model with interaction:\n   ", format(int_formula), "\n")
    cat("-----------------------------------------------------\n")
    cat("P-value:", format(round(p_value, 4), nsmall = 4), "\n")
    cat(ifelse(p_value < 0.05,
      "Interaction is statistically significant. Consider including it.\n",
      "Interaction is not statistically significant. Simpler model may be preferred.\n"
    ))
    cat("----------------------------------------------------\n")
  }

  invisible(list(
    model_no_interaction = model1,
    model_with_interaction = model2,
    p_value = p_value,
    test = test_label
  ))
}
