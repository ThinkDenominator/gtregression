#' Compare models with and without interaction term
#'
#' This function compares two models — one with and one without an interaction term —
#' to evaluate whether the interaction between exposure and a potential effect modifier
#' significantly improves model fit.
#'
#' @param data A data frame.
#' @param outcome Outcome variable (binary, count, or continuous).
#' @param exposure Main exposure variable (must be length 1).
#' @param covariates Optional vector of adjustment variables (default = NULL).
#' @param effect_modifier Variable to test for interaction with the exposure.
#' @param approach Modeling approach: "logit", "log-binomial", "poisson", "robpoisson", "negbin", "linear".
#' @param test Test for model comparison: "LRT" (default) or "Wald".
#' @param verbose Logical, whether to print interpretation.
#'
#' @return A list of model objects, p-value, and interpretation.
#' @export
interaction_models <- function(data, outcome, exposure, covariates = NULL,
                               effect_modifier, approach = "logit", test = c("LRT", "Wald"),
                               verbose = TRUE) {
  if (!approach %in% c("logit", "log-binomial", "poisson", "robpoisson", "negbin", "linear")) {
    stop("Invalid approach")
  }

  test <- match.arg(test)
  # Outcome validation
  outcome_vec <- data[[outcome]]
  is_binary <- function(x) is.factor(x) && length(levels(x)) == 2 || is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)
  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) && length(unique(x[!is.na(x)])) > 2
  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")) {
    if (!is_binary(outcome_vec)) stop("This approach requires a binary outcome.")
  }
  if (approach == "poisson") {
    if (is_binary(outcome_vec)) stop("Poisson regression is not appropriate for binary outcomes.")
    if (!is_count(outcome_vec)) stop("Poisson requires a count outcome.")
  }
  if (approach == "negbin") {
    if (!is_count(outcome_vec)) stop("Negative binomial requires a count outcome.")
  }
  if (approach == "linear") {
    if (!is_continuous(outcome_vec)) stop("Linear regression requires a continuous outcome.")
  }


  if (length(exposure) != 1) stop("Please provide exactly one exposure variable.")
  if (approach %in% c("margstd_delta", "margstd_boot")) stop("Marginal standardization methods are not supported for interaction tests.")

  # Load required packages
  if (test == "Wald") requireNamespace("lmtest")

  # Build formulas
  rhs <- c(exposure, effect_modifier, covariates)
  base_formula <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  int_formula <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + "), "+", paste0(exposure, ":", effect_modifier)))

  # Fit models
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

  # Perform test
  if (test == "LRT") {
    if (approach == "negbin") {
      comp <- anova(model1, model2)

      # Find the p-value column dynamically and robustly
      p_val_col <- grep("P\\(>Chi\\)", names(comp), value = TRUE)

      if (length(p_val_col) > 0 && !is.null(comp[[p_val_col]][2])) {
        p_value <- comp[[p_val_col]][2]
      } else {
        warning("Unable to extract p-value for negbin model. Returning NA.")
        p_value <- NA_real_
      }

    } else {
      comp <- anova(model1, model2, test = "LRT")
      p_value <- comp$"Pr(>Chi)"[2]
    }
    test_label <- "Likelihood Ratio Test"
  }

  else {
    comp <- lmtest::waldtest(model1, model2)
    p_value <- comp[2, "Pr(>Chisq)"]
    test_label <- "Wald Test"
  }

  # Robust SEs if robpoisson
  if (approach == "robpoisson") {
    model1 <- lmtest::coeftest(model1, vcov. = sandwich::vcovHC)
    model2 <- lmtest::coeftest(model2, vcov. = sandwich::vcovHC)

  }

  # Interpretation
  if (verbose) {
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("Interaction Term Assessment using", test_label, "\n")
    cat("Model without interaction:\n   ", format(base_formula), "\n")
    cat("Model with interaction:\n   ", format(int_formula), "\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("P-value:", format(round(p_value, 4), nsmall = 4), "\n")
    cat(ifelse(p_value < 0.05,
               "Interaction is statistically significant. Consider including it.\n",
               "Interaction is not statistically significant. Simpler model may be preferred.\n"))
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  }

  invisible(list(
    model_no_interaction = model1,
    model_with_interaction = model2,
    p_value = p_value,
    test = test_label
  ))
}
