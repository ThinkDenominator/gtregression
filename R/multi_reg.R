#' Multivariable regression (Adjusted Odds, Risk, or Rate Ratios)
#'
#' Performs multivariable regression with multiple exposures on a binary, count, or continuous outcome.
#' Depending on `approach`, returns either Adjusted Odds Ratios (OR), Risk Ratios (RR), Incidence Rate Ratios (IRR), or Beta coefficients.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable (binary, count, or continuous).
#' @param exposures A vector of predictor variables.
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (OR), `"log-binomial"` (RR), `"poisson"` (IRR),
#'   `"robpoisson"` (RR), `"linear"` (Beta coefficients)
#' @param summary Logical; if `TRUE`, prints model summary and diagnostics. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_regression` object with adjusted effect estimates.
#' @export
multi_reg <- function(data, outcome, exposures, approach = "logit", summary = FALSE) {
  `%>%` <- magrittr::`%>%`

  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "linear")
  if (!(approach %in% valid_approaches)) {
    stop("Invalid approach: ", approach,
         "\nValid options: ", paste(valid_approaches, collapse = ", "))
  }

  effect_label <- dplyr::case_when(
    approach == "logit" ~ "**Adjusted OR**",
    approach == "poisson" ~ "**Adjusted IRR**",
    approach == "linear" ~ "**Adjusted Beta**",
    TRUE ~ "**Adjusted RR**"
  )

  abbreviation <- dplyr::case_when(
    approach == "logit" ~ "OR = Odds Ratio",
    approach %in% c("log-binomial", "robpoisson") ~ "RR = Relative Risk",
    approach == "poisson" ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "Beta = Linear Regression Coefficient, CI = Confidence Interval"
  )

  remove_abbreviation <- dplyr::case_when(
    approach == "log-binomial" ~ "RR = Relative Risk",
    approach == "logit" ~ "OR = Odds Ratio",
    approach %in% c("poisson", "robpoisson") ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "CI = Confidence Interval"
  )

  # Outcome checks
  outcome_vec <- data[[outcome]]

  is_binary <- function(x) is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)) ||
    (is.factor(x) && length(levels(x)) == 2)

  is_count <- function(x) is.numeric(x) && all(x >= 0 & floor(x) == x, na.rm = TRUE)

  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("Binary outcome required for the selected approach: ", approach)
  }

  if (approach == "poisson" && !is_count(outcome_vec)) {
    stop("Poisson regression requires a count outcome (non-negative integers).")
  }

  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Continuous numeric outcome required for linear regression.")
  }

  # Formula
  formula_str <- paste(outcome, "~", paste(exposures, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  # Fit model
  fit_model <- tryCatch({
    if (approach == "logit") {
      stats::glm(model_formula, data = data, family = binomial("logit"))
    } else if (approach == "log-binomial") {
      stats::glm(model_formula, data = data, family = binomial("log"))
    } else if (approach == "poisson") {
      stats::glm(model_formula, data = data, family = poisson("log"))
    } else if (approach == "linear") {
      stats::lm(model_formula, data = data)
    } else {
      risks::riskratio(formula = model_formula, data = data, approach = "robpoisson")
    }
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })

  if (is.null(fit_model)) {
    stop("Could not fit the model. Please try different approach.")
  }

  # Optional: diagnostics for linear
  reg_check_linear <- function(model) {
    lmtest <- getNamespace("lmtest")
    car <- getNamespace("car")

    cat("\n📊 Diagnostics for multivariable linear regression:\n")

    # Breusch-Pagan
    bp <- lmtest::bptest(model)
    cat("• Breusch-Pagan test (Heteroskedasticity): p =", signif(bp$p.value, 4), "\n")
    if (bp$p.value < 0.05) {
      cat("  ↪ Heteroskedasticity detected. Residual variance may not be constant.\n")
    } else {
      cat("  ↪ No evidence of heteroskedasticity. Residuals appear homoscedastic.\n")
    }

    # Shapiro-Wilk
    sw <- shapiro.test(residuals(model))
    cat("• Shapiro-Wilk test (Normality of residuals): p =", signif(sw$p.value, 4), "\n")
    if (sw$p.value < 0.05) {
      cat("  ↪ Residuals may not be normally distributed. Caution with small samples.\n")
    } else {
      cat("  ↪ Residuals appear normally distributed.\n")
    }

    # Ramsey RESET
    reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
    cat("• Ramsey RESET test (Functional form): p =", signif(reset$p.value, 4), "\n")
    if (reset$p.value < 0.05) {
      cat("  ↪ Model may be mis-specified. Consider adding nonlinear terms or interactions.\n")
    } else {
      cat("  ↪ Functional form appears adequate.\n")
    }

    # Cook’s distance
    cooks <- cooks.distance(model)
    n <- nobs(model)
    high_infl <- sum(cooks > (4 / n), na.rm = TRUE)
    cat("• Cook’s Distance: ", high_infl, "observation(s) > 4/n (", round(4 / n, 4), ")\n")
    if (high_infl > 0) {
      cat("  ↪ There are influential points. Review for data quality or leverage.\n")
    } else {
      cat("  ↪ No strong influential observations.\n")
    }
  }

  # Summary output
  if (summary) {
    cat("\nSummary for multivariable model:\n")
    print(summary(fit_model))
    if (approach == "linear") reg_check_linear(fit_model)
  }

  final_tbl <- gtsummary::tbl_regression(fit_model, exponentiate = approach != "linear") %>%
    gtsummary::modify_header(estimate = effect_label) %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label))) %>%
    gtsummary::remove_abbreviation(remove_abbreviation) %>%
    gtsummary::modify_abbreviation(abbreviation)

  return(final_tbl)
}
