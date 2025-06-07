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
#' @return A `gtsummary::tbl_regression` object with adjusted effect estimates.
#' @importFrom broom tidy
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

  # Outcome validation
  outcome_vec <- data[[outcome]]

  is_binary <- function(x) is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)) ||
    (is.factor(x) && length(levels(x)) == 2)

  is_count <- function(x) is.numeric(x) && all(x >= 0 & floor(x) == x, na.rm = TRUE)

  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("Binary outcome required for the selected approach: ", approach)
  }

  if (approach == "poisson") {
    if (is_binary(outcome_vec)) stop("Poisson regression is not appropriate for binary outcomes.")
    if (!is_count(outcome_vec)) stop("Poisson requires a count outcome.")
  }

  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Continuous numeric outcome required for linear regression.")
  }

  # Clean missing data
  data_clean <- data %>%
    dplyr::filter(!is.na(.data[[outcome]])) %>%
    tidyr::drop_na(dplyr::any_of(exposures))

  if (nrow(data_clean) == 0) {
    stop("No complete cases remaining after removing missing values.")
  }

  # Formula
  formula_str <- paste(outcome, "~", paste(exposures, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  # Fit model
  fit_model <- tryCatch({
    if (approach == "logit") {
      stats::glm(model_formula, data = data_clean, family = binomial("logit"))
    } else if (approach == "log-binomial") {
      stats::glm(model_formula, data = data_clean, family = binomial("log"))
    } else if (approach == "poisson") {
      stats::glm(model_formula, data = data_clean, family = poisson("log"))
    } else if (approach == "linear") {
      stats::lm(model_formula, data = data_clean)
    } else {
      risks::riskratio(formula = model_formula, data = data_clean, approach = "robpoisson")
    }
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })

  if (is.null(fit_model)) {
    stop("Could not fit the model. Please try a different approach or check your data.")
  }

  # Optional: diagnostics for linear
  reg_check_linear <- function(model) {
    lmtest <- getNamespace("lmtest")
    car <- getNamespace("car")

    cat("\n*** Diagnostics for multivariable linear regression: ***\n")

    # Breusch-Pagan
    bp <- lmtest::bptest(model)
    cat("* Breusch-Pagan test (Heteroskedasticity): p =", signif(bp$p.value, 4), "\n")
    cat(ifelse(bp$p.value < 0.05,
               "  -> Heteroskedasticity detected. Residual variance may not be constant.\n",
               "  -> No evidence of heteroskedasticity.\n"))

    # Shapiro-Wilk
    sw <- shapiro.test(residuals(model))
    cat("* Shapiro-Wilk test (Normality): p =", signif(sw$p.value, 4), "\n")
    cat(ifelse(sw$p.value < 0.05,
               "  -> Residuals may not be normally distributed.\n",
               "  -> Residuals appear normally distributed.\n"))

    # Ramsey RESET
    reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
    cat("* Ramsey RESET test (Functional form): p =", signif(reset$p.value, 4), "\n")
    cat(ifelse(reset$p.value < 0.05,
               "  -> Model may be mis-specified.\n",
               "  -> Functional form appears adequate.\n"))

    # Cook's Distance
    cooks <- cooks.distance(model)
    high_infl <- sum(cooks > 4 / nobs(model), na.rm = TRUE)
    cat("* Cook's Distance: ", high_infl, " influential point(s)\n")
  }

  if (summary) {
    cat("\n Model Summary:\n")
    print(summary(fit_model))
    if (approach == "linear") reg_check_linear(fit_model)
  }

  final_tbl <- gtsummary::tbl_regression(
    fit_model,
    exponentiate = approach != "linear",
    conf.level = 0.95,
    conf.method = "wald",
    tidy_fun = broom::tidy
  ) %>%
    gtsummary::modify_header(estimate = effect_label) %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label))) %>%
    gtsummary::remove_abbreviation(remove_abbreviation) %>%
    gtsummary::modify_abbreviation(abbreviation)

  attr(final_tbl, "approach") <- approach
  attr(final_tbl, "source") <- "multi_reg"

  return(final_tbl)
}
