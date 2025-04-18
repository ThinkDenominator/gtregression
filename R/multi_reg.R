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
#'   `"robpoisson"` (RR), `"margstd_boot"` (RR), `"margstd_delta"` (RR), `"linear"` (Beta coefficients)
#' @param summary Logical; if `TRUE`, prints model summary after fitting. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_regression` object with adjusted effect estimates.
#' @export

multi_reg <- function(data, outcome, exposures, approach = "logit", summary = FALSE) {
  requireNamespace("risks", quietly = TRUE)
  requireNamespace("gtsummary", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)

  valid_approaches <- c("logit", "log-binomial", "poisson",
                        "robpoisson", "margstd_boot", "margstd_delta", "linear")

  if (!(approach %in% valid_approaches)) {
    stop("Invalid approach: ", approach,
         "\nValid options: ", paste(valid_approaches, collapse = ", "))
  }

  message("Running multi_reg using approach: ", approach)

  effect_label <- dplyr::case_when(
    approach == "logit" ~ "**Adjusted OR**",
    approach == "poisson" ~ "**Adjusted IRR**",
    approach == "linear" ~ "**Adjusted Beta**",
    TRUE ~ "**Adjusted RR**"
  )

  remove_abbreviation <- dplyr::case_when(
    approach == "log-binomial" ~ "RR = Relative Risk",
    approach %in% c("logit", "margstd_boot", "margstd_delta") ~ "OR = Odds Ratio",
    approach %in% c("poisson", "robpoisson") ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "CI = Confidence Interval"
  )

  abbreviation <- dplyr::case_when(
    approach == "logit" ~ "OR = Odds Ratio",
    approach %in% c("log-binomial", "margstd_boot", "margstd_delta", "robpoisson") ~ "RR = Relative Risk",
    approach == "poisson" ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "Beta = Linear Regression Coefficient, CI = Confidence Interval"
  )

  # Outcome validation
  outcome_vec <- data[[outcome]]

  is_binary <- function(x) {
    if (is.factor(x)) {
      return(length(levels(x)) == 2)
    } else if (is.numeric(x)) {
      return(all(x %in% c(0, 1), na.rm = TRUE))
    } else if (is.logical(x)) {
      return(TRUE)
    }
    return(FALSE)
  }

  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  }

  is_continuous <- function(x) {
    is.numeric(x) && length(unique(x)) > 10 && !is_count(x)
  }

  if (approach %in% c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")) {
    if (!is_binary(outcome_vec)) {
      stop("The outcome must be binary for the selected approach: ", approach)
    }
  }

  if (approach == "poisson") {
    if (!is_count(outcome_vec)) {
      stop("Poisson regression requires a count outcome (non-negative integers).")
    }
  }

  if (approach == "linear") {
    if (!is_continuous(outcome_vec)) {
      stop("Linear regression requires a continuous numeric outcome.")
    }
  }

  # Build model formula
  formula_str <- paste(outcome, "~", paste(exposures, collapse = " + "))
  model_formula <- as.formula(formula_str)

  # Fit model
  fit_model <- tryCatch({
    if (approach == "logit") {
      stats::glm(model_formula, data = data, family = binomial(link = "logit"))
    } else if (approach == "log-binomial") {
      stats::glm(model_formula, data = data, family = binomial(link = "log"))
    } else if (approach == "poisson") {
      stats::glm(model_formula, data = data, family = poisson(link = "log"))
    } else if (approach == "linear") {
      stats::lm(model_formula, data = data)
    } else {
      risks::riskratio(formula = model_formula, data = data, approach = approach)
    }
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })

  if (is.null(fit_model)) {
    stop("Could not fit the model.")
  }

  if (summary) {
    cat("\n Model Summary:\n")
    print(summary(fit_model))
  }

  final_tbl <- gtsummary::tbl_regression(fit_model, exponentiate = !(approach == "linear")) %>%
    gtsummary::modify_header(estimate = effect_label) %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label))) %>%
    gtsummary::remove_abbreviation(remove_abbreviation) %>%
    gtsummary::modify_abbreviation(abbreviation)

  print(final_tbl)
  return(final_tbl)
}
