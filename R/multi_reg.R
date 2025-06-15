#' Multivariable Regression (Adjusted Odds, Risk, or Rate Ratios)
#'
#' Fits multivariable regression models for binary, count, or continuous outcomes and returns a publication-ready summary table using `gtsummary`.
#' Depending on the specified `approach`, the function estimates adjusted Odds Ratios (OR), Risk Ratios (RR), Incidence Rate Ratios (IRR), or Beta coefficients.
#'
#' @param data A data frame containing the analysis variables.
#' @param outcome The name of the outcome variable (binary, count, or continuous). Must be a character string.
#' @param exposures A character vector of predictor variables to be included in the model.
#' @param approach Modeling approach to use. One of:
#'   - `"logit"` for logistic regression (OR),
#'   - `"log-binomial"` for log-binomial regression (RR),
#'   - `"poisson"` for Poisson regression (IRR),
#'   - `"robpoisson"` for robust Poisson regression (RR),
#'   - `"linear"` for linear regression (Beta coefficients).
#'
#' @return An object of class `multi_reg`, which includes:
#' - A `gtsummary::tbl_regression` object with adjusted effect estimates,
#' - A list of fitted model(s) accessible via `$models`,
#' - Tidy model summaries accessible via `$model_summaries`.
#'
#' @section Accessors:
#' \describe{
#'   \item{\code{$models}}{List of fitted model objects.}
#'   \item{\code{$model_summaries}}{A tibble of tidy regression summaries for each model.}
#' }
#'
#' @seealso [uni_reg()], [plot_reg()], [plot_reg_combine()]
#'
#' @examples
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   data(PimaIndiansDiabetes2, package = "mlbench")
#'   pima <- dplyr::mutate(PimaIndiansDiabetes2, diabetes = diabetes == "pos")
#'   model <- multi_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "mass"),
#'     approach = "logit"
#'   )
#'   print(model)
#' }
#'
#' @importFrom broom tidy
#' @export

multi_reg <- function(data,
                      outcome,
                      exposures,
                      approach = "logit") {
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

  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("Binary outcome required for the selected approach: ", approach)
  }

  if (approach == "poisson"&& !is_count(outcome_vec)) {
    stop("Count outcome required for Poisson regression.")
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

  # Reg check function for linear regression just like in STATA

  reg_check_linear <- function(model, exposure) {
    lmtest <- getNamespace("lmtest")

    # Tests
    bp <- lmtest::bptest(model)
    sw <- shapiro.test(residuals(model))
    reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
    cooks <- cooks.distance(model)
    n <- nobs(model)
    high_infl <- sum(cooks > (4 / n), na.rm = TRUE)

    tibble::tibble(
      Exposure = exposure,
      Test = c("Breusch-Pagan", "Shapiro-Wilk", "RESET", "Cook's Distance"),
      Statistic = c(
        paste0("p = ", signif(bp$p.value, 4)),
        paste0("p = ", signif(sw$p.value, 4)),
        paste0("p = ", signif(reset$p.value, 4)),
        paste0(high_infl, " obs > 4/n (", round(4 / n, 4), ")")
      ),
      Interpretation = c(
        if (bp$p.value < 0.05) "Heteroskedasticity detected: residual variance may not be constant." else "No evidence of heteroskedasticity.",
        if (sw$p.value < 0.05) "Residuals may not be normally distributed. Use caution with small samples." else "Residuals appear normally distributed.",
        if (reset$p.value < 0.05) "Model may be mis-specified. Consider adding non-linear terms or interactions." else "Functional form appears adequate.",
        if (high_infl > 0) "Influential points detected. Review for outliers or high-leverage observations." else "No strong influential observations detected."
      )
    )
  }
  model_list <- list(fit_model)
  names(model_list) <- "multivariable_model"

  model_summaries <- list(summary(fit_model))
  names(model_summaries) <- "multivariable_model"



  result <- gtsummary::tbl_regression(
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

  if (approach != "linear") {
    reg_diagnostics <- "Regression diagnostics only available for 'linear' models."
  } else {
    reg_diagnostics <- list(multivariable_model = reg_check_linear(fit_model, "multivariable_model"))
  }

  attr(result, "approach") <- approach
  attr(result, "source") <- "multi_reg"
  class(result) <- c("multi_reg", class(result))
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- model_summaries
  attr(result, "reg_diagnostics") <- reg_diagnostics


  result
}
#' @export
`$.multi_reg` <- function(x, name) {
  if (name == "models") {
    model_list <- attr(x, "models")
    lapply(model_list, print)
    return(invisible(model_list))
  }
  if (name == "model_summaries") {
    return(attr(x, "model_summaries"))
  }
  if (name == "reg_check"){
    return(attr(x, "reg_diagnostics"))
  }
  if (name == "table") {
    return(x)
  }
  # Fall back to default behavior for other gtsummary fields
  NextMethod("$")
}
