#' Univariate regression (Odds, Risk, or Rate Ratios)
#'
#' Performs univariate regression for each exposure on a binary, continuous, or count outcome.
#' Depending on `approach`, returns either Odds Ratios (OR), Risk Ratios (RR), or Incidence Rate Ratios (IRR).
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable (binary, continuous, or count).
#' @param exposures A vector of predictor variables.
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (OR), `"log-binomial"` (RR), `"poisson"` (IRR),
#'   `"robpoisson"` (RR), `"linear"` (Beta coefficients)
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#' @details This function requires the following packages: `dplyr`, `purrr`, `gtsummary`, `risks`.
#' @return A `gtsummary::tbl_stack` object of exponentiated unadjusted estimates- publication ready tables
#' @section Accessors:
#' \describe{
#'   \item{\code{$models}}{Extracts the underlying model objects.}
#'   \item{\code{$model_summaries}}{Extracts tidy summaries for each model.}
#' }
#' @export
uni_reg <- function(data,
                    outcome,
                    exposures,
                    approach = "logit") {
  `%>%` <- magrittr::`%>%`
  pkgs <- c("gtsummary", "purrr", "dplyr", "stats", "rlang")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
  }
  if (approach == "robpoisson" && !requireNamespace("risks", quietly = TRUE)) {
    stop("Package 'risks' is required for robust Poisson regression.")
  }
  if (approach == "linear") {
    if (!requireNamespace("car", quietly = TRUE) ||
        !requireNamespace("lmtest", quietly = TRUE)) {
      stop("Packages 'car' and 'lmtest' are required for linear regression diagnostics.")
    }
  }

  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "linear")
  if (!approach %in% valid_approaches) {
    stop("Invalid approach. Choose one of: ", paste(valid_approaches, collapse = ", "))
  }

  if (!outcome %in% names(data)) stop("Outcome variable not found in dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposure variables not found in dataset.")

  # outcome validation

  outcome_vec <- data[[outcome]]

  is_binary <- function(x) is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)) ||
    (is.factor(x) && length(levels(x)) == 2)

  is_count <- function(x) is.numeric(x) && all(x >= 0 & floor(x) == x, na.rm = TRUE)

  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("Binary outcome required for the selected approach: ", approach)
  }

  if (approach == "poisson"&& !is_count(outcome_vec)) {
    stop("Count outcome required for Poisson regression.")
  }

  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Continuous numeric outcome required for linear regression.")
  }

  label_est <- dplyr::case_when(
    approach == "logit" ~ "**OR**",
    approach == "poisson" ~ "**IRR**",
    approach == "linear" ~ "**Beta**",
    TRUE ~ "**RR**"
  )

  abbreviation <- dplyr::case_when(
    approach == "logit" ~ "OR = Odds Ratio",
    approach %in% c("log-binomial", "robpoisson") ~ "RR = Relative Risk",
    approach == "poisson" ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "Beta = Linear Regression Coefficient, CI = Confidence Interval"
  )


  remove_abbrev <- dplyr::case_when(
    approach == "log-binomial" ~ "RR = Relative Risk",
    approach == "logit" ~ "OR = Odds Ratio",
    approach %in% c("poisson", "robpoisson") ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "CI = Confidence Interval"
  )

  fit_model <- function(exposure) {
    fmla <- stats::as.formula(paste(outcome, "~", exposure))
    if (approach == "logit") {
      stats::glm(fmla, data = data, family = binomial("logit"))
    } else if (approach == "log-binomial") {
      stats::glm(fmla, data = data, family = binomial("log"))
    } else if (approach == "poisson") {
      stats::glm(fmla, data = data, family = poisson("log"))
    } else if (approach == "linear") {
      stats::lm(fmla, data = data)
    } else {
      risks::riskratio(formula = fmla, data = data, approach = "robpoisson")
    }
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


  model_list <- purrr::map(exposures, function(x) {
    tryCatch(fit_model(x), error = function(e) {
      warning("Model failed for '", x, "' using approach '", approach, "': ", e$message)
      NULL
    })
  })

  model_list <- model_list[!sapply(model_list, is.null)]
  names(model_list) <- exposures

  if (length(model_list) == 0) stop("All models failed. Please check your data or exposures.")

  tbl_list <- purrr::map(model_list,
                         ~gtsummary::tbl_regression(.x,
                                                    exponentiate = approach != "linear",
                                                    conf.method = "wald",
                                                    tidy_fun = broom::tidy))
  stacked <- gtsummary::tbl_stack(purrr::map(tbl_list, ~gtsummary::modify_header(.x, estimate = label_est)))

  result <- stacked %>%
    gtsummary::remove_abbreviation(remove_abbrev) %>%
    gtsummary::modify_abbreviation(abbreviation)
  model_summaries <- purrr::map(model_list, summary)
  names(model_summaries) <- exposures

  if (approach != "linear") {
    reg_diagnostics <- "Regression diagnostics only available for 'linear' models."
  } else {
    reg_diagnostics <- purrr::imap(model_list, function(m, x) reg_check_linear(m, x))
  }

  attr(result, "approach") <- approach
  attr(result, "source") <- "uni_reg"
  class(result) <- c("uni_reg", class(result))
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- model_summaries
  attr(result, "reg_diagnostics") <- reg_diagnostics


  result
}
#' @export
`$.uni_reg` <- function(x, name) {
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
