#' Univariate regression (Odds, Risk, or Rate Ratios)
#'
#' Performs univariate regression for each exposure on a binary or count outcome.
#' Depending on `approach`, returns either Odds Ratios (OR), Risk Ratios (RR), or Incidence Rate Ratios (IRR).
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable (binary or count).
#' @param exposures A vector of predictor variables.
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (OR), `"log-binomial"` (RR), `"poisson"` (IRR),
#'   `"robpoisson"` (RR), `"linear"` (Beta coefficients)
#' @param summary Logical; if `TRUE`, prints model summaries for each univariate model. Default is `FALSE`.
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#' @details This function requires the following packages: `dplyr`, `purrr`, `gtsummary`, `risks`.
#' @return A `gtsummary::tbl_stack` object of exponentiated unadjusted estimates.
#' @export
uni_reg <- function(data, outcome, exposures, approach = "logit", summary = FALSE) {
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

  if (approach == "poisson") {
    if (is_binary(outcome_vec)) stop("Poisson regression is not appropriate for binary outcomes.")
    if (!is_count(outcome_vec)) stop("Poisson requires a count outcome.")
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

  reg_check_linear <- function(model, exposure) {
    cat("\n*** Diagnostics for multivariable linear regression: ***\n")
    lmtest <- getNamespace("lmtest")
    car <- getNamespace("car")

    # Breusch-Pagan
    bp <- lmtest::bptest(model)
    cat("* Breusch-Pagan test (Heteroskedasticity): p =", signif(bp$p.value, 4), "\n")
    if (bp$p.value < 0.05) {
      cat("  -> Heteroskedasticity detected. Residual variance may not be constant.\n")
    } else {
      cat("  -> No evidence of heteroskedasticity. Residuals appear homoscedastic.\n")
    }

    # Shapiro-Wilk
    sw <- shapiro.test(residuals(model))
    cat("* Shapiro-Wilk test (Normality of residuals): p =", signif(sw$p.value, 4), "\n")
    if (sw$p.value < 0.05) {
      cat("  -> Residuals may not be normally distributed. Caution with small samples.\n")
    } else {
      cat("  -> Residuals appear normally distributed.\n")
    }

    # Ramsey RESET
    reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
    cat("* Ramsey RESET test (Functional form): p =", signif(reset$p.value, 4), "\n")
    if (reset$p.value < 0.05) {
      cat("  -> Model may be mis-specified. Consider adding nonlinear terms or interactions.\n")
    } else {
      cat("  -> Functional form appears adequate.\n")
    }

    # Cook's distance
    cooks <- cooks.distance(model)
    n <- nobs(model)
    high_infl <- sum(cooks > (4 / n), na.rm = TRUE)
    cat("* Cook's Distance: ", high_infl, " observation(s) > 4/n (", round(4 / n, 4), ")\n")
    if (high_infl > 0) {
      cat("  -> There are influential points. Review for data quality or leverage.\n")
    } else {
      cat("  -> No strong influential observations.\n")
    }

  }


  model_list <- purrr::map(exposures, function(x) {
    tryCatch(fit_model(x), error = function(e) {
      warning("Model failed for '", x, "' using approach '", approach, "': ", e$message)
      NULL
    })
  })

  model_list <- model_list[!sapply(model_list, is.null)]

  if (length(model_list) == 0) stop("All models failed. Please check your data or exposures.")

  if (isTRUE(summary)) {
    purrr::walk2(model_list, exposures, function(m, x) {
      if (!is.null(m)) {
        cat("\nSummary for", x, ":\n")
        print(summary(m))
        if (approach == "linear") reg_check_linear(m, x)
      }
    })
  }

  tbl_list <- purrr::map(model_list,
                         ~gtsummary::tbl_regression(.x,
                                                    exponentiate = approach != "linear",
                                                    conf.method = "wald",
                                                    tidy_fun = broom::tidy))
  stacked <- gtsummary::tbl_stack(purrr::map(tbl_list, ~gtsummary::modify_header(.x, estimate = label_est)))

  result <- stacked %>%
    gtsummary::remove_abbreviation(remove_abbrev) %>%
    gtsummary::modify_abbreviation(abbreviation)

  attr(result, "approach") <- approach
  attr(result, "source") <- "uni_reg"

  return(result)
}
