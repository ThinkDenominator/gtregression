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
#'   `"robpoisson"` (RR), `"margstd_boot"` (RR), `"margstd_delta"` (RR), `"linear"` (Beta coefficients)
#' @param summary Logical; if `TRUE`, prints model summaries for each univariate model. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_stack` object of exponentiated unadjusted estimates.
#' @export

uni_reg <- function(data, outcome, exposures, approach = "logit", summary= FALSE) {
  # Dependencies (attach only what's needed)
  requireNamespace("risks", quietly = TRUE)
  requireNamespace("gtsummary", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)

  # Define valid approaches
  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson",
                        "margstd_boot", "margstd_delta", "linear")

  # Validate
  if (!(approach %in% valid_approaches)) {
    stop(paste("Invalid approach:", approach,
               "\nValid options:", paste(valid_approaches, collapse = ", ")))
  }

  message("Running uni_reg using approach: ", approach)

  # Label estimate type
  effect_label <- dplyr::case_when(
    approach == "logit" ~ "**OR**",
    approach == "poisson" ~ "**IRR**",
    approach == "linear" ~ "**Beta**",
    TRUE ~ "**RR**"
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
    approach %in% c("poisson") ~ "IRR = Incidence Rate Ratio",
    approach == "linear" ~ "Beta = Linear Regression Coefficient, CI = Confidence Interval"
  )

  # Validate outcome variable
  outcome_vec <- data[[outcome]]

  # Binary check
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

  # Count check
  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  }

  # Continuous check
  is_continuous <- function(x) {
    is.numeric(x) && length(unique(x)) > 10 && !is_count(x)
  }

  # Outcome checks based on approach
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

  # Fit model function
  fit_model <- function(exposure) {
    formula <- as.formula(paste(outcome, "~", exposure))

    if (approach == "logit") {
      stats::glm(formula, data = data, family = binomial(link = "logit"))
    } else if (approach == "log-binomial") {
      stats::glm(formula, data = data, family = binomial(link = "log"))
    } else if (approach == "poisson") {
      stats::glm(formula, data = data, family = poisson(link = "log"))
    } else if (approach == "linear") {
      stats::lm(formula, data = data)
    } else {
      risks::riskratio(formula = formula, data = data, approach = approach)
    }
  }

  # Fit models
  model_list <- purrr::map(exposures, function(exposure) {
    tryCatch(
      fit_model(exposure),
      error = function(e) {
        warning("Model failed for ", exposure, " using approach: ", approach, "\n", e$message)
        NULL
      }
    )
  })

  # Optionally print model summaries
  if (summary) {
    message("🧾 Printing model summaries:")
    purrr::walk2(model_list, exposures, function(model, var) {
      if (!is.null(model)) {
        cat("\n Summary for exposure:", var, "\n")
        print(summary(model))
      }
    })
  }

  # Convert to gtsummary tables
  tbl_list <- purrr::map(model_list, function(fit) {
    if (!is.null(fit)) {
      gtsummary::tbl_regression(fit, exponentiate = !(approach == "linear"))
    } else {
      NULL
    }
  })

  tbl_list <- tbl_list[!sapply(tbl_list, is.null)]

  if (length(tbl_list) == 0) {
    stop("All models failed to converge.")
  }

  # Combine and label
  stacked_tbl <- purrr::map(tbl_list, function(tbl) {
    tbl %>%
      gtsummary::modify_header(estimate = effect_label)
  }) %>%
    gtsummary::tbl_stack()

  final_tbl <- stacked_tbl %>%
    gtsummary::remove_abbreviation(remove_abbreviation) %>%
    gtsummary::modify_abbreviation(abbreviation)

  print(final_tbl)
  return(final_tbl)
}
