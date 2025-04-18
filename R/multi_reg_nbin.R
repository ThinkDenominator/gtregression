#' Multivariable Negative Binomial Regression (Adjusted Risk Ratios)
#'
#' This function calculates adjusted risk ratios for multiple exposures using **Negative Binomial Regression**.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable (count).
#' @param exposures A vector of predictor variables.
#' @param summary Logical; if `TRUE`, prints model summary. Default is `FALSE`.
#'
#' @return A publication-ready `gtsummary` table with "Adjusted IRR".
#' @export

multi_reg_nbin <- function(data, outcome, exposures, summary = FALSE) {
  # Load required packages
  requireNamespace("MASS", quietly = TRUE)
  requireNamespace("gtsummary", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("broom", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("stats", quietly = TRUE)

  message("Running multi_reg_nbin for Negative Binomial Regression")

  # Validate outcome
  outcome_vec <- data[[outcome]]
  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  }
  if (!is_count(outcome_vec)) {
    stop("Negative binomial regression requires a count outcome (non-negative integers).")
  }

  # Create model formula
  formula_str <- paste(outcome, "~", paste(exposures, collapse = " + "))
  model_formula <- as.formula(formula_str)

  # Handle missing values
  data_clean <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(outcome))) %>%
    tidyr::drop_na(dplyr::any_of(exposures))

  if (nrow(data_clean) == 0) {
    warning("No valid data available after handling missing values.")
    return(NULL)
  }

  # Fit model
  fit_model <- tryCatch({
    MASS::glm.nb(model_formula, data = data_clean, control = stats::glm.control(maxit = 200))
  }, warning = function(w) {
    warning(paste("Negative binomial model warning:", w$message))
    return(NULL)
  }, error = function(e) {
    warning(paste("Negative binomial model failed:", e$message))
    return(NULL)
  })

  # Stop if failed
  if (is.null(fit_model)) {
    warning("Skipping output due to model fitting failure.")
    return(NULL)
  }

  # Print summary if requested
  if (summary) {
    cat("\n Model Summary:\n")
    print(summary(fit_model))
  }

  # Format output
  final_tbl <- gtsummary::tbl_regression(fit_model, exponentiate = TRUE) %>%
    gtsummary::modify_header(estimate = "**Adjusted IRR**") %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label)))

  print(final_tbl)
  return(final_tbl)
}
