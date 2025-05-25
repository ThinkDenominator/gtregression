#' Multivariable Negative Binomial Regression (Adjusted Risk Ratios)
#'
#' Calculates adjusted incidence rate ratios (IRRs) using Negative Binomial Regression.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A vector of predictor variables.
#' @param summary Logical; if `TRUE`, prints model summary. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_regression` object with exponentiated estimates labeled as Adjusted IRRs.
#' @export
multi_reg_nbin <- function(data, outcome, exposures, summary = FALSE) {
  `%>%` <- magrittr::`%>%`

  message("Running multi_reg_nbin for Negative Binomial Regression")

  # Validate outcome
  is_count <- function(x) is.numeric(x) && all(x >= 0 & floor(x) == x, na.rm = TRUE)

  if (!outcome %in% names(data)) stop("Outcome variable not found.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in dataset.")
  if (!is_count(data[[outcome]])) stop("Negative binomial regression requires a count outcome (non-negative integers).")

  # Build formula
  model_formula <- stats::as.formula(paste(outcome, "~", paste(exposures, collapse = " + ")))

  # Remove missing values
  data_clean <- data %>%
    dplyr::filter(!is.na(.data[[outcome]])) %>%
    tidyr::drop_na(dplyr::any_of(exposures))

  if (nrow(data_clean) == 0) {
    warning("No valid data available after removing missing values.")
    return(NULL)
  }

  # Fit model
  fit_model <- tryCatch({
    MASS::glm.nb(model_formula, data = data_clean, control = glm.control(maxit = 200))
  }, warning = function(w) {
    warning("Negative binomial model warning: ", w$message)
    return(NULL)
  }, error = function(e) {
    warning("Negative binomial model failed: ", e$message)
    return(NULL)
  })

  if (is.null(fit_model)) {
    warning("Skipping output due to model fitting failure.")
    return(NULL)
  }

  if (summary) {
    cat("\nSummary for multivariable negative binomial model:\n")
    print(summary(fit_model))
  }

  # Return gtsummary table
  result <- gtsummary::tbl_regression(fit_model, exponentiate = TRUE) %>%
    gtsummary::modify_header(estimate = "**Adjusted IRR**") %>%
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio") %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label)))

  return(result)
}
