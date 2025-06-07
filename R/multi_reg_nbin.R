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

  # Input validation

  if (!outcome %in% names(data)) stop("Outcome variable not found.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in dataset.")


  # Outcome validation
  outcome_vec <- data[[outcome]]

  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  is_binary <- function(x) {
    is.atomic(x) && is.numeric(x) &&
      all(!is.na(x)) &&
      all(x %in% c(0, 1))
  }
  if (is_binary(outcome_vec)) stop("Negative binomial regression is not appropriate for binary outcomes.")
  if (!is_count(data[[outcome]])) stop("Outcome must be a non-negative count variable.")

  message("Running multi_reg_nbin for Negative Binomial Regression")

  # Remove missing values
  data_clean <- data %>%
    dplyr::filter(!is.na(.data[[outcome]])) %>%
    tidyr::drop_na(dplyr::any_of(exposures))

  if (nrow(data_clean) == 0) {
    warning("No valid observations remaining after removing missing values.")
    return(NULL)
  }

  # Check exposure variation
  insufficient_vars <- exposures[sapply(data_clean[exposures], function(x) length(unique(x)) < 2)]
  if (length(insufficient_vars) > 0) {
    warning("Skipping model: exposure(s) with insufficient variation: ", paste(insufficient_vars, collapse = ", "))
    return(NULL)
  }

  # Build model formula
  model_formula <- stats::as.formula(paste(outcome, "~", paste(exposures, collapse = " + ")))

  # Fit model
  fit_model <- tryCatch({
    MASS::glm.nb(model_formula, data = data_clean, control = glm.control(maxit = 200))
  }, warning = function(w) {
    warning("Model warning: ", w$message)
    return(NULL)
  }, error = function(e) {
    warning("Model failed: ", e$message)
    return(NULL)
  })

  if (is.null(fit_model)) {
    message("Model fitting failed. No results to return.")
    return(NULL)
  }

  if (summary) {
    cat("\nModel summary:\n")
    print(summary(fit_model))
  }

  # Format results
  result <- gtsummary::tbl_regression(fit_model, exponentiate = TRUE) %>%
    gtsummary::modify_header(estimate = "**Adjusted IRR**") %>%
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio") %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label)))

  attr(result, "approach") <- "nbin"
  attr(result, "source") <- "multi_reg_nbin"

  return(result)
}
