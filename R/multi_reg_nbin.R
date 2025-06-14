#' Multivariable Negative Binomial Regression (Adjusted IRRs)
#'
#' Calculates adjusted incidence rate ratios (IRRs) using Negative Binomial Regression.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A vector of predictor variables.
#'
#' @return A `gtsummary::tbl_regression` object with exponentiated estimates labeled as Adjusted IRRs,
#' along with `$models`, `$model_summaries`, and `$table`.
#' @export
multi_reg_nbin <- function(data, outcome, exposures) {
  `%>%` <- magrittr::`%>%`

  # Validate inputs
  if (!outcome %in% names(data)) stop("Outcome variable not found.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in dataset.")

  # Outcome validation
  outcome_vec <- data[[outcome]]
  is_count <- function(x) is.numeric(x) && all(!is.na(x)) && all(x >= 0 & x == floor(x))

  if (!is_count(outcome_vec)) {
    stop("Negative binomial regression requires a non-negative count outcome (e.g., number of events).")
  }

  # Clean data
  data_clean <- data %>%
    dplyr::filter(!is.na(.data[[outcome]])) %>%
    tidyr::drop_na(dplyr::any_of(exposures))

  if (nrow(data_clean) == 0) stop("No valid observations after removing missing values.")

  # Check variation in exposures
  insufficient_vars <- exposures[sapply(data_clean[exposures], function(x) length(unique(x)) < 2)]
  if (length(insufficient_vars) > 0) {
    stop("Exposure(s) with insufficient variation: ", paste(insufficient_vars, collapse = ", "))
  }

  # Build and fit model
  model_formula <- stats::as.formula(paste(outcome, "~", paste(exposures, collapse = " + ")))
  fit_model <- tryCatch({
    MASS::glm.nb(model_formula, data = data_clean, control = glm.control(maxit = 200))
  }, error = function(e) {
    stop("Model fitting failed: ", e$message)
  })

  # Format result
  result <- gtsummary::tbl_regression(fit_model, exponentiate = TRUE) %>%
    gtsummary::modify_header(estimate = "**Adjusted IRR**") %>%
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio") %>%
    gtsummary::modify_table_body(~ dplyr::mutate(., label = as.character(label)))

  # Attach metadata and class
  attr(result, "approach") <- "negbin"
  attr(result, "source") <- "multi_reg_nbin"
  attr(result, "models") <- list(fit_model)
  attr(result, "model_summaries") <- list(summary(fit_model))
  class(result) <- c("multi_reg_nbin", class(result))

  return(result)
}

#' @export
`$.multi_reg_nbin` <- function(x, name) {
  if (name == "models") {
    model_list <- attr(x, "models")
    lapply(model_list, print)
    return(invisible(model_list))
  }
  if (name == "model_summaries") {
    return(attr(x, "model_summaries"))
  }
  if (name == "table") {
    return(x)
  }
  NextMethod("$")
}
