#' Multivariable Regression (Adjusted Odds, Risk, or Rate Ratios)
#'
#' Fits multivariable regression models for binary, count, or continuous
#' outcomes and returns a publication-ready summary table using `gtsummary`.
#' Depending on the specified `approach`, the function estimates
#' adjusted Odds Ratios (OR), Risk Ratios (RR), Incidence Rate Ratios (IRR),
#' or Beta coefficients.
#'
#' @param data A data frame containing the analysis variables.
#' @param outcome The name of the outcome variable. Must be a character string.
#' @param exposures A character vector of predictor variables to include.
#' @param approach Modeling approach to use. One of:
#'   - `"logit"` for logistic regression (OR),
#'   - `"log-binomial"` for log-binomial regression (RR),
#'   - `"poisson"` for Poisson regression (IRR),
#'   - `"robpoisson"` for robust Poisson regression (RR),
#'   - `"linear"` for linear regression (Beta coefficients),
#'   - `"negbin"` for negative binomial regression (IRR).
#'
#' @return An object of class `multi_reg`, extending
#'   `gtsummary::tbl_regression`. Additional components can be accessed using:
#'   \itemize{
#'     \item \code{x$models}: List of fitted model objects.
#'     \item \code{x$model_summaries}: List of summary outputs.
#'     \item \code{x$reg_check}: Regression diagnostics (only for linear models).
#'     \item \code{x$table}: Returns the main regression table.
#'   }
#' @section Accessors:
#' \describe{
#'   \item{\code{$models}}{List of fitted model objects.}
#'   \item{\code{$model_summaries}}{A tibble of tidy regression
#'   summaries for each model.}
#' }
#'
#' @seealso [uni_reg()], [plot_reg()], [plot_reg_combine()]
#'
#' @examples
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   data(PimaIndiansDiabetes2, package = "mlbench")
#'   pima <- dplyr::mutate(PimaIndiansDiabetes2,
#'   diabetes = ifelse(diabetes == "pos", 1, 0))
#'   model <- multi_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "mass"),
#'     approach = "logit"
#'   )
#'  print(model)
#' }
#'
#' @importFrom broom tidy
#' @export
multi_reg <- function(data,
                      outcome,
                      exposures,
                      approach = "logit") {
  # Validate inputs through internal helpers
  .validate_multi_inputs(data, outcome, exposures, approach)

  # Edit labels and abbreviation through internal helpers
  effect_label <- .get_effect_label_adjusted(approach)
  abbreviation <- .get_abbreviation(approach)
  remove_abbreviation <- .get_remove_abbreviation(approach)

  # Fit univariate models for each exposure
  fit_model <- .fit_multi_model(data, outcome, exposures, approach)

  # Stop if no models succeeded
  if (is.null(fit_model)) stop("Model fitting failed.", call. = FALSE)

  # Create a list of models
  model_list <- list(fit_model)

  names(model_list) <- "multivariable_model"

  model_summaries <- list(summary(fit_model))

  names(model_summaries) <- "multivariable_model"

  # exponentiate and create gtsummary table
  result <- fit_model |>
    gtsummary::tbl_regression(
      exponentiate = approach != "linear",
      conf.level = 0.95,
      conf.method = "wald",
      tidy_fun = broom::tidy
    ) |>
    gtsummary::modify_header(estimate = effect_label) |>
    gtsummary::modify_table_body(~ {
      .x$label <- as.character(.x$label)
      .x
    }) |>
    gtsummary::remove_abbreviation(remove_abbreviation) |>
    gtsummary::modify_abbreviation(abbreviation)

  # Get N_obs from the fitted model to add footnote as complete case analysis
  result <- result |>
    gtsummary::modify_source_note(
      paste("N =", unique(na.omit(result$table_body$N_obs))[1],
            "complete observations included in the multivariate model")
    )
  # Reg check for linear approach only
  reg_diagnostics <- if (approach == "linear") {
    list(multivariable_model = .reg_check_linear(fit_model,
                                                 "multivariable_model"))
  } else {
    "Regression diagnostics only available for 'linear' models."
  }
  # Add metadata as attributes to support other functions of the package
  attr(result, "approach") <- approach
  attr(result, "source") <- "multi_reg"
  class(result) <- c("multi_reg", class(result))
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- model_summaries
  attr(result, "reg_diagnostics") <- reg_diagnostics

  # return result
  result
}

# S3 print
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
  if (name == "reg_check") {
    return(attr(x, "reg_diagnostics"))
  }
  if (name == "table") {
    return(x)
  }
  # Fall back to default behaviour for other gtsummary fields
  NextMethod("$")
}
