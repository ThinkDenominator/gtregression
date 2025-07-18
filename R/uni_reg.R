#' Univariate regression (Odds, Risk, or Rate Ratios)
#'
#' Performs univariate regression for each exposure on a binary,
#' continuous, or count outcome.
#' Depending on `approach`, returns either Odds Ratios (OR), Risk Ratios (RR),
#' or Incidence Rate Ratios (IRR).
#'
#' @param data A data frame containing the variables.
#' @param outcome outcome variable (binary, continuous, or count).
#' @param exposures A vector of predictor variables.
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (OR), `"log-binomial"` (RR), `"poisson"` (IRR),
#'   `"robpoisson"` (RR), `"linear"` (Beta coefficients), `"negbin"` (IRR)
#' @importFrom broom tidy
#' @details This function requires the following packages:
#' `dplyr`, `purrr`, `gtsummary`, `risks`.
#' @return A list of class `uni_reg` and `gtsummary::tbl_stack`, including:
#' \itemize{
#'   \item A publication-ready regression table (`tbl_stack`)
#'   \item Accessor elements:
#'     \itemize{
#'       \item `$models`: Fitted regression models for each exposure
#'       \item `$model_summaries`: Tidy model summaries
#'       \item `$reg_check`: Diagnostics (only for linear regression)
#'     }
#' }
#' @examples
#' data(PimaIndiansDiabetes2, package = "mlbench")
#' library(dplyr)
#' pima <- PimaIndiansDiabetes2 |>
#'   dplyr::mutate(diabetes = ifelse(diabetes == "pos", 1, 0))
#' uni_reg(pima, outcome = "diabetes", exposures = "age", approach = "logit")
#' @seealso \code{\link{multi_reg}}, \code{\link{plot_reg}}
#' @family regression functions
#' @export
uni_reg <- function(data,
                    outcome,
                    exposures,
                    approach = "logit") {

  # Validate inputs through internal helpers
  .validate_uni_inputs(data, outcome, exposures, approach)

  # Edit labels and abbreviation through internal helpers
  label_est <- .get_effect_label(approach)
  abbreviation <- .get_abbreviation(approach)
  remove_abbrev <- .get_remove_abbreviation(approach)

  # Create a list of models, one for each exposure
  model_list <- lapply(exposures, function(x)
  # fit model using internal helper function
    .fit_uni_model(data, outcome, x, approach))

  # Assign names and remove failed models (NULLs)
  names(model_list) <- exposures
  model_list <- Filter(Negate(is.null), model_list)

  # Stop if no models succeeded
  if (length(model_list) == 0)
    stop("All models failed. Please check your data or exposures.")

  # Create a list of formatted gtsummary tables (one per exposure)
  tbl_list <- Map(function(fit, var) {
    gtsummary::tbl_regression(fit, exponentiate = approach != "linear") |>
      gtsummary::modify_header(estimate = label_est) |>
      gtsummary::add_n(location = "label")
  }, model_list, names(model_list))

  # Stack the tables into one combined output
  result <- gtsummary::tbl_stack(tbl_list) |>
    gtsummary::remove_abbreviation(remove_abbrev) |>
    gtsummary::modify_abbreviation(abbreviation)

  # get model summaries
  model_summaries <- lapply(model_list, summary)

  # Reg check for linear approach only
  reg_diagnostics <- if (approach == "linear") {
    Map(.reg_check_linear, model_list, names(model_list))
  } else {
    "Regression diagnostics only available for 'linear' models."
  }

  # Add metadata as attributes to support other functions of the package
  attr(result, "approach") <- approach
  attr(result, "source") <- "uni_reg"
  class(result) <- c("uni_reg", class(result))
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- model_summaries
  attr(result, "reg_diagnostics") <- reg_diagnostics

  # return result
  result
}
# S3 print
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
  if (name == "reg_check") {
    return(attr(x, "reg_diagnostics"))
  }
  if (name == "table") {
    return(x)
  }
  # Fall back to default behaviour for other gtsummary fields
  NextMethod("$")
}
