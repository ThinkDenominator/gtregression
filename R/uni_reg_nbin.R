#' Univariate Negative Binomial Regression
#'
#' Performs univariate negative binomial regression for each
#' specified exposure on a count outcome.
#' Returns a publication-ready stacked regression table using `gtsummary`,
#' with incidence rate ratios (IRRs) and confidence intervals.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable (character).
#' @param exposures A character vector of predictor variables.
#'
#' @return An object of class `uni_reg_nbin`, which includes:
#' - A `gtsummary::tbl_stack` object with exponentiated IRRs,
#' - A list of model objects accessible via `$models`,
#' - Tidy summaries of models via `$model_summaries`,
#' - The stacked table via `$table`.
#'
#' @section Accessors:
#' \describe{
#'   \item{\code{$models}}{List of fitted negative binomial model objects.}
#'   \item{\code{$model_summaries}}{A tibble of tidy model summaries.}
#'   \item{\code{$table}}{A `gtsummary::tbl_stack` object of unadjusted IRRs.}
#' }
#'
#' @seealso [multi_reg_nbin()], [plot_reg()], [check_dispersion()]
#'
#' @examples
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   data(PimaIndiansDiabetes2, package = "mlbench")
#'   pima <- dplyr::mutate(
#'     PimaIndiansDiabetes2,
#'     pregnant = as.integer(pregnant)
#'   )
#'   reg <- uni_reg_nbin(
#'     data = pima,
#'     outcome = "pregnant",
#'     exposures = c("age", "mass")
#'   )
#'   reg$table
#' }
#' @importFrom broom tidy
#' @importFrom MASS glm.nb
#' @export
uni_reg_nbin <- function(data, outcome, exposures) {
  # Validate outcome and exposures
  .validate_nb_inputs(data, outcome, exposures)

  model_list <- lapply(exposures, function(exposure) {
    .fit_uni_model_nbin(data, outcome, exposure)
  })
  names(model_list) <- exposures
  model_list <- Filter(Negate(is.null), model_list)

  if (length(model_list) == 0)
    stop("All models failed. Please check your data or exposures.")

  tbl_list <- mapply(function(fit, var) {
    gtsummary::tbl_regression(fit, exponentiate = TRUE) |>
      gtsummary::modify_header(estimate = "**IRR**") |>
      gtsummary::add_n(location = "label")
  }, model_list, names(model_list), SIMPLIFY = FALSE)

  stacked <- gtsummary::tbl_stack(tbl_list)

  result <- stacked |>
    gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio") |>
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio")


  model_summaries <- lapply(model_list, summary)
  names(model_summaries) <- exposures

  attr(result, "approach") <- "negbin"
  attr(result, "source") <- "uni_reg_nbin"
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- model_summaries
  class(result) <- c("uni_reg_nbin", class(result))

  return(result)
}

#' @export
`$.uni_reg_nbin` <- function(x, name) {
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
