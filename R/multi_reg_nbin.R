#' Multivariable Negative Binomial Regression (Adjusted IRRs)
#'
#' Performs multivariable negative binomial regression
#' using all specified exposures on a count outcome.
#' Returns a publication-ready regression table with
#' adjusted incidence rate ratios (IRRs) using `gtsummary`.
#'
#' @param data A data frame containing the variables.
#' @param outcome name of the count outcome variable.
#' @param exposures A character vector specifying the predictor (exposure)
#'
#' @return An object of class `multi_reg_nbin`, which includes:
#' - A `gtsummary::tbl_regression` object with exponentiated IRRs,
#' - The fitted model object accessible via `$models`,
#' - A tidy model summary via `$model_summaries`,
#' - The regression table via `$table`.
#'
#' @section Accessors:
#' \describe{
#'   \item{\code{$models}}{Fitted negative binomial model object
#'   (class `negbin`).}
#'   \item{\code{$model_summaries}}{Tidy summary table of model estimates.}
#'   \item{\code{$table}}
#'   {Formatted `gtsummary` regression table with adjusted IRRs.}
#' }
#'
#' @seealso [uni_reg_nbin()], [plot_reg()], [check_dispersion()]
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data(Pima.tr, package = "MASS")
#'   multi_nb <- multi_reg_nbin(
#'     data = Pima.tr,
#'     outcome = "glu",
#'     exposures = c("age", "bmi")
#'   )
#'   multi_nb$table
#' }
#'
#' @importFrom MASS glm.nb
#' @importFrom broom tidy
#' @export
multi_reg_nbin <- function(data, outcome, exposures) {
  data_clean <- .validate_nb_multi_inputs(data, outcome, exposures)

  # Build and fit model
  model_formula <- stats::as.formula(paste(outcome, "~",
                                           paste(exposures, collapse = " + ")))
  fit_model <- tryCatch(
    {
      MASS::glm.nb(model_formula, data = data_clean,
                   control = glm.control(maxit = 200))
    },
    error = function(e) {
      stop("Model fitting failed: ", e$message)
    }
  )

  # Format result
  result <- fit_model |>
    gtsummary::tbl_regression(exponentiate = TRUE) |>
    gtsummary::modify_header(estimate = "**Adjusted IRR**") |>
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio") |>
    gtsummary::modify_table_body(~ {
      .x$label <- as.character(.x$label)
      .x
    })

  # Extract N_obs from the fitted model
  result <- result |>
    gtsummary::modify_source_note(
      paste("N =", unique(na.omit(result$table_body$N_obs))[1],
            "complete observations included in the multivariate model")
    )

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
