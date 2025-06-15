#' Univariate Negative Binomial Regression
#'
#' Performs univariate negative binomial regression for each specified exposure on a count outcome.
#' Returns a publication-ready stacked regression table using `gtsummary`, with incidence rate ratios (IRRs) and confidence intervals.
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
#' set.seed(123)
#' dummy_data <- data.frame(
#'   y = MASS::rnegbin(200, mu = 3, theta = 1.2),
#'   x1 = sample(c("Low", "Medium", "High"), 200, replace = TRUE),
#'   x2 = sample(c("Yes", "No"), 200, replace = TRUE)
#' )
#'
#' result <- uni_reg_nbin(data = dummy_data, outcome = "y", exposures = c("x1", "x2"))
#' result$table
#'
#' @importFrom broom tidy
#' @importFrom MASS glm.nb
#' @export
uni_reg_nbin <- function(data, outcome, exposures) {
  `%>%` <- magrittr::`%>%`

  # Validate outcome and exposures
  if (!outcome %in% names(data)) stop("Outcome variable not found in dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in dataset.")

  outcome_vec <- data[[outcome]]
  is_count <- function(x) is.numeric(x) && all(!is.na(x)) && all(x >= 0 & x == floor(x))

  if (!is_count(outcome_vec)) {
    stop("Negative binomial regression requires a non-negative count outcome (e.g., number of events).")
  }

  # Fit function
  fit_model <- function(exposure) {
    tryCatch({
      data_clean <- data %>%
        dplyr::filter(!is.na(.data[[exposure]]), !is.na(.data[[outcome]])) %>%
        droplevels()
      if (nrow(data_clean) == 0 || length(unique(data_clean[[exposure]])) < 2) return(NULL)
      MASS::glm.nb(stats::as.formula(paste(outcome, "~", exposure)), data = data_clean)
    }, error = function(e) {
      warning("Model failed for ", exposure, ": ", e$message)
      return(NULL)
    })
  }

  model_list <- purrr::map(exposures, fit_model)
  names(model_list) <- exposures
  model_list <- model_list[!sapply(model_list, is.null)]

  if (length(model_list) == 0) stop("All models failed. Please check your data or exposures.")

  tbl_list <- purrr::map(model_list,
                         ~gtsummary::tbl_regression(.x,
                                                    exponentiate = TRUE,
                                                    conf.method = "wald",
                                                    tidy_fun = broom::tidy))
  stacked <- gtsummary::tbl_stack(purrr::map(tbl_list, ~gtsummary::modify_header(.x, estimate = "**IRR**")))

  result <- stacked %>%
    gtsummary::remove_abbreviation("IRR = Incidence Rate Ratio") %>%
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio")

  model_summaries <- purrr::map(model_list, summary)
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
