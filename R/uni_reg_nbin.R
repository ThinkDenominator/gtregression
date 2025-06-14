#' Univariate Negative Binomial Regression
#'
#' Fits negative binomial regression models for each exposure on a count outcome.
#' Returns a publication-ready stacked table using `gtsummary`.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A vector of predictor variables.
#'
#' @return A `gtsummary::tbl_stack` object of exponentiated unadjusted IRRs with `$models`, `$model_summaries`, and `$table` accessors.
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
