#' Univariate Negative Binomial Regression
#'
#' Fits negative binomial regression models for each exposure on a count outcome.
#' Returns a publication-ready stacked table using `gtsummary`.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A vector of predictor variables.
#' @param summary Logical; if `TRUE`, prints model summaries. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_stack` object of exponentiated unadjusted IRRs.
#' @export
uni_reg_nbin <- function(data, outcome, exposures, summary = FALSE) {
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

  message("Running uni_reg_nbin using negative binomial regression...")

  fit_model <- function(exposure) {
    tryCatch({
      data_clean <- data %>%
        dplyr::filter(!is.na(.data[[exposure]]), !is.na(.data[[outcome]])) %>%
        droplevels()

      if (nrow(data_clean) == 0) {
        warning("Skipping ", exposure, ": no complete cases.")
        return(NULL)
      }

      if (length(unique(data_clean[[exposure]])) < 2) {
        warning("Skipping ", exposure, ": insufficient variation.")
        return(NULL)
      }

      model <- MASS::glm.nb(stats::as.formula(paste(outcome, "~", exposure)), data = data_clean)

      if (summary) {
        cat("\nSummary for", exposure, ":\n")
        print(summary(model))
      }

      gtsummary::tbl_regression(model, exponentiate = TRUE)

    }, warning = function(w) {
      warning("Model warning for ", exposure, ": ", w$message)
      return(NULL)
    }, error = function(e) {
      warning("Model failed for ", exposure, ": ", e$message)
      return(NULL)
    })
  }

  model_list <- purrr::map(exposures, fit_model)
  model_list <- model_list[!sapply(model_list, is.null)]

  if (length(model_list) == 0) {
    stop("All negative binomial models failed.")
  }

  final_tbl <- gtsummary::tbl_stack(model_list) %>%
    gtsummary::modify_header(estimate = "**IRR**") %>%
    gtsummary::modify_abbreviation("IRR = Incidence Rate Ratio")
  attr(final_tbl, "approach") <- "nbin"
  attr(final_tbl, "source") <- "uni_reg_nbin"

  return(final_tbl)
}
