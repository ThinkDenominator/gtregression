#' Univariate Negative Binomial Regression
#'
#' This function fits negative binomial regression models for multiple exposures.
#' It returns a publication-ready table using `gtsummary`.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A vector of predictor variables.
#' @param summary Logical; if `TRUE`, prints model summaries. Default is `FALSE`.
#'

#' @return A stacked summary table of negative binomial models.
#' @export
uni_reg_nbin <- function(data, outcome, exposures, summary = FALSE) {
  # Dependencies
  requireNamespace("MASS", quietly = TRUE)
  requireNamespace("gtsummary", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  requireNamespace("forcats", quietly = TRUE)

  message("Running uni_reg_nbin using negative binomial regression")

  # Validate outcome: must be count
  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  }

  if (!is_count(data[[outcome]])) {
    stop("Negative binomial regression requires a count outcome (non-negative integers).")
  }

  # Fit model function
  fit_model <- function(exposure) {
    tryCatch({
      data_clean <- data %>%
        dplyr::filter(!is.na(.data[[exposure]]), !is.na(.data[[outcome]])) %>%
        droplevels()

      if (is.factor(data_clean[[exposure]]) && length(unique(data_clean[[exposure]])) < 2) {
        warning(paste("Skipping", exposure, "due to insufficient levels."))
        return(NULL)
      }

      model <- MASS::glm.nb(as.formula(paste(outcome, "~", exposure)), data = data_clean, control = glm.control(maxit = 100))

      if (summary) {
        cat("\n Summary for exposure:", exposure, "\n")
        print(summary(model))
      }

      gtsummary::tbl_regression(model, exponentiate = TRUE)
    }, warning = function(w) {
      warning(paste("Negative binomial model warning for", exposure, ":", w$message))
      return(NULL)
    }, error = function(e) {
      warning(paste("Negative binomial model failed for", exposure, ":", e$message))
      return(NULL)
    })
  }

  # Run models
  model_list <- purrr::map(exposures, fit_model)
  model_list <- model_list[!sapply(model_list, is.null)]

  if (length(model_list) == 0) {
    stop("All negative binomial models failed to converge.")
  }

  # Combine and return
  final_tbl <- gtsummary::tbl_stack(model_list)
  print(final_tbl)
  return(final_tbl)
}
