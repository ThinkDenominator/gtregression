#' Fit Univariate Negative Binomial Regression Model (Internal)
#'
#' Fits a univariate negative binomial regression model using `glm.nb()` for a count outcome and single predictor.
#'
#' @param data A `data.frame` with complete observations for outcome and exposures.
#' @param outcome A string. Name of the count outcome variable.
#' @param exposure A single predictor variable name.
#'
#' @return A fitted model object (of class `negbin`) or `NULL` if model fitting fails or data is insufficient.
#' @keywords internal

.fit_uni_model_nbin <- function(data, outcome, exposure) {
  formula <- as.formula(paste(outcome, "~", exposure))
  tryCatch(
    {
      data_clean <- data[!is.na(data[[outcome]]) &
                           !is.na(data[[exposure]]), , drop = FALSE]
      if (nrow(data_clean) == 0 || length(unique(data_clean[[exposure]])) < 2) {
        return(NULL)
      }

      MASS::glm.nb(formula, data = data_clean)
    },
    error = function(e) {
      warning("Model failed for ", exposure, ": ", e$message)
      NULL
    }
  )
}
