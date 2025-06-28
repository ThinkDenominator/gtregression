#' @param data A `data.frame` with complete obs for outcome and exposures.
#' @param outcome A string. Name of the outcome variable.
#' @param exposures A character vector of predictor (exposure) variable names.
#' @return A fitted model object (e.g., `glm`, `lm`, or `glm.nb`) or `NULL`

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
