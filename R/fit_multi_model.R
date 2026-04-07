#' Fit multivariable regression model (Internal)
#'
#' Fits a regression model based on the selected approach.
#'
#' @param data A data.frame containing the analysis variables.
#' @param outcome A character scalar giving the outcome variable name.
#' @param exposures A character vector of exposure variable(s).
#' @param approach A regression approach. One of "logit", "log-binomial",
#'   "poisson", "linear", "robpoisson", or "negbin".
#' @param adjust_for Optional character vector of adjustment variables.
#' @param interaction Optional character scalar specifying an interaction term,
#'   e.g. "bmi*sex".
#'
#' @return A fitted model object or `NULL` if fitting fails.
#' @keywords internal
#' @noRd
.fit_multi_model <- function(data,
                             outcome,
                             exposures,
                             approach,
                             adjust_for = NULL,
                             interaction = NULL) {

  rhs_terms <- unique(c(exposures, adjust_for))

  if (!is.null(interaction) && length(interaction) > 0) {
    rhs_terms <- unique(c(rhs_terms, interaction))
  }

  formula_str <- paste(outcome, "~", paste(rhs_terms, collapse = " + "))
  formula <- stats::as.formula(formula_str)

  tryCatch(
    {
      switch(
        approach,
        "logit" = stats::glm(
          formula = formula,
          data = data,
          family = stats::binomial("logit")
        ),
        "log-binomial" = stats::glm(
          formula = formula,
          data = data,
          family = stats::binomial("log")
        ),
        "poisson" = stats::glm(
          formula = formula,
          data = data,
          family = stats::poisson("log")
        ),
        "linear" = stats::lm(
          formula = formula,
          data = data
        ),
        "robpoisson" = risks::riskratio(
          formula = formula,
          data = data,
          approach = "robpoisson"
        ),
        "negbin" = MASS::glm.nb(
          formula = formula,
          data = data,
          control = stats::glm.control(maxit = 200)
        ),
        stop(
          paste0(
            "Invalid approach: '", approach,
            "'. Choose from: logit, log-binomial, poisson, ",
            "robpoisson, linear, negbin."
          ),
          call. = FALSE
        )
      )
    },
    error = function(e) {
      warning(
        paste0(
          "Model fitting failed for exposure(s) ",
          paste(exposures, collapse = ", "),
          " using approach '", approach, "': ",
          e$message
        ),
        call. = FALSE
      )
      NULL
    }
  )
}
