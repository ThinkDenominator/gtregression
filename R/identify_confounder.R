#' Identify Confounders Using the Change-in-Estimate Method
#'
#' This function evaluates whether one or more variables are confounders of
#' an exposure and outcome by comparing crude and adjusted effect estimates.
#' A variable is considered a confounder if inclusion in the model changes
#' the effect size by more than a specified threshold (default: 10%).
#'
#' Supports multiple regression approaches including logistic, log-binomial,
#' Poisson, negative binomial, robust Poisson, and linear regression.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable
#' @param exposure The name of the main exposure variable to test.
#' @param potential_confounder variable to test as potential confounder
#' @param approach The regression modeling approach. One of:
#'   \code{"logit"} (logistic), \code{"log-binomial"}, \code{"poisson"},
#'   \code{"negbin"} (negative binomial),
#'   \code{"robpoisson"}, or \code{"linear"}.
#' @param threshold A value specifying the percent change in the estimate
#' If the absolute percent change from the crude to adjusted estimate
#' exceeds this threshold, the variable is flagged as a confounder.
#'
#' @return A `tibble` summarizing:
#' \itemize{
#'   \item Crude effect estimate (e.g., OR, RR, IRR, Beta)
#'   \item Adjusted effect estimate (after including each potential confounder)
#'   \item Percent change in estimate
#'   \item Confounder flag (TRUE/FALSE)
#' }
#'
#' @importFrom stats glm binomial poisson lm coef as.formula
#' @importFrom MASS glm.nb
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @examples
#' data <- data_PimaIndiansDiabetes
#' identify_confounder(
#'   data = data,
#'   outcome = "glucose",
#'   exposure = "insulin",
#'   potential_confounder = "age_cat",
#'   approach = "linear"
#' )
#'
#' @export

identify_confounder <- function(data, outcome, exposure, potential_confounder,
                                approach = "logit", threshold = 10) {
  .validate_outcome_by_approach(data[[outcome]], approach)

  get_model <- function(data, formula, approach) {
    switch(approach,
      "logit" = glm(formula, data = data, family = binomial("logit")),
      "log-binomial" = glm(formula, data = data, family = binomial("log")),
      "poisson" = glm(formula, data = data, family = poisson("log")),
      "negbin" = MASS::glm.nb(formula, data = data),
      "linear" = lm(formula, data = data),
      "robpoisson" = risks::riskratio(formula = formula, data = data,
                                      approach = "robpoisson"),
      stop("Unsupported approach.")
    )
  }

  extract_estimate <- function(model, exposure, approach) {
    if (inherits(model, "glm") || inherits(model, "lm")) {
      coefs <- coef(model)
      idx <- grep(paste0("^", exposure), names(coefs))
      if (length(idx) == 0) {
        return(NA)
      }
      est <- unname(coefs[idx[1]])
      return(if (approach == "linear") est else exp(est))
    }
    if (inherits(model, "riskratio")) {
      if (!is.null(model$summary) && "term" %in% names(model$summary)) {
        idx <- grep(paste0("^", exposure), model$summary$term)
        if (length(idx) == 0) {
          return(NA)
        }
        return(model$summary$RR[idx[1]])
      }
    }
    return(NA)
  }

  # Crude model
  crude_model <- tryCatch(get_model(data,
                                    as.formula(paste(outcome, "~", exposure)),
                                    approach), error = function(e) NULL)
  crude_est <- if (!is.null(crude_model))
    extract_estimate(crude_model, exposure, approach) else NA

  # Multiple potential confounders
  if (length(potential_confounder) > 1) {
    results <- lapply(potential_confounder, function(var) {
      adj_fmla <- as.formula(paste(outcome, "~",
                                   paste(c(exposure, var), collapse = " + ")))
      adj_model <- tryCatch(get_model(data, adj_fmla, approach),
                            error = function(e) NULL)
      adj_est <- if (!is.null(adj_model))
        extract_estimate(adj_model, exposure, approach) else NA
      pct_change <- if (is.na(adj_est) ||
                        is.na(crude_est)) NA
      else abs((adj_est - crude_est) / crude_est) * 100

      is_conf <- if (!is.na(pct_change)) pct_change >= threshold else NA

      tibble::tibble(
        covariate = var,
        crude_est = round(crude_est, 3),
        adjusted_est = round(adj_est, 3),
        pct_change = round(pct_change, 2),
        is_confounder = is_conf
      )
    })

    result_tbl <- dplyr::bind_rows(results)

    print(result_tbl)
    cat("\nNotes:\n")
    cat("* Confounding is suggested if percent change >=", threshold, "%.\n")
    cat("* This method does not assess effect modification.\n")
    cat("* Use DAGs or domain knowledge to support confounder
        identification.\n")

    return(invisible(result_tbl))
  }


  adj_fmla <- as.formula(paste(outcome, "~",
                               paste(c(exposure, potential_confounder),
                                     collapse = " + ")))
  adj_model <- tryCatch(get_model(data, adj_fmla, approach),
                        error = function(e) NULL)
  adj_est <- if (!is.null(adj_model))
    extract_estimate(adj_model, exposure, approach) else NA
  pct_change <- if (is.na(adj_est) ||
                    is.na(crude_est)) NA
  else abs((adj_est - crude_est) / crude_est) * 100

  is_conf <- if (!is.na(pct_change)) pct_change >= threshold
  else NA

  cat("\n------------------------------------------------------------\n")
  cat("Crude Estimate:      ", format(round(crude_est, 3), nsmall = 3), "\n")
  cat("Adjusted Estimate:   ", format(round(adj_est, 3), nsmall = 3), "\n")
  cat("% Change from Crude: ", format(round(pct_change, 2), nsmall = 2), "%\n")
  cat("------------------------------------------------------------\n")
  cat("Confounding:         ", ifelse(is_conf, "Yes", "No"), "\n")
  cat("------------------------------------------------------------\n")
  cat("Notes:\n")
  cat("* Confounding is suggested if percent change >=", threshold, "%.\n")
  cat("* This method does not assess effect modification.\n")
  cat("* Use DAGs or domain knowledge to support confounder identification.\n")

  return(invisible(list(
    crude = crude_est,
    adjusted = adj_est,
    percent_change = pct_change,
    is_confounder = is_conf
  )))
}
