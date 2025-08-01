#' Identify Confounders Using the Change-in-Estimate Method
#'
#' Identifies whether one or more variables are confounders by comparing
#' the crude and adjusted effect estimates of a primary exposure on an outcome.
#' A variable is flagged as a confounder if its inclusion changes the estimate
#' by more than a specified threshold (default = 10%).
#'
#' Supports logistic, log-binomial, Poisson, robust Poisson, negative binomial,
#' and linear regression approaches.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable (character string).
#' @param exposure The primary exposure variable (character string).
#' @param potential_confounder One or more variables to test as potential confounders.
#' @param approach The regression modeling approach. One of:
#'   \code{"logit"}, \code{"log-binomial"}, \code{"poisson"},
#'   \code{"negbin"}, \code{"robpoisson"}, or \code{"linear"}.
#' @param threshold Numeric. Percent change threshold to define confounding
#'   (default = 10). If the absolute percent change exceeds this, the variable
#'   is flagged as a confounder.
#'
#' @return If one confounder is provided, prints crude and adjusted estimates
#' with a confounding flag. If multiple are given, returns a tibble with:
#' \describe{
#'   \item{covariate}{Name of potential confounder.}
#'   \item{crude_est}{Crude effect estimate.}
#'   \item{adjusted_est}{Adjusted estimate including the confounder.}
#'   \item{pct_change}{Percent change from crude to adjusted.}
#'   \item{is_confounder}{Logical: whether confounding threshold is exceeded.}
#' }
#'
#' @details
#' This method does not evaluate effect modification. Use causal diagrams
#' (e.g., DAGs) and subject-matter knowledge to supplement decisions.
#'
#' @seealso [check_convergence()], [interaction_models()]
#'
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

  # mandatory checks
  .validate_outcome_by_approach(data[[outcome]], approach)

  # Generate models
  get_model <- function(data, formula, approach) {
    switch(approach,
      "logit" = glm(formula, data = data, family = binomial("logit")),
      "log-binomial" = glm(formula, data = data, family = binomial("log")),
      "poisson" = glm(formula, data = data, family = poisson("log")),
      "negbin" = MASS::glm.nb(formula, data = data),
      "linear" = lm(formula, data = data),
      "robpoisson" = risks::riskratio(formula = formula, data = data,
                                      approach = "robpoisson"),
      stop("Unsupported approach.", call. = FALSE)
    )
  }
  # Extract point estimate for exposure
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
    message("\nNotes:\n")
    message("* Confounding is suggested if percent change >=", threshold, "%.\n")
    message("* This method does not assess effect modification.\n")
    message("* Use DAGs or domain knowledge to support confounder
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

  message("\n------------------------------------------------------------\n")
  message("Crude Estimate:      ", format(round(crude_est, 3), nsmall = 3), "\n")
  message("Adjusted Estimate:   ", format(round(adj_est, 3), nsmall = 3), "\n")
  message("% Change from Crude: ", format(round(pct_change, 2), nsmall = 2), "%\n")
  message("------------------------------------------------------------\n")
  message("Confounding:         ", ifelse(is_conf, "Yes", "No"), "\n")
  message("------------------------------------------------------------\n")
  message("Notes:\n")
  message("* Confounding is suggested if percent change >=", threshold, "%.\n")
  message("* This method does not assess effect modification.\n")
  message("* Use DAGs or domain knowledge to support confounder identification.\n")

  return(invisible(list(
    crude = crude_est,
    adjusted = adj_est,
    percent_change = pct_change,
    is_confounder = is_conf
  )))
}
