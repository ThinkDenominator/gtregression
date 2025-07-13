#' Stepwise Model Selection with Evaluation Metrics
#'
#' Performs stepwise model selection using forward, backward, or both directions
#'  across different regression approaches.
#' Returns a summary table with evaluation
#' metrics (AIC, BIC, log-likelihood, deviance) and the best model.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome A character string indicating the outcome variable.
#' @param exposures vector of predictor variables to consider in the model.
#' @param approach Regression method. One of:
#'   \code{"logit"}, \code{"log-binomial"}, \code{"poisson"},
#'   \code{"robpoisson"}, \code{"negbin"}, or \code{"linear"}.
#' @param direction Stepwise selection direction. One of:
#'   \code{"forward"} (default), \code{"backward"}, or \code{"both"}.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item \code{results_table}: A tibble summarising each tested model's metric
#'   (AIC, BIC, deviance, log-likelihood, adjusted R² if applicable).
#'   \item \code{best_model}: The best-fitting model object based on low AIC.
#'   \item \code{all_models}: A named list of all fitted models.
#' }
#'
#' @importFrom stats AIC BIC anova as.formula binomial coef cooks.distance
#' @importFrom stats deviance glm glm.control lm logLik na.omit nobs poisson
#' @importFrom stats predict residuals shapiro.test
#' @importFrom MASS glm.nb
#' @importFrom utils data
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @examples
#' data <- data_PimaIndiansDiabetes
#' stepwise <- select_models(
#'   data = data,
#'   outcome = "glucose",
#'   exposures = c("age", "pregnant", "mass"),
#'   approach = "linear",
#'   direction = "forward"
#' )
#' summary(stepwise)
#' stepwise$results_table
#' stepwise$best_model
#'
#' @export
select_models <- function(data, outcome, exposures, approach = "logit",
                          direction = "forward") {
  # Validate outcome type
  .validate_outcome_by_approach(data[[outcome]], approach)

  # Internal function to fit a model with selected variables
  # Returns a model object and stores the formula as an attribute
  fit_model <- function(vars) {
    fmla_str <- if (length(vars) > 0) {
      paste(outcome, "~", paste(vars, collapse = " + "))
    } else {
      paste(outcome, "~ 1")
    }
    model <- if (approach == "negbin") {
      MASS::glm.nb(eval(parse(text = fmla_str)), data = data)
    } else if (approach == "linear") {
      lm(eval(parse(text = fmla_str)), data = data)
    } else {
      family <- switch(approach,
        "logit" = binomial(link = "logit"),
        "log-binomial" = binomial(link = "log"),
        "poisson" = poisson(link = "log"),
        "robpoisson" = poisson(link = "log"),
        stop("Unsupported approach: must be one of 'logit', 'log-binomial',
             'poisson', 'robpoisson', 'negbin', or 'linear'")
      )
      glm(as.formula(fmla_str), family = family, data = data)
    }


    attr(model, "formula_str") <- fmla_str
    return(model)
  }
  all_models <- list()
  model_metrics <- list()

  # Initialize stepwise search
  step <- 1

  if (direction == "forward") {
    selected_vars <- c()
  } else if (direction == "backward") {
    selected_vars <- exposures
  } else if (direction == "both") {
    selected_vars <- c()
  } else {
    stop("Invalid direction: choose from 'forward', 'backward', or 'both'")
  }
  # Stepwise logic: determine which predictors to add/drop
  # based on the specified direction
  repeat {
    best_model <- fit_model(selected_vars)
    all_models[[step]] <- best_model
    aic_best <- AIC(best_model)

    # Forward candidates
    add_candidates <- setdiff(exposures, selected_vars)
    forward_models <- lapply(add_candidates, function(var)
      fit_model(c(selected_vars, var)))
    forward_aics <- vapply(forward_models, AIC, FUN.VALUE = numeric(1))
    best_forward <- if (length(forward_aics)) min(forward_aics) else Inf
    best_forward_idx <- if (length(forward_aics)) which.min(forward_aics)
    else NA

    # Backward candidates
    drop_candidates <- if (length(selected_vars) > 1)
      lapply(selected_vars, function(var) setdiff(selected_vars, var))
    else list()
    backward_models <- lapply(drop_candidates, fit_model)
    backward_aics <- vapply(backward_models, AIC, FUN.VALUE = numeric(1))
    best_backward <- if (length(backward_aics)) min(backward_aics)
    else Inf
    best_backward_idx <- if (length(backward_aics)) which.min(backward_aics)
    else NA

    improved <- FALSE

    if (direction == "forward" && best_forward < aic_best - 1e-5) {
      selected_vars <- c(selected_vars, add_candidates[[best_forward_idx]])
      improved <- TRUE
    } else if (direction == "backward" && best_backward < aic_best - 1e-5) {
      selected_vars <- drop_candidates[[best_backward_idx]]
      improved <- TRUE
    } else if (direction == "both") {
      if (best_forward < best_backward && best_forward < aic_best - 1e-5) {
        selected_vars <- c(selected_vars, add_candidates[[best_forward_idx]])
        improved <- TRUE
      } else if (best_backward < aic_best - 1e-5) {
        selected_vars <- drop_candidates[[best_backward_idx]]
        improved <- TRUE
      }
    }

    model_formula_str <- attr(best_model, "formula_str")

    if (approach == "linear") {
      model_metrics[[step]] <- tibble::tibble(
        model_id = step,
        formula = model_formula_str,
        n_predictors = length(selected_vars),
        AIC = aic_best,
        BIC = BIC(best_model),
        logLik = as.numeric(logLik(best_model)),
        deviance = deviance(best_model),
        adj_r2 = summary(best_model)$adj.r.squared
      )
    } else {
      model_metrics[[step]] <- tibble::tibble(
        model_id = step,
        formula = model_formula_str,
        n_predictors = length(selected_vars),
        AIC = aic_best,
        BIC = BIC(best_model),
        logLik = as.numeric(logLik(best_model)),
        deviance = deviance(best_model),
        selected_vars = paste(selected_vars, collapse = " + ")
      )
    }

    step <- step + 1
    if (!improved) break
  }

  # Combine model metrics, identify best model, and return results
  metrics_tbl <- dplyr::bind_rows(model_metrics)
  best_row <- which.min(metrics_tbl$AIC)
  final_best_model <- all_models[[best_row]]
  final_best_model$call$formula <- as.formula(model_formula_str)
  attr(final_best_model, "selected_vars") <- selected_vars

  return(list(
    results_table = metrics_tbl,
    best_model = final_best_model,
    all_models = all_models
  ))
}
