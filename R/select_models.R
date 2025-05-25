#' Stepwise Model Selection with Evaluation Metrics
#'
#' This function performs stepwise model selection (forward, backward, or both)
#' across multiple regression approaches and returns a summary of tested models.
#'
#' @param data A data frame.
#' @param outcome The outcome variable (character).
#' @param exposures A character vector of predictor variables.
#' @param approach Regression method: "logit", "log-binomial", "poisson",
#'   "robpoisson", "negbin", or "linear".
#' @param direction Direction of selection: "forward" (add one-by-one), "backward" (drop one-by-one), or "both" (stepwise in both directions). Default is "forward".
#'
#' @return A list with model summaries, including:
#'   - `results_table`: a tibble of metrics
#'   - `best_model`: the final model object
#'   - `all_models`: list of all tested models
#'
#' @export
select_models <- function(data, outcome, exposures, approach = "logit", direction = "forward") {
  library(dplyr)
  library(stats)
  library(MASS)

  # Validate outcome type
  outcome_vec <- data[[outcome]]
  is_binary <- function(x) is.factor(x) && length(levels(x)) == 2 || is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)
  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) && length(unique(x[!is.na(x)])) > 2
  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")) {
    if (!is_binary(outcome_vec)) stop("This approach requires a binary outcome.")
  } else if (approach %in% c("poisson", "negbin")) {
    if (!is_count(outcome_vec)) stop("Count outcome required for Poisson or negbin.")
  } else if (approach == "linear") {
    if (!is_continuous(outcome_vec)) stop("Continuous numeric outcome required for linear regression.")
  }

  # Model fitting wrapper
  fit_model <- function(vars) {
    fmla <- if (length(vars) > 0) {
      as.formula(paste(outcome, "~", paste(vars, collapse = " + ")))
    } else {
      as.formula(paste(outcome, "~ 1"))
    }

    if (approach == "negbin") {
      return(MASS::glm.nb(fmla, data = data))
    } else if (approach == "linear") {
      return(lm(fmla, data = data))
    } else {
      family <- switch(
        approach,
        "logit" = binomial(link = "logit"),
        "log-binomial" = binomial(link = "log"),
        "poisson" = poisson(link = "log"),
        "robpoisson" = poisson(link = "log"),
        stop("Unsupported approach")
      )
      return(glm(fmla, family = family, data = data))
    }
  }

  all_models <- list()
  model_metrics <- list()
  step <- 1

  # INITIAL setup for each direction
  if (direction == "forward") {
    selected_vars <- c()
  } else if (direction == "backward") {
    selected_vars <- exposures
  } else if (direction == "both") {
    selected_vars <- c()
  } else {
    stop("Invalid direction: choose from 'forward', 'backward', or 'both'")
  }

  repeat {
    best_model <- fit_model(selected_vars)
    all_models[[step]] <- best_model
    aic_best <- AIC(best_model)

    # Try forward step
    add_candidates <- setdiff(exposures, selected_vars)
    forward_models <- lapply(add_candidates, function(var) fit_model(c(selected_vars, var)))
    forward_aics <- sapply(forward_models, AIC)
    best_forward <- if (length(forward_aics)) min(forward_aics) else Inf
    best_forward_idx <- if (length(forward_aics)) which.min(forward_aics) else NA

    # Try backward step
    drop_candidates <- if (length(selected_vars) > 1) lapply(selected_vars, function(var) setdiff(selected_vars, var)) else list()
    backward_models <- lapply(drop_candidates, fit_model)
    backward_aics <- sapply(backward_models, AIC)
    best_backward <- if (length(backward_aics)) min(backward_aics) else Inf
    best_backward_idx <- if (length(backward_aics)) which.min(backward_aics) else NA

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

    model_formula_str <- if (length(selected_vars) > 0) {
      paste(outcome, "~", paste(selected_vars, collapse = " + "))
    } else {
      paste(outcome, "~ 1")
    }

    if (approach == "linear") {
      model_metrics[[step]] <- tibble(
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
      model_metrics[[step]] <- tibble(
        model_id = step,
        formula = model_formula_str,
        n_predictors = length(selected_vars),
        AIC = aic_best,
        BIC = BIC(best_model),
        logLik = as.numeric(logLik(best_model)),
        deviance = deviance(best_model)
      )
    }

    step <- step + 1
    if (!improved) break
  }

  metrics_tbl <- bind_rows(model_metrics)
  best_row <- which.min(metrics_tbl$AIC)

  return(list(
    results_table = metrics_tbl,
    best_model = all_models[[best_row]],
    all_models = all_models
  ))
}
