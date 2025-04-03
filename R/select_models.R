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
#' @param direction Direction of stepwise selection: "forward", "backward", or "both".
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

  # Outcome validation
  outcome_vec <- data[[outcome]]
  is_binary <- function(x) is.factor(x) && length(levels(x)) == 2 || is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)
  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) && length(unique(x[!is.na(x)])) > 2
  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")) {
    if (!is_binary(outcome_vec)) stop("This approach requires a binary outcome.")
  }
  if (approach == "poisson") {
    if (is_binary(outcome_vec)) stop("Poisson regression is not appropriate for binary outcomes.")
    if (!is_count(outcome_vec)) stop("Poisson requires a count outcome.")
  }
  if (approach == "negbin") {
    if (!is_count(outcome_vec)) stop("Negative binomial requires a count outcome.")
  }
  if (approach == "linear") {
    if (!is_continuous(outcome_vec)) stop("Linear regression requires a continuous outcome.")
  }

  # Define model family
  family <- switch(
    approach,
    "logit" = binomial(link = "logit"),
    "log-binomial" = binomial(link = "log"),
    "poisson" = poisson(link = "log"),
    "robpoisson" = poisson(link = "log"),
    "linear" = gaussian(),
    "negbin" = NULL,
    stop("Unsupported approach")
  )

  # Setup
  all_models <- list()
  model_metrics <- list()
  selected_vars <- c()
  remaining_vars <- exposures
  step <- 1

  generate_model <- function(vars) {
    fmla <- as.formula(paste(outcome, "~", paste(vars, collapse = " + ")))
    if (approach == "negbin") {
      MASS::glm.nb(fmla, data = data)
    } else {
      if (approach == "linear") {
        lm(fmla, data = data)
      } else {
        glm(fmla, data = data, family = family)
      }

    }
  }

  repeat {
    candidates <- lapply(remaining_vars, function(var) c(selected_vars, var))
    models <- lapply(candidates, generate_model)

    aic_vals <- sapply(models, AIC)
    best_idx <- which.min(aic_vals)
    selected_vars <- candidates[[best_idx]]
    remaining_vars <- setdiff(remaining_vars, selected_vars)

    best_model <- models[[best_idx]]
    all_models[[step]] <- best_model

    if (approach == "linear") {
      adj_r2_val <- summary(best_model)$adj.r.squared
      model_metrics[[step]] <- tibble(
        model_id = step,
        formula = paste(outcome, "~", paste(selected_vars, collapse = " + ")),
        n_predictors = length(selected_vars),
        AIC = AIC(best_model),
        BIC = BIC(best_model),
        logLik = as.numeric(logLik(best_model)),
        deviance = deviance(best_model),
        adj_r2 = adj_r2_val
      )
    } else {
      model_metrics[[step]] <- tibble(
        model_id = step,
        formula = paste(outcome, "~", paste(selected_vars, collapse = " + ")),
        n_predictors = length(selected_vars),
        AIC = AIC(best_model),
        BIC = BIC(best_model),
        logLik = as.numeric(logLik(best_model)),
        deviance = deviance(best_model)
        # No adj_r2 here
      )
    }

    step <- step + 1
    if (length(remaining_vars) == 0) break
  }




  metrics_tbl <- bind_rows(model_metrics)
  best_row <- which.min(metrics_tbl$AIC)

  list(
    results_table = metrics_tbl,
    best_model = all_models[[best_row]],
    all_models = all_models
  )
}
