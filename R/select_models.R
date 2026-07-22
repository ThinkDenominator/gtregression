#' Stepwise Model Selection with Evaluation Metrics
#'
#' Performs stepwise model selection using forward, backward, or both directions
#' across different regression approaches. The function returns a summary table
#' with evaluation metrics and the best model based on the lowest AIC.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome A single character string indicating the outcome variable.
#'   Quoted and bare names are accepted.
#' @param exposures Character vector of predictor variables to consider. Quoted
#'   names are recommended in scripts, and bare names are also accepted.
#' @param approach Regression method. One of:
#'   \code{"logit"}, \code{"logbinomial"}, \code{"poisson"},
#'   \code{"robpoisson"}, \code{"negbin"}, or \code{"linear"}.
#' @param direction Stepwise selection direction. One of:
#'   \code{"forward"} (default), \code{"backward"}, or \code{"both"}.
#' @param format Output format for the viewing table. One of
#'   \code{"flextable"} (default), \code{"gt"}, or \code{"tibble"}. Use
#'   \code{format = "tibble"} to keep only the original list structure.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item \code{results_table}: A tibble summarising each accepted step's
#'   model metrics (AIC, BIC, deviance, log-likelihood, and adjusted R-squared
#'   for linear models).
#'   \item \code{best_model}: The best-fitting model object based on lowest AIC.
#'   \item \code{all_models}: A named list of the accepted stepwise models.
#'   \item \code{direction}: Stepwise selection direction used.
#'   \item \code{table}: A formatted \code{gt_tbl} or \code{flextable} when
#'   \code{format} is \code{"gt"} or \code{"flextable"}.
#' }
#'
#' @importFrom stats AIC BIC anova as.formula binomial coef cooks.distance
#' @importFrom stats deviance glm glm.control lm logLik na.omit nobs poisson
#' @importFrom stats predict reformulate residuals shapiro.test
#' @importFrom MASS glm.nb
#' @importFrom utils data
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @examples
#' data <- data_birthwt
#' stepwise <- select_models(
#'   data = data,
#'   outcome = "bwt",
#'   exposures = c("age", "lwt", "smoke"),
#'   approach = "linear",
#'   direction = "forward"
#' )
#' stepwise$results_table
#' stepwise$best_model
#'
#' @export
select_models <- function(data, outcome, exposures, approach = "logit",
                          direction = "forward",
                          format = c("flextable", "gt", "tibble")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !nzchar(outcome)) {
    stop("`outcome` must be a single character variable name.", call. = FALSE)
  }
  if (!is.character(exposures) || length(exposures) < 1L ||
      anyNA(exposures) || any(!nzchar(exposures))) {
    stop("`exposures` must be a non-empty character vector.", call. = FALSE)
  }
  missing_vars <- setdiff(c(outcome, exposures), names(data))
  if (length(missing_vars)) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }

  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit", "logbinomial", "poisson", "robpoisson", "negbin", "linear")
  )
  approach <- .normalize_approach(approach)
  .validate_approach(approach, context = "select_models")
  direction <- .choice_arg(
    substitute(direction),
    env = parent.frame(),
    choices = c("forward", "backward", "both")
  )
  if (!is.character(direction) || length(direction) != 1L ||
      is.na(direction) || !direction %in% c("forward", "backward", "both")) {
    stop(
      "`direction` must be one of: forward, backward, both.",
      call. = FALSE
    )
  }
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  format <- match.arg(format, c("flextable", "gt", "tibble"))

  .validate_outcome_by_approach(data[[outcome]], approach)
  model_data <- data
  if (approach == "robpoisson" && is.factor(model_data[[outcome]])) {
    model_data[[outcome]] <- as.integer(model_data[[outcome]]) - 1L
  }

  fit_model <- function(vars) {
    fmla <- stats::reformulate(vars, response = outcome)
    fmla_str <- paste(deparse(fmla), collapse = "")

    model <- if (approach == "negbin") {
      MASS::glm.nb(fmla, data = model_data)
    } else if (approach == "linear") {
      stats::lm(fmla, data = model_data)
    } else {
      family <- switch(
        approach,
        "logit" = stats::binomial(link = "logit"),
        "logbinomial" = stats::binomial(link = "log"),
        "poisson" = stats::poisson(link = "log"),
        "robpoisson" = stats::poisson(link = "log")
      )
      stats::glm(fmla, family = family, data = model_data)
    }

    attr(model, "formula_str") <- fmla_str
    attr(model, "selected_vars") <- vars
    model
  }

  model_metric <- function(model, model_id, vars) {
    out <- tibble::tibble(
      model_id = model_id,
      formula = attr(model, "formula_str"),
      n_predictors = length(vars),
      AIC = stats::AIC(model),
      BIC = stats::BIC(model),
      logLik = as.numeric(stats::logLik(model)),
      deviance = stats::deviance(model)
    )

    if (approach == "linear") {
      dplyr::mutate(out, adj_r2 = summary(model)$adj.r.squared)
    } else {
      dplyr::mutate(out, selected_vars = paste(vars, collapse = " + "))
    }
  }

  selected_vars <- if (direction == "backward") exposures else character(0)
  all_models <- list()
  model_metrics <- list()
  selected_vars_by_step <- list()
  step <- 1L

  repeat {
    current_vars <- selected_vars
    current_model <- fit_model(current_vars)
    model_name <- paste0("model_", step)
    all_models[[model_name]] <- current_model
    selected_vars_by_step[[model_name]] <- current_vars
    model_metrics[[model_name]] <- model_metric(current_model, step, current_vars)
    current_aic <- stats::AIC(current_model)

    add_candidates <- setdiff(exposures, current_vars)
    forward_models <- lapply(add_candidates, function(var) {
      fit_model(c(current_vars, var))
    })
    forward_aics <- vapply(forward_models, stats::AIC, FUN.VALUE = numeric(1))
    best_forward <- if (length(forward_aics)) min(forward_aics) else Inf
    best_forward_idx <- if (length(forward_aics)) which.min(forward_aics) else NA_integer_

    drop_candidates <- if (length(current_vars) > 1L) {
      lapply(current_vars, function(var) setdiff(current_vars, var))
    } else {
      list()
    }
    backward_models <- lapply(drop_candidates, fit_model)
    backward_aics <- vapply(backward_models, stats::AIC, FUN.VALUE = numeric(1))
    best_backward <- if (length(backward_aics)) min(backward_aics) else Inf
    best_backward_idx <- if (length(backward_aics)) which.min(backward_aics) else NA_integer_

    improved <- FALSE

    if (direction == "forward" && best_forward < current_aic - 1e-5) {
      selected_vars <- c(current_vars, add_candidates[[best_forward_idx]])
      improved <- TRUE
    } else if (direction == "backward" && best_backward < current_aic - 1e-5) {
      selected_vars <- drop_candidates[[best_backward_idx]]
      improved <- TRUE
    } else if (direction == "both") {
      if (best_forward < best_backward && best_forward < current_aic - 1e-5) {
        selected_vars <- c(current_vars, add_candidates[[best_forward_idx]])
        improved <- TRUE
      } else if (best_backward < current_aic - 1e-5) {
        selected_vars <- drop_candidates[[best_backward_idx]]
        improved <- TRUE
      }
    }

    step <- step + 1L
    if (!improved) break
  }

  metrics_tbl <- dplyr::bind_rows(model_metrics)
  best_row <- which.min(metrics_tbl$AIC)
  best_name <- names(model_metrics)[[best_row]]
  final_best_model <- all_models[[best_name]]
  attr(final_best_model, "selected_vars") <- selected_vars_by_step[[best_name]]

  out <- list(
    results_table = metrics_tbl,
    best_model = final_best_model,
    all_models = all_models,
    direction = direction
  )

  if (format != "tibble") {
    out$table <- .build_select_models_table(
      metrics_tbl,
      direction = direction,
      format = format
    )
  }

  out
}

#' Build formatted select_models table
#' @keywords internal
#' @noRd
.build_select_models_table <- function(metrics_tbl,
                                       direction = c("forward", "backward", "both"),
                                       format = c("flextable", "gt")) {
  direction <- match.arg(direction, c("forward", "backward", "both"))
  format <- match.arg(format, c("flextable", "gt"))
  direction_note <- paste0("Selection direction: ", direction, ".")
  caveat_note <- paste(
    "Screening aid only; compare candidate models with study design,",
    "clinical or subject-matter judgement, and model diagnostics."
  )

  display <- metrics_tbl |>
    dplyr::mutate(
      AIC = round(.data$AIC, 2),
      BIC = round(.data$BIC, 2),
      logLik = round(.data$logLik, 2),
      deviance = round(.data$deviance, 2),
      best = .data$AIC == min(.data$AIC, na.rm = TRUE)
    )

  if ("adj_r2" %in% names(display)) {
    display <- dplyr::mutate(display, adj_r2 = round(.data$adj_r2, 3))
  }

  if (format == "gt") {
    labels <- c(
      model_id = "Model",
      formula = "Formula",
      n_predictors = "Predictors",
      logLik = "Log-likelihood",
      selected_vars = "Selected variables",
      adj_r2 = "Adjusted R-squared",
      best = "Best AIC"
    )
    labels <- labels[names(labels) %in% names(display)]
    left_cols <- intersect(c("formula", "selected_vars"), names(display))

    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Stepwise model selection")
    tbl <- do.call(gt::cols_label, c(list(.data = tbl), as.list(labels)))
    tbl <- tbl |>
      gt::cols_align(align = "left", columns = dplyr::all_of(left_cols)) |>
      gt::cols_align(
        align = "center",
        columns = dplyr::all_of(setdiff(names(display), left_cols))
      ) |>
      gt::fmt(
        columns = "best",
        fns = function(x) ifelse(x, "Yes", "No")
      ) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#e7f5ec"),
        locations = gt::cells_body(rows = .data$best)
      ) |>
      gt::tab_source_note(gt::md(direction_note)) |>
      gt::tab_source_note(gt::md(caveat_note))

    return(tbl)
  }

  display$best <- ifelse(display$best, "Yes", "No")
  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Stepwise model selection")
  left_cols <- intersect(c("formula", "selected_vars"), names(display))
  ft <- flextable::align(ft, j = left_cols, align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = setdiff(names(display), c("formula", "selected_vars")),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::bg(ft, i = which(display$best == "Yes"), bg = "#e7f5ec", part = "body")
  ft <- flextable::add_footer_lines(ft, values = c(direction_note, caveat_note))
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
