#' Stepwise model selection with fit metrics
#'
#' Compare candidate models added or removed stepwise, then return the accepted
#' steps, model-fit metrics, and the lowest-AIC model.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome A single character string indicating the outcome variable.
#'   Quoted and bare names are accepted. Not used for survival approaches when
#'   \code{time} and \code{event} are supplied.
#' @param exposures Character vector of predictor variables to consider. Quoted
#'   names are recommended in scripts, and bare names are also accepted.
#' @param approach Regression method. One of:
#'   \code{"logit"}, \code{"logbinomial"}, \code{"poisson"},
#'   \code{"robpoisson"}, \code{"negbin"}, \code{"linear"}, \code{"cox"}, or
#'   \code{"survreg"}.
#' @param time,event Survival time and event indicator for \code{approach = "cox"}
#'   or \code{approach = "survreg"}. Quoted and bare names are accepted.
#' @param distribution Parametric survival distribution for
#'   \code{approach = "survreg"}. One of \code{"weibull"},
#'   \code{"exponential"}, \code{"lognormal"}, or \code{"loglogistic"}.
#'   Common spellings such as \code{"log-normal"} and \code{"log-logistic"} are
#'   also accepted.
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
#'   \item \code{n_used}, \code{n_excluded}: number of complete cases used for
#'   every candidate model and number of rows excluded because of missing
#'   values in the outcome or any candidate variable.
#'   \item \code{all_models}: A named list of the accepted stepwise models.
#'   \item \code{direction}: Stepwise selection direction used.
#'   \item \code{table}: A formatted \code{gt_tbl} or \code{flextable} when
#'   \code{format} is \code{"gt"} or \code{"flextable"}.
#' }
#'
#' @details
#' Treat stepwise selection as a screening tool. It is best used alongside the
#' study question, clinical judgement, and model diagnostics rather than as an
#' automatic final-model rule.
#'
#' Information criteria are only comparable between models fitted to the same
#' observations. \code{select_models()} therefore restricts the data to
#' complete cases across the outcome (or \code{time} and \code{event}) and
#' all candidate variables before selection, reports the N used and the number
#' of rows excluded in a message and in the table note, and stores them as
#' \code{n_used} and \code{n_excluded}. Backward (and stepwise) selection can
#' remove the last variable and so reach the intercept-only model.
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
                          time = NULL,
                          event = NULL,
                          distribution = "weibull",
                          direction = "forward",
                          format = c("flextable", "gt", "tibble")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  exposures <- .vars_arg(
    substitute(exposures), env = parent.frame(), data_names = names(data)
  )
  if (!is.character(exposures) || length(exposures) < 1L ||
      anyNA(exposures) || any(!nzchar(exposures))) {
    stop("`exposures` must be a non-empty character vector.", call. = FALSE)
  }

  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit", "logbinomial", "poisson", "robpoisson",
                "negbin", "linear", "cox", "survreg")
  )
  approach <- .normalize_approach(approach)
  .validate_approach(approach, context = "select_models")
  is_survival <- approach %in% c("cox", "survreg")

  if (is_survival) {
    time <- .single_var_arg(substitute(time), data = data, env = parent.frame())
    event <- .single_var_arg(substitute(event), data = data, env = parent.frame())
    outcome <- NULL
  } else {
    if (missing(outcome)) {
      stop("`outcome` must be supplied for non-survival model selection.", call. = FALSE)
    }
    outcome <- .vars_arg(
      substitute(outcome), env = parent.frame(), data_names = names(data)
    )
    if (!is.character(outcome) || length(outcome) != 1L ||
        is.na(outcome) || !nzchar(outcome)) {
      stop("`outcome` must be a single character variable name.", call. = FALSE)
    }
  }

  distribution <- if (approach == "survreg") {
    .surv_distribution_arg(
      substitute(distribution),
      env = parent.frame(),
      multiple = FALSE,
      arg = "distribution"
    )
  } else {
    .choice_arg(substitute(distribution), env = parent.frame())
  }

  missing_vars <- if (is_survival) {
    setdiff(c(time, event, exposures), names(data))
  } else {
    setdiff(c(outcome, exposures), names(data))
  }
  if (length(missing_vars)) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }
  outcome_roles <- if (is_survival) c(time, event) else outcome
  role_clash <- intersect(exposures, outcome_roles)
  if (length(role_clash)) {
    stop(
      "The outcome variable cannot also be a candidate exposure: ",
      paste(role_clash, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(exposures)) {
    stop(
      "Duplicate exposure(s): ", paste(unique(exposures[duplicated(exposures)]), collapse = ", "),
      ". List each exposure once.",
      call. = FALSE
    )
  }
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

  if (is_survival) {
    model_data <- .validate_cox_inputs(data, time, event, exposures, adjust_for = NULL)
  } else {
    .validate_outcome_by_approach(data[[outcome]], approach)
    model_data <- data
  }
  if (approach == "robpoisson" && is.factor(model_data[[outcome]])) {
    model_data[[outcome]] <- as.integer(model_data[[outcome]]) - 1L
  }

  # All candidate models must be fitted to ONE sample, otherwise AIC/BIC are
  # not comparable: restrict to complete cases across the outcome (or time and
  # event) and every candidate variable before selection starts.
  n_input <- nrow(data)
  selection_vars <- if (is_survival) {
    unique(c(time, event, exposures))
  } else {
    unique(c(outcome, exposures))
  }
  complete_rows <- stats::complete.cases(
    model_data[, selection_vars, drop = FALSE]
  )
  model_data <- model_data[complete_rows, , drop = FALSE]
  n_used <- nrow(model_data)
  n_excluded <- n_input - n_used
  if (n_used == 0L) {
    stop(
      "No complete cases across the outcome and all candidate variables.",
      call. = FALSE
    )
  }
  message(
    "select_models(): all candidate models are fitted to the same N = ",
    n_used, " complete cases",
    if (n_excluded > 0L) {
      paste0(
        " (", n_excluded, " of ", n_input,
        " rows excluded because of missing values in the outcome or a ",
        "candidate variable)"
      )
    } else {
      ""
    },
    "."
  )

  fit_model <- function(vars) {
    if (is_survival) {
      bt <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
      rhs <- if (length(vars)) paste(bt(vars), collapse = " + ") else "1"
      fmla <- stats::as.formula(
        paste0("survival::Surv(", bt(time), ", ", bt(event), ") ~ ", rhs)
      )
    } else {
      fmla <- if (length(vars)) {
        stats::reformulate(vars, response = outcome)
      } else {
        stats::as.formula(paste0("`", outcome, "` ~ 1"))
      }
    }
    fmla_str <- paste(deparse(fmla), collapse = "")

    model <- if (approach == "cox") {
      survival::coxph(fmla, data = model_data, model = TRUE)
    } else if (approach == "survreg") {
      fit <- survival::survreg(fmla, data = model_data, dist = distribution, model = TRUE)
      attr(fit, "gtregression_events") <- sum(model_data[[event]] == 1, na.rm = TRUE)
      attr(fit, "gtregression_distribution") <- distribution
      fit
    } else if (approach == "negbin") {
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
      model_terms = .gtregression_terms_label(vars),
      n_predictors = length(vars),
      AIC = stats::AIC(model),
      BIC = stats::BIC(model),
      logLik = as.numeric(stats::logLik(model)),
      deviance = .safe_numeric(stats::deviance(model))
    )

    if (approach == "linear") {
      dplyr::mutate(out, adj_r2 = summary(model)$adj.r.squared)
    } else if (approach == "cox") {
      smry <- tryCatch(summary(model), error = function(e) NULL)
      concordance <- if (!is.null(smry) && !is.null(smry$concordance)) {
        suppressWarnings(as.numeric(smry$concordance[1]))
      } else {
        NA_real_
      }
      dplyr::mutate(
        out,
        concordance = concordance,
        events = .safe_numeric(model$nevent),
        selected_vars = paste(vars, collapse = " + ")
      )
    } else if (approach == "survreg") {
      dplyr::mutate(
        out,
        distribution = distribution,
        scale = .safe_numeric(model$scale),
        events = .safe_numeric(attr(model, "gtregression_events", exact = TRUE)),
        selected_vars = paste(vars, collapse = " + ")
      )
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

    # Dropping the last variable (the intercept-only model) is a candidate
    # too, as in stats::step().
    drop_candidates <- if (length(current_vars) >= 1L) {
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
    direction = direction,
    n_used = n_used,
    n_excluded = n_excluded
  )

  if (format != "tibble") {
    out$table <- .build_select_models_table(
      metrics_tbl,
      direction = direction,
      format = format,
      n_used = n_used,
      n_excluded = n_excluded
    )
  }

  out
}

#' Build formatted select_models table
#' @keywords internal
#' @noRd
.build_select_models_table <- function(metrics_tbl,
                                       direction = c("forward", "backward", "both"),
                                       format = c("flextable", "gt"),
                                       n_used = NULL,
                                       n_excluded = NULL) {
  direction <- match.arg(direction, c("forward", "backward", "both"))
  format <- match.arg(format, c("flextable", "gt"))
  direction_note <- paste0("Selection direction: ", direction, ".")
  if (!is.null(n_used)) {
    direction_note <- paste0(
      direction_note,
      " All models fitted to the same N = ", n_used, " complete cases",
      if (!is.null(n_excluded) && n_excluded > 0L) {
        paste0(" (", n_excluded, " rows with missing values excluded)")
      } else {
        ""
      },
      "."
    )
  }
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
  display <- display[, setdiff(names(display), c("formula", "selected_vars")), drop = FALSE]

  if ("adj_r2" %in% names(display)) {
    display <- dplyr::mutate(display, adj_r2 = round(.data$adj_r2, 3))
  }

  if (format == "gt") {
    labels <- c(
      model_id = "Model",
      model_terms = "Selected variables",
      n_predictors = "Predictors",
      logLik = "Log-likelihood",
      adj_r2 = "Adjusted R-squared",
      best = "Best AIC"
    )
    labels <- labels[names(labels) %in% names(display)]
    left_cols <- intersect("model_terms", names(display))

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
      gt::tab_source_note(gt::md(caveat_note)) |>
      .compact_gt_source_notes()

    return(tbl)
  }

  display$best <- ifelse(display$best, "Yes", "No")
  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Stepwise model selection")
  left_cols <- intersect("model_terms", names(display))
  ft <- flextable::align(ft, j = left_cols, align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = setdiff(names(display), "model_terms"),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::bg(ft, i = which(display$best == "Yes"), bg = "#e7f5ec", part = "body")
  ft <- flextable::add_footer_lines(ft, values = c(direction_note, caveat_note))
  ft <- .compact_flex_footer(ft)
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
