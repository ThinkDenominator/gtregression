#' Multinomial logistic regression
#'
#' Fit crude, exposure-specific adjusted, or joint multivariable multinomial
#' logistic models for an unordered outcome with three or more categories.
#'
#' @param data A data frame containing the outcome and predictors.
#' @param outcome An unordered factor outcome with at least three levels. The
#'   first level is the reference outcome category.
#' @param exposures Predictor variables to report.
#' @param adjust_for Optional covariates included but not reported.
#' @param multivariable Logical; fit all exposures in one model.
#' @param multivariate Optional alias for `multivariable`.
#' @param model_stats Logical; return model-fit and sample-size statistics.
#' @param format Output table format: `"flextable"` or `"gt"`.
#' @param theme Table styling preset.
#' @param show_ref Logical; display categorical predictor reference rows.
#'
#' @return A `gtregression` object containing a side-by-side table of outcome
#'   comparisons, fitted models, statistics, and one standard regression object
#'   per comparison in `$per_outcome`.
#'
#' @details
#' Each odds ratio compares one non-reference outcome category with the first
#' outcome level. Outcome categories are nominal: their order has no numeric or
#'   severity interpretation. Use `relevel()` before fitting to choose another
#' reference category.
#'
#' @examples
#' set.seed(1501)
#' example_data <- data.frame(
#'   destination = factor(sample(c("Home", "Rehabilitation", "Hospital"), 120,
#'                               replace = TRUE),
#'                        levels = c("Home", "Rehabilitation", "Hospital")),
#'   treatment = factor(sample(c("Control", "Intervention"), 120, TRUE)),
#'   age = rnorm(120, 65, 10)
#' )
#' multinomial_reg(
#'   example_data,
#'   outcome = destination,
#'   exposures = treatment,
#'   adjust_for = age
#' )
#'
#' @export
multinomial_reg <- function(data,
                            outcome,
                            exposures,
                            adjust_for = NULL,
                            multivariable = FALSE,
                            multivariate = NULL,
                            model_stats = FALSE,
                            format = c("flextable", "gt"),
                            theme = "minimal",
                            show_ref = TRUE) {
  outcome <- .single_var_arg(substitute(outcome), data, parent.frame())
  exposures <- .vars_arg(substitute(exposures), parent.frame(),
                         data_names = names(data))
  adjust_for <- .vars_arg(
    substitute(adjust_for), parent.frame(), allow_null = TRUE,
    data_names = names(data)
  )
  format <- .choice_arg(
    substitute(format), parent.frame(), choices = c("flextable", "gt")
  )
  # Store one validated format (not the default vector c("flextable", "gt")).
  if (!is.character(format) || !length(format) || anyNA(format) ||
      !all(format %in% c("flextable", "gt"))) {
    stop("`format` must be one of: flextable, gt.", call. = FALSE)
  }
  format <- match.arg(format, c("flextable", "gt"))
  theme <- .choice_arg(substitute(theme), parent.frame())
  .validate_theme_arg(theme)
  multivariable <- .ordinal_multivariable_arg(multivariable, multivariate)
  if (!.ordinal_scalar_logical(model_stats) ||
      !.ordinal_scalar_logical(show_ref)) {
    stop("`model_stats` and `show_ref` must be TRUE or FALSE.", call. = FALSE)
  }
  .validate_multinomial_inputs(data, outcome, exposures, adjust_for)

  adjusted <- length(adjust_for) > 0L || isTRUE(multivariable)
  model_terms <- if (isTRUE(multivariable)) {
    list(multivariable_model = exposures)
  } else {
    stats::setNames(as.list(exposures), exposures)
  }
  fits <- tidy_parts <- stats_parts <- list()
  outcome_levels <- levels(droplevels(data[[outcome]]))
  comparison_levels <- outcome_levels[-1L]

  for (model_name in names(model_terms)) {
    reported <- model_terms[[model_name]]
    predictors <- unique(c(reported, adjust_for))
    clean <- .prepare_multinomial_data(data, outcome, predictors)
    if (!identical(levels(clean[[outcome]]), outcome_levels)) {
      lost <- setdiff(outcome_levels, levels(clean[[outcome]]))
      stop(
        "Every complete-case model must retain all observed outcome categories. ",
        "For ",
        if (identical(model_name, "multivariable_model")) {
          "the multivariable model"
        } else {
          paste0("exposure `", model_name, "`")
        },
        ", the outcome level(s) ", paste0("'", lost, "'", collapse = ", "),
        " have no complete cases (missing values in: ",
        paste(predictors, collapse = ", "), ").",
        call. = FALSE
      )
    }
    fit <- .fit_multinomial_model(clean, outcome, predictors)
    fits[[model_name]] <- fit
    stats_parts[[model_name]] <- .multinomial_model_stats(
      fit, clean, outcome, model_name
    )
    tidy_parts[[model_name]] <- lapply(comparison_levels, function(category) {
      do.call(rbind, lapply(reported, function(exposure) {
        .tidy_multinomial_fixed(fit, clean, exposure, category)
      }))
    })
    names(tidy_parts[[model_name]]) <- comparison_levels
  }

  variable_labels <- .var_label_map(data, unique(c(exposures, adjust_for)))
  per_outcome <- stats::setNames(vector("list", length(comparison_levels)),
                                 comparison_levels)
  for (category in comparison_levels) {
    body <- do.call(rbind, lapply(tidy_parts, `[[`, category))
    rownames(body) <- NULL
    per_outcome[[category]] <- .multinomial_component(
      body = body,
      data = data,
      outcome = outcome,
      category = category,
      reference = outcome_levels[1L],
      exposures = exposures,
      adjust_for = adjust_for,
      variable_labels = variable_labels,
      adjusted = adjusted,
      multivariable = multivariable,
      models = fits,
      show_ref = show_ref,
      format = format,
      theme = theme
    )
  }

  spanners <- paste0(comparison_levels, " vs ", outcome_levels[1L])
  merged <- do.call(
    merge_tables,
    c(per_outcome, list(spanners = spanners, format = format, theme = theme))
  )
  combined_body <- do.call(rbind, lapply(names(per_outcome), function(category) {
    body <- per_outcome[[category]]$table_body
    body$outcome_level <- category
    body$outcome_reference <- outcome_levels[1L]
    body
  }))
  rownames(combined_body) <- NULL

  result <- list(
    table = merged$table,
    table_body = combined_body,
    table_display = merged$table_display,
    models = fits,
    model_summaries = lapply(fits, summary),
    model_stats = if (isTRUE(model_stats)) do.call(rbind, stats_parts) else NULL,
    per_outcome = per_outcome,
    variable_labels = variable_labels,
    footnotes = merged$footnotes,
    # Header structure of the combined table, stored so that merge_tables()
    # and modify_table() do not depend on reading it back from the rendered
    # table.
    column_labels = merged$column_labels,
    spanners = merged$spanners,
    theme = merged$theme,
    outcome = outcome,
    outcome_levels = outcome_levels,
    outcome_reference = outcome_levels[1L],
    exposures = exposures,
    adjust_for = adjust_for,
    adjusted_mode = adjusted,
    multivariable = isTRUE(multivariable),
    approach = "multinomial_logit",
    format = format,
    source = "multinomial_reg",
    show_ref = isTRUE(show_ref)
  )
  class(result) <- c("gtregression", "multinomial_reg", class(result))
  result
}

#' @keywords internal
#' @noRd
.validate_multinomial_inputs <- function(data, outcome, exposures, adjust_for) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  variables <- unique(c(outcome, exposures, adjust_for))
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop("Variables not found in `data`: ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }
  if (!length(exposures)) {
    stop("`exposures` must contain at least one variable.", call. = FALSE)
  }
  if (outcome %in% c(exposures, adjust_for) ||
      length(intersect(exposures, adjust_for))) {
    stop("Outcome, exposures, and adjustment variables must not overlap.",
         call. = FALSE)
  }
  if (!is.factor(data[[outcome]]) || is.ordered(data[[outcome]])) {
    stop("`outcome` must be an unordered factor.", call. = FALSE)
  }
  if (nlevels(droplevels(data[[outcome]])) < 3L) {
    stop("Multinomial regression requires at least three observed outcome levels.",
         call. = FALSE)
  }
  .validate_exposures(data, unique(c(exposures, adjust_for)))
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.prepare_multinomial_data <- function(data, outcome, predictors) {
  variables <- unique(c(outcome, predictors))
  clean <- data[stats::complete.cases(data[, variables, drop = FALSE]), , drop = FALSE]
  if (!nrow(clean)) stop("No complete observations are available.", call. = FALSE)
  clean[[outcome]] <- droplevels(clean[[outcome]])
  if (nlevels(clean[[outcome]]) < 3L) {
    all_levels <- levels(droplevels(factor(data[[outcome]])))
    lost <- setdiff(all_levels, levels(clean[[outcome]]))
    stop(
      "Complete cases must contain at least three outcome levels.",
      if (length(lost)) {
        paste0(
          " With predictor(s) ", paste(predictors, collapse = ", "),
          ", the outcome level(s) ", paste0("'", lost, "'", collapse = ", "),
          " have no complete cases."
        )
      },
      call. = FALSE
    )
  }
  for (variable in predictors) {
    if (is.character(clean[[variable]])) clean[[variable]] <- factor(clean[[variable]])
    if (is.ordered(clean[[variable]])) {
      clean[[variable]] <- factor(
        clean[[variable]], levels = levels(clean[[variable]]), ordered = FALSE
      )
    }
    if (is.factor(clean[[variable]])) clean[[variable]] <- droplevels(clean[[variable]])
  }
  clean
}

#' @keywords internal
#' @noRd
.fit_multinomial_model <- function(data, outcome, predictors) {
  formula <- stats::reformulate(predictors, response = outcome)
  nnet::multinom(formula, data = data, Hess = TRUE, trace = FALSE, model = TRUE)
}

#' @keywords internal
#' @noRd
.tidy_multinomial_fixed <- function(fit, data, exposure, category) {
  summary_fit <- summary(fit)
  coefficients <- summary_fit$coefficients
  standard_errors <- summary_fit$standard.errors
  if (is.null(dim(coefficients))) {
    coefficients <- matrix(coefficients, nrow = 1L,
                           dimnames = list(category, names(coefficients)))
    standard_errors <- matrix(standard_errors, nrow = 1L,
                              dimnames = dimnames(coefficients))
  }
  fixed_matrix <- stats::model.matrix(stats::formula(fit), data = data)
  assignment <- attr(fixed_matrix, "assign")
  names(assignment) <- colnames(fixed_matrix)
  term_labels <- gsub(
    "`", "", attr(stats::terms(fit), "term.labels"), fixed = TRUE
  )
  term_index <- match(exposure, term_labels)
  selected_names <- intersect(
    names(assignment)[assignment == term_index], colnames(coefficients)
  )
  if (!length(selected_names)) {
    stop("No estimable coefficient for exposure '", exposure, "'.", call. = FALSE)
  }
  estimate <- coefficients[category, selected_names, drop = TRUE]
  se <- standard_errors[category, selected_names, drop = TRUE]
  z <- stats::qnorm(0.975)
  x <- data[[exposure]]
  nonref <- data.frame(
    exposure = exposure,
    level = if (is.factor(x)) levels(x)[-1L] else exposure,
    estimate = exp(estimate),
    conf.low = exp(estimate - z * se),
    conf.high = exp(estimate + z * se),
    p.value = 2 * stats::pnorm(abs(estimate / se), lower.tail = FALSE),
    ref = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.factor(x)) return(nonref)
  reference <- data.frame(
    exposure = exposure, level = levels(x)[1L], estimate = 1,
    conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_,
    ref = TRUE, stringsAsFactors = FALSE
  )
  rbind(reference, nonref)
}

#' @keywords internal
#' @noRd
.multinomial_model_stats <- function(fit, data, outcome, model_name) {
  data.frame(
    model = model_name,
    AIC = stats::AIC(fit),
    BIC = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)),
    n = .model_observation_count(fit),
    outcome_levels = nlevels(data[[outcome]]),
    convergence = fit$convergence,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.multinomial_component <- function(body, data, outcome, category, reference,
                                   exposures, adjust_for, variable_labels,
                                   adjusted, multivariable, models, show_ref,
                                   format, theme) {
  effect_label <- if (adjusted) "Adjusted OR (95% CI)" else "OR (95% CI)"
  display <- .make_display_multi(
    body, data, outcome, effect_label, variable_labels, show_ref
  )
  display <- .add_model_n_column(display, models)
  footnotes <- c(
    "OR = Odds Ratio; CI = Confidence Interval.",
    if (.has_visible_reference(body, show_ref)) .ref_note() else NULL,
    paste0("Outcome comparison: ", category, " vs ", reference, "."),
    if (length(adjust_for)) {
      .display_adjustment_note(adjust_for, variable_labels)
    } else NULL,
    if (isTRUE(multivariable) && length(exposures) > 1L) {
      "Adjusted for the other reported variables in the model."
    } else NULL,
    .model_n_footnote(models)
  )
  format <- match.arg(format, c("flextable", "gt"))
  theme <- .resolve_theme(theme)
  table <- if (format == "gt") {
    .build_gt_multi(display, effect_label, footnotes, theme)
  } else {
    .build_flextable_multi(display, effect_label, footnotes, theme)
  }
  out <- list(
    table = table, table_body = body, table_display = display, models = models,
    variable_labels = variable_labels, footnotes = footnotes,
    outcome = outcome, outcome_level = category,
    outcome_reference = reference, exposures = exposures,
    adjust_for = adjust_for, adjusted_mode = adjusted,
    multivariable = isTRUE(multivariable), approach = "multinomial_logit",
    format = format, source = "multinomial_outcome", show_ref = show_ref
  )
  class(out) <- c("gtregression", "multinomial_outcome", class(out))
  out
}
