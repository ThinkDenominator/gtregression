#' Random-intercept multilevel regression
#'
#' Fit a linear or logistic mixed-effects model with one grouping variable and
#' return publication-ready fixed-effect estimates.
#'
#' @param data A data frame containing the outcome, predictors, and group.
#' @param outcome Continuous outcome for `approach = "linear"` or binary outcome
#'   for `approach = "logit"`.
#' @param exposures Predictor variables to report.
#' @param group Grouping variable for the random intercept.
#' @param adjust_for Optional covariates included in the model but not reported.
#' @param approach `"linear"` or `"logit"`.
#' @param format Output table format: `"flextable"` or `"gt"`.
#' @param theme Table styling preset.
#' @param model_stats Logical; return model fit, clustering, random-effect, and
#'   ICC statistics.
#' @param show_ref Logical; display categorical reference rows.
#'
#' @return A `gtregression` object containing the publication table, numeric
#'   estimates, fitted mixed model, model statistics, and analysis metadata.
#'
#' @details
#' This intentionally focused workflow supports one random intercept. Random
#' slopes, crossed effects, and nested random-effects formulas are outside this
#' function's scope and should be fitted directly with specialist software.
#'
#' For linear models, the ICC is the random-intercept variance divided by total
#' random-intercept plus residual variance. For logistic models, a latent-scale
#' ICC is calculated using residual variance pi-squared over three.
#'
#' @examples
#' set.seed(71)
#' multilevel_data <- data.frame(
#'   clinic = factor(rep(seq_len(20), each = 12)),
#'   treatment = factor(rep(c("Control", "Intervention"), 120)),
#'   age = rnorm(240, 58, 9)
#' )
#' clinic_effect <- rnorm(20, 0, 3)
#' multilevel_data$score <- 50 + 2 * (multilevel_data$treatment == "Intervention") +
#'   0.2 * multilevel_data$age + clinic_effect[multilevel_data$clinic] + rnorm(240)
#'
#' multilevel_reg(
#'   multilevel_data,
#'   outcome = score,
#'   exposures = c(treatment, age),
#'   group = clinic,
#'   approach = linear,
#'   model_stats = TRUE
#' )
#'
#' @export
multilevel_reg <- function(data,
                           outcome,
                           exposures,
                           group,
                           adjust_for = NULL,
                           approach = c("linear", "logit"),
                           format = c("flextable", "gt"),
                           theme = c("minimal"),
                           model_stats = FALSE,
                           show_ref = TRUE) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for `multilevel_reg()`.", call. = FALSE)
  }
  outcome <- .cox_single_var_arg(substitute(outcome), data, parent.frame())
  exposures <- .vars_arg(substitute(exposures), parent.frame(),
                         data_names = names(data))
  group <- .cox_single_var_arg(substitute(group), data, parent.frame())
  adjust_for <- .vars_arg(
    substitute(adjust_for), parent.frame(), allow_null = TRUE,
    data_names = names(data)
  )
  approach <- .choice_arg(
    substitute(approach), parent.frame(), choices = c("linear", "logit")
  )
  approach <- match.arg(approach, c("linear", "logit"))
  format <- .choice_arg(
    substitute(format), parent.frame(), choices = c("flextable", "gt")
  )
  theme <- .choice_arg(substitute(theme), parent.frame())
  flags <- list(model_stats = model_stats, show_ref = show_ref)
  if (!all(vapply(flags, function(x) is.logical(x) && length(x) == 1L &&
                  !is.na(x), logical(1)))) {
    stop("`model_stats` and `show_ref` must each be TRUE or FALSE.", call. = FALSE)
  }

  clean <- .validate_multilevel_inputs(
    data, outcome, exposures, group, adjust_for, approach
  )
  fit <- .fit_multilevel_model(
    clean, outcome, exposures, group, adjust_for, approach
  )
  table_body <- do.call(
    rbind,
    lapply(exposures, function(variable) {
      .tidy_multilevel_fixed(fit, variable, approach)
    })
  )
  variable_labels <- .var_label_map(
    data, unique(c(exposures, adjust_for, group))
  )
  effect_label <- if (approach == "linear") {
    "Adjusted Beta (95% CI)"
  } else {
    "Adjusted OR (95% CI)"
  }
  display <- .make_display_multi(
    table_body, clean, outcome, effect_label, variable_labels, show_ref
  )
  display <- .add_model_n_column(display, list(multilevel_model = fit))
  group_label <- .label_var(group, variable_labels)
  footnotes <- c(
    .abbrev_note(approach),
    if (.has_visible_reference(table_body, show_ref)) .ref_note() else NULL,
    if (length(adjust_for)) {
      .display_adjustment_note(adjust_for, variable_labels)
    } else NULL,
    if (length(exposures) > 1L) {
      "Adjusted for the other reported variables in the model."
    } else NULL,
    paste0("Random-intercept model clustered by ", group_label, "."),
    .model_n_footnote(list(multilevel_model = fit))
  )
  format <- match.arg(format, c("flextable", "gt"))
  .validate_theme_arg(theme)
  theme <- .resolve_theme(theme)
  table <- if (format == "gt") {
    .build_gt_multi(display, effect_label, footnotes, theme)
  } else {
    .build_flextable_multi(display, effect_label, footnotes, theme)
  }
  .message_hidden_ref_rows("multilevel_reg", table_body, show_ref)
  stats <- .multilevel_model_stats(fit, clean, group, approach)

  result <- list(
    table = table,
    table_body = table_body,
    table_display = display,
    models = list(multilevel_model = fit),
    model_summaries = list(multilevel_model = summary(fit)),
    model_stats = if (isTRUE(model_stats)) stats else NULL,
    variable_labels = variable_labels,
    footnotes = footnotes,
    outcome = outcome,
    exposures = unique(exposures),
    adjust_for = unique(adjust_for),
    group = group,
    approach = approach,
    outcome_approach = approach,
    source = "multilevel_reg",
    format = format,
    adjusted_mode = TRUE,
    multivariable = TRUE,
    show_ref = isTRUE(show_ref),
    singular = stats$singular,
    convergence_messages = attr(fit, "gtregression_convergence_messages")
  )
  fmt_class <- if (format == "gt") "gt_multilevel" else "ft_multilevel"
  class(result) <- c("gtregression", "multilevel_reg", fmt_class, class(result))
  result
}

#' @keywords internal
#' @noRd
.validate_multilevel_inputs <- function(data, outcome, exposures, group,
                                        adjust_for, approach) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  required <- unique(c(outcome, exposures, group, adjust_for))
  if (!all(required %in% names(data))) {
    stop("One or more requested variables were not found in `data`.", call. = FALSE)
  }
  if (!length(exposures)) {
    stop("`exposures` must contain at least one variable.", call. = FALSE)
  }
  if (outcome %in% c(exposures, adjust_for, group) ||
      group %in% c(exposures, adjust_for) || any(exposures %in% adjust_for)) {
    stop("Outcome, exposures, adjustment variables, and group must not overlap.", call. = FALSE)
  }
  .validate_exposures(data, unique(c(exposures, adjust_for)))
  if (approach == "linear" && !is.numeric(data[[outcome]])) {
    stop("A linear multilevel model requires a numeric outcome.", call. = FALSE)
  }
  if (approach == "logit") {
    .cox_event01(data[[outcome]])
  }
  vars <- unique(c(outcome, exposures, group, adjust_for))
  clean <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]
  if (!nrow(clean)) stop("No complete cases are available.", call. = FALSE)
  clean[[group]] <- factor(clean[[group]])
  if (nlevels(clean[[group]]) < 2L) {
    stop("`group` must contain at least two observed clusters.", call. = FALSE)
  }
  if (any(table(clean[[group]]) < 2L)) {
    warning("At least one cluster contains only one observation.", call. = FALSE)
  }
  if (approach == "logit") clean[[outcome]] <- .cox_event01(clean[[outcome]])
  clean
}

#' @keywords internal
#' @noRd
.fit_multilevel_model <- function(data, outcome, exposures, group, adjust_for,
                                  approach) {
  fixed <- paste(.surv_bt(unique(c(exposures, adjust_for))), collapse = " + ")
  fixed_formula <- stats::as.formula(paste0(.surv_bt(outcome), " ~ ", fixed))
  formula <- stats::as.formula(paste0(
    .surv_bt(outcome), " ~ ", fixed, " + (1 | ", .surv_bt(group), ")"
  ))
  fit_messages <- character(0)
  fit <- withCallingHandlers(
    if (approach == "linear") {
      lme4::lmer(formula, data = data, REML = FALSE)
    } else {
      lme4::glmer(formula, data = data, family = stats::binomial())
    },
    warning = function(warning) {
      fit_messages <<- c(fit_messages, conditionMessage(warning))
      invokeRestart("muffleWarning")
    },
    message = function(message) {
      fit_messages <<- c(fit_messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )
  convergence <- fit@optinfo$conv$lme4$messages
  messages <- unique(c(fit_messages, convergence))
  singular <- lme4::isSingular(fit, tol = 1e-4)
  guidance <- character(0)
  if (singular) {
    guidance <- c(
      guidance,
      "The multilevel model has a singular fit; the random-intercept variance may be near zero."
    )
  }
  if (length(messages)) {
    guidance <- c(
      guidance,
      paste0("Convergence message: ", paste(messages, collapse = "; "))
    )
  }
  if (length(guidance)) {
    warning(paste(guidance, collapse = " "), call. = FALSE)
  }
  attr(fit, "gtregression_model_frame") <- data
  attr(fit, "gtregression_convergence_messages") <- messages
  fixed_matrix <- stats::model.matrix(fixed_formula, data = data)
  fixed_assign <- attr(fixed_matrix, "assign")
  names(fixed_assign) <- colnames(fixed_matrix)
  attr(fit, "gtregression_fixed_assign") <- fixed_assign
  attr(fit, "gtregression_fixed_terms") <- attr(
    stats::terms(fixed_formula), "term.labels"
  )
  fit
}

#' @keywords internal
#' @noRd
.tidy_multilevel_fixed <- function(fit, exposure, approach) {
  model_data <- attr(fit, "gtregression_model_frame")
  x <- model_data[[exposure]]
  if (is.character(x)) x <- factor(x)
  coefs <- summary(fit)$coefficients
  assignment <- attr(fit, "gtregression_fixed_assign")
  term_labels <- gsub(
    "`", "", attr(fit, "gtregression_fixed_terms"), fixed = TRUE
  )
  term_index <- match(exposure, term_labels)
  selected_names <- names(assignment)[assignment == term_index]
  selected <- rownames(coefs) %in% selected_names
  rows <- coefs[selected, , drop = FALSE]
  if (!nrow(rows)) {
    stop("No estimable coefficient for exposure '", exposure, "'.", call. = FALSE)
  }
  estimate <- rows[, "Estimate"]
  se <- rows[, "Std. Error"]
  z <- stats::qnorm(0.975)
  p <- 2 * stats::pnorm(abs(estimate / se), lower.tail = FALSE)
  transform <- if (approach == "logit") exp else identity
  nonref <- data.frame(
    exposure = exposure,
    level = if (is.factor(x)) levels(x)[-1L] else exposure,
    estimate = transform(estimate),
    conf.low = transform(estimate - z * se),
    conf.high = transform(estimate + z * se),
    p.value = p,
    ref = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.factor(x)) return(nonref)
  reference <- data.frame(
    exposure = exposure, level = levels(x)[1L],
    estimate = if (approach == "logit") 1 else 0,
    conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_,
    ref = TRUE, stringsAsFactors = FALSE
  )
  rbind(reference, nonref)
}

#' @keywords internal
#' @noRd
.multilevel_model_stats <- function(fit, data, group, approach) {
  variance <- as.data.frame(lme4::VarCorr(fit))
  random_variance <- variance$vcov[variance$grp == group][1L]
  residual_variance <- if (approach == "linear") stats::sigma(fit)^2 else pi^2 / 3
  icc <- random_variance / (random_variance + residual_variance)
  data.frame(
    model = "multilevel_model",
    AIC = stats::AIC(fit),
    BIC = stats::BIC(fit),
    logLik = as.numeric(stats::logLik(fit)),
    n = stats::nobs(fit),
    clusters = nlevels(data[[group]]),
    random_intercept_variance = random_variance,
    residual_variance = residual_variance,
    ICC = icc,
    ICC_scale = if (approach == "linear") "observed" else "latent logistic",
    singular = lme4::isSingular(fit, tol = 1e-4),
    stringsAsFactors = FALSE
  )
}
