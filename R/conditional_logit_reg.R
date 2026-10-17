#' Conditional logistic regression for matched case-control studies
#'
#' Fits conditional logistic regression models for individually or frequency
#' matched case-control data and returns publication-ready matched odds ratios.
#'
#' @param data A data frame containing the outcome, predictors, and matched-set
#'   identifier.
#' @param outcome Binary case-control outcome. The second factor/character level
#'   is treated as the case; numeric outcomes must use 0/1 or 1/2 coding.
#' @param exposures Predictor names. Quoted and bare names are accepted.
#' @param matched_set Variable identifying each matched case-control set.
#' @param adjust_for Optional covariates. In exposure-by-exposure mode, each
#'   exposure is fitted with these covariates.
#' @param multivariable Logical. If \code{TRUE}, one model includes all
#'   exposures and optional adjustment variables.
#' @param multivariate Optional alias for \code{multivariable}.
#' @param model_stats Logical. If \code{TRUE}, return model N, case and control
#'   counts, matched-set count, AIC, and log-likelihood.
#' @param format Output format: \code{"flextable"} (default) or \code{"gt"}.
#' @param theme Table theme preset.
#' @param show_ref Logical. Display reference rows for categorical predictors.
#'
#' @details
#' Matching is handled by conditioning on \code{matched_set}; no odds ratio is
#' estimated for that variable. Matched sets without both a case and a control
#' are not informative for conditional logistic regression and are excluded
#' with a message. This function is intended for matched case-control studies,
#' not complex survey analysis.
#'
#' @return A \code{gtregression} object containing the publication table,
#'   underlying estimates, fitted \code{clogit} models, summaries, model
#'   statistics, and matching metadata.
#'
#' @examples
#' matched_data <- data.frame(
#'   set = rep(1:8, each = 3),
#'   case = rep(c(1, 0, 0), 8),
#'   exposed = factor(rep(c("Yes", "No", "No"), 8),
#'                    levels = c("No", "Yes")),
#'   age = rep(40:47, each = 3) + rep(c(0, -1, 1), 8)
#' )
#'
#' conditional_logit_reg(
#'   matched_data,
#'   outcome = case,
#'   exposures = exposed,
#'   matched_set = set
#' )
#'
#' @importFrom survival clogit strata
#' @export
conditional_logit_reg <- function(data,
                                  outcome,
                                  exposures,
                                  matched_set,
                                  adjust_for = NULL,
                                  multivariable = FALSE,
                                  multivariate = NULL,
                                  model_stats = FALSE,
                                  format = c("flextable", "gt"),
                                  theme = "minimal",
                                  show_ref = TRUE) {
  outcome <- .single_var_arg(
    substitute(outcome), data = data, env = parent.frame()
  )
  exposures <- .vars_arg(substitute(exposures), env = parent.frame(),
                         data_names = names(data))
  matched_set <- .single_var_arg(
    substitute(matched_set), data = data, env = parent.frame()
  )
  adjust_for <- .vars_arg(
    substitute(adjust_for), env = parent.frame(), allow_null = TRUE,
    data_names = names(data)
  )
  format <- .choice_arg(
    substitute(format), env = parent.frame(), choices = c("flextable", "gt")
  )
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  if (!is.null(multivariate)) {
    if (!is.logical(multivariate) || length(multivariate) != 1L || is.na(multivariate)) {
      stop("`multivariate` must be TRUE or FALSE.", call. = FALSE)
    }
    if (!identical(multivariable, FALSE) && !identical(multivariable, multivariate)) {
      stop("Use only one of `multivariable` or `multivariate`.", call. = FALSE)
    }
    multivariable <- multivariate
  }
  if (!is.logical(multivariable) || length(multivariable) != 1L || is.na(multivariable)) {
    stop("`multivariable` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(model_stats) || length(model_stats) != 1L || is.na(model_stats)) {
    stop("`model_stats` must be TRUE or FALSE.", call. = FALSE)
  }
  .validate_show_ref(show_ref)
  format <- match.arg(format, c("flextable", "gt"))
  .validate_theme_arg(theme)
  theme <- .resolve_theme(theme)

  .validate_conditional_logit_inputs(
    data, outcome, exposures, matched_set, adjust_for
  )
  adjusted <- length(adjust_for) > 0L || isTRUE(multivariable)
  model_terms <- if (isTRUE(multivariable)) list(multivariable_model = exposures) else {
    stats::setNames(as.list(exposures), exposures)
  }

  fits <- list()
  tidy_parts <- list()
  stats_parts <- list()
  informative_sets <- list()

  for (model_name in names(model_terms)) {
    reported <- model_terms[[model_name]]
    predictors <- unique(c(reported, adjust_for))
    prepared <- .prepare_conditional_logit_data(
      data, outcome, predictors, matched_set
    )
    fit <- .fit_conditional_logit(
      prepared$data, outcome, predictors, matched_set
    )
    td <- do.call(rbind, lapply(reported, function(exposure) {
      .tidy_cox(fit, exposure)
    }))
    if (is.null(td) || !nrow(td)) {
      stop("No estimable matched odds ratios for model '", model_name, "'.",
           call. = FALSE)
    }

    fits[[model_name]] <- fit
    tidy_parts[[model_name]] <- td
    informative_sets[[model_name]] <- prepared$sets
    stats_parts[[model_name]] <- .conditional_logit_stats(
      fit, prepared$data, outcome, matched_set, model_name
    )
  }

  table_body <- do.call(rbind, tidy_parts)
  rownames(table_body) <- NULL
  variable_labels <- .var_label_map(data, unique(c(exposures, adjust_for)))
  effect_label <- if (adjusted) "Adjusted matched OR (95% CI)" else {
    "Matched OR (95% CI)"
  }
  display <- .make_display_multi(
    table_body,
    data,
    outcome,
    effect_label,
    variable_labels = variable_labels,
    show_ref = show_ref
  )
  display <- .add_model_n_column(display, fits)
  .message_hidden_ref_rows("conditional_logit_reg", table_body, show_ref)

  set_counts <- vapply(informative_sets, length, integer(1))
  set_note <- if (length(unique(set_counts)) == 1L) {
    paste0("Conditional logistic regression included ", set_counts[1],
           " informative matched sets.")
  } else {
    "The number of informative matched sets varies across models because of missing data."
  }
  footnotes <- c(
    "Matched OR = matched Odds Ratio.",
    if (.has_visible_reference(table_body, show_ref)) .ref_note() else NULL,
    paste0("Matched by set identifier: ", .label_var(matched_set, .var_label_map(data, matched_set)), "."),
    if (length(adjust_for)) .display_adjustment_note(adjust_for, variable_labels) else NULL,
    if (isTRUE(multivariable)) "Adjusted for the other exposures in the model." else NULL,
    set_note,
    .model_n_footnote(fits)
  )

  table <- if (format == "gt") {
    .build_gt_multi(display, effect_label, footnotes, theme)
  } else {
    .build_flextable_multi(display, effect_label, footnotes, theme)
  }

  out <- list(
    table = table,
    table_body = table_body,
    table_display = display,
    models = fits,
    model_summaries = lapply(fits, .model_summary),
    model_stats = if (isTRUE(model_stats)) do.call(rbind, stats_parts) else NULL,
    variable_labels = variable_labels,
    footnotes = footnotes,
    outcome = outcome,
    exposures = exposures,
    matched_set = matched_set,
    adjust_for = adjust_for,
    adjusted_mode = adjusted,
    multivariable = isTRUE(multivariable),
    informative_sets = informative_sets,
    approach = "conditional_logit",
    format = format,
    source = "conditional_logit_reg",
    show_ref = isTRUE(show_ref)
  )
  fmt_class <- if (format == "gt") "gt_conditional_logit" else {
    "ft_conditional_logit"
  }
  class(out) <- c("gtregression", "conditional_logit_reg", fmt_class, class(out))
  out
}

#' @keywords internal
#' @noRd
.validate_conditional_logit_inputs <- function(data, outcome, exposures,
                                               matched_set, adjust_for) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  variables <- unique(c(outcome, exposures, matched_set, adjust_for))
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop("Variables not found in `data`: ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }
  if (outcome %in% c(exposures, adjust_for, matched_set)) {
    stop("`outcome` cannot also be a predictor or matched-set variable.",
         call. = FALSE)
  }
  if (matched_set %in% c(exposures, adjust_for)) {
    stop("`matched_set` cannot also be an exposure or adjustment variable.",
         call. = FALSE)
  }
  if (length(intersect(exposures, adjust_for))) {
    stop("`exposures` and `adjust_for` must not overlap.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.prepare_conditional_logit_data <- function(data, outcome, predictors, matched_set) {
  variables <- unique(c(outcome, predictors, matched_set))
  out <- data[stats::complete.cases(data[, variables, drop = FALSE]), , drop = FALSE]
  out[[outcome]] <- .cox_event01(out[[outcome]])
  for (variable in predictors) {
    if (is.character(out[[variable]])) out[[variable]] <- factor(out[[variable]])
    if (is.factor(out[[variable]])) out[[variable]] <- droplevels(out[[variable]])
  }
  if (!nrow(out)) stop("No complete observations are available.", call. = FALSE)

  split_outcome <- split(out[[outcome]], out[[matched_set]], drop = TRUE)
  informative <- names(Filter(function(x) any(x == 1L) && any(x == 0L), split_outcome))
  if (!length(informative)) {
    stop("No informative matched sets contain both a case and a control.",
         call. = FALSE)
  }
  excluded <- length(split_outcome) - length(informative)
  if (excluded > 0L) {
    message("conditional_logit_reg(): excluded ", excluded,
            " matched set(s) without both a case and a control.")
  }
  out <- out[as.character(out[[matched_set]]) %in% informative, , drop = FALSE]
  list(data = out, sets = informative)
}

#' @keywords internal
#' @noRd
.fit_conditional_logit <- function(data, outcome, predictors, matched_set) {
  rhs <- paste(c(.surv_bt(predictors), paste0("strata(",
                                              .surv_bt(matched_set), ")")),
               collapse = " + ")
  formula <- stats::as.formula(paste(.surv_bt(outcome), "~", rhs))
  survival::clogit(formula, data = data, model = TRUE)
}

#' @keywords internal
#' @noRd
.conditional_logit_stats <- function(fit, data, outcome, matched_set, model_name) {
  data.frame(
    model = model_name,
    AIC = .safe_numeric(stats::AIC(fit)),
    logLik = .safe_numeric(as.numeric(stats::logLik(fit))),
    matched_sets = length(unique(data[[matched_set]])),
    cases = sum(data[[outcome]] == 1L),
    controls = sum(data[[outcome]] == 0L),
    n = nrow(data),
    stringsAsFactors = FALSE
  )
}
