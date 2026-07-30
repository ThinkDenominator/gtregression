#' Compare Models With and Without an Interaction Term
#'
#' Fits two models, one with and one without an interaction term between an
#' exposure and a potential effect modifier. The models are compared using a
#' likelihood ratio test or Wald test to assess statistical evidence of
#' interaction.
#'
#' Use this function when the interaction is planned or clinically/causally
#' motivated and you want a focused model comparison. Mantel-Haenszel estimation
#' is not used here because this function tests an explicit interaction term in
#' a regression model. For broader screening of candidate confounders or effect
#' modifiers, including Mantel-Haenszel-supported checks when appropriate, use
#' \code{\link{identify_confounder}()}.
#'
#' @param data A data frame containing all required variables.
#' @param outcome Outcome variable name. Quoted and bare names are accepted.
#'   Required for ordinary regression approaches. Leave unset for Cox and
#'   parametric survival approaches and supply \code{time} and \code{event}
#'   instead.
#' @param exposure Main exposure variable name. Quoted and bare names are
#'   accepted.
#' @param covariates Optional character vector of additional covariates. Quoted
#'   names are recommended in scripts, and bare names are also accepted.
#' @param effect_modifier Variable name for the potential effect modifier.
#'   Quoted and bare names are accepted.
#' @param approach Regression approach. One of \code{"logit"},
#'   \code{"logbinomial"}, \code{"poisson"}, \code{"robpoisson"},
#'   \code{"negbin"}, \code{"linear"}, \code{"cox"}, or
#'   \code{"survreg"}.
#' @param time Survival time variable name for \code{approach = "cox"} or
#'   \code{approach = "survreg"}. Quoted and bare names are accepted.
#' @param event Event indicator variable name for survival approaches.
#' @param distribution Parametric survival distribution for
#'   \code{approach = "survreg"}. One of \code{"weibull"},
#'   \code{"exponential"}, \code{"lognormal"}, or \code{"loglogistic"}.
#' @param test Statistical test for model comparison. One of \code{"LRT"} or
#'   \code{"Wald"}.
#' @param alpha Significance threshold used to classify the interaction result.
#' @param verbose Logical; if \code{TRUE}, prints a short interpretation.
#' @param format Output format for the viewing table. One of
#'   \code{"flextable"} (default), \code{"gt"}, or \code{"tibble"}. Use
#'   \code{format = "tibble"} to keep only the original list structure.
#'
#' @return A list with model objects, formulas, p-value, decision, and a
#'   one-row summary tibble. When \code{format} is \code{"gt"} or
#'   \code{"flextable"}, the list also includes \code{table}.
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   dplyr::mutate(
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     race = factor(race, levels = c(1, 2, 3),
#'                   labels = c("White", "Black", "Other"))
#'   )
#'
#' interaction_models(
#'   data = birthwt_data,
#'   outcome = low,
#'   exposure = smoke,
#'   effect_modifier = race,
#'   covariates = c(age, lwt),
#'   approach = logit
#' )
#'
#' lung_data <- data_lungcancer |>
#'   dplyr::mutate(
#'     trt = factor(trt, levels = c(1, 2),
#'                  labels = c("Standard treatment", "Test treatment")),
#'     prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
#'   )
#'
#' interaction_models(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposure = trt,
#'   effect_modifier = prior,
#'   covariates = c(age, karno),
#'   approach = cox
#' )
#'
#' @importFrom stats glm anova binomial coef lm poisson reformulate terms
#' @importFrom survival coxph survreg Surv
#' @importFrom MASS glm.nb
#' @seealso \code{\link{identify_confounder}()} for broader confounding and
#'   effect-modification screening.
#' @export
interaction_models <- function(data,
                               outcome = NULL,
                               exposure,
                               covariates = NULL,
                               effect_modifier,
                               approach = "logit",
                               time = NULL,
                               event = NULL,
                               distribution = "weibull",
                               test = c("LRT", "Wald"),
                               alpha = 0.05,
                               verbose = FALSE,
                               format = c("flextable", "gt", "tibble")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  outcome <- if (missing(outcome) || identical(substitute(outcome), quote(NULL))) {
    NULL
  } else {
    .vars_arg(substitute(outcome), env = parent.frame())
  }
  exposure <- .vars_arg(substitute(exposure), env = parent.frame())
  covariates <- .vars_arg(substitute(covariates), env = parent.frame(), allow_null = TRUE)
  effect_modifier <- .vars_arg(substitute(effect_modifier), env = parent.frame())
  if (!is.character(exposure) || length(exposure) != 1L ||
      is.na(exposure) || !nzchar(exposure)) {
    stop("`exposure` must be a single character variable name.", call. = FALSE)
  }
  if (!is.character(effect_modifier) || length(effect_modifier) != 1L ||
      is.na(effect_modifier) || !nzchar(effect_modifier)) {
    stop("`effect_modifier` must be a single character variable name.",
         call. = FALSE)
  }
  if (!is.null(covariates) &&
      (!is.character(covariates) || anyNA(covariates) ||
       any(!nzchar(covariates)))) {
    stop("`covariates` must be NULL or a character vector of variable names.",
         call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L ||
      is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit", "logbinomial", "poisson", "robpoisson", "negbin",
                "linear", "cox", "surv", "survreg")
  )
  approach <- .normalize_approach(approach)
  .validate_approach(approach, context = "interaction_models")
  is_survival <- approach %in% c("cox", "survreg")
  if (!is_survival &&
      (!is.character(outcome) || length(outcome) != 1L ||
       is.na(outcome) || !nzchar(outcome))) {
    stop("`outcome` must be a single character variable name.", call. = FALSE)
  }
  time <- if (missing(time) || identical(substitute(time), quote(NULL))) {
    NULL
  } else {
    .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  }
  event <- if (missing(event) || identical(substitute(event), quote(NULL))) {
    NULL
  } else {
    .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  }
  distribution <- if (identical(approach, "survreg")) {
    .surv_distribution_arg(
      substitute(distribution),
      env = parent.frame(),
      multiple = FALSE,
      arg = "distribution"
    )
  } else {
    "weibull"
  }

  test <- .choice_arg(
    substitute(test),
    env = parent.frame(),
    choices = c("LRT", "Wald"),
    lower = FALSE
  )
  test <- match.arg(test, c("LRT", "Wald"))
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  format <- match.arg(format, c("flextable", "gt", "tibble"))

  if (is_survival && (is.null(time) || is.null(event))) {
    stop("`time` and `event` are required when `approach` is cox or survreg.", call. = FALSE)
  }

  needed <- if (is_survival) {
    unique(c(time, event, exposure, effect_modifier, covariates))
  } else {
    unique(c(outcome, exposure, effect_modifier, covariates))
  }
  missing_vars <- setdiff(needed, names(data))
  if (length(missing_vars)) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }

  if (!is_survival) {
    .validate_outcome_by_approach(data[[outcome]], approach)
  } else {
    .validate_cox_inputs(
      data = data,
      time = time,
      event = event,
      exposures = unique(c(exposure, effect_modifier, covariates)),
      adjust_for = NULL
    )
  }

  model_data <- stats::na.omit(data[, needed, drop = FALSE])
  if (nrow(model_data) == 0L) {
    stop("No complete cases available for model fitting.", call. = FALSE)
  }

  if (is_survival) {
    model_data[[event]] <- .cox_event01(model_data[[event]])
  }

  if (approach == "robpoisson" &&
      (is.factor(model_data[[outcome]]) || is.character(model_data[[outcome]]))) {
    lev <- .interaction_binary_levels(model_data[[outcome]])
    if (length(lev) == 2L) {
      model_data[[outcome]] <- ifelse(model_data[[outcome]] == lev[[2]], 1L, 0L)
    }
  }

  base_terms <- unique(c(exposure, effect_modifier, covariates))
  interaction_term <- paste0(exposure, ":", effect_modifier)
  if (is_survival) {
    response <- paste0("survival::Surv(", .surv_bt(time), ", ", .surv_bt(event), ")")
    base_formula <- stats::as.formula(
      paste(response, "~", .survival_rhs(base_terms))
    )
    interaction_formula <- stats::as.formula(
      paste(response, "~", .survival_rhs(base_terms, paste0(exposure, "*", effect_modifier)))
    )
  } else {
    base_formula <- stats::reformulate(base_terms, response = outcome)
    interaction_formula <- stats::reformulate(
      unique(c(base_terms, interaction_term)),
      response = outcome
    )
  }

  fit_model <- function(formula) {
    switch(
      approach,
      "logit" = stats::glm(formula, data = model_data,
                           family = stats::binomial("logit")),
      "logbinomial" = stats::glm(formula, data = model_data,
                                 family = stats::binomial("log")),
      "poisson" = stats::glm(formula, data = model_data,
                             family = stats::poisson("log")),
      "robpoisson" = stats::glm(formula, data = model_data,
                                family = stats::poisson("log")),
      "negbin" = MASS::glm.nb(formula, data = model_data),
      "linear" = stats::lm(formula, data = model_data),
      "cox" = survival::coxph(formula, data = model_data, model = TRUE),
      "survreg" = survival::survreg(formula, data = model_data, dist = distribution, model = TRUE),
      stop("Unsupported regression approach.", call. = FALSE)
    )
  }

  model_no_interaction <- tryCatch(
    fit_model(base_formula),
    error = function(e) structure(list(error = conditionMessage(e)),
                                  class = "interaction_fit_error")
  )
  model_with_interaction <- tryCatch(
    fit_model(interaction_formula),
    error = function(e) structure(list(error = conditionMessage(e)),
                                  class = "interaction_fit_error")
  )

  if (inherits(model_no_interaction, "interaction_fit_error") ||
      inherits(model_with_interaction, "interaction_fit_error")) {
    stop("Model fitting failed for one or both models.", call. = FALSE)
  }

  comparison <- .interaction_compare_models(
    model_no_interaction,
    model_with_interaction,
    test = test,
    approach = approach
  )

  p_value <- comparison$p_value
  has_interaction <- if (is.na(p_value)) NA else p_value < alpha
  decision <- if (is.na(has_interaction)) {
    "not_estimable"
  } else if (has_interaction) {
    "interaction"
  } else {
    "no_interaction"
  }
  interpretation <- .interaction_interpretation(
    decision = decision,
    p_value = p_value,
    alpha = alpha,
    exposure = exposure,
    effect_modifier = effect_modifier
  )

  interaction_terms <- .interaction_terms(model_with_interaction, interaction_term)

  robust_no_interaction <- NULL
  robust_with_interaction <- NULL
  if (approach == "robpoisson") {
    robust_no_interaction <- lmtest::coeftest(
      model_no_interaction,
      vcov. = sandwich::vcovHC(model_no_interaction, type = "HC0")
    )
    robust_with_interaction <- lmtest::coeftest(
      model_with_interaction,
      vcov. = sandwich::vcovHC(model_with_interaction, type = "HC0")
    )
  }

  summary <- tibble::tibble(
    outcome = if (is_survival) paste0(time, "/", event) else outcome,
    exposure = exposure,
    effect_modifier = effect_modifier,
    approach = approach,
    test = comparison$test_label,
    p_value = p_value,
    alpha = alpha,
    has_interaction = has_interaction,
    decision = decision,
    interpretation = interpretation
  )

  result <- list(
    summary = summary,
    model_no_interaction = model_no_interaction,
    model_with_interaction = model_with_interaction,
    robust_no_interaction = robust_no_interaction,
    robust_with_interaction = robust_with_interaction,
    formula_no_interaction = base_formula,
    formula_with_interaction = interaction_formula,
    interaction_terms = interaction_terms,
    comparison = comparison$comparison,
    p_value = p_value,
    alpha = alpha,
    has_interaction = has_interaction,
    decision = decision,
    interpretation = interpretation,
    test = comparison$test_label,
    approach = approach,
    source = "interaction_models"
  )
  if (format != "tibble") {
    result$table <- .build_interaction_models_table(summary, format = format)
  }
  class(result) <- c("interaction_models_result", class(result))

  if (verbose) {
    message(interpretation)
  }

  result
}

#' Build formatted interaction_models table
#' @keywords internal
#' @noRd
.build_interaction_models_table <- function(summary_tbl,
                                            format = c("flextable", "gt")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Screening aid only; interaction decisions should be interpreted with",
    "subject-matter knowledge, study design, and stratum-specific estimates."
  )

  display <- summary_tbl |>
    dplyr::mutate(
      p_value = dplyr::if_else(
        is.na(.data$p_value),
        "",
        ifelse(.data$p_value < 0.001, "<0.001",
               formatC(.data$p_value, digits = 3, format = "f"))
      ),
      alpha = formatC(.data$alpha, digits = 3, format = "f"),
      has_interaction = dplyr::case_when(
        is.na(.data$has_interaction) ~ "",
        .data$has_interaction ~ "Yes",
        TRUE ~ "No"
      ),
      decision = dplyr::recode(
        .data$decision,
        interaction = "Interaction",
        no_interaction = "No interaction",
        not_estimable = "Not estimable",
        .default = .data$decision
      )
    ) |>
    dplyr::transmute(
      "Outcome" = .data$outcome,
      "Exposure" = .data$exposure,
      "Effect modifier" = .data$effect_modifier,
      "Approach" = .data$approach,
      "Test" = .data$test,
      "p-value" = .data$p_value,
      "Alpha" = .data$alpha,
      "Interaction?" = .data$has_interaction,
      "Decision" = .data$decision,
      "Interpretation" = .data$interpretation
    )

  if (format == "gt") {
    return(
      gt::gt(display) |>
        gt::tab_header(title = "Interaction screening") |>
        gt::cols_align(
          align = "left",
          columns = c("Outcome", "Exposure", "Effect modifier",
                      "Approach", "Test", "Decision", "Interpretation")
        ) |>
        gt::cols_align(
          align = "center",
          columns = c("p-value", "Alpha", "Interaction?")
        ) |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_source_note(gt::md(note)) |>
        .compact_gt_source_notes()
    )
  }

  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Interaction screening")
  ft <- flextable::align(
    ft,
    j = c("Outcome", "Exposure", "Effect modifier",
          "Approach", "Test", "Decision", "Interpretation"),
    align = "left",
    part = "all"
  )
  ft <- flextable::align(
    ft,
    j = c("p-value", "Alpha", "Interaction?"),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- .compact_flex_footer(ft)
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}

.interaction_binary_levels <- function(x) {
  x <- x[!is.na(x)]
  if (is.factor(x)) {
    levels(droplevels(x))
  } else {
    sort(unique(x))
  }
}

.interaction_compare_models <- function(model1, model2, test, approach) {
  if (test == "LRT") {
    comp <- tryCatch(
      suppressWarnings(stats::anova(model1, model2, test = "LRT")),
      error = function(e) NULL
    )
    p_value <- .interaction_extract_lrt_p(comp)
    if (is.na(p_value)) {
      p_value <- .interaction_lrt_from_loglik(model1, model2)
    }
    return(list(
      comparison = comp,
      p_value = p_value,
      test_label = "Likelihood Ratio Test"
    ))
  }

  comp <- tryCatch(
    suppressWarnings(lmtest::waldtest(model1, model2)),
    error = function(e) NULL
  )
  p_value <- if (!is.null(comp) && nrow(comp) >= 2L) {
    cols <- grep("^Pr\\(", names(comp), value = TRUE)
    if (length(cols)) suppressWarnings(as.numeric(comp[[cols[[1]]]][2])) else NA_real_
  } else {
    NA_real_
  }

  list(
    comparison = comp,
    p_value = p_value,
    test_label = "Wald Test"
  )
}

.interaction_extract_lrt_p <- function(comp) {
  if (is.null(comp) || nrow(comp) < 2L) {
    return(NA_real_)
  }
  cols <- grep("^Pr\\(", names(comp), value = TRUE)
  if (!length(cols)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(comp[[cols[[1]]]][2]))
}

.interaction_lrt_from_loglik <- function(model1, model2) {
  ll1 <- tryCatch(stats::logLik(model1), error = function(e) NULL)
  ll2 <- tryCatch(stats::logLik(model2), error = function(e) NULL)
  if (is.null(ll1) || is.null(ll2)) {
    return(NA_real_)
  }
  df <- attr(ll2, "df") - attr(ll1, "df")
  chisq <- 2 * (as.numeric(ll2) - as.numeric(ll1))
  if (!is.finite(df) || !is.finite(chisq) || df <= 0 || chisq < 0) {
    return(NA_real_)
  }
  stats::pchisq(chisq, df = df, lower.tail = FALSE)
}

.interaction_terms <- function(model, interaction_term) {
  coef_names <- names(stats::coef(model))
  if (is.null(coef_names)) {
    return(character(0))
  }
  parts <- strsplit(interaction_term, ":", fixed = TRUE)[[1]]
  coef_names[vapply(
    coef_names,
    function(x) all(vapply(parts, grepl, logical(1), x = x, fixed = TRUE)),
    logical(1)
  )]
}

.interaction_interpretation <- function(decision,
                                        p_value,
                                        alpha,
                                        exposure,
                                        effect_modifier) {
  if (decision == "interaction") {
    return(paste0(
      "Evidence of interaction between ", exposure, " and ", effect_modifier,
      " (p = ", formatC(p_value, digits = 4, format = "f"),
      "); consider reporting stratum-specific effects or including the interaction term."
    ))
  }
  if (decision == "no_interaction") {
    return(paste0(
      "No statistical evidence of interaction between ", exposure, " and ",
      effect_modifier, " at alpha = ", alpha, "."
    ))
  }
  paste0(
    "Interaction could not be estimated for ", exposure, " and ",
    effect_modifier, "."
  )
}
