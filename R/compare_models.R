#' Compare Prespecified Regression Models
#'
#' Compare gtregression candidate models side by side using model-fit statistics.
#' This is intended for transparent model comparison after you have already
#' fitted the candidate models with functions such as \code{multi_reg()},
#' \code{cox_reg()}, or \code{surv_reg()}.
#'
#' @param ... Two or more gtregression model objects, or one list containing
#'   them. Inputs should be outputs from \code{multi_reg()}, \code{cox_reg()},
#'   or \code{surv_reg()}.
#' @param model_names Optional character vector of names to display. If omitted,
#'   names supplied in \code{...} are used; otherwise models are labelled
#'   \code{Model 1}, \code{Model 2}, etc.
#' @param nested Logical. If \code{TRUE}, likelihood-ratio statistics are
#'   calculated sequentially by comparing each model with the previous model.
#'   Use this only when models are nested and supplied in the intended order.
#' @param primary_exposure Optional exposure or exact coefficient name to track
#'   across models. For Cox models this can be used to show the hazard ratio
#'   and percentage change in the log-effect estimate across candidate models.
#' @param exponentiate Logical. If \code{NULL}, Cox, logistic, Poisson,
#'   negative-binomial, and parametric survival models are exponentiated by
#'   default, while linear models are not.
#' @param digits Number of digits for model statistics and estimates.
#' @param p_digits Number of digits for p-values.
#' @param format Output format. Defaults to \code{"flextable"}.
#' @param theme Table theme preset.
#'
#' @return A \code{gtregression} object with:
#' \itemize{
#'   \item \code{table}: publication-ready table
#'   \item \code{table_body}: raw comparison statistics
#'   \item \code{table_display}: formatted display data
#'   \item \code{models}: fitted models compared
#'   \item \code{comparison_status}: whether models appear to use the same
#'     analysis sample
#'   \item \code{comparison_warnings}: caution messages that are highlighted
#'     in rendered tables when interpretation needs extra care
#' }
#'
#' @details
#' \code{compare_models()} does not refit models and does not perform hidden
#' complete-case filtering. It compares models already fitted by gtregression
#' and extracts the single fitted model stored in each object's \code{models}
#' element. The reported N, event counts, and fit statistics therefore come from
#' the model already fitted by \code{multi_reg()}, \code{cox_reg()}, or
#' \code{surv_reg()}. This keeps model comparison separate from model
#' selection: compare candidate models first, then choose the final model using
#' clinical, epidemiological, and statistical judgement.
#'
#' Likelihood-ratio p-values are meaningful only for nested models fitted to
#' the same analysis sample. \code{compare_models()} checks whether the fitted
#' models appear to use the same analysis sample using retained model row
#' identifiers when available; otherwise it compares N and event counts. It
#' also checks whether sequential model pairs appear to be nested when
#' \code{nested = TRUE}. Rendered warnings are context-aware: no warning about
#' different analysis samples is shown when the compared models use the same
#' observations, and no nested-model warning is shown when sequential models
#' appear nested. AIC, BIC, log-likelihood, and likelihood-ratio statistics
#' remain visible when warnings are needed, but should then be interpreted with
#' the displayed caution.
#'
#' @examples
#' data("data_lungcancer", package = "gtregression")
#'
#' lung_data <- data_lungcancer
#'
#' cox_1 <- cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = trt
#' )
#'
#' cox_2 <- cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = trt,
#'   adjust_for = c(age, karno)
#' )
#'
#' compare_models(
#'   cox_1,
#'   cox_2,
#'   primary_exposure = trt
#' )
#'
#' compare_models(
#'   cox_1,
#'   cox_2,
#'   model_names = c("Treatment only", "Treatment + age + performance"),
#'   primary_exposure = trt
#' )
#'
#' @export
compare_models <- function(...,
                           model_names = NULL,
                           nested = TRUE,
                           primary_exposure = NULL,
                           exponentiate = NULL,
                           digits = 2,
                           p_digits = 3,
                           format = c("flextable", "gt", "tibble"),
                           theme = c("minimal")) {
  input_names <- .compare_models_input_names(substitute(list(...)))
  models <- .compare_models_list(...)
  if (length(models) < 2L) {
    stop("Supply at least two gtregression model objects to compare.", call. = FALSE)
  }

  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  format <- match.arg(format, c("flextable", "gt", "tibble"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  theme <- theme[1]
  theme <- .resolve_theme(theme)

  primary_exposure <- .vars_arg(
    substitute(primary_exposure),
    env = parent.frame(),
    allow_null = TRUE
  )

  model_names <- .compare_model_names(models, model_names, input_names = input_names)
  names(models) <- model_names

  table_body <- .compare_models_body(
    models = models,
    nested = nested,
    primary_exposure = primary_exposure,
    exponentiate = exponentiate
  )
  comparison_status <- .compare_models_status(models, table_body)
  table_body$comparison_status <- comparison_status$status
  table_body$comparison_status_detail <- comparison_status$detail

  table_display <- .compare_models_display(
    table_body,
    digits = digits,
    p_digits = p_digits,
    primary_exposure = primary_exposure
  )
  table_notes <- .compare_models_notes(
    body = table_body,
    nested = nested,
    primary_exposure = primary_exposure,
    comparison_status = comparison_status
  )

  out <- list(
    table = NULL,
    table_body = table_body,
    table_display = table_display,
    models = models,
    model_names = model_names,
    primary_exposure = primary_exposure,
    comparison_status = comparison_status,
    comparison_warnings = table_notes$warnings,
    footnotes = c(table_notes$warnings, table_notes$notes),
    format = format,
    source = "compare_models"
  )

  if (format != "tibble") {
    out$table <- .build_compare_models_table(
      display = table_display,
      body = table_body,
      format = format,
      theme = theme,
      nested = nested,
      primary_exposure = primary_exposure,
      comparison_status = comparison_status,
      table_notes = table_notes
    )
  }

  class(out) <- c("gtregression", "compare_models")
  out
}

#' @keywords internal
#' @noRd
.compare_models_input_names <- function(expr) {
  exprs <- as.list(expr)[-1L]
  if (!length(exprs)) {
    return(character(0))
  }

  explicit <- names(exprs)
  if (is.null(explicit)) {
    explicit <- rep("", length(exprs))
  }

  if (length(exprs) == 1L && identical(explicit, "")) {
    return(character(0))
  }

  labels <- vapply(seq_along(exprs), function(i) {
    if (nzchar(explicit[i])) {
      return(explicit[i])
    }

    if (is.symbol(exprs[[i]])) {
      return(as.character(exprs[[i]]))
    }

    ""
  }, character(1))

  labels
}

#' @keywords internal
#' @noRd
.compare_models_list <- function(...) {
  inputs <- list(...)
  if (
    length(inputs) == 1L &&
      is.list(inputs[[1L]]) &&
      !.is_compare_gtregression_model(inputs[[1L]])
  ) {
    inputs <- inputs[[1L]]
  }

  if (!length(inputs)) {
    stop("Supply gtregression model objects to compare.", call. = FALSE)
  }

  invalid_inputs <- vapply(inputs, function(x) !.is_compare_gtregression_model(x), logical(1))
  if (any(invalid_inputs)) {
    stop(
      "All inputs must be gtregression objects from multi_reg(), cox_reg(), or surv_reg().",
      call. = FALSE
    )
  }

  models <- lapply(inputs, .compare_extract_model)
  model_terms <- Map(.compare_gtregression_terms, inputs, models)
  for (i in seq_along(models)) {
    attr(models[[i]], "gtregression_model_terms") <- model_terms[[i]]
  }

  invalid <- vapply(models, function(x) !.is_compare_fitted_model(x), logical(1))
  if (any(invalid)) {
    stop(
      "The supplied gtregression objects must contain fitted models with a logLik() method.",
      call. = FALSE
    )
  }

  names(models) <- names(inputs)
  models
}

#' @keywords internal
#' @noRd
.is_compare_gtregression_model <- function(x) {
  inherits(x, "gtregression") &&
    (inherits(x, "multi_reg") || inherits(x, "cox_reg") || inherits(x, "surv_reg")) &&
    is.list(x$models) &&
    length(x$models) > 0L
}

#' @keywords internal
#' @noRd
.is_compare_fitted_model <- function(x) {
  is.list(x) && !is.null(tryCatch(stats::logLik(x), error = function(e) NULL))
}

#' @keywords internal
#' @noRd
.compare_extract_model <- function(x) {
  if (.is_compare_gtregression_model(x)) {
    if (length(x$models) != 1L) {
      stop(
        paste0(
          "Each gtregression object supplied to compare_models() must contain one fitted model. ",
          "Use multivariable = TRUE for a single full model, or supply one exposure per ",
          "candidate model when using adjusted-exposure mode."
        ),
        call. = FALSE
      )
    }
    return(x$models[[1L]])
  }

  x
}

#' @keywords internal
#' @noRd
.compare_model_names <- function(models, model_names = NULL, input_names = NULL) {
  if (!is.null(model_names)) {
    if (!is.character(model_names) || length(model_names) != length(models)) {
      stop("`model_names` must be a character vector with one name per model.", call. = FALSE)
    }
    return(model_names)
  }

  nm <- names(models)
  if (!is.null(nm) && all(nzchar(nm))) {
    return(nm)
  }

  if (!is.null(input_names) && length(input_names) == length(models) && all(nzchar(input_names))) {
    return(input_names)
  }

  paste("Model", seq_along(models))
}

#' @keywords internal
#' @noRd
.compare_models_body <- function(models,
                                 nested = TRUE,
                                 primary_exposure = NULL,
                                 exponentiate = NULL) {
  rows <- lapply(seq_along(models), function(i) {
    fit <- models[[i]]
    ll <- .safe_loglik(fit)
    type <- .compare_model_type(fit)
    exp_flag <- .compare_exponentiate(fit, exponentiate)
    primary <- .compare_primary_effect(fit, primary_exposure, exp_flag)

    data.frame(
      model = names(models)[i],
      model_type = type,
      formula = .compare_model_formula(fit),
      model_terms = .compare_model_terms_label(fit),
      n = .compare_model_n(fit),
      events = .compare_model_events(fit),
      parameters = .safe_numeric(attr(ll, "df", exact = TRUE)),
      AIC = .safe_numeric(stats::AIC(fit)),
      BIC = .safe_numeric(stats::BIC(fit)),
      logLik = .safe_numeric(as.numeric(ll)),
      LR_chisq = NA_real_,
      LR_df = NA_real_,
      p.value = NA_real_,
      nested_comparison = NA,
      concordance = .compare_model_concordance(fit),
      primary_term = primary$term,
      primary_log_estimate = primary$log_estimate,
      primary_estimate = primary$estimate,
      primary_pct_change = NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)

  if (isTRUE(nested) && nrow(out) > 1L) {
    for (i in 2:nrow(out)) {
      out$nested_comparison[i] <- .compare_models_nested_pair(models[[i - 1L]], models[[i]])
      lr <- 2 * (out$logLik[i] - out$logLik[i - 1L])
      df <- out$parameters[i] - out$parameters[i - 1L]
      if (is.finite(lr) && is.finite(df) && lr >= 0 && df > 0) {
        out$LR_chisq[i] <- lr
        out$LR_df[i] <- df
        out$p.value[i] <- stats::pchisq(lr, df = df, lower.tail = FALSE)
      }
    }
  }

  ref_coef <- out$primary_log_estimate[is.finite(out$primary_log_estimate)][1]
  if (!is.na(ref_coef) && is.finite(ref_coef) && !isTRUE(all.equal(ref_coef, 0))) {
    out$primary_pct_change <- ((out$primary_log_estimate - ref_coef) / abs(ref_coef)) * 100
  }

  out$best_AIC <- is.finite(out$AIC) & out$AIC == min(out$AIC, na.rm = TRUE)
  out$best_BIC <- is.finite(out$BIC) & out$BIC == min(out$BIC, na.rm = TRUE)

  out
}

#' @keywords internal
#' @noRd
.compare_models_nested_pair <- function(smaller, larger) {
  small_terms <- .compare_model_terms(smaller)
  large_terms <- .compare_model_terms(larger)

  if (is.null(small_terms) || is.null(large_terms)) {
    return(NA)
  }

  all(small_terms %in% large_terms)
}

#' @keywords internal
#' @noRd
.compare_model_terms <- function(model) {
  out <- tryCatch(attr(stats::terms(model), "term.labels"), error = function(e) NULL)
  if (is.null(out)) {
    return(NULL)
  }
  as.character(out)
}

#' @keywords internal
#' @noRd
.gtregression_terms_label <- function(vars) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (!length(vars)) {
    return("Intercept only")
  }
  paste(vars, collapse = " + ")
}

#' @keywords internal
#' @noRd
.compare_gtregression_terms <- function(x, fit) {
  terms <- character(0)
  if (.is_compare_gtregression_model(x)) {
    terms <- c(terms, x$exposures)
    if (isTRUE(x$adjusted_mode)) {
      terms <- c(terms, x$adjust_for)
    }
    terms <- c(terms, x$interaction)
  }

  if (!length(terms)) {
    terms <- .compare_model_terms(fit)
  }

  .gtregression_terms_label(terms)
}

#' @keywords internal
#' @noRd
.compare_model_terms_label <- function(model) {
  stored <- attr(model, "gtregression_model_terms", exact = TRUE)
  if (!is.null(stored) && length(stored) == 1L && nzchar(stored)) {
    return(stored)
  }

  .gtregression_terms_label(.compare_model_terms(model))
}

#' @keywords internal
#' @noRd
.compare_models_status <- function(models, table_body) {
  rows <- lapply(models, .compare_model_rows)
  have_rows <- vapply(rows, function(x) length(x) > 0L, logical(1))

  same_rows <- NA
  if (all(have_rows) && length(rows) > 1L) {
    first_rows <- rows[[1L]]
    same_rows <- all(vapply(rows[-1L], identical, logical(1), y = first_rows))
  }

  same_n <- length(unique(table_body$n[is.finite(table_body$n)])) <= 1L
  finite_events <- table_body$events[is.finite(table_body$events)]
  same_events <- !length(finite_events) || length(unique(finite_events)) <= 1L

  same_sample <- if (!is.na(same_rows)) {
    same_rows
  } else {
    same_n && same_events
  }

  if (isTRUE(same_sample)) {
    detail <- if (!is.na(same_rows)) {
      "Same analysis sample; assessed using retained model row identifiers."
    } else {
      "Same analysis sample; assessed using N and event counts because retained row identifiers were unavailable."
    }

    return(list(
      status = "Same analysis sample",
      same_sample = TRUE,
      row_check_available = all(have_rows),
      detail = detail
    ))
  }

  detail <- if (!is.na(same_rows)) {
    "Different analysis sample; retained model row identifiers differed between models."
  } else {
    "Different analysis sample; N or event counts differed between models."
  }

  list(
    status = "Different analysis sample",
    same_sample = FALSE,
    row_check_available = all(have_rows),
    detail = detail
  )
}

#' @keywords internal
#' @noRd
.compare_model_rows <- function(model) {
  mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  rn <- if (!is.null(mf)) rownames(mf) else NULL
  if (!is.null(rn) && length(rn)) {
    return(as.character(rn))
  }

  stored <- tryCatch(model$model, error = function(e) NULL)
  rn <- if (!is.null(stored)) rownames(stored) else NULL
  if (!is.null(rn) && length(rn)) {
    return(as.character(rn))
  }

  character(0)
}

#' @keywords internal
#' @noRd
.safe_loglik <- function(model) {
  tryCatch(stats::logLik(model), error = function(e) structure(NA_real_, df = NA_real_))
}

#' @keywords internal
#' @noRd
.compare_model_type <- function(model) {
  if (inherits(model, "coxph")) return("Cox regression")
  if (inherits(model, "survreg")) return("Parametric survival")
  if (inherits(model, "negbin")) return("Negative binomial")
  if (inherits(model, "glm")) {
    fam <- tryCatch(model$family$family, error = function(e) "")
    if (identical(fam, "binomial")) return("Logistic regression")
    if (identical(fam, "poisson")) return("Poisson regression")
    return("Generalized linear model")
  }
  if (inherits(model, "lm")) return("Linear regression")
  class(model)[1]
}

#' @keywords internal
#' @noRd
.compare_exponentiate <- function(model, exponentiate = NULL) {
  if (!is.null(exponentiate)) {
    return(isTRUE(exponentiate))
  }

  if (inherits(model, c("coxph", "survreg", "negbin"))) {
    return(TRUE)
  }

  if (inherits(model, "glm")) {
    fam <- tryCatch(model$family$family, error = function(e) "")
    return(fam %in% c("binomial", "poisson"))
  }

  FALSE
}

#' @keywords internal
#' @noRd
.compare_model_formula <- function(model) {
  out <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(out)) {
    return(NA_character_)
  }
  paste(deparse(out), collapse = " ")
}

#' @keywords internal
#' @noRd
.compare_model_n <- function(model) {
  if (inherits(model, "coxph") && !is.null(model$n)) {
    return(.safe_numeric(model$n))
  }
  .safe_numeric(stats::nobs(model))
}

#' @keywords internal
#' @noRd
.compare_model_events <- function(model) {
  if (inherits(model, "coxph") && !is.null(model$nevent)) {
    return(.safe_numeric(model$nevent))
  }

  if (inherits(model, "survreg")) {
    attr_events <- attr(model, "gtregression_events", exact = TRUE)
    if (!is.null(attr_events)) {
      return(.safe_numeric(attr_events))
    }
  }

  NA_real_
}

#' @keywords internal
#' @noRd
.compare_model_concordance <- function(model) {
  if (!inherits(model, "coxph")) {
    return(NA_real_)
  }
  out <- tryCatch(summary(model)$concordance[1], error = function(e) NA_real_)
  .safe_numeric(out)
}

#' @keywords internal
#' @noRd
.compare_primary_effect <- function(model, primary_exposure = NULL, exponentiate = TRUE) {
  empty <- list(term = NA_character_, log_estimate = NA_real_, estimate = NA_real_)
  if (is.null(primary_exposure) || !length(primary_exposure)) {
    return(empty)
  }

  coefs <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(coefs) || !length(coefs)) {
    return(empty)
  }

  names_coefs <- names(coefs)
  primary_exposure <- as.character(primary_exposure)[1]
  idx <- which(names_coefs == primary_exposure)

  if (!length(idx)) {
    idx <- which(startsWith(names_coefs, primary_exposure))
  }

  if (length(idx) != 1L) {
    return(empty)
  }

  log_est <- suppressWarnings(as.numeric(coefs[idx]))
  est <- if (isTRUE(exponentiate)) exp(log_est) else log_est

  list(
    term = names_coefs[idx],
    log_estimate = log_est,
    estimate = est
  )
}

#' @keywords internal
#' @noRd
.compare_models_display <- function(table_body,
                                    digits = 2,
                                    p_digits = 3,
                                    primary_exposure = NULL) {
  display <- data.frame(
    Model = table_body$model,
    Variables = table_body$model_terms,
    N = .fmt_count(table_body$n),
    Parameters = .fmt_count(table_body$parameters),
    AIC = .fmt_num_compare(table_body$AIC, digits),
    BIC = .fmt_num_compare(table_body$BIC, digits),
    `Best AIC` = ifelse(table_body$best_AIC, "Yes", "No"),
    `Best BIC` = ifelse(table_body$best_BIC, "Yes", "No"),
    `Log-likelihood` = .fmt_num_compare(table_body$logLik, digits),
    `LR chi-square` = .fmt_num_compare(table_body$LR_chisq, digits),
    df = .fmt_count(table_body$LR_df),
    `p-value` = .fmt_p_compare(table_body$p.value, p_digits),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (any(is.finite(table_body$events))) {
    display <- data.frame(
      display[, c("Model", "Variables", "N"), drop = FALSE],
      Events = .fmt_count(table_body$events),
      display[, setdiff(names(display), c("Model", "Variables", "N")), drop = FALSE],
      check.names = FALSE
    )
  }

  if (any(is.finite(table_body$concordance))) {
    display$Concordance <- .fmt_num_compare(table_body$concordance, 3)
  }

  if (!is.null(primary_exposure) && any(is.finite(table_body$primary_estimate))) {
    display$`Primary estimate` <- .fmt_num_compare(table_body$primary_estimate, digits)
    display$`Change from first` <- ifelse(
      is.finite(table_body$primary_pct_change),
      paste0(.fmt_num_compare(table_body$primary_pct_change, digits), "%"),
      ""
    )
  }

  display
}

#' @keywords internal
#' @noRd
.fmt_num_compare <- function(x, digits = 2) {
  out <- ifelse(is.finite(x), formatC(x, format = "f", digits = digits), "")
  as.character(out)
}

#' @keywords internal
#' @noRd
.fmt_count <- function(x) {
  out <- ifelse(is.finite(x), formatC(x, format = "f", digits = 0), "")
  as.character(out)
}

#' @keywords internal
#' @noRd
.fmt_p_compare <- function(x, digits = 3) {
  out <- rep("", length(x))
  ok <- is.finite(x)
  if (any(ok)) {
    threshold <- 10^(-digits)
    out[ok & x < threshold] <- paste0("<", formatC(threshold, format = "f", digits = digits))
    out[ok & x >= threshold] <- formatC(x[ok & x >= threshold], format = "f", digits = digits)
  }
  out
}

#' @keywords internal
#' @noRd
.compare_status_note <- function(comparison_status = NULL) {
  if (is.null(comparison_status) || is.null(comparison_status$status)) {
    return("Comparison status: not assessed.")
  }

  paste0(
    "Comparison status: ",
    comparison_status$status,
    ". ",
    comparison_status$detail
  )
}

#' @keywords internal
#' @noRd
.compare_nested_status_note <- function(body, nested = TRUE) {
  if (!isTRUE(nested)) {
    return("Nested-model status: not assessed because `nested = FALSE`.")
  }

  checked <- body$nested_comparison[!is.na(body$nested_comparison)]
  if (!length(checked)) {
    return("Nested-model status: not assessed for the first model.")
  }

  if (any(checked %in% FALSE)) {
    return("Nested-model status: one or more sequential model comparisons are not nested.")
  }

  "Nested-model status: sequential models are nested."
}

#' @keywords internal
#' @noRd
.compare_models_notes <- function(body,
                                  nested = TRUE,
                                  primary_exposure = NULL,
                                  comparison_status = NULL) {
  notes <- c(
    .compare_status_note(comparison_status),
    .compare_nested_status_note(body, nested = nested),
    "Compare prespecified candidate models; lower AIC or BIC indicates better relative fit among the compared models."
  )

  warnings <- character(0)

  if (!is.null(comparison_status) && isFALSE(comparison_status$same_sample)) {
    warnings <- c(
      warnings,
      paste0(
        "Different analysis sample: Models were fitted to different analysis samples because of missing data or differing inclusion criteria. ",
        "AIC, BIC, log-likelihood and likelihood-ratio statistics are presented for completeness but should not be interpreted as formal model-selection criteria across different datasets."
      )
    )
  } else {
    notes <- c(
      notes,
      "Models were fitted to the same analysis sample. AIC, BIC, log-likelihood and likelihood-ratio tests may be interpreted as formal model-comparison statistics when the models are nested as required."
    )
  }

  if (isTRUE(nested) && any(body$nested_comparison %in% FALSE)) {
    warnings <- c(
      warnings,
      "Non-nested comparison: Likelihood-ratio statistics should be interpreted with caution because one or more sequential model pairs do not appear to be nested based on their model terms."
    )
  }

  if (!is.null(primary_exposure) && any(is.finite(body$primary_estimate))) {
    notes <- c(
      notes,
      "Primary estimate change is calculated on the coefficient/log-effect scale before exponentiation and can help assess robustness across candidate models."
    )
  }

  list(
    warnings = unique(warnings[nzchar(warnings)]),
    notes = unique(notes[nzchar(notes)])
  )
}

#' @keywords internal
#' @noRd
.build_compare_models_table <- function(display,
                                        body,
                                        format = c("flextable", "gt"),
                                        theme = c("minimal"),
                                        nested = TRUE,
                                        primary_exposure = NULL,
                                        comparison_status = NULL,
                                        table_notes = NULL) {
  format <- match.arg(format, c("flextable", "gt"))
  if (is.null(table_notes)) {
    table_notes <- .compare_models_notes(
      body = body,
      nested = nested,
      primary_exposure = primary_exposure,
      comparison_status = comparison_status
    )
  }
  warnings <- table_notes$warnings
  notes <- table_notes$notes
  status_notes <- notes[grepl("^(Comparison status|Nested-model status):", notes)]
  routine_notes <- notes[!notes %in% status_notes]

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Model comparison") |>
      gt::cols_align(align = "left", columns = c("Model", "Variables")) |>
      gt::cols_align(align = "center", columns = setdiff(names(display), c("Model", "Variables"))) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      )

    if (length(status_notes)) {
      tbl <- gt::tab_source_note(tbl, gt::md(paste(status_notes, collapse = "<br>")))
    }

    if (length(warnings)) {
      warning_md <- paste(paste0("**", warnings, "**"), collapse = "<br>")
      tbl <- gt::tab_source_note(tbl, gt::md(warning_md))
    }

    if (length(routine_notes)) {
      tbl <- gt::tab_source_note(tbl, gt::md(paste(routine_notes, collapse = "<br>")))
    }

    tbl <- .compact_gt_source_notes(tbl)

    best_rows <- which(body$best_AIC | body$best_BIC)
    if (length(best_rows)) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_fill(color = "#e8f5e9"),
        locations = gt::cells_body(rows = best_rows)
      )
    }

    if ("header_shaded" %in% theme) {
      tbl <- gt::tab_options(tbl, column_labels.background.color = "#f6f8fa")
    }
    if ("zebra" %in% theme) {
      tbl <- gt::opt_row_striping(tbl)
    }
    if ("compact" %in% theme) {
      tbl <- gt::tab_options(tbl, data_row.padding = gt::px(2))
    }
    return(tbl)
  }

  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Model comparison")
  ft <- flextable::align(ft, j = c("Model", "Variables"), align = "left", part = "all")
  ft <- flextable::align(ft, j = setdiff(names(display), c("Model", "Variables")), align = "center", part = "all")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  if ("header_shaded" %in% theme) {
    ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  }
  if ("zebra" %in% theme && nrow(display) > 1L) {
    ft <- flextable::bg(ft, i = seq(1, nrow(display), by = 2), bg = "#f6f8fa", part = "body")
  }
  best_rows <- which(body$best_AIC | body$best_BIC)
  if (length(best_rows)) {
    ft <- flextable::bg(ft, i = best_rows, bg = "#e8f5e9", part = "body")
  }
  if ("compact" %in% theme) {
    ft <- flextable::padding(ft, padding = 2, part = "body")
  }
  footer_lines <- c(status_notes, warnings, routine_notes)
  ft <- flextable::add_footer_lines(ft, values = footer_lines)
  if (length(warnings)) {
    warning_rows <- length(status_notes) + seq_len(length(warnings))
    ft <- flextable::bold(ft, i = warning_rows, bold = TRUE, part = "footer")
    ft <- flextable::bg(ft, i = warning_rows, bg = "#fff3cd", part = "footer")
    ft <- flextable::color(ft, i = warning_rows, color = "#5f4700", part = "footer")
  }
  ft <- .compact_flex_footer(ft)
  note_rows <- if (length(footer_lines)) seq_len(length(footer_lines)) else integer(0)
  note_rows <- setdiff(note_rows, if (length(warnings)) warning_rows else integer(0))
  if (length(note_rows)) {
    ft <- flextable::italic(ft, i = note_rows, italic = TRUE, part = "footer")
  }
  flextable::autofit(ft)
}
