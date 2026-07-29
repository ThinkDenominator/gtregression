#' Compare Prespecified Regression Models
#'
#' Compare gtregression candidate models side by side using model-fit statistics.
#' This is intended for transparent model comparison after you have already
#' fitted the candidate models with functions such as \code{multi_reg()},
#' \code{cox_reg()}, or \code{surv_reg()}.
#'
#' @param ... Two or more gtregression model objects, fitted model objects, or
#'   one list containing them. Recommended inputs are outputs from
#'   \code{multi_reg()}, \code{cox_reg()}, or \code{surv_reg()}. Raw fitted
#'   models with standard \code{AIC()}, \code{BIC()}, \code{logLik()}, and
#'   \code{nobs()} methods are also accepted for advanced workflows.
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
#' }
#'
#' @details
#' \code{compare_models()} does not refit models and does not perform hidden
#' complete-case filtering. When supplied with gtregression outputs, it extracts
#' the fitted model stored in the object's \code{models} element. The reported
#' N, event counts, and fit statistics therefore come from the model already
#' fitted by \code{multi_reg()}, \code{cox_reg()}, or \code{surv_reg()}. This
#' keeps model comparison separate from model selection: compare candidate
#' models first, then choose the final model using clinical, epidemiological,
#' and statistical judgement.
#'
#' Likelihood-ratio p-values are meaningful only for nested models fitted to
#' the same analysis sample. If the models are not nested, or if model sample
#' sizes differ, use AIC/BIC and subject-matter reasoning instead.
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
  models <- .compare_models_list(...)
  if (length(models) < 2L) {
    stop("Supply at least two fitted models to compare.", call. = FALSE)
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

  model_names <- .compare_model_names(models, model_names)
  names(models) <- model_names

  table_body <- .compare_models_body(
    models = models,
    nested = nested,
    primary_exposure = primary_exposure,
    exponentiate = exponentiate
  )

  table_display <- .compare_models_display(
    table_body,
    digits = digits,
    p_digits = p_digits,
    primary_exposure = primary_exposure
  )

  out <- list(
    table = NULL,
    table_body = table_body,
    table_display = table_display,
    models = models,
    model_names = model_names,
    primary_exposure = primary_exposure,
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
      primary_exposure = primary_exposure
    )
  }

  class(out) <- c("gtregression", "compare_models")
  out
}

#' @keywords internal
#' @noRd
.compare_models_list <- function(...) {
  inputs <- list(...)
  if (
    length(inputs) == 1L &&
      is.list(inputs[[1L]]) &&
      !.is_compare_fitted_model(inputs[[1L]]) &&
      !.is_compare_gtregression_model(inputs[[1L]])
  ) {
    inputs <- inputs[[1L]]
  }

  if (!length(inputs)) {
    stop("Supply gtregression model objects or fitted model objects to compare.", call. = FALSE)
  }

  models <- lapply(inputs, .compare_extract_model)

  invalid <- vapply(models, function(x) !.is_compare_fitted_model(x), logical(1))
  if (any(invalid)) {
    stop(
      "All inputs must be gtregression model objects or fitted model objects with a logLik() method.",
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
          "For cox_reg(), surv_reg(), or multi_reg(adjust_for = ...), supply one exposure per candidate model."
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
.compare_model_names <- function(models, model_names = NULL) {
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
      n = .compare_model_n(fit),
      events = .compare_model_events(fit),
      parameters = .safe_numeric(attr(ll, "df", exact = TRUE)),
      AIC = .safe_numeric(stats::AIC(fit)),
      BIC = .safe_numeric(stats::BIC(fit)),
      logLik = .safe_numeric(as.numeric(ll)),
      LR_chisq = NA_real_,
      LR_df = NA_real_,
      p.value = NA_real_,
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
      display[, c("Model", "N"), drop = FALSE],
      Events = .fmt_count(table_body$events),
      display[, setdiff(names(display), c("Model", "N")), drop = FALSE],
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
.build_compare_models_table <- function(display,
                                        body,
                                        format = c("flextable", "gt"),
                                        theme = c("minimal"),
                                        nested = TRUE,
                                        primary_exposure = NULL) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- c(
    "Compare prespecified candidate models; lower AIC or BIC indicates better relative fit among the compared models.",
    "Likelihood-ratio p-values are sequential and should be interpreted only for nested models fitted to the same analysis sample."
  )

  if (!is.null(primary_exposure) && any(is.finite(body$primary_estimate))) {
    note <- c(
      note,
      "Primary estimate change is calculated on the coefficient/log-effect scale before exponentiation."
    )
  }

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Model comparison") |>
      gt::cols_align(align = "left", columns = "Model") |>
      gt::cols_align(align = "center", columns = setdiff(names(display), "Model")) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_source_note(gt::md(paste(note, collapse = "<br>"))) |>
      .compact_gt_source_notes()

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
  ft <- flextable::align(ft, j = "Model", align = "left", part = "all")
  ft <- flextable::align(ft, j = setdiff(names(display), "Model"), align = "center", part = "all")
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
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- .compact_flex_footer(ft)
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
