#' Causal mediation analysis
#'
#' Estimate direct, indirect, total, and proportion mediated effects from
#' regression models.
#'
#' @param data A data frame.
#' @param exposure Exposure/treatment variable. Quoted and bare names are
#'   accepted.
#' @param mediator Mediator variable. Quoted and bare names are accepted.
#' @param outcome Outcome variable. Quoted and bare names are accepted.
#' @param covariates Optional character vector of covariate names. Quoted names
#'   are recommended in scripts, and bare names are also accepted.
#' @param mediator_approach Mediator model. Currently \code{"linear"}.
#' @param outcome_approach Outcome model. One of \code{"linear"} or
#'   \code{"logit"}. Logistic mediation effects are reported on the predicted
#'   probability-difference scale.
#' @param exposure_value Exposure value used as the treatment level. If
#'   \code{NULL}, the second factor level, value 1 for 0/1 variables, or the
#'   75th percentile for continuous exposures is used.
#' @param reference_value Exposure value used as the reference level. If
#'   \code{NULL}, the first factor level, value 0 for 0/1 variables, or the
#'   25th percentile for continuous exposures is used.
#' @param sims Number of non-parametric bootstrap replicates used for confidence
#'   intervals.
#' @param conf_level Confidence level for intervals.
#' @param seed Optional random seed for reproducible bootstrap intervals.
#' @param format One of \code{"flextable"} (default) or \code{"gt"}.
#' @param theme Table theme.
#'
#' @details
#' This function is for planned mediation questions, not automatic causal
#' discovery. Causal interpretation requires the usual mediation assumptions,
#' including no unmeasured exposure-outcome, exposure-mediator, or
#' mediator-outcome confounding, correct temporal order, and suitable model
#' specification. Use a directed acyclic graph (DAG) and subject-matter
#' knowledge before interpreting the estimates causally.
#'
#' For \code{outcome_approach = "linear"}, effects are mean differences. For
#' \code{outcome_approach = "logit"}, effects are predicted probability
#' differences, not odds ratios.
#'
#' @return A list of class \code{c("gtregression", "mediation_analysis", ...)}
#' with elements:
#' \describe{
#'   \item{table}{A formatted \code{flextable} or \code{gt_tbl}.}
#'   \item{table_body}{Data frame of mediation effect estimates.}
#'   \item{table_display}{Formatted data frame used to build the table.}
#'   \item{models}{Fitted mediator and outcome models.}
#'   \item{boot}{Bootstrap replicate estimates.}
#'   \item{values}{Reference and exposure values used.}
#'   \item{variable_labels}{Named character vector of display labels.}
#'   \item{call}{Matched function call.}
#' }
#'
#' @examples
#' med <- mediation_analysis(
#'   data = data_diabetes_mediation,
#'   exposure = obesity,
#'   mediator = glucose,
#'   outcome = diabetes,
#'   covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
#'   outcome_approach = logit,
#'   sims = 50,
#'   seed = 123
#' )
#'
#' med$table
#' med$table_body
#'
#' # HTML-first output
#' mediation_analysis(
#'   data = data_diabetes_mediation,
#'   exposure = obesity,
#'   mediator = glucose,
#'   outcome = diabetes,
#'   covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
#'   outcome_approach = logit,
#'   format = gt,
#'   sims = 50,
#'   seed = 123
#' )$table
#'
#' @export
mediation_analysis <- function(data,
                               exposure,
                               mediator,
                               outcome,
                               covariates = NULL,
                               mediator_approach = "linear",
                               outcome_approach = "linear",
                               exposure_value = NULL,
                               reference_value = NULL,
                               sims = 1000,
                               conf_level = 0.95,
                               seed = NULL,
                               format = c("flextable", "gt"),
                               theme = c("minimal")) {
  exposure <- .vars_arg(substitute(exposure), env = parent.frame())
  mediator <- .vars_arg(substitute(mediator), env = parent.frame())
  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  covariates <- .vars_arg(substitute(covariates), env = parent.frame(), allow_null = TRUE)

  mediator_approach <- .choice_arg(
    substitute(mediator_approach),
    env = parent.frame(),
    choices = c("linear")
  )
  outcome_approach <- .choice_arg(
    substitute(outcome_approach),
    env = parent.frame(),
    choices = c("linear", "logit")
  )
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("flextable", "gt"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  theme <- .resolve_theme(theme)

  .validate_mediation_inputs(
    data = data,
    exposure = exposure,
    mediator = mediator,
    outcome = outcome,
    covariates = covariates,
    mediator_approach = mediator_approach,
    outcome_approach = outcome_approach,
    sims = sims,
    conf_level = conf_level
  )

  vars <- unique(c(exposure, mediator, outcome, covariates))
  df <- data[, vars, drop = FALSE]
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 10L) {
    stop("At least 10 complete observations are required.", call. = FALSE)
  }

  values <- .mediation_values(
    df[[exposure]],
    exposure_value = exposure_value,
    reference_value = reference_value
  )

  model_m <- .mediation_fit_mediator(df, exposure, mediator, covariates)
  model_y <- .mediation_fit_outcome(df, exposure, mediator, outcome, covariates, outcome_approach)
  point <- .mediation_effects(
    df = df,
    exposure = exposure,
    mediator = mediator,
    model_m = model_m,
    model_y = model_y,
    reference_value = values$reference_value,
    exposure_value = values$exposure_value,
    outcome_approach = outcome_approach
  )

  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  boot <- .mediation_bootstrap(
    df = df,
    exposure = exposure,
    mediator = mediator,
    outcome = outcome,
    covariates = covariates,
    outcome_approach = outcome_approach,
    reference_value = values$reference_value,
    exposure_value = values$exposure_value,
    sims = as.integer(sims)
  )

  table_body <- .mediation_table_body(point, boot, conf_level)
  table_display <- .mediation_table_display(table_body)
  variable_labels <- .var_label_map(data, vars)
  attr(table_display, "variable_labels") <- variable_labels

  footnotes <- .mediation_footnotes(
    outcome_approach = outcome_approach,
    exposure = .label_var(exposure, variable_labels),
    mediator = .label_var(mediator, variable_labels),
    outcome = .label_var(outcome, variable_labels),
    covariates = .mediation_covariate_labels(covariates, variable_labels),
    values = values,
    sims = sims
  )

  tbl <- if (identical(format, "gt")) {
    .build_gt_mediation(table_display, footnotes, theme)
  } else {
    .build_flex_mediation(table_display, footnotes, theme)
  }

  res <- list(
    table = tbl,
    table_body = table_body,
    table_display = table_display,
    models = list(mediator = model_m, outcome = model_y),
    boot = boot,
    values = values,
    complete_data = df,
    variable_labels = variable_labels,
    exposure = exposure,
    mediator = mediator,
    outcome = outcome,
    covariates = covariates,
    mediator_approach = mediator_approach,
    outcome_approach = outcome_approach,
    format = format,
    source = "mediation_analysis",
    call = match.call()
  )
  class(res) <- c(
    "gtregression",
    "mediation_analysis",
    if (identical(format, "gt")) "gt_mediation" else "ft_mediation",
    class(res)
  )
  res
}

.validate_mediation_inputs <- function(data,
                                       exposure,
                                       mediator,
                                       outcome,
                                       covariates,
                                       mediator_approach,
                                       outcome_approach,
                                       sims,
                                       conf_level) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  vars <- unique(c(exposure, mediator, outcome, covariates))
  missing <- setdiff(vars, names(data))
  if (length(missing)) {
    stop("Variables not found in `data`: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(unique(c(exposure, mediator, outcome))) != 3L) {
    stop("`exposure`, `mediator`, and `outcome` must be different variables.", call. = FALSE)
  }
  if (!identical(mediator_approach, "linear")) {
    stop("`mediator_approach` currently supports only `linear`.", call. = FALSE)
  }
  if (!outcome_approach %in% c("linear", "logit")) {
    stop("`outcome_approach` must be `linear` or `logit`.", call. = FALSE)
  }
  if (!is.numeric(data[[mediator]])) {
    stop("`mediator` must be numeric for `mediator_approach = linear`.", call. = FALSE)
  }
  if (identical(outcome_approach, "linear") && !is.numeric(data[[outcome]])) {
    stop("`outcome` must be numeric for `outcome_approach = linear`.", call. = FALSE)
  }
  if (identical(outcome_approach, "logit") && length(stats::na.omit(unique(data[[outcome]]))) != 2L) {
    stop("`outcome` must have exactly two non-missing values for `outcome_approach = logit`.", call. = FALSE)
  }
  if (!is.numeric(sims) || length(sims) != 1L || is.na(sims) || sims < 20) {
    stop("`sims` must be a single number greater than or equal to 20.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}

.mediation_values <- function(x, exposure_value = NULL, reference_value = NULL) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  observed <- stats::na.omit(x)
  if (is.factor(observed)) {
    lev <- levels(observed)
    if (length(lev) < 2L) {
      stop("`exposure` must have at least two observed levels.", call. = FALSE)
    }
    ref <- reference_value %||% lev[1]
    exp <- exposure_value %||% lev[2]
    return(list(reference_value = ref, exposure_value = exp))
  }

  ux <- sort(unique(observed))
  if (length(ux) < 2L) {
    stop("`exposure` must contain at least two observed values.", call. = FALSE)
  }

  if (is.numeric(observed) && all(ux %in% c(0, 1))) {
    ref <- reference_value %||% 0
    exp <- exposure_value %||% 1
  } else if (is.numeric(observed)) {
    ref <- reference_value %||% unname(stats::quantile(observed, 0.25, na.rm = TRUE, names = FALSE))
    exp <- exposure_value %||% unname(stats::quantile(observed, 0.75, na.rm = TRUE, names = FALSE))
  } else {
    ref <- reference_value %||% ux[1]
    exp <- exposure_value %||% ux[2]
  }

  list(reference_value = ref, exposure_value = exp)
}

.mediation_formula <- function(lhs, rhs) {
  stats::as.formula(paste(lhs, "~", paste(rhs, collapse = " + ")))
}

.mediation_fit_mediator <- function(df, exposure, mediator, covariates) {
  stats::lm(.mediation_formula(mediator, c(exposure, covariates)), data = df)
}

.mediation_fit_outcome <- function(df, exposure, mediator, outcome, covariates, outcome_approach) {
  f <- .mediation_formula(outcome, c(exposure, mediator, covariates))
  if (identical(outcome_approach, "logit")) {
    y <- df[[outcome]]
    if (is.factor(y)) {
      df[[outcome]] <- stats::relevel(y, ref = levels(y)[1])
    }
    return(stats::glm(f, data = df, family = stats::binomial()))
  }
  stats::lm(f, data = df)
}

.mediation_newdata <- function(df, exposure, mediator, exposure_value, mediator_value) {
  nd <- df
  nd[[exposure]] <- .mediation_cast_value(exposure_value, df[[exposure]])
  nd[[mediator]] <- mediator_value
  nd
}

.mediation_cast_value <- function(value, template) {
  if (is.factor(template)) {
    return(factor(value, levels = levels(template)))
  }
  value
}

.mediation_predict_mediator <- function(model_m, df, exposure, exposure_value) {
  nd <- df
  nd[[exposure]] <- .mediation_cast_value(exposure_value, df[[exposure]])
  stats::predict(model_m, newdata = nd, type = "response")
}

.mediation_predict_outcome <- function(model_y, nd, outcome_approach) {
  if (identical(outcome_approach, "logit")) {
    return(stats::predict(model_y, newdata = nd, type = "response"))
  }
  stats::predict(model_y, newdata = nd, type = "response")
}

.mediation_effects <- function(df,
                               exposure,
                               mediator,
                               model_m,
                               model_y,
                               reference_value,
                               exposure_value,
                               outcome_approach) {
  m_ref <- .mediation_predict_mediator(model_m, df, exposure, reference_value)
  m_exp <- .mediation_predict_mediator(model_m, df, exposure, exposure_value)

  y_exp_m_exp <- .mediation_predict_outcome(
    model_y,
    .mediation_newdata(df, exposure, mediator, exposure_value, m_exp),
    outcome_approach
  )
  y_ref_m_ref <- .mediation_predict_outcome(
    model_y,
    .mediation_newdata(df, exposure, mediator, reference_value, m_ref),
    outcome_approach
  )
  y_exp_m_ref <- .mediation_predict_outcome(
    model_y,
    .mediation_newdata(df, exposure, mediator, exposure_value, m_ref),
    outcome_approach
  )

  total <- mean(y_exp_m_exp - y_ref_m_ref, na.rm = TRUE)
  direct <- mean(y_exp_m_ref - y_ref_m_ref, na.rm = TRUE)
  indirect <- mean(y_exp_m_exp - y_exp_m_ref, na.rm = TRUE)
  prop <- if (isTRUE(all.equal(total, 0))) NA_real_ else indirect / total

  c(total = total, direct = direct, indirect = indirect, proportion = prop)
}

.mediation_bootstrap <- function(df,
                                 exposure,
                                 mediator,
                                 outcome,
                                 covariates,
                                 outcome_approach,
                                 reference_value,
                                 exposure_value,
                                 sims) {
  out <- matrix(NA_real_, nrow = sims, ncol = 4L)
  colnames(out) <- c("total", "direct", "indirect", "proportion")
  n <- nrow(df)

  for (i in seq_len(sims)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    b <- df[idx, , drop = FALSE]
    out[i, ] <- tryCatch({
      model_m <- .mediation_fit_mediator(b, exposure, mediator, covariates)
      model_y <- .mediation_fit_outcome(b, exposure, mediator, outcome, covariates, outcome_approach)
      .mediation_effects(
        df = b,
        exposure = exposure,
        mediator = mediator,
        model_m = model_m,
        model_y = model_y,
        reference_value = reference_value,
        exposure_value = exposure_value,
        outcome_approach = outcome_approach
      )
    }, error = function(e) rep(NA_real_, 4L))
  }

  as.data.frame(out)
}

.mediation_table_body <- function(point, boot, conf_level) {
  alpha <- 1 - conf_level
  q <- c(alpha / 2, 1 - alpha / 2)
  effects <- c("total", "direct", "indirect", "proportion")
  labels <- c("Total effect", "Direct effect", "Indirect effect", "Proportion mediated")
  interpretation <- c(
    "Overall exposure-outcome association",
    "Association not through the mediator",
    "Association through the mediator",
    "Share of total effect through the mediator"
  )

  ci <- t(vapply(effects, function(effect) {
    vals <- boot[[effect]]
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      return(c(NA_real_, NA_real_))
    }
    stats::quantile(vals, probs = q, na.rm = TRUE, names = FALSE)
  }, numeric(2)))

  p <- vapply(effects, function(effect) {
    vals <- boot[[effect]]
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      return(NA_real_)
    }
    min(1, 2 * min(mean(vals <= 0), mean(vals >= 0)))
  }, numeric(1))

  data.frame(
    effect = effects,
    Effect = labels,
    estimate = unname(point[effects]),
    conf.low = ci[, 1],
    conf.high = ci[, 2],
    p.value = p,
    Interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

.mediation_table_display <- function(table_body) {
  fmt_num <- function(x, digits = 3) {
    ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
  }
  fmt_p <- .fmt_p
  out <- data.frame(
    Effect = table_body$Effect,
    Estimate = fmt_num(table_body$estimate),
    `95% CI` = paste0(fmt_num(table_body$conf.low), " to ", fmt_num(table_body$conf.high)),
    `p-value` = fmt_p(table_body$p.value),
    Interpretation = table_body$Interpretation,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out
}

.mediation_covariate_labels <- function(covariates, label_map) {
  if (is.null(covariates) || !length(covariates)) {
    return(NULL)
  }
  vapply(covariates, .label_var, character(1), label_map = label_map)
}

.mediation_footnotes <- function(outcome_approach,
                                 exposure,
                                 mediator,
                                 outcome,
                                 covariates,
                                 values,
                                 sims) {
  scale_note <- if (identical(outcome_approach, "logit")) {
    "Effects are predicted probability differences from logistic outcome models."
  } else {
    "Effects are mean differences from linear outcome models."
  }
  value_note <- paste0(
    "Comparison: ",
    exposure,
    " = ",
    values$exposure_value,
    " vs ",
    values$reference_value,
    "; mediator = ",
    mediator,
    "; outcome = ",
    outcome,
    "."
  )
  adj_note <- if (!is.null(covariates) && length(covariates)) {
    paste0("Adjusted for ", paste(covariates, collapse = ", "), ".")
  } else {
    "No covariates included."
  }
  c(
    scale_note,
    paste0(value_note, " Bootstrap replicates = ", sims, "."),
    adj_note,
    "Causal interpretation requires DAG-supported no-unmeasured-confounding and correct temporal-order assumptions."
  )
}

.build_flex_mediation <- function(df, footnotes, theme) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Install 'flextable' or use format='gt'.", call. = FALSE)
  }
  ft <- flextable::flextable(df)
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::align(ft, j = "Effect", align = "left", part = "all")
  ft <- flextable::align(ft, j = setdiff(names(df), c("Effect", "Interpretation")), align = "center", part = "all")
  ft <- flextable::align(ft, j = "Interpretation", align = "left", part = "all")
  if ("header_shaded" %in% theme) {
    ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  }
  if ("compact" %in% theme) {
    ft <- flextable::padding(ft, padding = 2)
  }
  ft <- flextable::autofit(ft)
  ft <- flextable::add_footer_lines(ft, values = footnotes)
  .compact_flex_footer(ft)
}

.build_gt_mediation <- function(df, footnotes, theme) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Install 'gt' or use format='flextable'.", call. = FALSE)
  }
  tb <- gt::gt(df) |>
    gt::cols_align("left", columns = c("Effect", "Interpretation")) |>
    gt::cols_align("center", columns = c("Estimate", "95% CI", "p-value")) |>
    gt::tab_options(
      table.font.names = "system-ui",
      data_row.padding = gt::px(4),
      table.background.color = "white"
    ) |>
    gt::tab_style(gt::cell_text(weight = "bold"), gt::cells_column_labels())
  if ("header_shaded" %in% theme) {
    tb <- gt::tab_options(tb, column_labels.background.color = "#f6f8fa")
  }
  if ("zebra" %in% theme) {
    tb <- gt::opt_row_striping(tb)
  }
  if ("compact" %in% theme) {
    tb <- gt::tab_options(tb, data_row.padding = gt::px(2))
  }
  tb <- gt::tab_source_note(tb, source_note = footnotes)
  .compact_gt_source_notes(tb)
}
