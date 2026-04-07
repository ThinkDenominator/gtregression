#' Internal helper: assess one exposure-candidate pair
#' @keywords internal
#' @noRd
.identify_one_confounder <- function(data,
                                     outcome,
                                     exposure,
                                     candidate,
                                     approach = "logit",
                                     threshold = 10,
                                     emm_threshold = 10,
                                     emm_test = c("both", "estimate", "interaction"),
                                     interaction_alpha = 0.05) {
  emm_test <- match.arg(emm_test)

  .validate_outcome_by_approach(data[[outcome]], approach)

  vars_needed <- unique(c(outcome, exposure, candidate))
  dat <- stats::na.omit(data[, vars_needed, drop = FALSE])

  if (nrow(dat) == 0) {
    stop("No complete cases available for analysis.", call. = FALSE)
  }

  get_model <- function(data, formula, approach) {
    switch(
      approach,
      "logit" = glm(formula, data = data, family = binomial("logit")),
      "log-binomial" = glm(formula, data = data, family = binomial("log")),
      "poisson" = glm(formula, data = data, family = poisson("log")),
      "negbin" = MASS::glm.nb(formula, data = data),
      "linear" = lm(formula, data = data),
      "robpoisson" = risks::riskratio(
        formula = formula,
        data = data,
        approach = "robpoisson"
      ),
      stop("Unsupported approach.", call. = FALSE)
    )
  }

  extract_estimate <- function(model, exposure, approach) {
    if (inherits(model, c("glm", "lm"))) {
      coefs <- stats::coef(model)
      idx <- grep(paste0("^", exposure), names(coefs))
      if (!length(idx)) return(NA_real_)
      est <- unname(coefs[idx[1]])
      return(if (approach == "linear") est else exp(est))
    }

    if (inherits(model, "riskratio")) {
      sm <- model$summary
      if (!is.null(sm) && "term" %in% names(sm)) {
        idx <- grep(paste0("^", exposure), sm$term)
        if (!length(idx)) return(NA_real_)
        return(sm$RR[idx[1]])
      }
    }

    NA_real_
  }

  safe_fit <- function(formula, data, approach) {
    tryCatch(
      get_model(data, formula, approach),
      error = function(e) NULL
    )
  }

  # crude model
  crude_formula <- stats::as.formula(paste(outcome, "~", exposure))
  crude_model <- safe_fit(crude_formula, dat, approach)
  crude_est <- if (!is.null(crude_model)) {
    extract_estimate(crude_model, exposure, approach)
  } else {
    NA_real_
  }

  # adjusted model
  adj_formula <- stats::as.formula(
    paste(outcome, "~", paste(c(exposure, candidate), collapse = " + "))
  )
  adj_model <- safe_fit(adj_formula, dat, approach)
  adj_est <- if (!is.null(adj_model)) {
    extract_estimate(adj_model, exposure, approach)
  } else {
    NA_real_
  }

  pct_change <- if (anyNA(c(crude_est, adj_est)) || crude_est == 0) {
    NA_real_
  } else {
    abs((adj_est - crude_est) / crude_est) * 100
  }

  is_confounder <- if (!is.na(pct_change)) pct_change >= threshold else NA

  # stratified estimates
  strata_levels <- unique(stats::na.omit(dat[[candidate]]))

  stratum_estimates <- lapply(strata_levels, function(lev) {
    dsub <- dat[dat[[candidate]] == lev, , drop = FALSE]
    mod <- safe_fit(crude_formula, dsub, approach)
    est <- if (!is.null(mod)) extract_estimate(mod, exposure, approach) else NA_real_

    data.frame(
      candidate = candidate,
      stratum = as.character(lev),
      n = nrow(dsub),
      estimate = est,
      stringsAsFactors = FALSE
    )
  })

  stratum_estimates <- do.call(rbind, stratum_estimates)

  n_strata_total <- length(strata_levels)
  n_strata_estimable <- sum(!is.na(stratum_estimates$estimate))
  min_stratum_n <- if (nrow(stratum_estimates) > 0) {
    min(stratum_estimates$n, na.rm = TRUE)
  } else {
    NA_integer_
  }

  # estimate-based EMM screen
  valid_strata <- stats::na.omit(stratum_estimates$estimate)

  emm_statistic <- NA_real_
  emm_by_estimate <- NA

  if (length(valid_strata) >= 2) {
    denom <- min(abs(valid_strata))
    if (!is.na(denom) && denom > 0) {
      emm_statistic <- (max(valid_strata) - min(valid_strata)) / denom * 100
      emm_by_estimate <- emm_statistic >= emm_threshold
    }
  }

  # interaction-based EMM screen
  interaction_p <- NA_real_
  emm_by_interaction <- NA

  int_formula <- stats::as.formula(
    paste(outcome, "~", paste(c(exposure, candidate, paste0(exposure, "*", candidate)), collapse = " + "))
  )

  int_model <- safe_fit(int_formula, dat, approach)

  if (!is.null(adj_model) &&
      !is.null(int_model) &&
      inherits(adj_model, c("glm", "lm")) &&
      inherits(int_model, c("glm", "lm"))) {
    lr <- tryCatch(
      stats::anova(adj_model, int_model, test = "LRT"),
      error = function(e) NULL
    )

    if (!is.null(lr) && nrow(lr) >= 2 && "Pr(>Chi)" %in% names(lr)) {
      interaction_p <- suppressWarnings(lr$`Pr(>Chi)`[2])
      emm_by_interaction <- !is.na(interaction_p) && interaction_p < interaction_alpha
    }
  }

  # final EMM basis
  emm_basis <- "none"

  if (isTRUE(emm_by_estimate) && isTRUE(emm_by_interaction)) {
    emm_basis <- "both"
  } else if (isTRUE(emm_by_estimate)) {
    emm_basis <- "estimate"
  } else if (isTRUE(emm_by_interaction)) {
    emm_basis <- "interaction"
  }

  is_effect_modifier <- switch(
    emm_test,
    "estimate" = isTRUE(emm_by_estimate),
    "interaction" = isTRUE(emm_by_interaction),
    "both" = isTRUE(emm_by_estimate) || isTRUE(emm_by_interaction)
  )

  # decision strength
  # EMM signal strength
  emm_signal_strength <- "none"
  if (isTRUE(emm_by_estimate) && isTRUE(emm_by_interaction)) {
    emm_signal_strength <- "strong"
  } else if (isTRUE(emm_by_estimate) || isTRUE(emm_by_interaction)) {
    emm_signal_strength <- "possible"
  }

  # final overall decision
  # final overall decision
  if (isTRUE(is_effect_modifier)) {
    decision <- "effect_modification"
    decision_strength <- emm_signal_strength

    reason <- switch(
      emm_basis,
      "both" = "Stratum-specific estimates differ and the interaction test also supports effect modification.",
      "estimate" = "Stratum-specific estimates differ meaningfully, although the interaction test is not strongly significant.",
      "interaction" = "Interaction test supports effect modification, although estimate spread is less marked.",
      "Effect modification detected."
    )

    recommendation <- paste0(
      "Report stratum-specific effects for ", exposure,
      " across levels of ", candidate, "."
    )

    model_build_recommendation <- "include_interaction"

  } else if (isTRUE(is_confounder)) {
    decision <- "confounding"

    decision_strength <- if (emm_signal_strength == "none") {
      "strong"
    } else {
      "possible"
    }

    reason <- paste0(
      "No strong effect modification detected under the chosen EMM rule; crude vs adjusted estimate changed by ",
      round(pct_change, 1), "%."
    )

    recommendation <- paste0("Adjust for ", candidate, ".")
    model_build_recommendation <- "adjust_only"

  } else {
    decision <- "no_role"
    decision_strength <- "none"

    reason <- "No strong effect modification and little change between crude and adjusted estimates."

    recommendation <- paste0(
      "No clear statistical evidence to include ", candidate,
      " as a confounder."
    )

    model_build_recommendation <- "exclude"
  }

  list(
    exposure = exposure,
    candidate = candidate,
    crude = crude_est,
    adjusted = adj_est,
    percent_change = pct_change,
    is_confounder = is_confounder,
    stratum_estimates = stratum_estimates,
    n_strata_total = n_strata_total,
    n_strata_estimable = n_strata_estimable,
    min_stratum_n = min_stratum_n,
    emm_basis = emm_basis,
    emm_signal_strength = emm_signal_strength,
    emm_statistic = emm_statistic,
    is_effect_modifier = is_effect_modifier,
    interaction_p = interaction_p,
    decision = decision,
    decision_strength = decision_strength,
    reason = reason,
    recommendation = recommendation,
    model_build_recommendation = model_build_recommendation
  )
}

#' Identify confounders and effect modifiers
#'
#' Assesses whether one or more candidate variables act as confounders or
#' effect modifiers for one or more exposures.
#'
#' The function first assesses possible effect modification using
#' stratum-specific estimates and/or an interaction test. If no important
#' effect modification is detected, it then assesses confounding using the
#' change-in-estimate approach.
#'
#' @param data A data frame.
#' @param outcome Outcome variable name.
#' @param exposure Exposure variable name(s). Can be a character scalar or vector.
#' @param potential_confounder Candidate confounder/effect-modifier variable
#'   name(s). Can be a character scalar or vector.
#' @param approach Regression approach. One of \code{"logit"},
#'   \code{"log-binomial"}, \code{"poisson"}, \code{"robpoisson"},
#'   \code{"linear"}, or \code{"negbin"}.
#' @param threshold Percent change threshold for confounding assessment.
#' @param emm_threshold Threshold for relative spread in stratum-specific
#'   estimates when using estimate-based effect-modification screening.
#' @param emm_test One of \code{"interaction"}, \code{"both"}, or
#'   \code{"estimate"}.
#' @param interaction_alpha Alpha threshold for interaction p-values.
#'
#' @return
#' If a single exposure-candidate pair is supplied, returns a detailed list.
#'
#' If multiple combinations are supplied, returns a list with:
#' \describe{
#'   \item{summary}{A tibble with one row per exposure-candidate combination.}
#'   \item{details}{A named list of detailed results for each combination.}
#' }
#'
#' @export
identify_confounder <- function(data,
                                outcome,
                                exposure,
                                potential_confounder,
                                approach = "logit",
                                threshold = 10,
                                emm_threshold = 10,
                                emm_test = c("interaction", "both", "estimate"),
                                interaction_alpha = 0.05,
                                format = c("gt", "flextable"),
                                theme = c("minimal")) {

  emm_test <- match.arg(emm_test)
  format <- match.arg(format)
  theme <- .resolve_theme(theme)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(outcome) || length(outcome) != 1L) {
    stop("`outcome` must be a single character string.", call. = FALSE)
  }

  if (!outcome %in% names(data)) {
    stop("Outcome variable not found in the dataset.", call. = FALSE)
  }

  if (!is.character(exposure) || length(exposure) < 1L) {
    stop("`exposure` must be a character vector with at least one variable.", call. = FALSE)
  }

  if (!is.character(potential_confounder) || length(potential_confounder) < 1L) {
    stop(
      "`potential_confounder` must be a character vector with at least one variable.",
      call. = FALSE
    )
  }

  if (!all(exposure %in% names(data))) {
    stop("One or more exposure variables were not found in the dataset.", call. = FALSE)
  }

  if (!all(potential_confounder %in% names(data))) {
    stop(
      "One or more potential confounder variables were not found in the dataset.",
      call. = FALSE
    )
  }

  combos <- expand.grid(
    exposure = exposure,
    candidate = potential_confounder,
    stringsAsFactors = FALSE
  )

  details <- vector("list", nrow(combos))
  names(details) <- paste0(combos$exposure, "__", combos$candidate)

  for (i in seq_len(nrow(combos))) {
    exp_i <- combos$exposure[i]
    cand_i <- combos$candidate[i]

    if (identical(exp_i, cand_i)) {
      details[[i]] <- list(
        exposure = exp_i,
        candidate = cand_i,
        crude = NA_real_,
        adjusted = NA_real_,
        percent_change = NA_real_,
        is_confounder = NA,
        stratum_estimates = NULL,
        n_strata_total = NA_integer_,
        n_strata_estimable = NA_integer_,
        min_stratum_n = NA_integer_,
        emm_statistic = NA_real_,
        emm_basis = "none",
        is_effect_modifier = NA,
        interaction_p = NA_real_,
        decision = "invalid",
        decision_strength = "none",
        reason = "Exposure and candidate variable are the same.",
        recommendation = "Choose different variables for exposure and candidate.",
        model_build_recommendation = "exclude"
      )
      next
    }

    details[[i]] <- .identify_one_confounder(
      data = data,
      outcome = outcome,
      exposure = exp_i,
      candidate = cand_i,
      approach = approach,
      threshold = threshold,
      emm_threshold = emm_threshold,
      emm_test = emm_test,
      interaction_alpha = interaction_alpha
    )
  }

  summary_tbl <- dplyr::bind_rows(lapply(details, function(x) {
    tibble::tibble(
      exposure = x$exposure,
      candidate = x$candidate,
      decision = x$decision,
      decision_strength = x$decision_strength,
      reason = x$reason,
      recommendation = x$recommendation,
      emm_basis = x$emm_basis,
      emm_signal_strength = x$emm_signal_strength,
      interaction_p = round(x$interaction_p, 3),
      emm_statistic = round(x$emm_statistic, 1),
      crude_est = round(x$crude, 3),
      adjusted_est = round(x$adjusted, 3),
      percent_change = round(x$percent_change, 2),
      is_confounder = x$is_confounder,
      is_effect_modifier = x$is_effect_modifier,
      n_strata_total = x$n_strata_total,
      n_strata_estimable = x$n_strata_estimable,
      min_stratum_n = x$min_stratum_n
    )
  }))

  summary_table <- .build_identify_confounder_table(
    summary_tbl = summary_tbl,
    format = format,
    theme = theme
  )

  if (nrow(combos) == 1L) {
    res <- details[[1]]
    res$table <- summary_table

    message("\n------------------------------------------------------------")
    message("Exposure:            ", res$exposure)
    message("Candidate variable:  ", res$candidate)
    message("Crude Estimate:      ", format(round(res$crude, 3), nsmall = 3))
    message("Adjusted Estimate:   ", format(round(res$adjusted, 3), nsmall = 3))
    message("% Change from Crude: ", format(round(res$percent_change, 2), nsmall = 2), "%")
    message("Interaction p-value: ", ifelse(
      is.na(res$interaction_p), "NA",
      format(round(res$interaction_p, 3), nsmall = 3)
    ))
    message("EMM basis:           ", res$emm_basis)
    message("Decision:            ", res$decision)
    message("Decision strength:   ", res$decision_strength)
    message("Reason:              ", res$reason)
    message("Recommendation:      ", res$recommendation)
    message("------------------------------------------------------------\n")

    return(invisible(res))
  }

  message("\nSummary of assessed exposure-candidate combinations:\n")
  print(summary_tbl)

  message(
    "\nNotes:\n",
    "* Effect modification is assessed first.\n",
    "* Confounding is assessed only when no important effect modification is detected.\n",
    "* Use DAGs and subject-matter knowledge to support interpretation.\n"
  )

  out <- list(
    summary = summary_tbl,
    table = summary_table,
    details = details,
    format = format,
    source = "identify_confounder"
  )

  class(out) <- c("identify_confounder_result", class(out))
  invisible(out)
}

#' Build identify_confounder summary table
#' @keywords internal
#' @noRd
.build_identify_confounder_table <- function(summary_tbl,
                                             format = c("gt", "flextable"),
                                             theme = c("minimal")) {
  format <- match.arg(format)
  theme <- .resolve_theme(theme)

  df <- summary_tbl

  df_display <- df |>
    dplyr::mutate(
      crude_est = dplyr::if_else(
        is.na(.data$crude_est),
        "",
        formatC(.data$crude_est, digits = 3, format = "f")
      ),
      adjusted_est = dplyr::if_else(
        is.na(.data$adjusted_est),
        "",
        formatC(.data$adjusted_est, digits = 3, format = "f")
      ),
      interaction_p = dplyr::if_else(
        is.na(.data$interaction_p),
        "",
        formatC(.data$interaction_p, digits = 3, format = "f")
      ),
      emm_statistic = dplyr::if_else(
        is.na(.data$emm_statistic),
        "",
        formatC(.data$emm_statistic, digits = 1, format = "f")
      ),
      percent_change = dplyr::if_else(
        is.na(.data$percent_change),
        "",
        formatC(.data$percent_change, digits = 2, format = "f")
      ),
      is_confounder = dplyr::case_when(
        is.na(.data$is_confounder) ~ "",
        .data$is_confounder ~ "Yes",
        TRUE ~ "No"
      ),
      is_effect_modifier = dplyr::case_when(
        is.na(.data$is_effect_modifier) ~ "",
        .data$is_effect_modifier ~ "Yes",
        TRUE ~ "No"
      ),
      n_strata_total = dplyr::if_else(
        is.na(.data$n_strata_total),
        "",
        as.character(.data$n_strata_total)
      ),
      n_strata_estimable = dplyr::if_else(
        is.na(.data$n_strata_estimable),
        "",
        as.character(.data$n_strata_estimable)
      ),
      min_stratum_n = dplyr::if_else(
        is.na(.data$min_stratum_n),
        "",
        as.character(.data$min_stratum_n)
      )
    ) |>
    dplyr::select(
      "Exposure" = .data$exposure,
      "Candidate" = .data$candidate,
      "Decision" = .data$decision,
      "Strength" = .data$decision_strength,
      "Reason" = .data$reason,
      "Recommendation" = .data$recommendation,
      "EMM basis" = .data$emm_basis,
      "EMM signal" = .data$emm_signal_strength,
      "Interaction p" = .data$interaction_p,
      "EMM statistic" = .data$emm_statistic,
      "Crude estimate" = .data$crude_est,
      "Adjusted estimate" = .data$adjusted_est,
      "% change" = .data$percent_change,
      "Confounder?" = .data$is_confounder,
      "Effect modifier?" = .data$is_effect_modifier,
      "No. strata" = .data$n_strata_total,
      "Estimable strata" = .data$n_strata_estimable,
      "Min stratum n" = .data$min_stratum_n
    )

  if (format == "gt") {
    tbl <- gt::gt(df_display) |>
      gt::cols_align(align = "left", columns = c("Exposure", "Candidate", "Decision",
                                                 "Strength", "Reason", "Recommendation",
                                                 "EMM basis", "EMM signal")) |>
      gt::cols_align(align = "center",
                     columns = setdiff(names(df_display),
                                       c("Exposure", "Candidate", "Decision",
                                         "Strength", "Reason", "Recommendation",
                                         "EMM basis", "EMM signal"))) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      )

    if ("zebra" %in% theme) {
      tbl <- gt::opt_row_striping(tbl)
    }

    if ("compact" %in% theme) {
      tbl <- gt::tab_options(tbl, data_row.padding = gt::px(2))
    }

    if ("header_shaded" %in% theme) {
      tbl <- gt::tab_options(tbl, column_labels.background.color = "#f6f8fa")
    }

    if ("lines" %in% theme) {
      tbl <- gt::tab_options(
        tbl,
        table.border.top.style = "solid",
        table.border.top.color = "#DADADA",
        table.border.bottom.style = "solid",
        table.border.bottom.color = "#DADADA"
      )
    }

    return(tbl)
  }

  ft <- flextable::flextable(df_display)
  ft <- flextable::align(
    ft,
    j = c("Exposure", "Candidate", "Decision", "Strength", "Reason",
          "Recommendation", "EMM basis", "EMM signal"),
    align = "left",
    part = "all"
  )
  ft <- flextable::align(
    ft,
    j = setdiff(names(df_display),
                c("Exposure", "Candidate", "Decision", "Strength", "Reason",
                  "Recommendation", "EMM basis", "EMM signal")),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)

  if ("zebra" %in% theme) {
    ft <- flextable::bg(ft, i = seq(1, nrow(df_display), by = 2),
                        bg = "#f6f8fa", part = "body")
  }

  if ("compact" %in% theme) {
    ft <- flextable::padding(ft, padding = 2, part = "body")
  }

  if ("header_shaded" %in% theme) {
    ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  }

  ft <- flextable::autofit(ft)
  ft
}
