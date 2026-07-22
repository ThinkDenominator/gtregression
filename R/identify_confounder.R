#' Internal helper: assess one exposure-candidate pair
#' @keywords internal
#' @noRd
.identify_one_confounder <- function(data,
                                     outcome,
                                     exposure,
                                     candidate,
                                     approach = "logit",
                                     method = "change",
                                     threshold = 10,
                                     emm_threshold = 10,
                                     emm_test = c("both", "estimate", "interaction"),
                                     interaction_alpha = 0.05) {
  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","logbinomial","poisson","robpoisson","negbin","linear")
  )
  approach <- .normalize_approach(approach)
  method <- .choice_arg(substitute(method), env = parent.frame(), choices = c("change", "mh", "both"))
  emm_test <- .choice_arg(substitute(emm_test), env = parent.frame(), choices = c("both", "estimate", "interaction"))
  method <- match.arg(method, c("change", "mh", "both"))
  emm_test <- match.arg(emm_test, c("both", "estimate", "interaction"))

  .validate_outcome_by_approach(data[[outcome]], approach)

  vars_needed <- unique(c(outcome, exposure, candidate))
  dat <- stats::na.omit(data[, vars_needed, drop = FALSE])

  if (nrow(dat) == 0) {
    stop("No complete cases available for analysis.", call. = FALSE)
  }

  get_model <- function(data, formula, approach) {
    if (approach == "robpoisson") {
      response <- all.vars(formula)[[1]]
      if (is.factor(data[[response]]) || is.character(data[[response]])) {
        lev <- .binary_levels(data[[response]])
        if (length(lev) == 2L) {
          data[[response]] <- .as_binary01(data[[response]], lev)
        }
      }
    }

    switch(
      approach,
      "logit" = glm(formula, data = data, family = binomial("logit")),
      "logbinomial" = glm(formula, data = data, family = binomial("log")),
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

  mh <- .confounder_mh_estimate(
    data = dat,
    outcome = outcome,
    exposure = exposure,
    candidate = candidate,
    approach = approach
  )

  pct_change_mh <- if (anyNA(c(crude_est, mh$estimate)) || crude_est == 0) {
    NA_real_
  } else {
    abs((mh$estimate - crude_est) / crude_est) * 100
  }

  is_confounder_model <- if (!is.na(pct_change)) pct_change >= threshold else NA
  is_confounder_mh <- if (!is.na(pct_change_mh)) pct_change_mh >= threshold else NA
  is_confounder <- switch(
    method,
    "change" = is_confounder_model,
    "mh" = is_confounder_mh,
    "both" = if (isTRUE(is_confounder_model) || isTRUE(is_confounder_mh)) {
      TRUE
    } else if (isFALSE(is_confounder_model) && isFALSE(is_confounder_mh)) {
      FALSE
    } else {
      NA
    }
  )

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
      suppressWarnings(stats::anova(adj_model, int_model, test = "LRT")),
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
    decision <- "confounder"

    decision_strength <- if (emm_signal_strength == "none") {
      "strong"
    } else {
      "possible"
    }

    reason <- .confounder_reason(
      method = method,
      pct_change = pct_change,
      pct_change_mh = pct_change_mh,
      mh_status = mh$status
    )

    recommendation <- paste0("Adjust for ", candidate, ".")
    model_build_recommendation <- "adjust_only"

  } else {
    decision <- if ((method == "mh" && is.na(pct_change_mh)) ||
                    (all(is.na(c(pct_change, pct_change_mh))) &&
                     !isTRUE(is_effect_modifier))) {
      "not_estimable"
    } else {
      "no_evidence"
    }
    decision_strength <- "none"

    reason <- .no_confounder_reason(
      method = method,
      pct_change = pct_change,
      pct_change_mh = pct_change_mh,
      mh_status = mh$status
    )

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
    mh_estimate = mh$estimate,
    mh_method = mh$method,
    mh_status = mh$status,
    percent_change = pct_change,
    percent_change_model = pct_change,
    percent_change_mh = pct_change_mh,
    is_confounder = is_confounder,
    is_confounder_model = is_confounder_model,
    is_confounder_mh = is_confounder_mh,
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

.binary_levels <- function(x) {
  x <- x[!is.na(x)]
  if (is.factor(x)) {
    levels(droplevels(x))
  } else {
    sort(unique(x))
  }
}

.as_binary01 <- function(x, levels) {
  ifelse(x == levels[[2]], 1L, 0L)
}

.confounder_mh_estimate <- function(data,
                                    outcome,
                                    exposure,
                                    candidate,
                                    approach) {
  if (!approach %in% c("logit", "logbinomial", "robpoisson")) {
    return(list(
      estimate = NA_real_,
      method = NA_character_,
      status = "MH is available only for binary outcome models."
    ))
  }

  y_levels <- .binary_levels(data[[outcome]])
  x_levels <- .binary_levels(data[[exposure]])
  z_levels <- .binary_levels(data[[candidate]])

  if (length(y_levels) != 2L || length(x_levels) != 2L || length(z_levels) < 2L) {
    return(list(
      estimate = NA_real_,
      method = NA_character_,
      status = "MH requires binary outcome, binary exposure, and categorical strata."
    ))
  }

  y <- .as_binary01(data[[outcome]], y_levels)
  x <- .as_binary01(data[[exposure]], x_levels)
  z <- data[[candidate]]
  keep <- !is.na(y) & !is.na(x) & !is.na(z)
  y <- y[keep]
  x <- x[keep]
  z <- droplevels(factor(z[keep]))

  strata <- levels(z)
  if (length(strata) < 2L) {
    return(list(
      estimate = NA_real_,
      method = NA_character_,
      status = "MH requires at least two non-empty strata."
    ))
  }

  counts <- lapply(strata, function(s) {
    idx <- z == s
    a <- sum(y[idx] == 1L & x[idx] == 1L)
    b <- sum(y[idx] == 0L & x[idx] == 1L)
    c <- sum(y[idx] == 1L & x[idx] == 0L)
    d <- sum(y[idx] == 0L & x[idx] == 0L)
    c(a = a, b = b, c = c, d = d, n = a + b + c + d)
  })

  count_mat <- do.call(rbind, counts)
  count_mat <- count_mat[count_mat[, "n"] > 0, , drop = FALSE]
  if (nrow(count_mat) < 2L) {
    return(list(
      estimate = NA_real_,
      method = NA_character_,
      status = "MH requires at least two non-empty strata."
    ))
  }

  if (approach == "logit") {
    numerator <- sum(count_mat[, "a"] * count_mat[, "d"] / count_mat[, "n"])
    denominator <- sum(count_mat[, "b"] * count_mat[, "c"] / count_mat[, "n"])
    method <- "mh_or"
  } else {
    numerator <- sum(count_mat[, "a"] * (count_mat[, "c"] + count_mat[, "d"]) /
                       count_mat[, "n"])
    denominator <- sum(count_mat[, "c"] * (count_mat[, "a"] + count_mat[, "b"]) /
                         count_mat[, "n"])
    method <- "mh_rr"
  }

  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) {
    return(list(
      estimate = NA_real_,
      method = method,
      status = "MH estimate could not be calculated because of sparse strata."
    ))
  }

  list(
    estimate = unname(numerator / denominator),
    method = method,
    status = "calculated"
  )
}

.confounder_reason <- function(method, pct_change, pct_change_mh, mh_status) {
  if (method == "change") {
    return(paste0(
      "Crude vs adjusted estimate changed by ",
      round(pct_change, 1), "%."
    ))
  }
  if (method == "mh") {
    return(paste0(
      "Crude vs Mantel-Haenszel estimate changed by ",
      round(pct_change_mh, 1), "%."
    ))
  }
  paste0(
    "Confounding detected by change-in-estimate and/or Mantel-Haenszel method",
    if (!is.na(pct_change)) paste0("; model change = ", round(pct_change, 1), "%") else "",
    if (!is.na(pct_change_mh)) paste0("; MH change = ", round(pct_change_mh, 1), "%") else "",
    if (!identical(mh_status, "calculated")) paste0("; MH status: ", mh_status) else "",
    "."
  )
}

.no_confounder_reason <- function(method, pct_change, pct_change_mh, mh_status) {
  if (method == "change") {
    if (is.na(pct_change)) return("Model-based change-in-estimate could not be calculated.")
    return("No strong effect modification and little change between crude and adjusted estimates.")
  }
  if (method == "mh") {
    if (is.na(pct_change_mh)) return(mh_status)
    return("No strong effect modification and little change between crude and Mantel-Haenszel estimates.")
  }
  paste0(
    "No strong effect modification and no important change by the selected methods",
    if (!identical(mh_status, "calculated")) paste0("; MH status: ", mh_status) else "",
    "."
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
#' selected method.
#'
#' This is a screening aid for viewing and organising results. Confounding and
#' effect modification should be interpreted using subject-matter knowledge,
#' study design, and causal diagrams such as DAGs. Automated change-in-estimate
#' and interaction checks should not be used as the sole basis for model
#' adjustment.
#'
#' Use this function when you want to screen one or more candidate variables and
#' organise crude, adjusted, Mantel-Haenszel, and effect-modification signals in
#' one place. For a focused comparison of models with and without a planned
#' exposure-by-modifier interaction term, use \code{\link{interaction_models}()}.
#'
#' @param data A data frame.
#' @param outcome Outcome variable name.
#' @param exposure Exposure variable name(s). Can be a character scalar or vector.
#' @param potential_confounder Candidate confounder/effect-modifier variable
#'   name(s). Can be a character scalar or vector.
#' @param approach Regression approach. One of \code{"logit"},
#'   \code{"logbinomial"}, \code{"poisson"}, \code{"robpoisson"},
#'   \code{"linear"}, or \code{"negbin"}.
#' @param method Confounding assessment method. One of \code{"change"},
#'   \code{"mh"}, or \code{"both"}. \code{"change"} compares crude and
#'   adjusted model estimates. \code{"mh"} compares crude and
#'   Mantel-Haenszel pooled estimates and is available for binary outcome,
#'   binary exposure, and categorical strata. \code{"both"} uses either method.
#' @param threshold Percent change threshold for confounding assessment.
#' @param emm_threshold Threshold for relative spread in stratum-specific
#'   estimates when using estimate-based effect-modification screening.
#' @param emm_test One of \code{"interaction"}, \code{"both"}, or
#'   \code{"estimate"}.
#' @param interaction_alpha Alpha threshold for interaction p-values.
#' @param format Output table format. One of \code{"gt"} or
#'   \code{"flextable"}.
#' @param theme Table theme preset or primitives.
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
#' @seealso \code{\link{interaction_models}()} for focused model comparison of
#'   a planned interaction term.
#'
#' @export
identify_confounder <- function(data,
                                outcome,
                                exposure,
                                potential_confounder,
                                approach = "logit",
                                method = "change",
                                threshold = 10,
                                emm_threshold = 10,
                                emm_test = c("interaction", "both", "estimate"),
                                interaction_alpha = 0.05,
                                format = c("gt", "flextable"),
                                theme = c("minimal")) {

  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","logbinomial","poisson","robpoisson","negbin","linear")
  )
  approach <- .normalize_approach(approach)
  .validate_approach(approach, context = "identify_confounder")
  method <- .choice_arg(substitute(method), env = parent.frame(), choices = c("change", "mh", "both"))
  emm_test <- .choice_arg(substitute(emm_test), env = parent.frame(), choices = c("interaction", "both", "estimate"))
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("gt", "flextable"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  method <- match.arg(method, c("change", "mh", "both"))
  emm_test <- match.arg(emm_test, c("interaction", "both", "estimate"))
  format <- match.arg(format, c("gt", "flextable"))
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
        mh_estimate = NA_real_,
        mh_method = NA_character_,
        mh_status = "Exposure and candidate variable are the same.",
        percent_change = NA_real_,
        percent_change_model = NA_real_,
        percent_change_mh = NA_real_,
        is_confounder = NA,
        is_confounder_model = NA,
        is_confounder_mh = NA,
        stratum_estimates = NULL,
        n_strata_total = NA_integer_,
        n_strata_estimable = NA_integer_,
        min_stratum_n = NA_integer_,
        emm_statistic = NA_real_,
        emm_basis = "none",
        emm_signal_strength = "none",
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
      method = method,
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
      crude_est = round(x$crude, 3),
      adjusted_est = round(x$adjusted, 3),
      mh_est = round(x$mh_estimate, 3),
      percent_change = round(x$percent_change_model, 2),
      percent_change_model = round(x$percent_change_model, 2),
      percent_change_mh = round(x$percent_change_mh, 2),
      is_confounder = x$is_confounder,
      interaction_p = round(x$interaction_p, 3),
      is_effect_modifier = x$is_effect_modifier,
      decision = x$decision,
      recommendation = x$recommendation
    )
  }))

  summary_table <- .build_identify_confounder_table(
    summary_tbl = summary_tbl,
    format = format,
    theme = theme
  )

  if (nrow(combos) == 1L) {
    res <- details[[1]]
    res$summary <- summary_tbl
    res$table <- summary_table

    return(invisible(res))
  }

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
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("gt", "flextable"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format, c("gt", "flextable"))
  theme <- .resolve_theme(theme)

  df <- summary_tbl
  caveat <- paste(
    "Screening aid only; use DAGs, subject-matter knowledge, and study design",
    "to decide confounding and effect modification."
  )

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
      mh_est = dplyr::if_else(
        is.na(.data$mh_est),
        "",
        formatC(.data$mh_est, digits = 3, format = "f")
      ),
      interaction_p = dplyr::if_else(
        is.na(.data$interaction_p),
        "",
        formatC(.data$interaction_p, digits = 3, format = "f")
      ),
      percent_change = dplyr::if_else(
        is.na(.data$percent_change),
        "",
        formatC(.data$percent_change, digits = 2, format = "f")
      ),
      percent_change_model = dplyr::if_else(
        is.na(.data$percent_change_model),
        "",
        formatC(.data$percent_change_model, digits = 2, format = "f")
      ),
      percent_change_mh = dplyr::if_else(
        is.na(.data$percent_change_mh),
        "",
        formatC(.data$percent_change_mh, digits = 2, format = "f")
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
      decision = dplyr::recode(
        .data$decision,
        effect_modification = "Effect modifier",
        confounder = "Confounder",
        no_evidence = "No evidence",
        not_estimable = "Not estimable",
        invalid = "Invalid",
        .default = .data$decision
      )
    ) |>
    dplyr::transmute(
      "Exposure" = .data$exposure,
      "Candidate" = .data$candidate,
      "Crude estimate" = .data$crude_est,
      "Adjusted estimate" = .data$adjusted_est,
      "MH estimate" = .data$mh_est,
      "% change model" = .data$percent_change_model,
      "% change MH" = .data$percent_change_mh,
      "Confounder?" = .data$is_confounder,
      "Interaction p" = .data$interaction_p,
      "Effect modifier?" = .data$is_effect_modifier,
      "Decision" = .data$decision,
      "Recommendation" = .data$recommendation
    )

  if (format == "gt") {
    tbl <- gt::gt(df_display) |>
      gt::cols_align(align = "left", columns = c("Exposure", "Candidate",
                                                 "Decision", "Recommendation")) |>
      gt::cols_align(align = "center",
                     columns = setdiff(names(df_display),
                                       c("Exposure", "Candidate", "Decision",
                                         "Recommendation"))) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_source_note(gt::md(caveat))

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
    j = c("Exposure", "Candidate", "Decision", "Recommendation"),
    align = "left",
    part = "all"
  )
  ft <- flextable::align(
    ft,
    j = setdiff(names(df_display),
                c("Exposure", "Candidate", "Decision", "Recommendation")),
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

  ft <- flextable::add_footer_lines(ft, values = caveat)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  ft <- flextable::autofit(ft)
  ft
}
