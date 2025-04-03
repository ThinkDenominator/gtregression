#' Identify Confounder Using Crude and Stratified Models
#'
#' This function estimates the crude (unadjusted) effect of an exposure on a binary, count, or continuous outcome
#' and compares it with Mantel-Haenszel estimates from stratified models using a potential confounder.
#' It evaluates confounding based on the percent change in effect size and provides a heuristic check for effect modification.
#' Supports multiple regression approaches including logit, log-binomial, Poisson, negative binomial, robust Poisson, and marginal standardization methods.
#'
#' @param data A data frame.
#' @param outcome The name of the outcome variable (binary, count, or continuous depending on approach).
#' @param exposure The name of the exposure variable (must be binary or categorical).
#' @param potential_confounder A single variable to stratify by for confounder assessment.
#' @param approach Regression approach: one of "logit", "log-binomial", "poisson", "negbin", "linear", "robpoisson", "margstd_boot", or "margstd_delta".
#' @param threshold Numeric value specifying the percentage change in effect size considered meaningful for confounding (default is 10).
#'
#' @return A list (invisible) containing crude and stratified estimates, percent change, and logical indicators for confounding and effect modification. Also prints an interpretation summary to the console.
#' @export

identify_confounder <- function(data, outcome, exposure, potential_confounder,
                                approach = "logit", threshold = 10,
                                method = c("mh", "change")) {
  method <- match.arg(method)

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("MASS", quietly = TRUE)
  requireNamespace("risks", quietly = TRUE)

  # Outcome validation
  outcome_vec <- data[[outcome]]
  is_binary <- function(x) is.factor(x) && length(levels(x)) == 2 || is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)
  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) && length(unique(x[!is.na(x)])) > 2
  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")) {
    if (!is_binary(outcome_vec)) stop("This approach requires a binary outcome.")
  }
  if (approach == "poisson") {
    if (is_binary(outcome_vec)) stop("Poisson regression is not appropriate for binary outcomes.")
    if (!is_count(outcome_vec)) stop("Poisson requires a count outcome.")
  }
  if (approach == "negbin") {
    if (!is_count(outcome_vec)) stop("Negative binomial requires a count outcome.")
  }
  if (approach == "linear") {
    if (!is_continuous(outcome_vec)) stop("Linear regression requires a continuous outcome.")
  }

  # Core model fitting
  get_model <- function(data, formula, approach) {
    switch(approach,
           "logit" = glm(formula, data = data, family = binomial(link = "logit")),
           "log-binomial" = glm(formula, data = data, family = binomial(link = "log")),
           "poisson" = glm(formula, data = data, family = poisson(link = "log")),
           "negbin" = MASS::glm.nb(formula, data = data),
           "linear" = lm(formula, data = data),
           "robpoisson" = risks::riskratio(formula = formula, data = data, approach = "robpoisson"),
           "margstd_boot" = risks::riskratio(formula = formula, data = data, approach = "margstd_boot"),
           "margstd_delta" = risks::riskratio(formula = formula, data = data, approach = "margstd_delta"),
           stop("Unsupported approach.")
    )
  }

  extract_estimate <- function(model, exposure, approach) {
    if (inherits(model, "glm") || inherits(model, "lm")) {
      coefs <- coef(model)
      match_idx <- grep(paste0("^", exposure), names(coefs))
      if (length(match_idx) == 0) return(NA)
      est <- unname(coefs[match_idx[1]])
      return(if (approach == "linear") est else exp(est))
    }

    if (inherits(model, "riskratio")) {
      if (approach %in% c("margstd_delta", "margstd_boot")) {
        res_tbl <- model[[paste0(approach, "_res")]]
        if (!is.null(res_tbl)) {
          match_row <- grep(paste0("^", exposure), res_tbl$term)
          if (length(match_row) == 0) return(NA)
          return(exp(res_tbl$estimate[match_row[1]]))
        }
      }

      if (!is.null(model$summary) && "term" %in% names(model$summary)) {
        match_row <- grep(paste0("^", exposure), model$summary$term)
        if (length(match_row) == 0) return(NA)
        return(model$summary$RR[match_row[1]])
      }
    }

    return(NA)
  }

  # ---------------- Method: Change-in-estimate ----------------
  if (method == "change") {
    if (length(exposure) != 1) stop("Please provide only one exposure variable for change-in-estimate method.")

    crude_approach <- if (approach %in% c("margstd_delta", "margstd_boot")) "log-binomial" else approach
    crude_fmla <- as.formula(paste(outcome, "~", exposure))
    crude_model <- tryCatch(get_model(data, crude_fmla, crude_approach), error = function(e) NULL)
    crude_est <- if (!is.null(crude_model)) extract_estimate(crude_model, exposure, crude_approach) else NA

    if (length(potential_confounder) == 1) {
      adj_fmla <- as.formula(paste(outcome, "~", exposure, "+", potential_confounder))
      adj_model <- tryCatch(get_model(data, adj_fmla, approach), error = function(e) NULL)
      adj_est <- if (!is.null(adj_model)) extract_estimate(adj_model, exposure, approach) else NA
      pct_change <- abs((adj_est - crude_est) / crude_est) * 100
      is_conf <- pct_change >= threshold

      cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      cat("Crude Estimate:             ", format(round(crude_est, 3), nsmall = 3), "\n")
      cat("Adjusted Estimate:          ", format(round(adj_est, 3), nsmall = 3), "\n")
      cat("% Change from Crude:        ", format(round(pct_change, 2), nsmall = 2), "%\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      cat("Confounding (change-in-estimate):", ifelse(is_conf, "Yes", "No"), "\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      cat("Notes:\n")
      cat("• Confounding is suggested if % change ≥", threshold, "% (default threshold).\n")
      cat("• This method does not assess effect modification.\n")
      cat("• Ideally, confounders should be guided by DAGs or domain knowledge.\n")

      return(invisible(list(
        crude = crude_est,
        adjusted = adj_est,
        percent_change = pct_change,
        is_confounder = is_conf
      )))

    } else {
      # Multi-covariate mode
      results <- lapply(potential_confounder, function(var) {
        adj_fmla <- as.formula(paste(outcome, "~", exposure, "+", var))
        adj_model <- tryCatch(get_model(data, adj_fmla, approach), error = function(e) NULL)
        adj_est <- if (!is.null(adj_model)) extract_estimate(adj_model, exposure, approach) else NA
        pct_change <- abs((adj_est - crude_est) / crude_est) * 100
        is_conf <- pct_change >= threshold
        tibble::tibble(
          covariate = var,
          crude_est = round(crude_est, 3),
          adjusted_est = round(adj_est, 3),
          pct_change = round(pct_change, 2),
          is_confounder = is_conf
        )
      })
      summary_tbl <- dplyr::bind_rows(results)

      print(summary_tbl)
      cat("\nNotes:\n")
      cat("• Confounding is suggested if % change ≥", threshold, "% (default threshold).\n")
      cat("• This method does not assess effect modification.\n")
      cat("• Ideally, confounders should be guided by DAGs or domain knowledge— not statistical criteria alone.\n")

      return(invisible(summary_tbl))
    }
  }

  # ---------------- Method: MH Stratification ----------------
  if (length(potential_confounder) != 1) stop("Stratified (MH) method supports only one covariate.")

  message("Step 1: Estimating crude (unadjusted) effect of exposure on outcome")
  crude_approach <- if (approach %in% c("margstd_delta", "margstd_boot")) "log-binomial" else approach
  fmla <- as.formula(paste(outcome, "~", exposure))
  crude_model <- tryCatch(get_model(data, fmla, crude_approach), error = function(e) NULL)
  crude_est <- if (!is.null(crude_model)) extract_estimate(crude_model, exposure, crude_approach) else NA

  message("\n Step 2: Stratified analysis by: ", potential_confounder)
  strat_levels <- unique(na.omit(data[[potential_confounder]]))
  strat_ests <- c()
  for (lev in strat_levels) {
    subdata <- dplyr::filter(data, .data[[potential_confounder]] == lev)
    fit <- tryCatch(get_model(subdata, fmla, approach), error = function(e) NULL)
    strat_ests[as.character(lev)] <- if (!is.null(fit)) extract_estimate(fit, exposure, approach) else NA
  }

  if (all(is.na(strat_ests)) || is.na(crude_est)) {
    warning("Cannot compute MH estimate or crude estimate. Returning NA.")
    mh_est <- NA; pct_change <- NA; confounding <- NA; effect_mod <- NA
  } else {
    mh_est <- if (approach == "linear") {
      mean(unlist(strat_ests), na.rm = TRUE)
    } else {
      exp(mean(log(unlist(strat_ests)), na.rm = TRUE))
    }
    pct_change <- abs((mh_est - crude_est) / crude_est) * 100
    confounding <- pct_change >= threshold
    strat_range <- range(unlist(strat_ests), na.rm = TRUE)
    effect_mod <- diff(strat_range) > 0.2 * crude_est
  }

  cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Crude Estimate:            ", format(round(crude_est, 3), nsmall = 3), "\n")
  cat("Mantel-Haenszel Estimate:  ", format(round(mh_est, 3), nsmall = 3), "\n")
  cat("% Change from Crude:       ", format(round(pct_change, 2), nsmall = 2), "%\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Confounding:               ", ifelse(confounding, "Yes", "No"), "\n")
  cat("Effect Modification:       ", ifelse(effect_mod, "Possible (based on stratum spread)", "No"), "\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Notes:\n")
  cat("• Confounding is suggested if % change ≥", threshold, "% (default threshold).\n")
  cat("• Effect modification is a rough check based on spread across strata.\n")
  cat("• This does not replace formal interaction testing.\n")
  cat("• Sparse data in strata can bias estimates — interpret with caution.\n")
  cat("• Ideally, confounders should be identified using DAGs or prior knowledge— not statistical criteria alone.\n")

  return(invisible(list(
    crude = crude_est,
    mantel_haenszel = mh_est,
    percent_change = pct_change,
    is_confounder = confounding,
    effect_modification = effect_mod,
    stratum_estimates = strat_ests
  )))
}



