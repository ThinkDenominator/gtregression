#' Identify Confounder Using Crude and Stratified Models
#'
#' This function estimates the crude (unadjusted) effect of an exposure on a binary, count, or continuous outcome
#' and compares it with Mantel-Haenszel estimates from stratified models using a potential confounder,
#' or with adjusted models using the change-in-estimate method.
#' It evaluates confounding based on the percent change in effect size and provides a heuristic check for effect modification.
#' Supports multiple regression approaches including logit, log-binomial, Poisson, negative binomial, robust Poisson, and marginal standardization methods.
#'
#' @param data A data frame.
#' @param outcome The name of the outcome variable (binary, count, or continuous depending on approach).
#' @param exposure The name of the exposure variable (must be binary or categorical).
#' @param potential_confounder A single variable to stratify by for confounder assessment.
#' @param approach Regression approach: one of "logit", "log-binomial", "poisson", "negbin", "linear", "robpoisson", "margstd_boot", or "margstd_delta".
#' @param threshold Numeric value specifying the percentage change in effect size considered meaningful for confounding (default is 10).
#' @param method Method for confounder identification: "mh" for Mantel-Haenszel, "change" for change-in-estimate (default).
#'
#' @return A list (invisible) containing crude and adjusted or stratified estimates, percent change,
#' logical indicators for confounding and effect modification, and reason for NA values if applicable.
#' @export

identify_confounder <- function(data, outcome, exposure, potential_confounder,
                                approach = "logit", threshold = 10,
                                method = c("change", "mh")) {
  method <- match.arg(method)

  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("MASS", quietly = TRUE)
  requireNamespace("risks", quietly = TRUE)

  outcome_vec <- data[[outcome]]

  is_binary <- function(x) {
    is.factor(x) && length(levels(x)) == 2 ||
      is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)
  }
  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) && length(unique(x[!is.na(x)])) > 2
  }
  is_continuous <- function(x) {
    is.numeric(x) && length(unique(x)) > 10 && !is_count(x)
  }

  # Outcome validation
  if (approach %in% c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")) {
    if (!is_binary(outcome_vec)) stop("This approach requires a binary outcome.")
  }
  if (approach == "poisson" && (!is_count(outcome_vec) || is_binary(outcome_vec))) {
    stop("Poisson requires a count outcome.")
  }
  if (approach == "negbin" && !is_count(outcome_vec)) {
    stop("Negative binomial requires a count outcome.")
  }
  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Linear regression requires a continuous outcome.")
  }

  # ---------- Mantel-Haenszel method ----------
  if (method == "mh") {
    exposure_vec <- data[[exposure]]
    if (!is_binary(exposure_vec)) stop("Mantel-Haenszel requires a binary exposure.")
    if (!is_binary(outcome_vec)) stop("Mantel-Haenszel requires a binary outcome.")
    if (length(potential_confounder) != 1) stop("MH method requires a single stratifying variable.")

    confounder <- potential_confounder
    crude_tab <- table(data[[exposure]], data[[outcome]])
    crude_rr <- (crude_tab[2, 2] / sum(crude_tab[2, ])) / (crude_tab[1, 2] / sum(crude_tab[1, ]))

    strata <- na.omit(unique(data[[confounder]]))
    mh_numer <- 0
    mh_denom <- 0
    strat_ests <- c()
    skipped_strata <- c()

    for (lev in strata) {
      sub <- data |>
        dplyr::filter(.data[[confounder]] == lev) |>
        dplyr::filter(!is.na(.data[[exposure]]) & !is.na(.data[[outcome]]))

      if (nrow(sub) == 0) {
        message("Stratum '", lev, "' skipped: no complete cases for exposure/outcome.")
        skipped_strata <- c(skipped_strata, lev)
        strat_ests[lev] <- NA
        next
      }

      tab <- table(sub[[exposure]], sub[[outcome]])

      if (!all(c(0, 1) %in% rownames(tab)) || !all(c(0, 1) %in% colnames(tab))) {
        message("Stratum '", lev, "' skipped: incomplete 2×2 table.")
        skipped_strata <- c(skipped_strata, lev)
        strat_ests[lev] <- NA
        next
      }

      a <- tab["1", "1"]
      b <- tab["1", "0"]
      c <- tab["0", "1"]
      d <- tab["0", "0"]

      n1 <- a + b
      n0 <- c + d
      if (n1 == 0 || n0 == 0) {
        message("Stratum '", lev, "' skipped: no exposed or unexposed observations.")
        skipped_strata <- c(skipped_strata, lev)
        strat_ests[lev] <- NA
        next
      }

      rr_stratum <- (a / n1) / (c / n0)
      strat_ests[lev] <- rr_stratum

      mh_numer <- mh_numer + (a + c) * (a / n1)
      mh_denom <- mh_denom + (a + c) * (c / n0)
    }

    valid_ests <- strat_ests[!is.na(strat_ests)]
    if (length(valid_ests) == 0 || mh_numer == 0 || mh_denom == 0) {
      mh_rr <- NA
      pct_change <- NA
      is_conf <- NA
      effect_mod <- FALSE
      reason <- "No valid strata for MH estimation"
      message("Unable to compute Mantel-Haenszel estimate: all strata were invalid or sparse.")
    } else {
      mh_rr <- mh_numer / mh_denom
      pct_change <- abs((mh_rr - crude_rr) / crude_rr) * 100
      is_conf <- pct_change >= threshold
      effect_mod <- diff(range(valid_ests)) > 0.2 * crude_rr
      reason <- NULL
    }

    cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("Crude Estimate:             ", format(round(crude_rr, 3), nsmall = 3), "\n")
    cat("Mantel-Haenszel Estimate:   ", format(round(mh_rr, 3), nsmall = 3), "\n")
    cat("% Change from Crude:        ", format(round(pct_change, 2), nsmall = 2), "%\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("Confounding:                ", ifelse(is.na(is_conf), "NA", ifelse(is_conf, "Yes", "No")), "\n")
    cat("Effect Modification:        ", ifelse(effect_mod, "Possible", "No"), "\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("Notes:\n")
    cat("• Mantel-Haenszel method requires a binary exposure and binary outcome, with at least one valid stratum.\n")
    cat("• Effect modification is heuristically assessed via variation in stratum-specific RRs.\n")
    cat("• Crude and MH estimates are unadjusted risk ratios — they do not account for other covariates.\n")
    cat("• Sparse or unbalanced strata may bias the MH estimate — interpret with caution.\n")
    cat("• Use domain knowledge, Directed Acyclic Graphs (DAGs), or prior literature to guide confounder selection — not statistical criteria alone.\n")

    return(invisible(list(
      crude = crude_rr,
      mantel_haenszel = mh_rr,
      percent_change = pct_change,
      is_confounder = is_conf,
      effect_modification = effect_mod,
      stratum_estimates = strat_ests,
      skipped_strata = skipped_strata,
      reason = reason
    )))

  }

  # ---------- Change-in-estimate method ----------
  if (method == "change") {
    crude_approach <- if (approach %in% c("margstd_delta", "margstd_boot")) "log-binomial" else approach

    get_model <- function(data, formula, approach) {
      switch(approach,
             "logit" = glm(formula, data = data, family = binomial("logit")),
             "log-binomial" = glm(formula, data = data, family = binomial("log")),
             "poisson" = glm(formula, data = data, family = poisson("log")),
             "negbin" = MASS::glm.nb(formula, data = data),
             "linear" = lm(formula, data = data),
             "robpoisson" = risks::riskratio(formula, data = data, approach = "robpoisson"),
             "margstd_boot" = risks::riskratio(formula, data = data, approach = "margstd_boot"),
             "margstd_delta" = risks::riskratio(formula, data = data, approach = "margstd_delta"),
             stop("Unsupported approach.")
      )
    }

    extract_estimate <- function(model, exposure, approach) {
      if (inherits(model, "glm") || inherits(model, "lm")) {
        coefs <- coef(model)
        idx <- grep(paste0("^", exposure), names(coefs))
        if (length(idx) == 0) return(NA)
        est <- unname(coefs[idx[1]])
        return(if (approach == "linear") est else exp(est))
      }
      if (inherits(model, "riskratio")) {
        if (!is.null(model$summary) && "term" %in% names(model$summary)) {
          idx <- grep(paste0("^", exposure), model$summary$term)
          if (length(idx) == 0) return(NA)
          return(model$summary$RR[idx[1]])
        }
      }
      return(NA)
    }

    # Run crude model once
    crude_model <- tryCatch(get_model(data, as.formula(paste(outcome, "~", exposure)), crude_approach), error = function(e) NULL)
    crude_est <- if (!is.null(crude_model)) extract_estimate(crude_model, exposure, crude_approach) else NA

    # Handle multiple potential confounders
    if (length(potential_confounder) > 1) {
      results <- lapply(potential_confounder, function(var) {
        adj_fmla <- as.formula(paste(outcome, "~", paste(c(exposure, var), collapse = " + ")))
        adj_model <- tryCatch(get_model(data, adj_fmla, approach), error = function(e) NULL)
        adj_est <- if (!is.null(adj_model)) extract_estimate(adj_model, exposure, approach) else NA
        pct_change <- if (is.na(adj_est) || is.na(crude_est)) NA else abs((adj_est - crude_est) / crude_est) * 100
        is_conf <- if (!is.na(pct_change)) pct_change >= threshold else NA

        tibble::tibble(
          covariate = var,
          crude_est = round(crude_est, 3),
          adjusted_est = round(adj_est, 3),
          pct_change = round(pct_change, 2),
          is_confounder = is_conf
        )
      })

      result_tbl <- dplyr::bind_rows(results)

      print(result_tbl)
      cat("\nNotes:\n")
      cat("• Confounding is suggested if % change ≥", threshold, "% (default threshold).\n")
      cat("• This method does not assess effect modification.\n")
      cat("• Ideally, confounders should be guided by DAGs or domain knowledge.\n")

      return(invisible(result_tbl))
    }

    # Fallback for single confounder
    adj_fmla <- as.formula(paste(outcome, "~", paste(c(exposure, potential_confounder), collapse = " + ")))
    adj_model <- tryCatch(get_model(data, adj_fmla, approach), error = function(e) NULL)
    adj_est <- if (!is.null(adj_model)) extract_estimate(adj_model, exposure, approach) else NA
    pct_change <- if (is.na(adj_est) || is.na(crude_est)) NA else abs((adj_est - crude_est) / crude_est) * 100
    is_conf <- if (!is.na(pct_change)) pct_change >= threshold else NA

    cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("Crude Estimate:              ", format(round(crude_est, 3), nsmall = 3), "\n")
    cat("Adjusted Estimate:           ", format(round(adj_est, 3), nsmall = 3), "\n")
    cat("% Change from Crude:         ", format(round(pct_change, 2), nsmall = 2), "%\n")
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
  }
}
