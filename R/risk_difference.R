# R/risk_difference.R
#
# risk_difference(): absolute risks and the risk difference between two groups.
# The statistical engine is self-contained: it does not use the regression
# fitting/tidying helpers, approach lists, stats::prop.test() or
# risks::riskdiff().

# ---------------------------------------------------------------------------
# Small utilities
# ---------------------------------------------------------------------------

#' Quote values for messages
#' @keywords internal
#' @noRd
.rd_quote <- function(x) {
  paste0("'", as.character(x), "'", collapse = ", ")
}

#' Text key of a value (values are compared as text)
#' @keywords internal
#' @noRd
.rd_key <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.numeric(x)) {
    return(format(x, trim = TRUE, digits = 15, scientific = FALSE))
  }
  as.character(x)
}

#' Convert value-labelled vectors to factors
#' @keywords internal
#' @noRd
.rd_unlabel <- function(x, name) {
  if (!inherits(x, "haven_labelled")) {
    return(x)
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "`", name, "` is a value-labelled (haven) vector and package 'haven' ",
      "is not installed. Convert `", name, "` to a factor first.",
      call. = FALSE
    )
  }
  label <- attr(x, "label", exact = TRUE)
  out <- haven::as_factor(x)
  if (!is.null(label)) attr(out, "label") <- label
  out
}

# ---------------------------------------------------------------------------
# Validation and resolution of event / reference
# ---------------------------------------------------------------------------

#' Validate the arguments of risk_difference()
#' @keywords internal
#' @noRd
.rd_validate_args <- function(data, outcome, exposure, adjust_for, event,
                              reference, event_type, conf_level, format) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  one_name <- function(x, arg) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop("`", arg, "` must be a single column name.", call. = FALSE)
    }
    if (!x %in% names(data)) {
      stop("`", arg, "` column '", x, "' was not found in `data`.",
           call. = FALSE)
    }
  }
  one_name(outcome, "outcome")
  one_name(exposure, "exposure")
  if (identical(outcome, exposure)) {
    stop("`outcome` and `exposure` must be different variables.",
         call. = FALSE)
  }
  if (!is.null(adjust_for)) {
    if (!is.character(adjust_for) || !length(adjust_for) || anyNA(adjust_for)) {
      stop("`adjust_for` must be NULL or a character vector of column names.",
           call. = FALSE)
    }
    missing_vars <- setdiff(adjust_for, names(data))
    if (length(missing_vars)) {
      stop("`adjust_for` variable(s) not found in `data`: ",
           paste(missing_vars, collapse = ", "), ".", call. = FALSE)
    }
    if (outcome %in% adjust_for || exposure %in% adjust_for) {
      stop("`adjust_for` must not contain the outcome or the exposure.",
           call. = FALSE)
    }
  }
  scalar_value <- function(x, arg) {
    if (!is.null(x) && (!is.atomic(x) || length(x) != 1L || is.na(x))) {
      stop("`", arg, "` must be NULL or a single non-missing value.",
           call. = FALSE)
    }
  }
  scalar_value(event, "event")
  scalar_value(reference, "reference")
  if (!is.null(event_type) &&
      (!is.character(event_type) || length(event_type) != 1L ||
       is.na(event_type) || !event_type %in% c("harmful", "beneficial"))) {
    stop("`event_type` must be NULL, \"harmful\" or \"beneficial\".",
         call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.",
         call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1L || is.na(format) ||
      !format %in% c("gt", "flextable")) {
    stop("`format` must be \"gt\" or \"flextable\".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve the event level of a binary outcome
#'
#' @param y Outcome values of the analysis population (no missing values).
#' @return list(y01, event, non_event, inferred): `inferred` is TRUE when a
#'   message is due (event not given and coding is not logical or 0/1).
#' @keywords internal
#' @noRd
.rd_resolve_event <- function(y, outcome, event = NULL) {
  limit <- "risk_difference() supports a binary outcome only (version 1 limit)."
  if (is.factor(y)) {
    values <- levels(droplevels(y))
    # A two-level factor with one observed level (no events, or only events,
    # in the analysis population) is still unambiguous.
    if (length(values) == 1L && nlevels(y) == 2L) values <- levels(y)
    key <- as.character(y)
    kind <- "factor"
  } else if (is.logical(y)) {
    values <- c("FALSE", "TRUE")
    key <- as.character(y)
    kind <- "logical"
  } else if (is.numeric(y)) {
    observed <- sort(unique(y))
    key <- .rd_key(y)
    if (all(observed %in% c(0, 1))) {
      values <- c("0", "1")
      kind <- "zero_one"
    } else {
      values <- .rd_key(observed)
      kind <- if (identical(as.numeric(observed), c(1, 2))) "one_two" else "numeric"
    }
  } else if (is.character(y)) {
    values <- sort(unique(y))
    key <- y
    kind <- "character"
  } else {
    stop("Outcome `", outcome, "` has an unsupported type (",
         class(y)[1L], "). ", limit, call. = FALSE)
  }

  if (length(values) > 2L) {
    shown <- if (length(values) > 8L) c(values[1:8], "...") else values
    stop(
      "Outcome `", outcome, "` has ", length(values),
      " observed values (", paste(shown, collapse = ", "), "). ", limit,
      call. = FALSE
    )
  }
  if (length(values) < 2L) {
    stop(
      "Outcome `", outcome, "` has only one observed value (",
      .rd_quote(values), ") in the analysis population, so the event ",
      "cannot be identified. Code the outcome as logical, 0/1 or a ",
      "two-level factor.",
      call. = FALSE
    )
  }

  if (!is.null(event)) {
    event_key <- .rd_key(event)
    if (!event_key %in% values) {
      stop(
        "`event` (", .rd_quote(event_key), ") is not an observed value of `",
        outcome, "`. Observed values: ", .rd_quote(values), ".",
        call. = FALSE
      )
    }
    inferred <- FALSE
  } else {
    if (identical(kind, "numeric")) {
      stop(
        "Outcome `", outcome, "` is numeric with values ", .rd_quote(values),
        ". Use 0/1 or 1/2 coding, or name the event with `event = `. ", limit,
        call. = FALSE
      )
    }
    event_key <- values[2L]
    inferred <- !kind %in% c("logical", "zero_one")
  }

  list(
    y01 = as.integer(key == event_key),
    event = event_key,
    non_event = setdiff(values, event_key),
    inferred = inferred
  )
}

#' Resolve the reference and comparison groups of a two-group exposure
#'
#' @param x Exposure values of the analysis population (no missing values).
#' @return list(a01, reference, comparison, inferred)
#' @keywords internal
#' @noRd
.rd_resolve_reference <- function(x, exposure, reference = NULL) {
  limit <- "risk_difference() compares exactly two groups (version 1 limit)."
  if (is.factor(x)) {
    x <- droplevels(x)
    values <- levels(x)
    key <- as.character(x)
    kind <- "factor"
  } else if (is.logical(x)) {
    values <- as.character(sort(unique(x)))
    key <- as.character(x)
    kind <- "logical"
  } else if (is.numeric(x)) {
    observed <- sort(unique(x))
    if (length(observed) > 2L) {
      stop(
        "Exposure `", exposure, "` is numeric with ", length(observed),
        " distinct values. Continuous exposures and exposures with more ",
        "than two groups are not supported. ", limit,
        call. = FALSE
      )
    }
    values <- .rd_key(observed)
    key <- .rd_key(x)
    kind <- if (all(observed %in% c(0, 1))) "zero_one" else "numeric"
  } else if (is.character(x)) {
    values <- sort(unique(x))
    key <- x
    kind <- "character"
  } else {
    stop("Exposure `", exposure, "` has an unsupported type (",
         class(x)[1L], "). ", limit, call. = FALSE)
  }

  if (length(values) > 2L) {
    shown <- if (length(values) > 8L) c(values[1:8], "...") else values
    stop(
      "Exposure `", exposure, "` has ", length(values),
      " observed groups (", paste(shown, collapse = ", "), "). ", limit,
      " Restrict the data to two groups.",
      call. = FALSE
    )
  }
  if (length(values) < 2L) {
    stop(
      "Exposure `", exposure, "` has only one observed group (",
      .rd_quote(values), ") in the analysis population. ", limit,
      call. = FALSE
    )
  }

  if (!is.null(reference)) {
    ref_key <- .rd_key(reference)
    if (!ref_key %in% values) {
      stop(
        "`reference` (", .rd_quote(ref_key), ") is not an observed group of `",
        exposure, "`. Observed groups: ", .rd_quote(values), ".",
        call. = FALSE
      )
    }
    inferred <- FALSE
  } else {
    ref_key <- values[1L]
    inferred <- !kind %in% c("logical", "zero_one")
  }
  comparison <- setdiff(values, ref_key)

  list(
    a01 = as.integer(key == comparison),
    reference = ref_key,
    comparison = comparison,
    inferred = inferred
  )
}

# ---------------------------------------------------------------------------
# Crude analysis: Miettinen-Nurminen score interval and test
# ---------------------------------------------------------------------------

#' Restricted maximum-likelihood risks under p1 - p0 = d
#'
#' Closed-form solution of the likelihood cubic (Miettinen and Nurminen 1985;
#' Farrington and Manning 1990).
#' @return c(p1, p0)
#' @keywords internal
#' @noRd
.rd_mn_restricted <- function(x1, n1, x0, n0, d) {
  big_n <- n1 + n0
  l3 <- big_n
  l2 <- (n1 + 2 * n0) * d - big_n - (x1 + x0)
  l1 <- (n0 * d - big_n - 2 * x0) * d + (x1 + x0)
  l0 <- x0 * d * (1 - d)
  q <- l2^3 / (3 * l3)^3 - l1 * l2 / (6 * l3^2) + l0 / (2 * l3)
  p <- sqrt(max(l2^2 / (3 * l3)^2 - l1 / (3 * l3), 0))
  if (q < 0) p <- -p
  angle <- if (p == 0) {
    pi / 2
  } else {
    (pi + acos(max(min(q / p^3, 1), -1))) / 3
  }
  p0 <- 2 * p * cos(angle) - l2 / (3 * l3)
  p0 <- min(max(p0, max(0, -d)), min(1, 1 - d))
  p1 <- min(max(p0 + d, 0), 1)
  c(p1, p0)
}

#' Miettinen-Nurminen score statistic for H0: p1 - p0 = d
#' @keywords internal
#' @noRd
.rd_mn_statistic <- function(x1, n1, x0, n0, d) {
  big_n <- n1 + n0
  est <- x1 / n1 - x0 / n0
  r <- .rd_mn_restricted(x1, n1, x0, n0, d)
  v <- (r[1L] * (1 - r[1L]) / n1 + r[2L] * (1 - r[2L]) / n0) *
    big_n / (big_n - 1)
  if (!is.finite(v) || v <= 0) {
    # Degenerate variance: the statistic is 0 when the hypothesised value is
    # the observed one (pooled risk 0 or 1 at d = 0), otherwise infinite.
    if (abs(est - d) < 1e-12) return(0)
    return(sign(est - d) * Inf)
  }
  (est - d) / sqrt(v)
}

#' Miettinen-Nurminen interval and score test for a risk difference
#'
#' Group 1 is the comparison group, group 0 the reference group. No
#' continuity correction; variance factor N / (N - 1).
#' @return list(risk1, risk0, rd, low, high, statistic, p_value)
#' @keywords internal
#' @noRd
.rd_mn <- function(x1, n1, x0, n0, conf_level = 0.95) {
  est <- x1 / n1 - x0 / n0
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  edge <- 1e-10
  tol <- 1e-13

  lower_fun <- function(d) .rd_mn_statistic(x1, n1, x0, n0, d) - crit
  upper_fun <- function(d) .rd_mn_statistic(x1, n1, x0, n0, d) + crit

  low <- if (est <= -1 + 1e-12) {
    -1
  } else if (lower_fun(-1 + edge) <= 0) {
    -1
  } else {
    stats::uniroot(lower_fun, c(-1 + edge, est), tol = tol)$root
  }
  high <- if (est >= 1 - 1e-12) {
    1
  } else if (upper_fun(1 - edge) >= 0) {
    1
  } else {
    stats::uniroot(upper_fun, c(est, 1 - edge), tol = tol)$root
  }

  pooled <- (x1 + x0) / (n1 + n0)
  statistic <- if (pooled <= 0 || pooled >= 1) {
    0
  } else {
    .rd_mn_statistic(x1, n1, x0, n0, 0)
  }
  list(
    risk1 = x1 / n1,
    risk0 = x0 / n0,
    rd = est,
    low = low,
    high = high,
    statistic = statistic,
    p_value = 2 * stats::pnorm(-abs(statistic))
  )
}

# ---------------------------------------------------------------------------
# Adjusted analysis: standardisation and influence-function standard error
# ---------------------------------------------------------------------------

#' Stop when the logistic working model is not estimable
#'
#' Aliased terms, non-convergence and fitted probabilities of 0 or 1 are
#' errors: no table is produced and no fallback is attempted.
#' @keywords internal
#' @noRd
.rd_check_logistic <- function(fit, fit_warnings = character(0), hint = "") {
  beta <- stats::coef(fit)
  x_mat <- stats::model.matrix(fit)
  if (anyNA(beta)) {
    # glm() drops the later of two collinear columns, so an exposure that is
    # determined by the adjustment variables shows up as an aliased
    # covariate: compare the rank with and without the exposure column.
    a_col <- match(".rd_a", colnames(x_mat))
    if (is.na(beta[[".rd_a"]]) ||
        qr(x_mat[, -a_col, drop = FALSE])$rank == qr(x_mat)$rank) {
      stop(
        "The adjusted risk difference cannot be estimated: the exposure is ",
        "aliased with (completely determined by) the adjustment variables.",
        hint,
        call. = FALSE
      )
    }
    assign_idx <- attr(x_mat, "assign")[is.na(beta)]
    term_labels <- attr(stats::terms(fit), "term.labels")
    aliased <- unique(gsub("`", "", term_labels[assign_idx], fixed = TRUE))
    stop(
      "The adjusted risk difference cannot be estimated: adjustment ",
      "variable(s) aliased in the logistic model (constant, or completely ",
      "determined by other terms): ", paste(aliased, collapse = ", "),
      ". Remove or recode ", if (length(aliased) > 1L) "them" else "it", ".",
      hint,
      call. = FALSE
    )
  }
  if (!isTRUE(fit$converged) ||
      any(grepl("did not converge", fit_warnings, fixed = TRUE))) {
    stop(
      "The adjusted risk difference cannot be estimated: the logistic model ",
      "did not converge.", hint,
      call. = FALSE
    )
  }
  mu <- as.vector(fit$fitted.values)
  if (any(grepl("fitted probabilities numerically 0 or 1", fit_warnings,
                fixed = TRUE)) ||
      isTRUE(fit$boundary) || any(mu < 1e-8) || any(mu > 1 - 1e-8)) {
    stop(
      "The adjusted risk difference cannot be estimated: the logistic model ",
      "has fitted probabilities of 0 or 1 (complete or quasi-complete ",
      "separation).", hint,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Standardised risks and risk difference from a main-effects logistic model
#'
#' @param y 0/1 outcome; `a` 0/1 exposure (1 = comparison group).
#' @param covariates Data frame of adjustment variables (may have 0 columns).
#' @param conf_level Confidence level of the Wald interval.
#' @return list with standardised risks, their influence-function standard
#'   errors, the risk difference, its standard error, interval, p-value, the
#'   fitted model and the per-observation influence values.
#' @keywords internal
#' @noRd
.rd_standardise <- function(y, a, covariates = NULL, conf_level = 0.95,
                            crude_hint = TRUE) {
  hint <- if (isTRUE(crude_hint)) {
    " Consider the crude analysis (`adjust_for = NULL`), which remains defined."
  } else {
    ""
  }
  n <- length(y)
  if (length(unique(y)) < 2L) {
    stop(
      "The adjusted risk difference cannot be estimated: the outcome is ",
      "constant (", if (all(y == 1)) "all events" else "no events",
      ") in the analysis population.", hint,
      call. = FALSE
    )
  }
  for (g in 0:1) {
    yg <- y[a == g]
    if (all(yg == 0) || all(yg == 1)) {
      stop(
        "The adjusted risk difference cannot be estimated: one group has ",
        if (all(yg == 0)) "no events" else "only events",
        ", so the logistic model is separated (fitted probabilities of 0 or 1).",
        hint,
        call. = FALSE
      )
    }
  }

  model_data <- data.frame(.rd_y = y, .rd_a = a)
  cov_names <- character(0)
  if (!is.null(covariates) && ncol(covariates)) {
    cov_names <- names(covariates)
    for (nm in cov_names) {
      v <- covariates[[nm]]
      if (is.character(v)) v <- factor(v)
      if (is.factor(v)) {
        v <- droplevels(v)
        if (is.ordered(v)) v <- factor(v, ordered = FALSE)
        stats::contrasts(v) <- NULL
      }
      if (length(unique(v)) < 2L) {
        stop(
          "The adjusted risk difference cannot be estimated: adjustment ",
          "variable `", nm, "` is constant in the analysis population ",
          "(aliased with the intercept). Remove it from `adjust_for`.",
          call. = FALSE
        )
      }
      model_data[[nm]] <- v
    }
  }
  rhs <- c(".rd_a", vapply(cov_names, .formula_name, character(1)))
  model_formula <- stats::as.formula(
    paste(".rd_y ~", paste(rhs, collapse = " + "))
  )

  fit_warnings <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      stats::glm(
        model_formula,
        family = stats::binomial(link = "logit"),
        data = model_data,
        control = stats::glm.control(epsilon = 1e-10, maxit = 100),
        contrasts = NULL
      ),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(
        "The adjusted risk difference cannot be estimated: the logistic ",
        "model could not be fitted (", conditionMessage(e), ").", hint,
        call. = FALSE
      )
    }
  )

  .rd_check_logistic(fit, fit_warnings, hint)
  beta <- stats::coef(fit)
  x_mat <- stats::model.matrix(fit)
  mu <- as.vector(fit$fitted.values)

  x1 <- x0 <- x_mat
  x1[, ".rd_a"] <- 1
  x0[, ".rd_a"] <- 0
  p1 <- as.vector(stats::plogis(x1 %*% beta))
  p0 <- as.vector(stats::plogis(x0 %*% beta))
  risk1 <- mean(p1)
  risk0 <- mean(p0)
  rd <- risk1 - risk0

  # Influence function of the stacked estimating equations
  #   phi_i = (p_i - mean) + g' A^{-1} x_i (y_i - mu_i),
  #   A = (1/n) sum mu_i (1 - mu_i) x_i x_i'.
  bread <- crossprod(x_mat * sqrt(mu * (1 - mu))) / n
  bread_inv <- tryCatch(solve(bread), error = function(e) NULL)
  if (is.null(bread_inv) || any(!is.finite(bread_inv))) {
    stop(
      "The adjusted risk difference cannot be estimated: the information ",
      "matrix of the logistic model is singular.", hint,
      call. = FALSE
    )
  }
  beta_influence <- (x_mat * (y - mu)) %*% bread_inv
  g1 <- colMeans(x1 * (p1 * (1 - p1)))
  g0 <- colMeans(x0 * (p0 * (1 - p0)))
  phi1 <- (p1 - risk1) + as.vector(beta_influence %*% g1)
  phi0 <- (p0 - risk0) + as.vector(beta_influence %*% g0)
  phi <- phi1 - phi0
  se1 <- sqrt(sum(phi1^2)) / n
  se0 <- sqrt(sum(phi0^2)) / n
  se <- sqrt(sum(phi^2)) / n
  if (!all(is.finite(c(rd, se, se1, se0))) || se <= 0) {
    stop(
      "The adjusted risk difference cannot be estimated: its standard error ",
      "is not finite and positive.", hint,
      call. = FALSE
    )
  }

  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  statistic <- rd / se
  list(
    risk1 = risk1, risk0 = risk0, se1 = se1, se0 = se0,
    rd = rd, se = se,
    low = max(-1, rd - crit * se),
    high = min(1, rd + crit * se),
    statistic = statistic,
    p_value = 2 * stats::pnorm(-abs(statistic)),
    model = fit,
    influence = phi
  )
}

# ---------------------------------------------------------------------------
# NNTB / NNTH
# ---------------------------------------------------------------------------

#' Number needed to treat (benefit / harm) from a risk difference
#'
#' Altman (1998): reciprocal of the unrounded risk difference; when the
#' interval of the risk difference includes 0 the interval runs through
#' infinity and is stored as two segments, never as a finite pair.
#' @keywords internal
#' @noRd
.rd_nnt <- function(rd, low, high, event_type) {
  type_of <- function(sign_value) {
    if (identical(event_type, "harmful")) {
      if (sign_value < 0) "NNTB" else "NNTH"
    } else {
      if (sign_value < 0) "NNTH" else "NNTB"
    }
  }
  type <- if (rd == 0) NA_character_ else type_of(sign(rd))
  estimate <- if (rd == 0) Inf else 1 / abs(rd)
  crosses <- low < 0 && high > 0

  if (crosses) {
    types <- c(type_of(-1), type_of(1))
    from <- c(1 / abs(low), 1 / abs(high))
    # The side of the point estimate first; benefit first when RD is 0.
    first <- if (is.na(type)) which(types == "NNTB") else which(types == type)
    ord <- c(first, setdiff(1:2, first))
    segments <- data.frame(
      type = types[ord], from = from[ord], to = c(Inf, Inf),
      stringsAsFactors = FALSE
    )
  } else {
    side <- if (rd != 0) sign(rd) else if (high > 0) 1 else -1
    limits <- abs(c(low, high))
    segments <- data.frame(
      type = type_of(side),
      from = 1 / max(limits),
      to = if (min(limits) == 0) Inf else 1 / min(limits),
      stringsAsFactors = FALSE
    )
  }
  list(
    type = type,
    estimate = estimate,
    crosses_infinity = crosses,
    segments = segments
  )
}

# ---------------------------------------------------------------------------
# Display: strings, footnotes, rendering
# ---------------------------------------------------------------------------

#' Format numbers with a true minus sign and no negative zero
#' @keywords internal
#' @noRd
.rd_fmt_num <- function(x, digits = 1) {
  r <- round(x, digits)
  s <- formatC(abs(r), format = "f", digits = digits)
  ifelse(r < 0, paste0("\u2212", s), s)
}

#' Format a confidence level for column headers ("95", "97.5")
#' @keywords internal
#' @noRd
.rd_fmt_level <- function(conf_level) {
  format(100 * conf_level, trim = TRUE, digits = 6, scientific = FALSE)
}

#' Display string for NNTB / NNTH (Altman 1998)
#' @keywords internal
#' @noRd
.rd_fmt_nnt <- function(nnt) {
  whole <- function(x) {
    ifelse(is.finite(x), formatC(ceiling(x - 1e-9), format = "d", big.mark = ","),
           "\u221e")
  }
  seg <- nnt$segments
  head <- if (is.na(nnt$type)) {
    "\u221e"
  } else {
    paste(nnt$type, whole(nnt$estimate))
  }
  if (isTRUE(nnt$crosses_infinity)) {
    interval <- paste0(
      seg$type[1L], " ", whole(seg$from[1L]), " to \u221e to ",
      seg$type[2L], " ", whole(seg$from[2L])
    )
  } else {
    interval <- paste0(whole(seg$from[1L]), " to ", whole(seg$to[1L]))
  }
  paste0(head, " (", interval, ")")
}

#' Default column headers of a risk-difference table
#' @keywords internal
#' @noRd
.rd_default_headers <- function(res) {
  adjusted <- identical(res$analysis, "adjusted")
  level <- .rd_fmt_level(res$conf_level)
  reserved <- c("Characteristic", "is_header", "Events / N", "Risk",
                "Adjusted risk", "p-value")
  group <- res$variable_labels[[res$exposure]]
  if (is.null(group) || is.na(group) || !nzchar(group) ||
      identical(group, res$exposure) || group %in% reserved) {
    group <- "Group"
  }
  c(
    group = group,
    events = "Events / N",
    risk = if (adjusted) "Adjusted risk" else "Risk",
    rd = paste0(if (adjusted) "Adjusted risk difference (" else
      "Risk difference (", level, "% CI)"),
    p = "p-value",
    if (!is.null(res$nnt)) c(nnt = paste0("NNT (", level, "% CI)"))
  )
}

#' Footnotes of a risk-difference table
#' @keywords internal
#' @noRd
.rd_footnotes <- function(res) {
  adjusted <- identical(res$analysis, "adjusted")
  label_of <- function(v) {
    out <- res$variable_labels[[v]]
    if (is.null(out) || is.na(out) || !nzchar(out)) v else out
  }
  lv <- res$level_labels
  contrast <- paste0(lv$comparison, " \u2212 ", lv$reference,
                     " (groups of ", label_of(res$exposure), ")")
  event_text <- paste0(label_of(res$outcome), " = ", lv$event)

  first <- if (adjusted) {
    paste0(
      "Adjusted risk = standardised proportion with the event (", event_text,
      "). Adjusted risk difference = ", contrast,
      ", in percentage points. Events / N are the observed counts."
    )
  } else {
    paste0(
      "Risk = proportion with the event (", event_text,
      "). Risk difference = ", contrast, ", in percentage points."
    )
  }
  method <- if (adjusted) {
    c(
      paste0(
        "Adjusted risks are standardised to the covariate distribution of ",
        "the analysis population using logistic regression adjusted for ",
        paste(vapply(res$adjust_for, label_of, character(1)), collapse = ", "),
        "."
      ),
      paste0("Confidence interval and p-value: Wald, influence-function ",
             "standard error.")
    )
  } else {
    "Confidence interval and p-value: Miettinen\u2013Nurminen score method."
  }
  excluded <- res$n$excluded
  n_note <- paste0(
    "N = ", res$n$analysed, "; ",
    if (excluded == 0L) {
      "no observations excluded."
    } else {
      paste0(excluded, if (excluded == 1L) " observation" else " observations",
             " excluded because of missing values.")
    }
  )
  nnt_note <- if (!is.null(res$nnt)) {
    paste0(
      "NNTB/NNTH = number needed to treat for one additional person to ",
      "benefit / be harmed (event classified as ", res$event_type,
      "). It applies to the follow-up period of the study and assumes the ",
      "risk difference reflects the effect of the intervention."
    )
  }
  abbreviations <- if (!isTRUE(res$remove_abbreviations)) {
    paste0(
      "CI = confidence interval; Ref. = reference group",
      if (!is.null(res$nnt)) {
        paste0("; NNTB = number needed to treat (benefit); ",
               "NNTH = number needed to treat (harm)")
      },
      "."
    )
  }
  c(first, method, n_note, nnt_note, abbreviations, res$caveats)
}

#' Display data frame (formatted strings as shown)
#' @keywords internal
#' @noRd
.rd_display <- function(res, headers) {
  tb <- res$table_body
  adjusted <- identical(res$analysis, "adjusted")
  shown_risk <- if (adjusted) tb$adjusted_risk else tb$risk
  # Two decimals only for genuinely rare, non-zero risks. A risk of exactly
  # 0% is shown as 0.0% and does not change the precision of the table.
  digits <- if (any(shown_risk > 0 & shown_risk < 0.01, na.rm = TRUE)) 2L else 1L
  pct <- function(x) .rd_fmt_num(100 * x, digits)

  lv <- res$level_labels
  cells <- list(
    group = c(lv$reference, lv$comparison),
    events = paste0(tb$events, " / ", tb$n),
    risk = paste0(pct(shown_risk), "%"),
    rd = c("Ref.", paste0(pct(tb$rd[2L]), " (", pct(tb$rd_low[2L]), " to ",
                          pct(tb$rd_high[2L]), ")")),
    p = c("", .fmt_p(tb$p_value[2L]))
  )
  if (!is.null(res$nnt)) cells$nnt <- c("", .rd_fmt_nnt(res$nnt))
  out <- as.data.frame(cells[names(headers)], stringsAsFactors = FALSE)
  names(out) <- unname(headers)
  rownames(out) <- NULL
  out
}

#' Render a risk-difference display frame with the package table styling
#'
#' The generic builders require the regression layout (`Characteristic`,
#' `is_header`), so this small renderer applies the same theme helpers.
#' @keywords internal
#' @noRd
.rd_render <- function(display, footnotes, format, theme, caption = NULL) {
  theme <- .resolve_theme(theme)
  first <- names(display)[1L]
  others <- names(display)[-1L]

  if (identical(format, "gt")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Install 'gt' or use format = \"flextable\".", call. = FALSE)
    }
    tb <- gt::gt(display)
    tb <- gt::cols_align(tb, "left", columns = dplyr::all_of(first))
    tb <- gt::cols_align(tb, "center", columns = dplyr::all_of(others))
    tb <- gt::tab_options(
      tb,
      table.font.names         = "system-ui",
      data_row.padding         = gt::px(4),
      heading.background.color = "white",
      table.background.color   = "white"
    )
    tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                        locations = gt::cells_column_labels())
    tb <- .apply_theme_gt(tb, display, theme)
    for (note in footnotes) {
      tb <- gt::tab_source_note(tb, source_note = note)
    }
    tb <- .compact_gt_source_notes(tb)
    if (!is.null(caption)) tb <- gt::tab_caption(tb, caption = caption)
    return(tb)
  }

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Install 'flextable' or use format = \"gt\".", call. = FALSE)
  }
  ft <- flextable::flextable(display)
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- .apply_theme_flex(ft, display, theme)
  ft <- flextable::align(ft, j = first, align = "left", part = "all")
  ft <- flextable::align(ft, j = others, align = "center", part = "all")
  if ("zebra" %in% theme) {
    ft <- flextable::bg(ft, i = seq(1L, nrow(display), by = 2L),
                        bg = "#f6f8fa", part = "body")
  }
  ft <- flextable::autofit(ft)
  ft <- flextable::add_footer_lines(ft, values = footnotes)
  ft <- .compact_flex_footer(ft)
  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption = caption)
  ft
}

#' (Re)build display, footnotes and rendered table of a risk-difference object
#' @keywords internal
#' @noRd
.rd_assemble <- function(res) {
  headers <- .rd_default_headers(res)
  custom <- res$header_labels
  if (length(custom)) {
    hit <- intersect(names(custom), names(headers))
    headers[hit] <- unname(custom[hit])
  }
  res$table_display <- .rd_display(res, headers)
  res$footnotes <- .rd_footnotes(res)
  res$table <- .rd_render(res$table_display, res$footnotes, res$format,
                          res$theme, caption = res$caption)
  res
}

#' Friendly early rejection of risk-difference objects
#' @keywords internal
#' @noRd
.rd_reject <- function(objects, fn) {
  is_rd <- vapply(objects, inherits, logical(1), what = "risk_difference")
  if (any(is_rd)) {
    stop("risk_difference() tables are not supported by ", fn, "().",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' modify_table() for risk-difference tables: safe generic operations only
#'
#' Supported: `caption`, `caveat`, `variable_labels`, `level_labels`,
#' `header_labels` and `remove_abbreviations`. Regression-specific options
#' are refused before anything is changed.
#' @keywords internal
#' @noRd
.rd_modify_table <- function(tbl, variable_labels = NULL, level_labels = NULL,
                             header_labels = NULL, caption = NULL,
                             caveat = NULL, remove_abbreviations = FALSE,
                             unsupported = character(0)) {
  if (length(unsupported)) {
    stop(
      "modify_table(): ", paste0("`", unsupported, "`", collapse = ", "),
      if (length(unsupported) > 1L) " are" else " is",
      " not supported for risk_difference() tables; nothing was changed. ",
      "Supported arguments: caption, caveat, variable_labels, level_labels, ",
      "header_labels, remove_abbreviations.",
      call. = FALSE
    )
  }
  vars <- c(tbl$outcome, tbl$exposure, tbl$adjust_for)
  new <- tbl

  if (!is.null(variable_labels)) {
    bad <- setdiff(names(variable_labels), vars)
    if (length(bad) || anyNA(variable_labels)) {
      stop(
        "`variable_labels` can relabel only the variables of this table (",
        paste(vars, collapse = ", "), ")",
        if (length(bad)) paste0("; not found: ", paste(bad, collapse = ", ")),
        ".", call. = FALSE
      )
    }
    new$variable_labels[names(variable_labels)] <- unname(variable_labels)
  }

  if (!is.null(level_labels)) {
    if (!is.list(level_labels) || is.null(names(level_labels)) ||
        any(!nzchar(names(level_labels)))) {
      stop("`level_labels` must be a named list.", call. = FALSE)
    }
    bad <- setdiff(names(level_labels), c(tbl$outcome, tbl$exposure))
    if (length(bad)) {
      stop(
        "`level_labels` can relabel only the groups of `", tbl$exposure,
        "` and the event of `", tbl$outcome, "`; not found: ",
        paste(bad, collapse = ", "), ".", call. = FALSE
      )
    }
    for (nm in names(level_labels)) {
      map <- level_labels[[nm]]
      .must_be_named_character(map, paste0("level_labels$", nm),
                               allow_null = FALSE)
      known <- if (identical(nm, tbl$exposure)) {
        c(reference = tbl$reference, comparison = tbl$comparison)
      } else {
        c(event = tbl$event)
      }
      bad <- setdiff(names(map), known)
      if (length(bad) || anyNA(map)) {
        stop(
          "`level_labels$", nm, "` must be named by the displayed level(s): ",
          .rd_quote(known),
          if (length(bad)) paste0("; not found: ", .rd_quote(bad)),
          ".", call. = FALSE
        )
      }
      for (role in names(known)) {
        if (known[[role]] %in% names(map)) {
          new$level_labels[[role]] <- unname(map[[known[[role]]]])
        }
      }
    }
  }

  if (!is.null(header_labels)) {
    defaults <- .rd_default_headers(new)
    current <- defaults
    if (length(tbl$header_labels)) {
      hit <- intersect(names(tbl$header_labels), names(current))
      current[hit] <- unname(tbl$header_labels[hit])
    }
    old_defaults <- .rd_default_headers(tbl)
    custom <- if (is.null(tbl$header_labels)) character(0) else tbl$header_labels
    for (i in seq_along(header_labels)) {
      nm <- names(header_labels)[i]
      value <- unname(header_labels[[i]])
      key <- c(
        names(current)[current == nm], names(defaults)[defaults == nm],
        names(old_defaults)[old_defaults == nm],
        if (nm %in% c("p.value", "p_value", "pvalue")) "p",
        if (identical(nm, "estimate")) "rd"
      )
      if (!length(key)) {
        stop("`header_labels` contains columns not found in the table: ", nm,
             ". Current headers: ", paste(current, collapse = "; "), ".",
             call. = FALSE)
      }
      if (is.na(value) || !nzchar(value) ||
          value %in% c("Characteristic", "is_header")) {
        stop("`header_labels` values must be non-empty text other than ",
             "'Characteristic' or 'is_header'.", call. = FALSE)
      }
      custom[[key[1L]]] <- value
    }
    final <- defaults
    final[names(custom)] <- unname(custom)
    if (anyDuplicated(final)) {
      stop("`header_labels` would give two columns the same header.",
           call. = FALSE)
    }
    new$header_labels <- custom
  }

  if (isTRUE(remove_abbreviations)) new$remove_abbreviations <- TRUE
  if (!is.null(caveat) && nzchar(caveat)) {
    new$caveats <- unique(c(new$caveats, caveat))
  }
  if (!is.null(caption)) new$caption <- caption
  .rd_assemble(new)
}

# ---------------------------------------------------------------------------
# Public function
# ---------------------------------------------------------------------------

#' Risk difference between two groups
#'
#' Reports the absolute risk of a binary outcome in each of two groups and
#' the risk difference between them, with a confidence interval and p-value,
#' as a publication-ready table. The crude analysis uses the
#' Miettinen-Nurminen score method; the adjusted analysis standardises the
#' risks from a logistic regression model. Optionally the number needed to
#' treat (NNTB / NNTH) is added.
#'
#' @param data A data frame.
#' @param outcome Binary outcome column. Quoted and bare names are accepted.
#'   Logical, numeric 0/1 or 1/2, two-level factor, two-value character and
#'   value-labelled (\pkg{haven}) variables are supported.
#' @param exposure Column defining exactly two groups (for example treatment
#'   arm or exposure status). Quoted and bare names are accepted.
#' @param adjust_for Optional adjustment variables (quoted or bare names).
#'   \code{NULL} (default) gives the crude analysis; otherwise the adjusted
#'   (standardised) analysis is reported.
#' @param reference Group used as the reference. The risk difference is the
#'   other (comparison) group minus the reference group. \code{NULL} uses the
#'   first factor level, \code{FALSE}, \code{0}, or the first value in sorted
#'   order.
#' @param event Value of \code{outcome} that counts as the event. \code{NULL}
#'   uses \code{TRUE}, \code{1} (for 0/1 coding), \code{2} (for 1/2 coding),
#'   the second factor level, or the second value in sorted order.
#' @param event_type \code{NULL} (default), \code{"harmful"} or
#'   \code{"beneficial"}. When given, a column with the number needed to treat
#'   is added (see Details). The event direction is needed to tell benefit
#'   from harm, so no NNT is shown without it.
#' @param conf_level Confidence level of the interval; default \code{0.95}.
#'   It changes the confidence interval only. The p-value is always the
#'   two-sided test of a risk difference of zero and does not depend on
#'   \code{conf_level}, so, for example, a 99\% interval can include zero
#'   while the p-value is below 0.05.
#' @param format \code{"gt"} (default) or \code{"flextable"}.
#' @param theme Table theme: a preset name (\code{"minimal"},
#'   \code{"clinical"}, \code{"striped"}, \code{"shaded"}, \code{"jama"}) or a
#'   vector of primitives, as in the regression functions. \code{NULL}
#'   (default) uses the package default theme (\code{"minimal"}).
#'
#' @details
#' \strong{Absolute risk and risk difference.} The risk in a group is the
#' proportion of its members with the event. The risk difference (RD) is the
#' risk in the comparison group minus the risk in the reference group. It is
#' stored as a difference of proportions (for example \code{-0.08}) and shown
#' in percentage points (\code{-8.0}). A negative RD means that the event is
#' less frequent in the comparison group. For a harmful event (for example
#' death) a negative RD is an absolute risk reduction and a positive RD an
#' absolute risk increase; for a beneficial event (for example cure) the
#' reading is reversed. These are readings of the same number, not separate
#' estimates, and the table reports the signed RD only. The resolved event,
#' reference and comparison are stored in the result, are always written in
#' the table footnote, and are announced in a message whenever they were
#' inferred from anything other than logical or 0/1 coding.
#'
#' \strong{Analysis population.} One complete-case population is used:
#' observations with a missing outcome or exposure (and, for the adjusted
#' analysis, any missing adjustment variable) are excluded. The numbers
#' supplied, analysed and excluded are stored and the footnote reports them.
#'
#' \strong{Crude analysis} (\code{adjust_for = NULL}). Risks are the observed
#' proportions. The confidence interval is the Miettinen-Nurminen score
#' interval (restricted maximum-likelihood variance with the
#' \eqn{N/(N-1)} factor, no continuity correction) and the p-value comes from
#' the same score statistic at a difference of zero (equal to Pearson's
#' chi-square multiplied by \eqn{(N-1)/N}). The method is defined when a
#' group has no events or only events; when no one, or everyone, has the
#' event the p-value is 1.
#'
#' \strong{Adjusted analysis.} A main-effects logistic regression of the
#' outcome on the exposure and the adjustment variables is fitted. Each
#' person's risk is predicted twice from their own covariates, once as a
#' member of each group; the predictions are averaged over the analysis
#' population to give the two adjusted (standardised) risks, and their
#' difference is the adjusted RD. The table shows the observed events and
#' group sizes next to the adjusted risks. The standard error is the
#' influence-function standard error of the stacked estimating equations
#' (logistic score equations and the two averages), which accounts for the
#' estimation of the model and for the sampling of the covariates and does
#' not rely on the logistic model being correct. The confidence interval is a
#' Wald interval, with limits restricted to the range -1 to 1, and the
#' p-value is two-sided normal. No small-sample correction is applied; in
#' simulations with about 80 to 100 participants coverage of the 95\%
#' interval was about 94\%. The analysis stops with an error, and returns no
#' table, when the model does not converge, when fitted probabilities reach 0
#' or 1 (separation, including a group with no events or only events), when
#' the exposure or an adjustment variable is aliased, or when the outcome is
#' constant.
#'
#' \strong{Interpretation.} The function describes a calculation and makes no
#' causal claim. In an individually randomised trial the adjusted RD is a
#' covariate-adjusted estimate of the marginal treatment difference, and it
#' remains valid when the logistic model is not exactly right (see the FDA
#' 2023 guidance on covariate adjustment). In a non-randomised study it is a
#' standardised risk difference; it can be read causally only if there is no
#' unmeasured confounding, the outcome model is correct and the groups
#' overlap in their covariates.
#'
#' \strong{NNTB and NNTH.} With \code{event_type}, the number needed to treat
#' is the reciprocal of the absolute value of the unrounded RD, shown rounded
#' up to a whole person. It is labelled NNTB (number needed to treat for one
#' additional person to benefit) when the comparison group does better and
#' NNTH (for one additional person to be harmed) when it does worse: for a
#' harmful event a negative RD gives NNTB and a positive RD NNTH, and the
#' reverse for a beneficial event. When the confidence interval of the RD
#' includes zero, the interval of the NNT runs from a finite NNTB through
#' infinity to a finite NNTH and is shown and stored in that form (Altman
#' 1998), never as a pair of finite limits. The NNT applies to the follow-up
#' period of the study and assumes that the RD reflects what the intervention
#' does.
#'
#' \strong{Limits of this version.} Binary outcome at a fixed follow-up;
#' exactly two groups; independent observations; main-effects logistic model
#' for adjustment. Not supported: more than two groups, continuous exposures,
#' stratified (Mantel-Haenszel) or clustered designs, non-inferiority margins,
#' time-to-event outcomes. The result can be printed, saved with
#' \code{\link{save_table}} and relabelled with \code{\link{modify_table}}
#' (caption, caveat, labels and headers only); it is not accepted by
#' \code{\link{merge_tables}} or the forest-plot functions.
#'
#' @return A list of class \code{c("risk_difference", "gtregression")} with
#' elements:
#' \describe{
#'   \item{table}{The rendered \code{gt_tbl} or \code{flextable}.}
#'   \item{table_display}{Data frame of the formatted cells as shown.}
#'   \item{table_body}{Data frame of numeric results, one row per group
#'   (reference first): \code{group}, \code{events}, \code{n}, \code{risk},
#'   \code{adjusted_risk}, \code{adjusted_risk_se}, \code{rd}, \code{rd_se},
#'   \code{rd_low}, \code{rd_high}, \code{p_value}. Risks and differences are
#'   proportions. \code{rd_se} is the influence-function standard error and
#'   is \code{NA} for the crude analysis, whose score interval is not based
#'   on a single standard error.}
#'   \item{footnotes}{Character vector of table footnotes.}
#'   \item{analysis}{\code{"crude"} or \code{"adjusted"}.}
#'   \item{outcome, exposure, adjust_for}{Variable names.}
#'   \item{event, reference, comparison}{Resolved event value and groups.}
#'   \item{event_type, conf_level}{As supplied.}
#'   \item{method}{List naming the estimate, interval, test and variance
#'   methods.}
#'   \item{n}{List with the numbers of rows \code{supplied}, \code{analysed}
#'   and \code{excluded}.}
#'   \item{nnt}{\code{NULL}, or a list with \code{type} (\code{"NNTB"},
#'   \code{"NNTH"} or \code{NA}), \code{estimate} (unrounded, may be
#'   \code{Inf}), \code{crosses_infinity} and \code{segments} (data frame
#'   with \code{type}, \code{from}, \code{to}; two rows ending at \code{Inf}
#'   when the interval runs through infinity).}
#'   \item{model}{The fitted logistic model (adjusted analysis), else
#'   \code{NULL}. In the model the outcome is \code{.rd_y} (1 = event) and
#'   the exposure is \code{.rd_a} (1 = comparison group).}
#'   \item{variable_labels, level_labels, theme}{Labels and theme used for
#'   display.}
#'   \item{source, format}{\code{"risk_difference"}; \code{"gt"} or
#'   \code{"flextable"}.}
#' }
#'
#' @references
#' Miettinen O, Nurminen M (1985). Comparative analysis of two rates.
#' \emph{Statistics in Medicine}, 4(2), 213-226.
#'
#' Newcombe RG (1998). Interval estimation for the difference between
#' independent proportions: comparison of eleven methods.
#' \emph{Statistics in Medicine}, 17(8), 873-890.
#'
#' Altman DG (1998). Confidence intervals for the number needed to treat.
#' \emph{BMJ}, 317(7168), 1309-1312.
#'
#' U.S. Food and Drug Administration (2023). Adjusting for Covariates in
#' Randomized Clinical Trials for Drugs and Biological Products: Guidance for
#' Industry.
#'
#' @seealso \code{\link{uni_reg}} and \code{\link{multi_reg}} for relative
#'   measures (odds ratios, risk ratios).
#'
#' @examples
#' # Crude: low birth weight by maternal smoking
#' rd <- risk_difference(data_birthwt, low, smoke)
#' rd$table_display
#' rd$footnotes
#'
#' # Adjusted (standardised), with the number needed to treat
#' adj <- risk_difference(
#'   data_birthwt, "low", "smoke",
#'   adjust_for = c("age", "lwt"),
#'   event_type = "harmful",
#'   format = "flextable"
#' )
#' adj$table_display
#' adj$table_body
#' adj$nnt
#'
#' # Factor coding: name the event and the reference explicitly
#' bw <- data_birthwt
#' bw$smoking <- factor(bw$smoke, levels = c(0, 1), labels = c("No", "Yes"))
#' bw$weight <- factor(bw$low, levels = c(0, 1), labels = c("Normal", "Low"))
#' risk_difference(bw, weight, smoking, event = "Low", reference = "No",
#'                 conf_level = 0.90)$table_display
#'
#' @export
risk_difference <- function(data,
                            outcome,
                            exposure,
                            adjust_for = NULL,
                            reference = NULL,
                            event = NULL,
                            event_type = NULL,
                            conf_level = 0.95,
                            format = "gt",
                            theme = NULL) {
  data_names <- if (is.data.frame(data)) names(data) else NULL
  outcome <- .vars_arg(substitute(outcome), env = parent.frame(),
                       data_names = data_names)
  exposure <- .vars_arg(substitute(exposure), env = parent.frame(),
                        data_names = data_names)
  adjust_for <- .vars_arg(substitute(adjust_for), env = parent.frame(),
                          allow_null = TRUE, data_names = data_names)
  format <- .choice_arg(substitute(format), env = parent.frame(),
                        choices = c("gt", "flextable"))
  event_type <- .choice_arg(substitute(event_type), env = parent.frame(),
                            choices = c("harmful", "beneficial"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  if (is.null(theme)) theme <- "minimal"
  .validate_theme_arg(theme)
  if (!is.null(adjust_for)) adjust_for <- unique(adjust_for)
  .rd_validate_args(data, outcome, exposure, adjust_for, event, reference,
                    event_type, conf_level, format)

  # ---- one complete-case analysis population -------------------------------
  vars <- c(outcome, exposure, adjust_for)
  d <- as.data.frame(data)[, vars, drop = FALSE]
  for (v in vars) d[[v]] <- .rd_unlabel(d[[v]], v)
  keep <- stats::complete.cases(d)
  n_counts <- list(
    supplied = nrow(d),
    analysed = sum(keep),
    excluded = nrow(d) - sum(keep)
  )
  if (n_counts$analysed == 0L) {
    stop("No complete observations for ", paste(vars, collapse = ", "), ".",
         call. = FALSE)
  }
  d <- d[keep, , drop = FALSE]

  # ---- event, reference and comparison -------------------------------------
  ev <- .rd_resolve_event(d[[outcome]], outcome, event)
  gr <- .rd_resolve_reference(d[[exposure]], exposure, reference)
  if (ev$inferred) {
    message(
      "Outcome `", outcome, "`: the event is '", ev$event,
      "' (risk = proportion with ", outcome, " = ", ev$event,
      "). Use `event = ` to change it."
    )
  }
  if (gr$inferred) {
    message(
      "Exposure `", exposure, "`: risk difference = '", gr$comparison,
      "' minus '", gr$reference, "' (reference group: '", gr$reference,
      "'). Use `reference = ` to change it."
    )
  }
  y <- ev$y01
  a <- gr$a01
  n0 <- sum(a == 0L)
  n1 <- sum(a == 1L)
  x0 <- sum(y[a == 0L])
  x1 <- sum(y[a == 1L])

  table_body <- data.frame(
    group = c(gr$reference, gr$comparison),
    events = c(x0, x1),
    n = c(n0, n1),
    risk = c(x0 / n0, x1 / n1),
    adjusted_risk = NA_real_,
    adjusted_risk_se = NA_real_,
    rd = NA_real_,
    rd_se = NA_real_,
    rd_low = NA_real_,
    rd_high = NA_real_,
    p_value = NA_real_,
    stringsAsFactors = FALSE
  )

  model <- NULL
  if (is.null(adjust_for)) {
    analysis <- "crude"
    est <- .rd_mn(x1, n1, x0, n0, conf_level)
    method <- list(
      estimate = "Difference in observed proportions",
      interval = "Miettinen-Nurminen score interval (no continuity correction)",
      test = "Miettinen-Nurminen score test",
      variance = "Restricted maximum-likelihood variance with N/(N-1) factor"
    )
  } else {
    analysis <- "adjusted"
    est <- .rd_standardise(y, a, d[, adjust_for, drop = FALSE], conf_level)
    model <- est$model
    table_body$adjusted_risk <- c(est$risk0, est$risk1)
    table_body$adjusted_risk_se <- c(est$se0, est$se1)
    table_body$rd_se[2L] <- est$se
    method <- list(
      estimate = paste0(
        "Standardisation over the analysis population from a main-effects ",
        "logistic regression"
      ),
      interval = "Wald interval, limits clipped to [-1, 1]",
      test = "Wald test (two-sided, normal)",
      variance = "influence-function standard error"
    )
  }
  table_body$rd[2L] <- est$rd
  table_body$rd_low[2L] <- est$low
  table_body$rd_high[2L] <- est$high
  table_body$p_value[2L] <- est$p_value

  nnt <- if (is.null(event_type)) {
    NULL
  } else {
    .rd_nnt(est$rd, est$low, est$high, event_type)
  }

  res <- list(
    table = NULL,
    table_display = NULL,
    table_body = table_body,
    footnotes = NULL,
    analysis = analysis,
    outcome = outcome,
    exposure = exposure,
    adjust_for = adjust_for,
    event = ev$event,
    reference = gr$reference,
    comparison = gr$comparison,
    event_type = event_type,
    conf_level = conf_level,
    method = method,
    n = n_counts,
    nnt = nnt,
    model = model,
    variable_labels = .var_label_map(data, vars),
    level_labels = list(event = ev$event, reference = gr$reference,
                        comparison = gr$comparison),
    theme = theme,
    source = "risk_difference",
    format = format
  )
  res <- .rd_assemble(res)
  class(res) <- c("risk_difference", "gtregression")
  res
}
