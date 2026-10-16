#' Internal validation helpers for model setup
#' Validate regression approach by context
#'
#' Ensures that only valid modeling approaches are used in each function context
#'
#' @param approach A character string specifying the regression method.
#' @param context The name of the function using the approach (e.g., "uni_reg").
#'
#' @return Stops with an informative error if the approach is not allowed.
#' @keywords internal
#' @noRd
.validate_approach <- function(approach, context = NULL) {
  approach_input <- approach
  approach <- .normalize_approach(approach)

  # Define valid approaches per context
  valid <- switch(context,
                  "uni_reg" = c("logit", "logbinomial", "poisson",
                                "robpoisson", "linear", "negbin", "firth"),
                  "multi_reg" = c("logit", "logbinomial", "poisson",
                                  "robpoisson", "linear", "negbin", "firth"),
                  "interaction_models" = c("logit", "logbinomial", "poisson",
                                           "robpoisson", "negbin", "linear",
                                           "cox", "survreg"),
                  "check_convergence" = c("logit", "logbinomial", "poisson",
                                          "robpoisson", "negbin"),
                  "identify_confounder" = c("logit", "logbinomial", "poisson",
                                            "robpoisson", "negbin", "linear",
                                            "cox", "survreg"),
                  "select_models" = c("logit", "logbinomial", "poisson",
                                      "robpoisson", "negbin", "linear",
                                      "cox", "survreg"),
                  stop("The function '", context, "' is not recognized.\n",
                       "Please use a valid function")
  )

  # Validate the chosen approach
  if (!approach %in% valid) {
    stop(approach_input, "  is not a valid approach for ", context, ".\n",
         "Valid options: ", paste(valid, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Exposure Variable(s) for Regression
#'
#' Ensures that the exposure variable has at least two non-missing levels or
#' sufficient numeric variation to support regression modelling.
#' @param exposures Character vector of column names to validate.
#' @param data A data frame containing the exposure variables.
#' @return Returns TRUE if valid; otherwise throws an error.
#' @keywords internal
.validate_exposures <- function(data, exposures) {
  # Initialize lists to hold variable names with different issues
  vars_all_na <- c()
  vars_invalid_cat <- c()
  vars_invalid_num <- c()
  vars_invalid_type <- c()

  for (var in exposures) {
    x <- data[[var]]

    # Check for all NAs
    if (all(is.na(x))) {
      vars_all_na <- c(vars_all_na, var)
      next
    }

    # Drop NA values for observed-level checks.
    x_non_na <- x[!is.na(x)]

    # Type checks should use the original extracted vector. Some classes can
    # lose useful metadata after subsetting, while the observed-level count
    # should use the non-missing values.
    is_categorical <- is.factor(x) || is.character(x)
    is_numeric <- is.numeric(x)

    if (is_categorical) {
      if (length(unique(as.character(x_non_na))) < 2) {
        vars_invalid_cat <- c(vars_invalid_cat, var)
      }
    } else if (is_numeric) {
      if (length(unique(x_non_na)) <= 1) {
        vars_invalid_num <- c(vars_invalid_num, var)
      }
    } else {
      vars_invalid_type <- c(vars_invalid_type, var)
    }
  }

  # Combine all problems into one message, one line per problem type
  describe <- function(prefix, vars) {
    if (!length(vars)) return(NULL)
    paste0(prefix, ": ", paste(vars, collapse = ", "), ".")
  }
  error_msgs <- c(
    describe("Exposure with all missing values", vars_all_na),
    describe("Exposure with <2 levels", vars_invalid_cat),
    describe("Exposure with no variation", vars_invalid_num),
    describe(
      "Exposure with unsupported types (use numeric, factor or character)",
      vars_invalid_type
    )
  )

  if (length(error_msgs) > 0) {
    stop(
      paste(c(error_msgs, "Please fix them to proceed."), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
# validate outcome against the approaches used
#' @keywords internal
#' @noRd
.validate_outcome_by_approach <- function(outcome, approach,
                                          allow_binary_poisson = FALSE) {
  approach <- .normalize_approach(approach)

  # Missing outcome values in cols
  if (all(is.na(outcome))) {
    stop("All values in the outcome variable are missing.",
         call. = FALSE)
  }
  # Conditional check binary outcomes
  is_binary <- function(outcome) {
    # Check for factor or character with exactly 2 levels
    if (is.factor(outcome) || is.character(outcome)) {
      return(length(unique(na.omit(outcome))) == 2)
    }

    # Logical outcomes: TRUE is the event
    if (is.logical(outcome)) {
      return(TRUE)
    }

    # Check for numeric with values in 0/1 or 1/2
    if (is.numeric(outcome)) {
      vals <- unique(na.omit(outcome))
      return(all(vals %in% c(0, 1)) || all(vals %in% c(1, 2)))
    }
    return(FALSE)
  }

  # check for Non-negative integers
  is_count <- function(outcome) {
    is.numeric(outcome) &&
      all(outcome >= 0 &
            outcome == floor(outcome), na.rm = TRUE) &&
      length(unique(outcome[!is.na(outcome)])) >= 1
  }

  # check for numeric- includes whole numbers and decimals
  is_continuous <- function(outcome) {
    is.numeric(outcome) && length(unique(na.omit(outcome))) > 2
  }

  # apply logic
  if (approach %in% c("logit", "firth", "logbinomial", "robpoisson") &&
      !is_binary(outcome)) {
    stop("This approach requires either a factor variable ",
         "or numeric variable coded as 0 and 1 (or 1 and 2). ",
         "Two-level character outcomes (second level = event) and logical ",
         "outcomes (TRUE = event) are also accepted.", call. = FALSE)
  }
  # uni_reg()/multi_reg() and the stratified functions recode two-level
  # factor, character and logical outcomes to 0/1 for Poisson models as well.
  if (isTRUE(allow_binary_poisson) && approach == "poisson" &&
      (is.logical(outcome) ||
      ((is.factor(outcome) || is.character(outcome)) && is_binary(outcome)))) {
    return(invisible(TRUE))
  }
  if (approach == "poisson" && !is_count(outcome)) {
    stop("Poisson regression requires a count outcome.", call. = FALSE)
  }
  if (approach == "negbin" && !is_count(outcome)) {
    stop("Negative binomial requires a count outcome.", call. = FALSE)
  }
  if (approach == "linear" && !is_continuous(outcome)) {
    stop("Linear regression requires a continuous outcome.", call. = FALSE)
  }
}

#' Binary-outcome approaches
#' @keywords internal
#' @noRd
.binary_outcome_approaches <- function() {
  c("logit", "firth", "logbinomial", "robpoisson")
}

#' Recode a binary outcome to 0/1
#'
#' One rule for every approach that models a binary outcome (`logit`, `firth`,
#' `logbinomial`, `robpoisson`) and for `poisson` when the outcome is not
#' numeric:
#' * two-level factor or character: the second level (factor order, or
#'   alphabetical order for character) is the event;
#' * logical: `TRUE` is the event;
#' * numeric (double or integer) coded 1/2: 2 is the event.
#'
#' The decision must be taken once on the full data, before any split into
#' strata, so that every stratum is fitted on the same coding.
#'
#' @return A list with `data` (outcome recoded when needed), `event` (label of
#'   the event level or `NULL`) and `reference`.
#' @keywords internal
#' @noRd
.recode_binary_outcome <- function(data, outcome, approach) {
  approach <- .normalize_approach(approach)
  out <- list(data = data, event = NULL, reference = NULL, recoded = FALSE)
  if (!outcome %in% names(data)) return(out)
  y <- data[[outcome]]
  binary <- approach %in% .binary_outcome_approaches()
  if (!binary && !identical(approach, "poisson")) return(out)

  keep_label <- attr(y, "label", exact = TRUE)
  set_y <- function(value, event, reference) {
    if (!is.null(keep_label)) attr(value, "label") <- keep_label
    data[[outcome]] <- value
    list(data = data, event = event, reference = reference, recoded = TRUE)
  }

  if (is.logical(y)) {
    return(set_y(as.numeric(y), "TRUE", "FALSE"))
  }
  if (is.factor(y) || is.character(y)) {
    y_chr <- as.character(y)
    lev <- if (is.factor(y)) {
      levels(y)[levels(y) %in% unique(y_chr[!is.na(y_chr)])]
    } else {
      sort(unique(y_chr[!is.na(y_chr)]))
    }
    if (length(lev) > 2L) return(out)
    if (length(lev) == 2L) {
      return(set_y(as.numeric(y_chr == lev[2L]), lev[2L], lev[1L]))
    }
    # A single observed level (for example inside one stratum): keep the
    # position of that level among the declared factor levels.
    if (is.factor(y) && nlevels(y) == 2L && length(lev) == 1L) {
      return(set_y(as.numeric(y_chr == levels(y)[2L]),
                   levels(y)[2L], levels(y)[1L]))
    }
    return(out)
  }
  if (binary && is.numeric(y)) {
    observed <- unique(y[!is.na(y)])
    if (length(observed) && all(observed %in% c(1, 2)) && any(observed == 2)) {
      return(set_y(as.numeric(y) - 1, "2", "1"))
    }
  }
  out
}

#' Recode the outcome once at the top of a user-facing function
#'
#' Emits a single message naming the event level for factor and character
#' outcomes (nothing is said for levels "0"/"1", logical or numeric outcomes).
#' @keywords internal
#' @noRd
.normalize_outcome_once <- function(data, outcome, approach, quiet = FALSE) {
  if (!is.data.frame(data) || !is.character(outcome) || length(outcome) != 1L ||
      !outcome %in% names(data)) {
    return(data)
  }
  y <- data[[outcome]]
  res <- .recode_binary_outcome(data, outcome, approach)
  if (isTRUE(res$recoded) && !isTRUE(quiet) &&
      (is.factor(y) || is.character(y)) &&
      !identical(c(res$reference, res$event), c("0", "1"))) {
    message(
      "Outcome `", outcome, "`: modelling the event '", res$event,
      "' (second level); reference level is '", res$reference, "'."
    )
  }
  res$data
}

#' Clear errors for variables used in more than one role
#' @keywords internal
#' @noRd
.validate_variable_roles <- function(outcome, exposures, adjust_for = NULL,
                                     stratifier = NULL) {
  if (is.character(exposures) && anyDuplicated(exposures)) {
    stop(
      "Duplicate exposure(s): ",
      paste(unique(exposures[duplicated(exposures)]), collapse = ", "),
      ". List each exposure once.",
      call. = FALSE
    )
  }
  if (is.character(outcome) && length(outcome) == 1L) {
    if (outcome %in% exposures) {
      stop("The outcome `", outcome, "` cannot also be listed as an exposure.",
           call. = FALSE)
    }
    if (!is.null(stratifier) && identical(stratifier, outcome)) {
      stop("The stratifier cannot be the outcome variable (`", outcome, "`).",
           call. = FALSE)
    }
  }
  if (!is.null(stratifier) && length(stratifier) == 1L) {
    clash <- intersect(stratifier, c(exposures, adjust_for))
    if (length(clash)) {
      stop("The stratifier `", stratifier,
           "` cannot also be an exposure or adjustment variable.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate a table theme
#' @keywords internal
#' @noRd
.validate_theme_arg <- function(theme) {
  presets <- c("minimal", "clinical", "striped", "shaded", "jama")
  primitives <- c("plain", "zebra", "lines", "labels_bold", "compact",
                  "header_shaded")
  ok <- is.character(theme) && length(theme) >= 1L && !anyNA(theme) &&
    all(tolower(theme) %in% c(presets, primitives))
  if (!ok) {
    stop(
      "Invalid `theme`. Use one of the presets: ",
      paste(presets, collapse = ", "),
      "; or a vector of primitives: ", paste(primitives, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Backtick non-syntactic variable names for formula construction
#' @keywords internal
#' @noRd
.formula_name <- function(x) {
  x <- as.character(x)
  bare <- gsub("`", "", x, fixed = TRUE)
  ifelse(make.names(bare) == bare, bare, paste0("`", bare, "`"))
}

#' Backtick the variables of an interaction string such as "a*b"
#' @keywords internal
#' @noRd
.formula_interaction <- function(interaction) {
  parts <- trimws(strsplit(interaction, "*", fixed = TRUE)[[1]])
  paste(.formula_name(parts), collapse = "*")
}

#' Prepare variables for ordinary regression engines
#'
#' Factor predictors are always displayed as level-versus-reference rows, so
#' they must be fitted with treatment contrasts: ordered factors (polynomial
#' contrasts) are made unordered and a `contrasts` attribute set by the user
#' (for example `contr.sum`) is removed. The fitting helpers additionally force
#' `options(contrasts = )` to treatment contrasts while the model is fitted.
#' Binary outcomes are normalised to 0/1 (see `.recode_binary_outcome()`).
#'
#' @keywords internal
#' @noRd
.prepare_regression_model_data <- function(data, outcome, predictors, approach) {
  out <- data
  for (variable in intersect(unique(predictors), names(out))) {
    value <- out[[variable]]
    if (is.factor(value) &&
        (is.ordered(value) || !is.null(attr(value, "contrasts", exact = TRUE)))) {
      label <- attr(value, "label", exact = TRUE)
      value <- factor(as.character(value), levels = levels(value),
                      ordered = FALSE)
      if (!is.null(label)) attr(value, "label") <- label
      out[[variable]] <- value
    }
  }

  .recode_binary_outcome(out, outcome, approach)$data
}

#' Fit with treatment contrasts whatever `options(contrasts = )` says
#' @keywords internal
#' @noRd
.with_treatment_contrasts <- function(expr) {
  old <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old), add = TRUE)
  force(expr)
}

#' Starting values for a log-binomial model
#'
#' Intercept `log(mean(y))`, all other coefficients 0: always a valid start
#' (fitted probabilities equal to the observed risk).
#' @keywords internal
#' @noRd
.logbinomial_start <- function(formula, data) {
  mf <- stats::model.frame(formula, data = data)
  y <- stats::model.response(mf)
  if (is.factor(y)) y <- as.numeric(y != levels(y)[1L])
  mm <- stats::model.matrix(stats::terms(mf), mf)
  start <- rep(0, ncol(mm))
  has_intercept <- "(Intercept)" %in% colnames(mm)
  risk <- mean(as.numeric(y))
  if (has_intercept && is.finite(risk) && risk > 0 && risk < 1) {
    start[match("(Intercept)", colnames(mm))] <- log(risk)
  } else if (has_intercept) {
    start[match("(Intercept)", colnames(mm))] <- log(0.5)
  }
  start
}

#' Fit a log-binomial model, retrying with explicit starting values
#'
#' The default `glm()` start is tried first so that models that already
#' converged are unchanged; only when it fails are starting values supplied.
#' @keywords internal
#' @noRd
.fit_logbinomial <- function(formula, data) {
  first <- tryCatch(
    stats::glm(formula, data = data, family = stats::binomial("log")),
    error = function(e) e
  )
  if (!inherits(first, "error")) return(first)
  start <- .logbinomial_start(formula, data)
  second <- tryCatch(
    suppressWarnings(
      stats::glm(formula, data = data, family = stats::binomial("log"),
                 start = start, control = stats::glm.control(maxit = 100))
    ),
    error = function(e) e
  )
  if (inherits(second, "error")) {
    stop(conditionMessage(second), call. = FALSE)
  }
  # A refit that stops on the boundary of the parameter space (fitted risks
  # of 1) without converging is not a usable estimate: report it as a failure
  # instead of printing its coefficients as a normal result.
  if (!isTRUE(second$converged)) {
    stop(
      "the log-binomial model did not converge",
      if (isTRUE(second$boundary)) " (fitted risks reached 1)" else "",
      call. = FALSE
    )
  }
  second
}

#' Hint appended to fitting errors
#' @keywords internal
#' @noRd
.fit_failure_hint <- function(approach) {
  if (identical(approach, "logbinomial")) {
    return(paste0(
      " Log-binomial models often fail to converge when fitted risks ",
      "approach 1; consider approach = \"robpoisson\" (robust Poisson), ",
      "which estimates the same risk ratios."
    ))
  }
  ""
}

# validate inputs for uni reg
#' @keywords internal
#' @noRd
.validate_uni_inputs <- function(data,
                                 outcome,
                                 exposures,
                                 approach) {
  # validate approach in the function
  .validate_approach(approach, context = "uni_reg")

  # check variable presence
  if (!outcome %in% names(data))
    stop("Outcome variable not found in the dataset.", call. = FALSE)

  # check variable presence >1
  if (!all(exposures %in% names(data)))
    stop("One or more exposure variables were not found in the dataset.",
         call. = FALSE)

  .validate_variable_roles(outcome, exposures)

  # outcome variable validation
  .validate_outcome_by_approach(data[[outcome]], approach,
                                allow_binary_poisson = TRUE)

  # Validate exposures
  .validate_exposures(data, exposures)
}

#' Validate inputs for multi_reg
#'
#' @keywords internal
#' @noRd
.validate_multi_inputs <- function(data,
                                   outcome,
                                   exposures,
                                   approach,
                                   adjust_for = NULL,
                                   interaction = NULL) {

  .validate_approach(approach, context = "multi_reg")

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(outcome) || length(outcome) != 1) {
    stop("`outcome` must be a single character string.", call. = FALSE)
  }

  .validate_variable_roles(outcome, exposures)
  exposures <- unique(exposures)

  if (!outcome %in% names(data)) {
    stop("Outcome variable not found in the dataset.", call. = FALSE)
  }

  if (!is.character(exposures) || length(exposures) < 1) {
    stop("`exposures` must be a character vector with at least one variable.",
         call. = FALSE)
  }

  if (!all(exposures %in% names(data))) {
    stop(
      "One or more exposure variables were not found in the dataset.",
      call. = FALSE
    )
  }

  .validate_outcome_by_approach(data[[outcome]], approach,
                                allow_binary_poisson = TRUE)
  .validate_exposures(data, exposures)

  adjusted_mode <- !is.null(adjust_for) && length(adjust_for) > 0

  if (adjusted_mode) {
    adjust_for <- unique(adjust_for)

    if (!is.character(adjust_for)) {
      stop("`adjust_for` must be a character vector.", call. = FALSE)
    }

    if (!all(adjust_for %in% names(data))) {
      stop(
        "One or more adjustment variables were not found in the dataset.",
        call. = FALSE
      )
    }

    if (outcome %in% adjust_for) {
      stop("Outcome variable cannot be included in `adjust_for`.", call. = FALSE)
    }

    if (any(exposures %in% adjust_for)) {
      stop(
        "In adjusted mode, `exposures` and `adjust_for` must not overlap.",
        call. = FALSE
      )
    }

    .validate_exposures(data, adjust_for)
  } else {
    adjust_for <- NULL
  }

  interaction_vars <- character(0)

  if (!is.null(interaction) && length(interaction) > 0) {
    if (!is.character(interaction) || length(interaction) != 1) {
      stop(
        "`interaction` must be a single character string such as 'bmi*sex'.",
        call. = FALSE
      )
    }

    if (grepl(":", interaction, fixed = TRUE)) {
      stop(
        "Use standard interaction syntax with '*', for example 'bmi*sex', not ':'.",
        call. = FALSE
      )
    }

    if (!grepl("\\*", interaction)) {
      stop(
        "`interaction` must contain '*', for example 'bmi*sex'.",
        call. = FALSE
      )
    }

    interaction_vars <- trimws(unlist(strsplit(interaction, "\\*")))
    interaction_vars <- interaction_vars[nzchar(interaction_vars)]

    if (length(interaction_vars) != 2) {
      stop(
        "`interaction` must contain exactly two variables, e.g. 'bmi*sex'.",
        call. = FALSE
      )
    }

    if (!all(interaction_vars %in% names(data))) {
      stop(
        "One or more variables in `interaction` were not found in the dataset.",
        call. = FALSE
      )
    }

    if (adjusted_mode && length(exposures) != 1) {
      stop(
        "When `interaction` is supplied with `adjust_for`, please provide a single exposure.",
        call. = FALSE
      )
    }

    if (!any(exposures %in% interaction_vars)) {
      stop(
        "The exposure must be part of the interaction term.",
        call. = FALSE
      )
    }
  } else {
    interaction <- NULL
  }

  vars_needed <- unique(c(outcome, exposures, adjust_for, interaction_vars))

  cc_idx <- stats::complete.cases(data[, vars_needed, drop = FALSE])
  data_clean <- data[cc_idx, , drop = FALSE]

  if (nrow(data_clean) == 0) {
    stop("No complete cases available for analysis.", call. = FALSE)
  }

  vars_to_check <- unique(c(exposures, adjust_for, interaction_vars))
  vars_to_check <- vars_to_check[vars_to_check %in% names(data_clean)]

  insufficient_vars <- vars_to_check[
    vapply(
      data_clean[, vars_to_check, drop = FALSE],
      function(x) length(unique(stats::na.omit(x))) < 2,
      logical(1)
    )
  ]

  if (length(insufficient_vars) > 0) {
    stop(
      "Variable(s) with less than 2 unique values after complete-case filtering: ",
      paste(insufficient_vars, collapse = ", "),
      call. = FALSE
    )
  }

  data_clean
}

#' Validate and split an interaction term
#'
#' @keywords internal
#' @noRd
.validate_interaction_term <- function(data,
                                       exposures,
                                       interaction = NULL,
                                       adjusted_mode = FALSE,
                                       exposure_by_exposure = FALSE) {
  if (is.null(interaction) || !length(interaction)) {
    return(character(0))
  }

  if (!is.character(interaction) || length(interaction) != 1L) {
    stop(
      "`interaction` must be a single character string such as 'bmi*sex'.",
      call. = FALSE
    )
  }

  if (grepl(":", interaction, fixed = TRUE)) {
    stop(
      "Use standard interaction syntax with '*', for example 'bmi*sex', not ':'.",
      call. = FALSE
    )
  }

  if (!grepl("\\*", interaction)) {
    stop(
      "`interaction` must contain '*', for example 'bmi*sex'.",
      call. = FALSE
    )
  }

  interaction_vars <- trimws(unlist(strsplit(interaction, "\\*")))
  interaction_vars <- interaction_vars[nzchar(interaction_vars)]

  if (length(interaction_vars) != 2L) {
    stop(
      "`interaction` must contain exactly two variables, e.g. 'bmi*sex'.",
      call. = FALSE
    )
  }

  if (!all(interaction_vars %in% names(data))) {
    stop(
      "One or more variables in `interaction` were not found in the dataset.",
      call. = FALSE
    )
  }

  if ((adjusted_mode || exposure_by_exposure) && length(exposures) != 1L) {
    stop(
      paste0(
        "When `interaction` is supplied in exposure-by-exposure mode, please provide a single ",
        "exposure or set `multivariable = TRUE`."
      ),
      call. = FALSE
    )
  }

  if (!any(exposures %in% interaction_vars)) {
    stop("The exposure must be part of the interaction term.", call. = FALSE)
  }

  interaction_vars
}
