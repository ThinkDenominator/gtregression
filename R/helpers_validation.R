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
  # Define valid approaches per context
  valid <- switch(context,
                  "uni_reg" = c("logit", "log-binomial", "poisson",
                                "robpoisson", "linear", "negbin"),
                  "multi_reg" = c("logit", "log-binomial", "poisson",
                                  "robpoisson", "linear", "negbin"),
                  "interaction_models" = c("logit", "log-binomial", "poisson",
                                           "robpoisson", "negbin", "linear"),
                  "check_convergence" = c("logit", "log-binomial", "poisson",
                                          "robpoisson", "negbin"),
                  "identify_confounder" = c("logit", "log-binomial", "poisson",
                                            "robpoisson", "negbin", "linear"),
                  "select_models" = c("logit", "log-binomial", "poisson",
                                      "robpoisson", "negbin", "linear"),
                  stop("The function '", context, "' is not recognized.\n",
                       "Please use a valid function")
  )

  # Validate the chosen approach
  if (!approach %in% valid) {
    stop(approach, "  is not a valid approach for ", context, ".\n",
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

    # Drop NA values for checks
    x_non_na <- x[!is.na(x)]

    # Type checks
    is_categorical <- is.factor(x_non_na) || is.character(x_non_na)
    is_numeric <- is.numeric(x_non_na)

    if (is_categorical) {
      if (length(unique(x_non_na)) < 2) {
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

  # Combine all errors into one message
  error_msgs <- c()

  if (length(vars_all_na) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with all missing values: ",
             paste(vars_all_na, collapse = ", "),
             "Please fix them to proceed." ),
      call. = FALSE
    )
  }

  if (length(vars_invalid_cat) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with <2 levels: ",
             paste(vars_invalid_cat, collapse = ", "),
             "Please fix them to proceed."),
      call. = FALSE
    )
  }

  if (length(vars_invalid_num) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with no variation: ",
             paste(vars_invalid_num, collapse = ", "),
             "Please fix them to proceed."),
      call. = FALSE
    )
  }

  if (length(vars_invalid_type) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with unsupported types: ",
             paste(vars_invalid_type, collapse = ", "),
             "Please fix them to proceed."),
      call. = FALSE
    )
  }

  # If any errors exist, stop with full message
  if (length(error_msgs) > 0) {
    stop(paste(error_msgs, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}
# validate outcome against the approaches used
#' @keywords internal
#' @noRd
.validate_outcome_by_approach <- function(outcome, approach) {
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
  if (approach %in% c("logit", "log-binomial", "robpoisson") &&
      !is_binary(outcome)) {
    stop("This approach requires either a factor variable ",
         "or numeric variable coded as 0 and 1 (or 1 and 2).", call. = FALSE)
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

  # outcome variable validation
  .validate_outcome_by_approach(data[[outcome]], approach)

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

  .validate_outcome_by_approach(data[[outcome]], approach)
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
