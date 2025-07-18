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
#' @param name Optional string for the exposure name
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
             "Please fix them to proceed." )
    )
  }

  if (length(vars_invalid_cat) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with <2 levels: ",
             paste(vars_invalid_cat, collapse = ", "),
             "Please fix them to proceed.")
    )
  }

  if (length(vars_invalid_num) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with no variation: ",
             paste(vars_invalid_num, collapse = ", "),
             "Please fix them to proceed.")
    )
  }

  if (length(vars_invalid_type) > 0) {
    error_msgs <- c(
      error_msgs,
      paste0("Exposure with unsupported types: ",
             paste(vars_invalid_type, collapse = ", "),
             "Please fix them to proceed.")
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
    stop("All values in the outcome variable are missing.")
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
         "or numeric variable coded as 0 and 1 (or 1 and 2).")
  }
  if (approach == "poisson" && !is_count(outcome)) {
    stop("Poisson regression requires a count outcome.")
  }
  if (approach == "negbin" && !is_count(outcome)) {
    stop("Negative binomial requires a count outcome.")
  }
  if (approach == "linear" && !is_continuous(outcome)) {
    stop("Linear regression requires a continuous outcome.")
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
    stop("Outcome variable not found in the dataset.")

  # check variable presence >1
  if (!all(exposures %in% names(data)))
    stop("One or more exposure variables were not found in the dataset.")

  # outcome variable validation
  .validate_outcome_by_approach(data[[outcome]], approach)

  # Validate exposures
  .validate_exposures(data, exposures)
}

# Validate Multivariate inputs for multi_reg
#' @keywords internal
#' @noRd
.validate_multi_inputs <- function(data,
                                   outcome,
                                   exposures,
                                   approach) {
  # validate approach in the function
  .validate_approach(approach, context = "multi_reg")

  # check variable presence
  if (!outcome %in% names(data))
    stop("Outcome variable not found in the dataset.")

  # check variable presence >1
  if (!all(exposures %in% names(data)))
    stop("One or more exposure variables were not found in the dataset.")

  # outcome variable validation
  .validate_outcome_by_approach(data[[outcome]], approach)

  # Validate exposures
  .validate_exposures(data, exposures)

  # clean the data for complete case analysis
  # select exposures and outcomes only and drop NAs in the selected cols
  data_clean <- stats::na.omit(data[, c(outcome, exposures),
                                          drop = FALSE])
  # Throw error for null data return
  if (nrow(data_clean) == 0)
    stop("No complete cases available for analysis.")

  # validate that each exposure has at least 2 unique values
  insufficient_vars <- exposures[vapply(data_clean[exposures],
                                        function(x)
                                          length(unique(na.omit(x))) < 2,
                                        logical(1))]
  if (length(insufficient_vars) > 0) {
    stop("Exposure(s) has less than 2 unique values: ",
         paste(insufficient_vars, collapse = ", "))
  }

  return(data_clean)
}
