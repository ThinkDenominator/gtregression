# validate approach

#' Validate regression approach by context
#'
#' Ensures that only valid modeling approaches are used in each function context
#'
#' @param approach A character string specifying the regression method.
#' @param context The name of the function using the approach (e.g., "uni_reg").
#'
#' @return Stops with an informative error if the approach is not allowed.
#' @keywords internal
.validate_approach <- function(approach, context = NULL) {
  # Define valid approaches per context
  valid <- switch(context,
                  "uni_reg" = c("logit", "log-binomial", "poisson",
                                "robpoisson", "linear"),
                  "uni_reg_nbin" = c("negbin"),
                  "multi_reg" = c("logit", "log-binomial", "poisson",
                                  "robpoisson", "linear"),
                  "multi_reg_nbin" = c("negbin"),
                  "interaction_models" = c("logit", "log-binomial", "poisson",
                                           "robpoisson", "negbin", "linear"),
                  "check_convergence" = c("logit", "log-binomial", "poisson",
                                          "robpoisson", "negbin"),
                  "identify_confounder" = c("logit", "log-binomial", "poisson",
                                            "robpoisson", "negbin", "linear"),
                  "select_models" = c("logit", "log-binomial", "poisson",
                                      "robpoisson", "negbin", "linear"),
                  stop("Unknown context '",
                       context, "' passed to .validate_approach().")
  )

  # Validate the chosen approach
  if (!approach %in% valid) {
    stop("Invalid approach '", approach, "' for context '", context, "'.\n",
         "Valid options: ", paste(valid, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}


.validate_outcome_by_approach <- function(outcome_vec, approach) {
  is_binary <- function(x) {
    (is.factor(x) && length(levels(x)) == 2) ||
      (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE))
  }

  is_count <- function(x) {
    is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE) &&
      length(unique(x[!is.na(x)])) > 2
  }

  is_continuous <- function(x) {
    is.numeric(x) && length(unique(x)) > 10
  }

  if (approach %in% c("logit", "log-binomial", "robpoisson") &&
      !is_binary(outcome_vec)) {
    stop("This approach requires a binary outcome.")
  }
  if (approach == "poisson" && !is_count(outcome_vec)) {
    stop("Count outcome required for Poisson regression.")
  }
  if (approach == "negbin" && !is_count(outcome_vec)) {
    stop("Negative binomial requires a count outcome.")
  }
  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Linear regression requires a continuous outcome.")
  }
}
# validate inputs
.validate_uni_inputs <- function(data, outcome, exposures, approach) {
  # validate approach in the func
  .validate_approach(approach, context = "uni_reg")

  # variable existence
  if (!outcome %in% names(data))
    stop("Outcome variable not found in dataset.")

  if (!all(exposures %in% names(data)))
    stop("One or more exposure variables not found in dataset.")

  # outcome validation
  .validate_outcome_by_approach(data[[outcome]], approach)
}

# Multivariate models

.validate_multi_inputs <- function(data, outcome, exposures, approach) {
  .validate_approach(approach, context = "multi_reg")

  if (!outcome %in% names(data))
    stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data)))
    stop("One or more exposure variables not found in the dataset.")

  .validate_outcome_by_approach(data[[outcome]], approach)

  data_clean <- data[!is.na(data[[outcome]]), , drop = FALSE]
  data_clean <- stats::na.omit(data_clean[, c(outcome, exposures),
                                          drop = FALSE])
  if (nrow(data_clean) == 0)
    stop("No complete cases remaining after removing missing values.")
  return(data_clean)
}

# For Negative binomial
.validate_nb_inputs <- function(data, outcome, exposures) {
  if (!outcome %in% names(data))
    stop("Outcome variable not found in the dataset.")

  if (!all(exposures %in% names(data)))
    stop("One or more exposure variables not found in the dataset.")

  .validate_outcome_by_approach(data[[outcome]], "negbin")
}
# Multivariate Negative binomial
.validate_nb_multi_inputs <- function(data, outcome, exposures) {
  if (!outcome %in% names(data))
    stop("Outcome variable not found in dataset.")
  if (!all(exposures %in% names(data)))
    stop("One or more exposures not found in dataset.")

  .validate_outcome_by_approach(data[[outcome]], "negbin")

  data_clean <- data[!is.na(data[[outcome]]), , drop = FALSE]
  data_clean <- stats::na.omit(data_clean[, c(outcome, exposures),
                                          drop = FALSE])

  if (nrow(data_clean) == 0)
    stop("No valid observations after removing missing values.")

  insufficient_vars <- exposures[vapply(data_clean[exposures],
                                        function(x) length(unique(x)) < 2)]
  if (length(insufficient_vars) > 0) {
    stop("Exposure(s) with insufficient variation: ",
         paste(insufficient_vars, collapse = ", "))
  }

  return(data_clean)
}
