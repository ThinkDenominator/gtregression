#' Stratified Univariate Regression (Odds, Risk, or Rate Ratios)
#'
#' Performs univariate regression for each exposure on a binary, count, or continuous outcome,
#' stratified by a specified variable. Produces one column per stratum.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable.
#' @param exposures A character vector of predictor variables.
#' @param stratifier The name of the variable to stratify results by.
#' @param approach Modeling approach to use. One of:
#'   `"logit"`, `"log-binomial"`, `"poisson"`, `"robpoisson"`, `"linear"`
#'
#' @return A `stratified_uni_reg` object with components:
#'   `table`, `models`, `model_summaries`, `reg_check`
#' @export
stratified_uni_reg <- function(data, outcome, exposures, stratifier,
                               approach = "logit") {
  `%>%` <- magrittr::`%>%`

  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "linear")
  if (!approach %in% valid_approaches) {
    stop("Invalid approach: ", approach,
         "\nValid options: ", paste(valid_approaches, collapse = ", "))
  }

  if (!stratifier %in% names(data)) stop("Stratifier not found in dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in dataset.")

  outcome_vec <- data[[outcome]]
  is_binary <- function(x) is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)) ||
    (is.factor(x) && length(levels(x)) == 2)
  is_count <- function(x) is.numeric(x) && all(x >= 0 & floor(x) == x, na.rm = TRUE)
  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("Binary outcome required for approach: ", approach)
  }
  if (approach == "poisson" && !is_count(outcome_vec)) {
    stop("Count outcome required for Poisson regression.")
  }
  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Continuous numeric outcome required for linear regression.")
  }

  message("Running stratified univariate regression by: ", stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()
  models_list <- list()
  summaries_list <- list()
  diagnostics_list <- list()

  for (lev in strata_levels) {
    message("  > Stratum: ", stratifier, " = ", lev)
    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      uni <- uni_reg(
        data = data_stratum,
        outcome = outcome,
        exposures = exposures,
        approach = approach
      )
    }, error = function(e) {
      warning("Skipping stratum ", lev, ": ", e$message)
      NULL
    })

    if (!is.null(result)) {
      tbl_list[[length(tbl_list) + 1]] <- result$table
      models_list[[lev]] <- attr(result, "models")
      summaries_list[[lev]] <- attr(result, "model_summaries")
      diagnostics_list[[lev]] <- attr(result, "reg_check")
      spanners <- c(spanners, paste0("**", stratifier, " = ", lev, "**"))
    }
  }

  if (length(tbl_list) == 0) stop("No valid models across strata.")

  merged_tbl <- gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)
  attr(merged_tbl, "models") <- models_list
  attr(merged_tbl, "model_summaries") <- summaries_list
  attr(merged_tbl, "reg_check") <- diagnostics_list
  attr(merged_tbl, "approach") <- approach
  attr(merged_tbl, "source") <- "stratified_uni_reg"
  class(merged_tbl) <- c("stratified_uni_reg", class(merged_tbl))

  return(merged_tbl)
}

#' @export
`$.stratified_uni_reg` <- function(x, name) {
  if (name == "table") return(x)
  if (name == "models") return(attr(x, "models"))
  if (name == "model_summaries") return(attr(x, "model_summaries"))
  if (name == "reg_check") return(attr(x, "reg_check"))
  NextMethod("$")
}
