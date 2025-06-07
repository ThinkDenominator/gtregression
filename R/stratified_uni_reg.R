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
#' @param summary Logical; if `TRUE`, prints model summaries for each stratum. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_merge` object with unadjusted estimates per stratum.
#' @export
stratified_uni_reg <- function(data, outcome, exposures, stratifier,
                               approach = "logit", summary = FALSE) {
  `%>%` <- magrittr::`%>%`

  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "linear")
  if (!approach %in% valid_approaches) {
    stop("Invalid approach: ", approach,
         "\nValid options: ", paste(valid_approaches, collapse = ", "))
  }

  if (!stratifier %in% names(data)) stop("Stratifier not found in the dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in the dataset.")

  # outcome validation

  outcome_vec <- data[[outcome]]

  is_binary <- function(x) is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1), na.rm = TRUE)) ||
    (is.factor(x) && length(levels(x)) == 2)

  is_count <- function(x) is.numeric(x) && all(x >= 0 & floor(x) == x, na.rm = TRUE)

  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10 && !is_count(x)

  if (approach %in% c("logit", "log-binomial", "robpoisson") && !is_binary(outcome_vec)) {
    stop("Binary outcome required for the selected approach: ", approach)
  }

  if (approach == "poisson") {
    if (is_binary(outcome_vec)) stop("Poisson regression is not appropriate for binary outcomes.")
    if (!is_count(outcome_vec)) stop("Poisson requires a count outcome.")
  }

  if (approach == "linear" && !is_continuous(outcome_vec)) {
    stop("Continuous numeric outcome required for linear regression.")
  }

  message("Running stratified univariate regression by: ", stratifier)

  # Drop missing strata
  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("  > Stratum: ", stratifier, " = ", lev)
    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      uni_reg(
        data = data_stratum,
        outcome = outcome,
        exposures = exposures,
        approach = approach,
        summary = summary
      )
    }, error = function(e) {
      warning("Skipping stratum ", lev, ": ", e$message)
      NULL
    })

    if (!is.null(result)) {
      tbl_list[[length(tbl_list) + 1]] <- result
      spanners <- c(spanners, paste0("**", stratifier, " = ", lev, "**"))
    }
  }

  if (length(tbl_list) == 0) {
    warning("No valid models across strata.")
    return(NULL)
  }

  merged_tbl <- gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)
  attr(merged_tbl, "approach") <- approach
  attr(merged_tbl, "source") <- "stratified_uni_reg"
  return(merged_tbl)
}
