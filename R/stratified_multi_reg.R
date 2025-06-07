#' Stratified Multivariable Regression (Adjusted OR, RR, IRR, or Beta)
#'
#' Performs multivariable regression of exposures on outcome,
#' stratified by a specified variable. NAs in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome The outcome variable (binary, count, or continuous).
#' @param exposures A character vector of exposure variables.
#' @param stratifier A categorical variable to stratify the data by.
#' @param approach The modeling approach ("logit", "robpoisson", "poisson", "log-binomial", etc.).
#' @param summary Logical; if `TRUE`, prints model summaries. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_merge` object with one column per stratum.
#' @export
stratified_multi_reg <- function(data, outcome, exposures, stratifier,
                                 approach = "logit", summary = FALSE) {


  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson",
                         "linear")

  if (!(approach %in% valid_approaches)) {
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

  message("Running stratified multivariable regression by: ", stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("**Stratum**: ", stratifier, " = ", lev)

    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      if ("summary" %in% names(formals(multi_reg))) {
        multi_reg(
          data = data_stratum,
          outcome = outcome,
          exposures = exposures,
          approach = approach,
          summary = summary
        )
      } else {
        multi_reg(
          data = data_stratum,
          outcome = outcome,
          exposures = exposures,
          approach = approach
        )
      }
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
  attr(merged_tbl, "source") <- "stratified_multi_reg"
  print(merged_tbl)
  return(merged_tbl)
}
