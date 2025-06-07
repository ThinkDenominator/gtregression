#' Stratified Multivariable Negative Binomial Regression
#'
#' Performs multivariable negative binomial regression of exposures on a count outcome,
#' stratified by a specified variable. NAs in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome The outcome variable (count).
#' @param exposures A character vector of exposure variables.
#' @param stratifier A categorical variable to stratify the data by.
#' @param summary Logical; if `TRUE`, prints model summaries. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_merge` table with one column per stratum.
#' @export
stratified_multi_nbin <- function(data, outcome, exposures, stratifier, summary = FALSE) {
  pkgs <- c("dplyr", "gtsummary", "rlang")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
  }

  # Validate inputs
  if (!stratifier %in% names(data)) stop("Stratifier not found in the dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in the dataset.")
  # Outcome validation
  outcome_vec <- data[[outcome]]

  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  is_binary <- function(x) {
    is.atomic(x) && is.numeric(x) &&
      all(!is.na(x)) &&
      all(x %in% c(0, 1))
  }
  if (is_binary(outcome_vec)) stop("Negative binomial regression is not appropriate for binary outcomes.")
  if (!is_count(data[[outcome]])) stop("Outcome must be a non-negative count variable.")

  message("Running stratified multivariable negative binomial regression by: ", stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("**Stratum**: ", stratifier, " = ", lev)
    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      multi_reg_nbin(
        data = data_stratum,
        outcome = outcome,
        exposures = exposures,
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
  attr(tbl_list, "approach") <- approach
  attr(tbl_list, "source") <- "stratified_multi_nbin"

  gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)
}
