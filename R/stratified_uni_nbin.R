#' Stratified Univariate Negative Binomial Regression
#'
#' Performs univariate negative binomial regression of exposures on outcome,
#' stratified by a specified variable. NAs in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A character vector of predictor variables.
#' @param stratifier A categorical variable to stratify the data by.
#' @param summary Logical; if `TRUE`, prints model summaries. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_merge` table with one column per stratum.
#' @export
stratified_uni_nbin <- function(data, outcome, exposures, stratifier, summary = FALSE) {
  # Load magrittr for CRAN-safe pipe
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("The 'magrittr' package is required for piping (%>%). Please install it.")
  }

  `%>%` <- magrittr::`%>%`

  # Check inputs
  if (!stratifier %in% names(data)) stop("Stratifier not found in the dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in the dataset.")

  # Count outcome validation
  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  if (!is_count(data[[outcome]])) {
    stop("Negative binomial regression requires a count outcome (non-negative integers).")
  }

  message("Running stratified negative binomial regression by: ", stratifier)

  # Exclude NA in stratifier
  data <- data %>% dplyr::filter(!is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("Stratum: ", stratifier, " = ", lev)

    data_stratum <- data %>% dplyr::filter(.data[[stratifier]] == lev)

    result <- tryCatch({
      uni_reg_nbin(
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

  merged_tbl <- gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)
  print(merged_tbl)
  return(merged_tbl)
}
