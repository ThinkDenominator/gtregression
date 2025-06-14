#' Stratified Multivariable Negative Binomial Regression
#'
#' Performs multivariable negative binomial regression of exposures on a count outcome,
#' stratified by a specified variable. NAs in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome The outcome variable (count).
#' @param exposures A character vector of exposure variables.
#' @param stratifier A categorical variable to stratify the data by.
#'
#' @return A `gtsummary::tbl_merge` object with attributes for models, summaries, and diagnostics.
#' @export
stratified_multi_nbin <- function(data, outcome, exposures, stratifier) {
  `%>%` <- magrittr::`%>%`

  # Input checks
  if (!stratifier %in% names(data)) stop("Stratifier not found in the dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in the dataset.")

  # Outcome validation
  is_count <- function(x) is.numeric(x) && all(!is.na(x)) && all(x >= 0 & x == floor(x))
  if (!is_count(data[[outcome]])) stop("Negative binomial regression requires a non-negative count outcome.")

  message("Running stratified multivariable negative binomial regression by: ", stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()
  model_list <- list()
  summary_list <- list()


  for (lev in strata_levels) {
    message("  > Stratum: ", stratifier, " = ", lev)
    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      multi_reg_nbin(
        data = data_stratum,
        outcome = outcome,
        exposures = exposures
      )
    }, error = function(e) {
      warning("Skipping stratum ", lev, ": ", e$message)
      NULL
    })

    if (!is.null(result)) {
      tbl_list[[length(tbl_list) + 1]] <- result$table
      model_list[[lev]] <- attr(result, "models")
      summary_list[[lev]] <- attr(result, "model_summaries")

      spanners <- c(spanners, paste0("**", stratifier, " = ", lev, "**"))
    }
  }

  # If all results are NULL (i.e., no valid models)
  if (length(tbl_list) == 0) stop("No valid models across strata.")

  # If only one stratum is valid and results in a single model, return directly
  if (length(tbl_list) == 1) {
    merged_tbl <- tbl_list[[1]]
    attr(merged_tbl, "models") <- model_list
    attr(merged_tbl, "model_summaries") <- summary_list
    attr(merged_tbl, "approach") <- "negbin"
    attr(merged_tbl, "source") <- "stratified_multi_nbin"
    class(merged_tbl) <- c("stratified_multi_nbin", class(merged_tbl))
    return(merged_tbl)
  }

  # Default case: multiple strata
  merged_tbl <- gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)

  attr(merged_tbl, "models") <- model_list
  attr(merged_tbl, "model_summaries") <- summary_list

  attr(merged_tbl, "approach") <- "negbin"
  attr(merged_tbl, "source") <- "stratified_multi_nbin"
  class(merged_tbl) <- c("stratified_multi_nbin", class(merged_tbl))

  return(merged_tbl)
}

#' @export
`$.stratified_multi_nbin` <- function(x, name) {
  if (name == "models") return(attr(x, "models"))
  if (name == "model_summaries") return(attr(x, "model_summaries"))

  if (name == "table") return(x)
  NextMethod("$")
}
