#' Stratified Univariate Negative Binomial Regression
#'
#' Performs univariate negative binomial regression of exposures on outcome,
#' stratified by a specified variable. NAs in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the count outcome variable.
#' @param exposures A character vector of predictor variables.
#' @param stratifier A categorical variable to stratify the data by.
#'
#' @return A `gtsummary::tbl_merge` object with components: `$table`, `$models`, `$model_summaries`,
#' @export
stratified_uni_nbin <- function(data, outcome, exposures, stratifier) {
  `%>%` <- magrittr::`%>%`

  if (!stratifier %in% names(data)) stop("Stratifier not found in the dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in the dataset.")

  is_count <- function(x) is.numeric(x) && all(!is.na(x)) && all(x >= 0 & x == floor(x))
  if (!is_count(data[[outcome]])) stop("Negative binomial regression requires a non-negative count outcome.")

  message("Running stratified univariate negative binomial regression by: ", stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  results <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("  > Stratum: ", stratifier, " = ", lev)

    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      uni_reg_nbin(
        data = data_stratum,
        outcome = outcome,
        exposures = exposures
      )
    }, error = function(e) {
      warning("Skipping stratum ", lev, ": ", e$message)
      NULL
    })

    if (!is.null(result)) {
      results[[lev]] <- result
      spanners <- c(spanners, paste0("**", stratifier, " = ", lev, "**"))
    }
  }

  if (length(results) == 0) return(NULL)


  # Extract components
  tbl_list <- lapply(results, function(x) x$table)
  model_list <- lapply(results, function(x) attr(x, "models"))
  summary_list <- lapply(results, function(x) attr(x, "model_summaries"))


  merged_tbl <- gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)

  result <- merged_tbl
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- summary_list

  attr(result, "approach") <- "negbin"
  attr(result, "source") <- "stratified_uni_nbin"
  class(result) <- c("stratified_uni_nbin", class(result))

  return(result)

}
#' @export
`$.stratified_uni_nbin` <- function(x, name) {
  if (name == "models") return(attr(x, "models"))
  if (name == "model_summaries") return(attr(x, "model_summaries"))

  if (name == "table") return(x)
  NextMethod("$")
}

