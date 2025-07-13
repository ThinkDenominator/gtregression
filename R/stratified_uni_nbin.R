#' Stratified Univariate Negative Binomial Regression
#'
#' Performs univariate negative binomial regression
#' stratified by a specified variable.
#' Missing values in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome A character string specifying the count outcome variable.
#' @param exposures A character vector specifying the exposure variables.
#' @param stratifier A character string specifying the stratifying variable.
#'
#' @return An object of class `stratified_uni_reg_nbin` with the following:
#' \describe{
#'   \item{\code{$table}}{A `gtsummary::tbl_merge` tables by stratum.}
#'   \item{\code{$models}}{A named list of fitted negbin models per stratum.}
#'   \item{\code{$model_summaries}}{A list of tidy summaries for each model.}
#' }
#'
#' @section Accessors:
#' Use `object$table`, `object$models`, or
#' `object$model_summaries` to extract components.
#'
#' @seealso [uni_reg_nbin()], [stratified_multi_reg()], [check_dispersion()]
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   data(Pima.tr, package = "MASS")
#'   Pima.tr$strata <- ifelse(Pima.tr$bmi > 30, "High", "Normal")
#'   strat_uni_nb <- stratified_uni_nbin(
#'     data = Pima.tr,
#'     outcome = "glu",
#'     exposures = c("age", "ped"),
#'     stratifier = "strata"
#'   )
#'   strat_uni_nb$table
#' }
#' @importFrom MASS glm.nb
#' @importFrom gtsummary tbl_merge
#' @importFrom broom tidy
#' @export

stratified_uni_nbin <- function(data, outcome, exposures, stratifier) {
  if (!stratifier %in% names(data))
    stop("Stratifier not found in the dataset.")

  # presence of outcome
  if (!outcome %in% names(data))
    stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data)))
    stop("One or more exposures not found in the dataset.")

  # Check for count outcome
  # check for Non-negative integers
  is_count <- function(x) {
    is.numeric(x) &&
      all(x >= 0 & x == floor(x), na.rm = TRUE) &&
      length(unique(x[!is.na(x)])) >= 1
  }
  if (!is_count(data[[outcome]]))
    stop("Negative binomial regression requires a non-negative count outcome.")

  message("Running stratified univariate negative binomial regression by: ",
          stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  results <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("  > Stratum: ", stratifier, " = ", lev)

    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch(
      {
        uni_reg_nbin(
          data = data_stratum,
          outcome = outcome,
          exposures = exposures
        )
      },
      error = function(e) {
        warning("Skipping stratum ", lev, ": ", e$message)
        NULL
      }
    )

    if (!is.null(result)) {
      results[[lev]] <- result
      spanners <- c(spanners, paste0("**", stratifier, " = ", lev, "**"))
    }
  }

  if (length(results) == 0) {
    return(NULL)
  }


  # Extract components

  model_list <- lapply(results, function(x) attr(x, "models"))
  summary_list <- lapply(results, function(x) attr(x, "model_summaries"))


  merged_tbl <- gtsummary::tbl_merge(results, tab_spanner = spanners)

  result <- merged_tbl

  # add meta data
  attr(result, "models") <- model_list
  attr(result, "model_summaries") <- summary_list
  attr(result, "approach") <- "negbin"
  attr(result, "source") <- "stratified_uni_nbin"
  class(result) <- c("stratified_uni_nbin", class(result))

  return(result)
}
#' @export
`$.stratified_uni_nbin` <- function(x, name) {
  if (name == "models") {
    return(attr(x, "models"))
  }
  if (name == "model_summaries") {
    return(attr(x, "model_summaries"))
  }

  if (name == "table") {
    return(x)
  }
  NextMethod("$")
}
