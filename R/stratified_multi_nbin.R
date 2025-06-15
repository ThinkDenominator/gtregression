#' Stratified Multivariable Negative Binomial Regression
#'
#' Performs multivariable negative binomial regression with multiple exposures on a count outcome,
#' stratified by a specified variable. Observations with missing values in the stratifier are excluded.
#'
#' @param data A data frame containing the variables.
#' @param outcome A character string specifying the name of the count outcome variable.
#' @param exposures A character vector of predictor (exposure) variables.
#' @param stratifier A character string specifying the variable used for stratification (must be categorical).
#'
#' @return An object of class `stratified_multi_reg_nbin` with the following components:
#' \describe{
#'   \item{\code{$table}}{A `gtsummary::tbl_merge` object combining stratified model tables.}
#'   \item{\code{$models}}{A named list of `glm.nb` model objects per stratum.}
#'   \item{\code{$model_summaries}}{A list of tidy model summaries using `broom::tidy()`.}
#'   \item{\code{$reg_check}}{Optional regression diagnostics per stratum (e.g., overdispersion).}
#' }
#'
#' @section Accessors:
#' Use `object$table`, `object$models`, `object$model_summaries`, or `object$reg_check` to extract components.
#'
#' @seealso [multi_reg_nbin()], [stratified_uni_reg_nbin()], [check_dispersion()]
#'
#' @examples
#' set.seed(123)
#' df <- dplyr::tibble(
#'   outcome = rnbinom(200, mu = 2, size = 1),
#'   exposure1 = sample(c("Low", "High"), 200, replace = TRUE),
#'   exposure2 = sample(c("A", "B"), 200, replace = TRUE),
#'   group = sample(c("M", "F"), 200, replace = TRUE)
#' )
#' strat_mod <- stratified_multi_nbin(
#'   data = df,
#'   outcome = "outcome",
#'   exposures = c("exposure1", "exposure2"),
#'   stratifier = "group"
#' )
#' strat_mod$table
#'
#' @importFrom MASS glm.nb
#' @importFrom gtsummary tbl_merge tbl_regression
#' @importFrom broom tidy
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
