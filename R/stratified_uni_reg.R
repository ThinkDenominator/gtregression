' Stratified Univariate Regression (Odds, Risk, or Rate Ratios)
#'
#' Performs univariate regression for each exposure on a binary, count, or continuous outcome,
#' stratified by a specified variable. Produces a stacked `gtsummary` table with one column per stratum,
#' along with underlying models and diagnostics.
#'
#' @param data A data frame containing the variables.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param exposures A character vector specifying the predictor (exposure) variables.
#' @param stratifier A character string specifying the variable by which to stratify the data.
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (Odds Ratios), `"log-binomial"` (Risk Ratios),
#'   `"poisson"` (Incidence Rate Ratios), `"robpoisson"` (Robust RR), `"linear"` (Beta coefficients).
#'
#' @return An object of class `stratified_uni_reg`, which includes:
#' - `table`: A `gtsummary::tbl_stack` object with stratified results,
#' - `models`: A list of fitted models for each stratum,
#' - `model_summaries`: A tidy list of model summaries,
#' - `reg_check`: A tibble of regression diagnostics (when available).
#'
#' @section Accessors:
#' \describe{
#'   \item{\code{$table}}{Stacked stratified regression table.}
#'   \item{\code{$models}}{List of fitted model objects for each stratum.}
#'   \item{\code{$model_summaries}}{List of tidy model summaries.}
#'   \item{\code{$reg_check}}{Diagnostic check results (when applicable).}
#' }
#'
#' @seealso [multi_reg()], [plot_reg()], [identify_confounder()]
#'
#' @examples
#' if (requireNamespace("mlbench", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   data(PimaIndiansDiabetes2, package = "mlbench")
#'   pima <- dplyr::mutate(
#'     PimaIndiansDiabetes2,
#'     diabetes = ifelse(diabetes == "pos", 1, 0),
#'     glucose_cat = dplyr::case_when(
#'       glucose < 140 ~ "Normal",
#'       glucose >= 140 ~ "High"
#'     )
#'   )
#'   stratified_uni <- stratified_uni_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "mass"),
#'     stratifier = "glucose_cat",
#'     approach = "logit"
#'   )
#'   stratified_uni$table
#' }
#'
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom gtsummary tbl_stack
#' @export

stratified_uni_reg <- function(data, outcome, exposures, stratifier,
                               approach = "logit") {
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
  is_continuous <- function(x) is.numeric(x) && length(unique(x)) > 10

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
      tbls <- purrr::imap(attr(result, "models"), function(fit, var) {
        gtsummary::tbl_regression(fit,
                                  exponentiate = approach != "linear",
                                  conf.method = "wald",
                                  tidy_fun = broom::tidy) |>
          gtsummary::add_n(location = "label")
      })

      stacked <- gtsummary::tbl_stack(tbls)

      tbl_list[[length(tbl_list) + 1]] <- stacked
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
