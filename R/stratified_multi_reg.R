#' Stratified Multivariable Regression (Adjusted OR, RR, IRR, or Beta)
#'
#' Performs multivariable regression with multiple exposures on
#' a binary, count, or continuous outcome,
#' stratified by a specified variable.
#' NA values in the stratifier are excluded from analysis.
#'
#' @param data A data frame containing the variables.
#' @param outcome name of the outcome variable.
#' @param exposures vector specifying the predictor (exposure) variables.
#' @param stratifier A character string specifying the stratifying variable.
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (Adjusted Odds Ratios), `"log-binomial"` (Adjusted Risk Ratios),
#'   `"poisson"` (Adjusted IRRs), `"robpoisson"` (Adjusted RRs),
#'   or `"linear"` (Beta coefficients), `"negbin"` (Adjusted IRRs).
#'
#' @return An object of class `stratified_multi_reg`, which includes:
#' - `table`: A `gtsummary::tbl_stack` object of regression tables by stratum,
#' - `models`: A named list of model objects for each stratum,
#' - `model_summaries`: A list of tidy model summaries,
#' - `reg_check`: Diagnostics results (if available for the model type).
#'
#' @section Accessors:
#' \describe{
#'   \item{\code{$table}}{Stacked table of stratified regression outputs.}
#'   \item{\code{$models}}{Named list of fitted models per stratum.}
#'   \item{\code{$model_summaries}}{Tidy summaries for each model.}
#'   \item{\code{$reg_check}}{Regression diagnostic checks (when applicable).}
#' }
#' @seealso [multi_reg()], [stratified_uni_reg()], [plot_reg()]
#' @examples
#' if (requireNamespace("mlbench", quietly = TRUE) &&
#'   requireNamespace("dplyr", quietly = TRUE)) {
#'   data(PimaIndiansDiabetes2, package = "mlbench")
#'   pima <- dplyr::mutate(
#'     PimaIndiansDiabetes2,
#'     diabetes = ifelse(diabetes == "pos", 1, 0),
#'     glucose_cat = dplyr::case_when(
#'       glucose < 140 ~ "Normal",
#'       glucose >= 140 ~ "High"
#'     )
#'   )
#'   stratified_multi <- stratified_multi_reg(
#'     data = pima,
#'     outcome = "diabetes",
#'     exposures = c("age", "mass"),
#'     stratifier = "glucose_cat",
#'     approach = "logit"
#'   )
#'   stratified_multi$table
#' }
#'
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom gtsummary tbl_stack
#' @export
stratified_multi_reg <- function(data, outcome, exposures, stratifier,
                                 approach = "logit") {

  # checks through internal helpers
  .validate_multi_inputs(data, outcome, exposures, approach)
  # check stratifier presence
  if (!stratifier %in% names(data)) stop("Stratifier not found in dataset.")

  # Inform user as it takes more time to complete
  message("Running stratified multivariable regression by: ", stratifier)

  # Remove observations with missing values in the stratifier
  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  # Initialise result containers
  tbl_list <- list()
  spanners <- character()
  models_list <- list()
  summaries_list <- list()
  diagnostics_list <- list()

  # loop through each stratum
  for (lev in strata_levels) {
    message("  > Stratum: ", stratifier, " = ", lev)

    # Subset data for the current stratum
    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    # multi model fit
    result <- tryCatch(
      {
        fit <- multi_reg(
          data = data_stratum,
          outcome = outcome,
          exposures = exposures,
          approach = approach
        )
      },
      error = function(e) {
        warning("Skipping stratum ", lev, ": ", e$message)
        NULL
      }
    )

    # If model was successfully fit, extract components
    if (!is.null(result)) {
      tbl_list[[length(tbl_list) + 1]] <- result$table
      models_list[[lev]] <- attr(result, "models")
      summaries_list[[lev]] <- attr(result, "model_summaries")
      diagnostics_list[[lev]] <- attr(result, "reg_check")
      spanners <- c(spanners, paste0("**", stratifier, " = ", lev, "**"))
    }
  }
  # Stop if no valid models were fit
  if (length(tbl_list) == 0) stop("No valid models across strata.")

  # Remove existing source notes otherwise cant edit them
  tbl_list <- lapply(tbl_list, function(tbl) gtsummary::remove_source_note(tbl))

  # Merge the cleaned tables with header spanners
  merged_tbl <- gtsummary::tbl_merge(tbl_list, tab_spanner = spanners)

  # Extract and clean N values from N_obs_* columns to add a footnote
  n_obs_cols <- grep("^N_obs_", names(merged_tbl$table_body), value = TRUE)

  n_values <- vapply(n_obs_cols, function(col) {
    n <- unique(na.omit(merged_tbl$table_body[[col]]))
    as.character(round(n))
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  #
  stratum_labels <- strata_levels

  # Create per-stratum footnote text
  final_note <- paste0(
    stratifier, " = ", stratum_labels, ": N = ", n_values,
    " complete cases included per stratum"
  )

  final_note <- paste(final_note, collapse = "<br>")

  # Add the note to the table
  merged_tbl <- gtsummary::modify_source_note(merged_tbl, final_note)

  # Add metadata as attributes
  attr(merged_tbl, "models") <- models_list
  attr(merged_tbl, "model_summaries") <- summaries_list
  attr(merged_tbl, "reg_check") <- diagnostics_list
  attr(merged_tbl, "approach") <- approach
  attr(merged_tbl, "source") <- "stratified_multi_reg"
  # Assign class for S3 accessors
  class(merged_tbl) <- c("stratified_multi_reg", class(merged_tbl))

  return(merged_tbl)
}

#' @export
`$.stratified_multi_reg` <- function(x, name) {
  if (name == "table") {
    return(x)
  }
  if (name == "models") {
    return(attr(x, "models"))
  }
  if (name == "model_summaries") {
    return(attr(x, "model_summaries"))
  }
  if (name == "reg_check") {
    return(attr(x, "reg_check"))
  }
  NextMethod("$")
}
