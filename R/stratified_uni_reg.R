#' Stratified Univariate Regression (Odds, Risk, or Rate Ratios)
#'
#' Performs univariate regression for each exposure on a binary or count outcome,
#' stratified by a specified variable. One table column per stratum.
#'
#' @param data A data frame containing the variables.
#' @param outcome The name of the outcome variable (binary or count).
#' @param exposures A character vector of predictor variables.
#' @param stratifier The name of the variable to stratify results by.
#' @param approach Modeling approach to use. One of:
#'   `"logit"`, `"log-binomial"`, `"poisson"`, `"robpoisson"`,
#'   `"margstd_boot"`, `"margstd_delta"`, `"linear"`
#' @param summary Logical; if `TRUE`, prints model summaries for each stratum. Default is `FALSE`.
#'
#' @return A `gtsummary::tbl_merge` object with unadjusted estimates per stratum.
#' @export
stratified_uni_reg <- function(data, outcome, exposures, stratifier,
                               approach = "logit", summary = FALSE) {
  requireNamespace("gtsummary", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson",
                        "margstd_boot", "margstd_delta", "linear")

  if (!(approach %in% valid_approaches)) {
    stop("Invalid approach: ", approach,
         "\nValid options: ", paste(valid_approaches, collapse = ", "))
  }

  if (!stratifier %in% names(data)) stop("Stratifier not found in the dataset.")
  if (!outcome %in% names(data)) stop("Outcome variable not found in the dataset.")
  if (!all(exposures %in% names(data))) stop("One or more exposures not found in the dataset.")

  message("Running stratified univariate regression by: ", stratifier)

  data <- dplyr::filter(data, !is.na(.data[[stratifier]]))
  strata_levels <- unique(data[[stratifier]])

  tbl_list <- list()
  spanners <- character()

  for (lev in strata_levels) {
    message("Stratum: ", stratifier, " = ", lev)

    data_stratum <- dplyr::filter(data, .data[[stratifier]] == lev)

    result <- tryCatch({
      if ("summary" %in% names(formals(uni_reg))) {
        uni_reg(
          data = data_stratum,
          outcome = outcome,
          exposures = exposures,
          approach = approach,
          summary = summary
        )
      } else {
        uni_reg(
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
  return(merged_tbl)
}
