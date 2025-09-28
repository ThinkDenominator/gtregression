" Stratified Univariate Regression (Odds, Risk, or Rate Ratios)
#"
#' Performs univariate regression for each exposure on a
#' binary, count, or continuous outcome,
#' stratified by a specified variable. Produces a stacked `gtsummary`
#' table with one column per stratum,
#' along with underlying models and diagnostics.
#'
#' @param data A data frame containing the variables.
#' @param outcome name of the outcome variable.
#' @param exposures A vector specifying the predictor (exposure) variables.
#' @param stratifier A character string specifying the stratifier
#' @param approach Modeling approach to use. One of:
#'   `"logit"` (Odds Ratios), `"log-binomial"` (Risk Ratios),
#'   `"poisson"` (Incidence Rate Ratios), `"robpoisson"` (Robust RR),
#'    `"linear"` (Beta coefficients), `"negbin"` (Incidence Rate Ratios),.
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
                               approach = "logit",
                               format   = c("gt","flextable"),
                               theme    = c("minimal")) {
  format <- match.arg(format)
  theme  <- .resolve_theme(theme)

  .validate_uni_inputs(data, outcome, exposures, approach)
  if (!stratifier %in% names(data)) stop("Stratifier not found in dataset.", call. = FALSE)

  data <- data[!is.na(data[[stratifier]]), , drop = FALSE]
  levs <- .strata_levels(data, stratifier)

  message("Running stratified univariate regression by: ", stratifier)

  per_stratum <- list()
  for (lv in levs) {
    message("  > Stratum: ", stratifier, " = ", lv)
    dlev <- data[data[[stratifier]] == lv, , drop = FALSE]
    res  <- tryCatch(
      uni_reg(dlev, outcome, exposures, approach = approach, format = format, theme = theme),
      error = function(e) { warning("Skipping stratum ", lv, ": ", e$message, call. = FALSE); NULL }
    )
    if (!is.null(res)) per_stratum[[as.character(lv)]] <- res
  }
  if (!length(per_stratum)) stop("No valid models across strata.", call. = FALSE)

  built <- .strata_build_wide_uni(data, outcome, exposures, stratifier, per_stratum)
  wide      <- built$wide
  spanners  <- built$spanners
  footnotes <- .footnotes_uni_strata(approach)
  eff_lab   <- .get_effect_label(approach)

  tbl <- if (format == "gt") {
    .build_gt_strata_wide_uni(wide, spanners, eff_lab, theme, footnotes)
  } else {
    .build_flex_strata_wide_uni(wide, spanners, eff_lab, theme, footnotes)
  }

  # collect metadata for users
  models_list <- lapply(per_stratum, `[[`, "models")
  summaries   <- lapply(per_stratum, `[[`, "model_summaries")
  diags       <- lapply(per_stratum, `[[`, "reg_check")

  fmt_class <- if (format == "gt") "gt_strata_uni" else "ft_strata_uni"
  out <- list(
    table         = tbl,
    table_display = wide,
    per_stratum   = per_stratum,
    models        = models_list,
    model_summaries = summaries,
    reg_check     = diags,
    by            = stratifier,
    levels        = levs,
    approach      = approach,
    format        = format,
    source        = "stratified_uni_reg"
  )
  class(out) <- c("gtregression","stratified_uni_reg", fmt_class, class(out))
  out
}


