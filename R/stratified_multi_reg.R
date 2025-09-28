#' Stratified multivariable regression (wide, adjusted; no gtsummary)
#'
#' Fits one multivariable model per stratum and returns a unified wide table:
#' a single "Characteristic" column and, under bold spanners for each stratum,
#' two columns: "Adjusted <effect>" and "p-value".
#'
#' The footer shows two lines:
#'   1) Abbreviations (from `.abbrev_note()`),
#'   2) Per-stratum complete-case N used in the multivariable model.
#'
#' @param data data.frame
#' @param outcome character scalar; outcome column name
#' @param exposures character vector; predictors included in each model
#' @param stratifier character scalar; stratifying variable
#' @param approach "logit","log-binomial","poisson","linear","robpoisson","negbin"
#' @param format "gt" (default) or "flextable"
#' @param theme preset (e.g. "minimal","striped","clinical","shaded","jama")
#'   or primitives c("plain","zebra","lines","labels_bold","compact","header_shaded")
#'
#' @return A list of class \code{c("gtregression","stratified_multi_reg", ...)} with:
#' \describe{
#'   \item{table}{A \code{gt_tbl} (format="gt") or \code{flextable} (format="flextable").}
#'   \item{table_display}{Wide data.frame used to build the table.}
#'   \item{per_stratum}{Named list of per-stratum results (models/summaries/diagnostics).}
#'   \item{models, model_summaries, reg_check}{Named lists by stratum.}
#'   \item{by, levels, approach, format, source}{Metadata fields.}
#' }
#' @importFrom stats nobs
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
#' @export
stratified_multi_reg <- function(data, outcome, exposures, stratifier,
                                 approach = "logit",
                                 format   = c("gt","flextable"),
                                 theme    = c("minimal")) {
  format <- match.arg(format)
  theme  <- .resolve_theme(theme)

  .validate_multi_inputs(data, outcome, exposures, approach)
  if (!stratifier %in% names(data)) stop("Stratifier not found in dataset.", call. = FALSE)

  data <- data[!is.na(data[[stratifier]]), , drop = FALSE]
  levs <- .strata_levels(data, stratifier)

  message("Running stratified multivariable regression by: ", stratifier)

  fits <- list(); tds <- list(); models <- list(); sums <- list(); diags <- list()
  for (lv in levs) {
    message("  > Stratum: ", stratifier, " = ", lv)
    dlev <- data[data[[stratifier]] == lv, , drop = FALSE]
    fit <- tryCatch(
      .fit_multi_model(dlev, outcome, exposures, approach),
      error = function(e) { warning("Skipping stratum ", lv, ": ", e$message, call. = FALSE); NULL }
    )
    if (is.null(fit)) next
    td <- .tidy_multi(fit, exposures, approach)
    if (is.null(td) || !nrow(td)) { warning("Skipping stratum ", lv, ": no estimable coefficients.", call. = FALSE); next }

    key <- as.character(lv)
    fits[[key]]   <- fit
    tds[[key]]    <- td
    models[[key]] <- list(multivariable_model = fit)
    sums[[key]]   <- list(multivariable_model = summary(fit))
    diags[[key]]  <- list(multivariable_model = if (approach == "linear") {
      .reg_check_linear(fit, exposure = "multivariable_model")
    } else {
      "Regression diagnostics available only for 'linear' models."
    })
  }
  if (!length(tds)) stop("No valid models across strata.", call. = FALSE)

  built <- .strata_build_wide_multi(data, exposures, stratifier, tds)
  wide      <- built$wide
  spanners  <- built$spanners
  eff_lab   <- paste("Adjusted", .get_effect_label(approach))
  footnotes <- .footnotes_multi_strata(approach, fits, stratifier)

  tbl <- if (format == "gt") {
    .build_gt_strata_wide_multi(wide, spanners, eff_lab, theme, footnotes)
  } else {
    .build_flex_strata_wide_multi(wide, spanners, eff_lab, theme, footnotes)
  }

  fmt_class <- if (format == "gt") "gt_strata_multi" else "ft_strata_multi"
  out <- list(
    table           = tbl,
    table_display   = wide,
    per_stratum     = tds,
    models          = models,
    model_summaries = sums,
    reg_check       = diags,
    by              = stratifier,
    levels          = levs,
    approach        = approach,
    format          = format,
    source          = "stratified_multi_reg"
  )
  class(out) <- c("gtregression","stratified_multi_reg", fmt_class, class(out))
  out
}

