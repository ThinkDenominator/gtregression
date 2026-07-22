#' Stratified multivariable regression
#'
#' Fits multivariable regression models within each stratum and returns a
#' unified wide table with one "Characteristic" column and, under bold
#' spanners for each stratum, two columns: "Adjusted <effect>" and "p-value".
#'
#' If \code{adjust_for = NULL}, all \code{exposures} are included in one
#' multivariable model within each stratum. If \code{adjust_for} is supplied,
#' one adjusted model is fitted per exposure within each stratum.
#'
#' @param data A data frame containing the variables.
#' @param outcome Character scalar; name of the outcome variable.
#' @param exposures Character vector of exposure variables to report.
#' @param stratifier Character scalar; name of the stratifying variable.
#' @param adjust_for Optional character vector of adjustment variables.
#'   This argument works the same way as in \code{multi_reg()}.
#' @param interaction Optional character scalar specifying one interaction term
#'   using standard formula syntax, e.g. \code{"bmi*sex"}
#' @param approach One of \code{"logit"}, \code{"logbinomial"}, \code{"poisson"},
#'   \code{"linear"}, \code{"robpoisson"}, or \code{"negbin"}
#' @param format One of \code{"gt"} (default) or \code{"flextable"}.
#' @param theme Preset name (e.g. \code{"minimal"}, \code{"striped"}, \code{"clinical"},
#'   \code{"shaded"}, \code{"jama"}) or primitives
#'   \code{c("plain","zebra","lines","labels_bold","compact","header_shaded")}
#'
#' @return A list of class \code{c("gtregression","stratified_multi_reg", ...)} with:
#' \describe{
#'   \item{\code{table}}{A \code{gt_tbl} (format = \code{"gt"}) or
#'   \code{flextable} (format = \code{"flextable"}).}
#'   \item{\code{table_display}}{Wide data frame used to build the table.}
#'   \item{\code{per_stratum}}{Named list of per-stratum regression results.}
#'   \item{\code{models}}{Named list of fitted models by stratum.}
#'   \item{\code{model_summaries}}{Named list of model summaries by stratum.}
#'   \item{\code{reg_check}}{Named list of diagnostics by stratum.}
#'   \item{\code{by}, \code{levels}, \code{approach}, \code{format}, \code{source}}{Metadata fields.}
#' }
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   transform(
#'     race = factor(race, levels = c(1, 2, 3),
#'                   labels = c("White", "Black", "Other")),
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
#'     ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1),
#'                  labels = c("Normal BW", "Low BW"))
#'   )
#'
#' stratified_multi <- stratified_multi_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "lwt", "smoke", "ht"),
#'   stratifier = "race",
#'   approach = logit
#' )
#'
#' stratified_adjusted <- stratified_multi_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("smoke", "ht", "ui"),
#'   stratifier = "race",
#'   adjust_for = c("age", "lwt"),
#'   approach = logit
#' )
#' @importFrom stats nobs
#' @export
stratified_multi_reg <- function(data,
                                 outcome,
                                 exposures,
                                 stratifier,
                                 adjust_for = NULL,
                                 interaction = NULL,
                                 approach = "logit",
                                 format = c("gt", "flextable"),
                                 theme = c("minimal")) {

  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","logbinomial","poisson","robpoisson","linear","negbin")
  )
  approach <- .normalize_approach(approach)
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("gt","flextable"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format)
  theme <- .resolve_theme(theme)

  if (!stratifier %in% names(data)) {
    stop("Stratifier not found in dataset.", call. = FALSE)
  }

  data <- data[!is.na(data[[stratifier]]), , drop = FALSE]
  levs <- .strata_levels(data, stratifier)

  message("Running stratified multivariable regression by: ", stratifier)

  per_stratum <- list()
  tds <- list()
  models <- list()
  sums <- list()
  diags <- list()
  n_by_stratum <- list()

  for (lv in levs) {
    message("  > Stratum: ", stratifier, " = ", lv)

    dlev <- data[data[[stratifier]] == lv, , drop = FALSE]
    key <- as.character(lv)

    res_i <- tryCatch(
      .run_multi_core(
        data = dlev,
        outcome = outcome,
        exposures = exposures,
        approach = approach,
        adjust_for = adjust_for,
        interaction = interaction
      ),
      error = function(e) {
        warning("Skipping stratum ", lv, ": ", e$message, call. = FALSE)
        NULL
      }
    )

    if (is.null(res_i)) {
      next
    }

    tds[[key]] <- res_i$table_body
    models[[key]] <- res_i$models
    sums[[key]] <- res_i$model_summaries
    diags[[key]] <- res_i$reg_check
    n_by_stratum[[key]] <- res_i$n_used

    per_stratum[[key]] <- list(
      table_body = res_i$table_body,
      models = res_i$models,
      model_summaries = res_i$model_summaries,
      reg_check = res_i$reg_check,
      n_used = res_i$n_used,
      adjusted_mode = res_i$adjusted_mode
    )
  }

  if (!length(tds)) {
    stop("No valid models across strata.", call. = FALSE)
  }

  built <- .strata_build_wide_multi(data, exposures, stratifier, tds)
  wide <- built$wide
  spanners <- built$spanners
  eff_lab <- paste("Adjusted", .get_effect_label(approach))

  footnotes <- c(
    .abbrev_note(approach),
    if (!is.null(adjust_for) && length(adjust_for) > 0) .adjustment_note(adjust_for) else NULL,
    if (!is.null(interaction)) .interaction_note(interaction) else NULL,
    .n_note_multi_strata(stratifier, n_by_stratum)
  )

  tbl <- if (format == "gt") {
    .build_gt_strata_wide_multi(wide, spanners, eff_lab, theme, footnotes)
  } else {
    .build_flex_strata_wide_multi(wide, spanners, eff_lab, theme, footnotes)
  }

  fmt_class <- if (format == "gt") "gt_strata_multi" else "ft_strata_multi"

  out <- list(
    table = tbl,
    table_display = wide,
    per_stratum = per_stratum,
    models = models,
    model_summaries = sums,
    reg_check = diags,
    by = stratifier,
    levels = levs,
    approach = approach,
    format = format,
    source = "stratified_multi_reg"
  )

  class(out) <- c("gtregression", "stratified_multi_reg", fmt_class, class(out))
  out
}
