#' Stratified univariable regression
#'
#' Fit univariable regression models within each level of a stratifier and
#' combine the results into a publication-ready stratified table rendered with
#' \pkg{gt} or \pkg{flextable}.
#'
#' @param data A data frame containing the variables.
#' @param outcome Character scalar; name of the outcome variable. Quoted and
#'   bare names are accepted.
#' @param exposures Character vector of exposure variables to model. Quoted
#'   names are recommended in scripts, and bare names are also accepted.
#' @param stratifier Character scalar; name of the stratifying variable. Quoted
#'   and bare names are accepted.
#' @param approach Modeling approach. One of \code{"logit"},
#'   \code{"logbinomial"}, \code{"poisson"}, \code{"robpoisson"},
#'   \code{"linear"}, or \code{"negbin"}.
#' @param format Output table format; one of \code{"flextable"} (default) or
#'   \code{"gt"}.
#' @param theme Table styling preset or theme primitives.
#'
#' @return A list of class
#'   \code{c("gtregression", "stratified_uni_reg", ...)} with elements:
#' \describe{
#'   \item{\code{table}}{A rendered \code{gt_tbl} or \code{flextable}.}
#'   \item{\code{table_display}}{Display-ready wide stratified results.}
#'   \item{\code{per_stratum}}{List of complete \code{uni_reg()} results by
#'   stratum.}
#'   \item{\code{models}}{List of fitted model objects by stratum.}
#'   \item{\code{model_summaries}}{List of model summaries by stratum.}
#'   \item{\code{reg_check}}{Regression diagnostics by stratum.}
#'   \item{\code{by}}{The stratifier variable.}
#'   \item{\code{levels}}{Strata included in the analysis.}
#'   \item{\code{approach}}{The regression approach used.}
#'   \item{\code{format}}{The output format used.}
#'   \item{\code{source}}{Function identifier
#'   (\code{"stratified_uni_reg"}).}
#' }
#'
#' @section Accessors:
#' \describe{
#'   \item{\code{$table}}{Rendered stratified regression table.}
#'   \item{\code{$table_display}}{Wide display data used to build the table.}
#'   \item{\code{$per_stratum}}{Full \code{uni_reg()} result objects by
#'   stratum.}
#'   \item{\code{$models}}{List of fitted model objects for each stratum.}
#'   \item{\code{$model_summaries}}{List of model summaries.}
#'   \item{\code{$reg_check}}{Diagnostic check results (when applicable).}
#' }
#'
#' @seealso [multi_reg()], [plot_reg()], [identify_confounder()]
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   transform(
#'     race = factor(race, levels = c(1, 2, 3),
#'                   labels = c("White", "Black", "Other")),
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1),
#'                  labels = c("Normal BW", "Low BW"))
#'   )
#'
#' stratified_uni <- stratified_uni_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "lwt", "smoke"),
#'   stratifier = "race",
#'   approach = "logit"
#' )
#'
#' stratified_uni$table
#'
#' @importFrom purrr map
#' @importFrom broom tidy


#' @export
stratified_uni_reg <- function(data, outcome, exposures, stratifier,
                               approach = "logit",
                               format   = c("flextable","gt"),
                               theme    = c("minimal")) {
  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  stratifier <- .vars_arg(substitute(stratifier), env = parent.frame())
  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","logbinomial","poisson","robpoisson","linear","negbin")
  )
  approach <- .normalize_approach(approach)
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("flextable","gt"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format, c("flextable","gt"))
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
