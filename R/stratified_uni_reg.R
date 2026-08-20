#' Stratified univariable regression
#'
#' Fit univariable regression models within each level of a stratifier and
#' combine the results into a stratified table rendered with
#' \pkg{gt} or \pkg{flextable}.
#'
#' @param data A data frame containing the variables.
#' @param outcome Character scalar; name of the outcome variable. Quoted and
#'   bare names are accepted.
#' @param exposures Character vector of exposure variables to model. Quoted
#'   names are recommended in scripts, and bare names are also accepted.
#' @param stratifier Character scalar; name of the stratifying variable. Quoted
#'   and bare names are accepted.
#' @param approach Modeling approach. One of \code{"logit"}, \code{"firth"},
#'   \code{"logbinomial"}, \code{"poisson"}, \code{"robpoisson"},
#'   \code{"linear"}, or \code{"negbin"}.
#' @param format Output table format; one of \code{"flextable"} (default) or
#'   \code{"gt"}.
#' @param theme Table styling preset or theme primitives.
#' @param show_ref Logical; if \code{TRUE} (default), display reference-category
#'   rows as \code{"Ref."}. If \code{FALSE}, hide reference rows; a message
#'   reminds users to use \code{show_ref = TRUE} when reference rows are needed.
#'
#' @details
#' If exposure variables have a \code{"label"} attribute, for example from
#' \code{labelled::var_label()}, those labels are used automatically in the
#' displayed table. Internal matching still uses the original column names.
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
#'   \item{\code{variable_labels}}{Named character vector of display labels used
#'   for exposure variables.}
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
#'   approach = "logit",
#'   format = "gt"
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
                               theme    = c("minimal"),
                               show_ref = TRUE) {
  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  stratifier <- .vars_arg(substitute(stratifier), env = parent.frame())
  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","firth","logbinomial","poisson","robpoisson","linear","negbin")
  )
  approach <- .normalize_approach(approach)
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("flextable","gt"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  .validate_show_ref(show_ref)

  format <- match.arg(format, c("flextable","gt"))
  theme  <- .resolve_theme(theme)
  variable_labels <- .var_label_map(data, unique(exposures))

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
      suppressMessages(uni_reg(
        dlev,
        outcome,
        exposures,
        approach = approach,
        format = format,
        theme = theme,
        show_ref = show_ref
      )),
      error = function(e) { warning("Skipping stratum ", lv, ": ", e$message, call. = FALSE); NULL }
    )
    if (!is.null(res)) per_stratum[[as.character(lv)]] <- res
  }
  if (!length(per_stratum)) stop("No valid models across strata.", call. = FALSE)

  built <- .strata_build_wide_uni(
    data,
    outcome,
    exposures,
    stratifier,
    per_stratum,
    variable_labels = variable_labels,
    show_ref = show_ref
  )
  wide      <- built$wide
  spanners  <- built$spanners
  has_ref <- any(unlist(
    lapply(per_stratum, function(x) x$table_body$ref %in% TRUE),
    use.names = FALSE
  ))
  .message_hidden_ref_rows(
    "stratified_uni_reg",
    do.call(rbind, lapply(per_stratum, `[[`, "table_body")),
    show_ref
  )
  footnotes <- c(
    .stratified_by_note(stratifier),
    .abbrev_note(approach),
    if (isTRUE(show_ref) && has_ref) .ref_note() else NULL
  )
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
    variable_labels = variable_labels,
    reg_check     = .as_reg_check_result(diags, format = format),
    by            = stratifier,
    levels        = levs,
    approach      = approach,
    format        = format,
    source        = "stratified_uni_reg",
    show_ref      = isTRUE(show_ref)
  )
  class(out) <- c("gtregression","stratified_uni_reg", fmt_class, class(out))
  out
}
