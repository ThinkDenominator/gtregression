#' Multivariable regression
#'
#' Create a publication-ready multivariable regression table using either
#' \pkg{gt} or \pkg{flextable}, without a gtsummary dependency.
#'
#' @param data data.frame
#' @param outcome character scalar; outcome column name
#' @param exposures character vector; exposure column names (all included in one model)
#' @param approach one of \code{"logit"}, \code{"log-binomial"}, \code{"poisson"},
#'   \code{"linear"}, \code{"robpoisson"}, or \code{"negbin"}
#' @param format one of \code{"gt"} (default) or \code{"flextable"}
#' @param theme preset name (e.g. \code{"minimal"}, \code{"striped"}, \code{"clinical"},
#'   \code{"shaded"}, \code{"jama"}) or primitives
#'   \code{c("plain","zebra","lines","labels_bold","compact","header_shaded")}
#'
#' @return A list of class \code{c("gtregression","multi_reg", ...)} with elements:
#' \describe{
#'   \item{table}{A \code{gt_tbl} (when \code{format="gt"}) or \code{flextable} (when \code{format="flextable"}).}
#'   \item{table_body}{Data frame of adjusted estimates and CIs (per level).}
#'   \item{table_display}{Data frame for display (headers + levels) without N column.}
#'   \item{models}{List with the single multivariable model.}
#'   \item{model_summaries}{\code{summary()} of the fitted model.}
#'   \item{reg_check}{Diagnostics for linear model; message otherwise.}
#'   \item{approach, format, source}{Metadata fields.}
#' }
#'
#' @examples
#' d <- mtcars
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   multi_reg(d, "am", c("mpg","cyl","gear"), approach = "logit", format = "gt")$table
#' }
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   multi_reg(d, "am", c("mpg","cyl","gear"), approach = "logit", format = "flextable")$table
#' }
#'
#' @importFrom stats qnorm residuals nobs
#' @export
multi_reg <- function(data,
                      outcome,
                      exposures,
                      approach = "logit",
                      format = c("gt","flextable"),
                      theme = c("minimal")) {

  format <- match.arg(format)
  theme  <- .resolve_theme(theme)

  # ---- validate inputs (package helper) -------------------------------------
  .validate_multi_inputs(data, outcome, exposures, approach)

  # engine tag for S3 chain
  fmt_class <- if (format == "gt") "gt_multi" else "ft_multi"

  # ---- fit single multivariable model ---------------------------------------
  fit <- .fit_multi_model(data, outcome, exposures, approach)
  if (is.null(fit)) stop("Model fitting failed.", call. = FALSE)

  # ---- tidy adjusted coefficients (+ reference rows for factors) ------------


  td <- .tidy_multi(fit, exposures, approach)
  if (is.null(td) || !nrow(td))
    stop("No estimable coefficients for supplied exposures.", call. = FALSE)

  # ---- build display (NO N COLUMN) ------------------------------------------
  effect_label_adj <- paste("Adjusted", .get_effect_label(approach))


  display_df <- .make_display_multi(td, data, outcome, effect_label_adj)

  # ---- builders for NO-N layout ---------------------------------------------

  .must_be_display_df_multi(display_df)
  # ---- build table and footnotes (two separate lines) -------------------------
  n_used <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)
  note_abbrev <- .abbrev_note(approach)
  n_note <- if (!is.na(n_used)) {
    paste0("N = ", n_used, " complete observations included in the multivariable model")
  } else {
    "N reflects complete observations included in the multivariable model"
  }
  footnotes <- c(note_abbrev, n_note)


  tbl <- if (format == "gt") {
    .build_gt_multi(display_df, effect_label_adj, footnotes, theme)
  } else {
    .build_flextable_multi(display_df, effect_label_adj, footnotes, theme)
  }


  # ---- summaries & diagnostics ----------------------------------------------
  model_list       <- list(multivariable_model = fit)
  model_summaries  <- list(multivariable_model = summary(fit))

  # always return a named list with the same key
  reg_diagnostics <- list(
    multivariable_model = if (approach == "linear") {
      .reg_check_linear(fit, exposure = "multivariable_model")
    } else {
      "Regression diagnostics available only for 'linear' models."
    }
  )



  # ---- return ----------------------------------------------------------------
  res <- list(
    table           = tbl,
    table_body      = td,
    table_display   = display_df,
    models          = model_list,
    model_summaries = model_summaries,
    reg_check       = reg_diagnostics,
    approach        = approach,
    format          = format,
    source          = "multi_reg"
  )
  class(res) <- c("gtregression", "multi_reg", fmt_class, class(res))
  res
}
