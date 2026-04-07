#' Multivariable regression
#'
#' Create a publication-ready multivariable regression table using either
#' \pkg{gt} or \pkg{flextable}, without a \pkg{gtsummary} dependency.
#'
#' @param data A \code{data.frame} containing the variables of interest.
#' @param outcome Character scalar; name of the outcome variable.
#' @param exposures Character vector; exposure variable(s) to report.
#'   If \code{adjust_for = NULL}, all exposures are included in a single
#'   multivariable model. If \code{adjust_for} is supplied, one adjusted
#'   model is fitted per exposure and only exposure-specific adjusted
#'   estimate(s) are displayed.
#' @param adjust_for Optional character vector of adjustment variables.
#'   Must not overlap with \code{exposures}.
#' @param interaction Optional character scalar specifying one interaction term
#'   using standard formula syntax, e.g. \code{"bmi*sex"}.
#'   When used with \code{adjust_for}, only a single exposure should be supplied.
#' @param approach Character scalar specifying the regression approach.
#'   One of \code{"logit"}, \code{"log-binomial"}, \code{"poisson"},
#'   \code{"linear"}, \code{"robpoisson"}, or \code{"negbin"}.
#' @param format Output table format; one of \code{"gt"} (default) or \code{"flextable"}.
#' @param theme Table styling preset (e.g. \code{"minimal"}, \code{"striped"},
#'   \code{"clinical"}, \code{"shaded"}, \code{"jama"}) or a character vector of
#'   primitives such as \code{c("plain","zebra","lines","labels_bold","compact","header_shaded")}.
#'
#' @return A list of class \code{c("gtregression","multi_reg", ...)} with elements:
#' \describe{
#'   \item{table}{A \code{gt_tbl} (if \code{format="gt"}) or \code{flextable}
#'   (if \code{format="flextable"}).}
#'   \item{table_body}{A data frame of adjusted estimates and confidence intervals
#'   for each exposure and level.}
#'   \item{table_display}{A data frame used for rendering the final table,
#'   including header and level rows.}
#'   \item{models}{A list of fitted model(s).}
#'   \item{model_summaries}{\code{summary()} output for the fitted model(s).}
#'   \item{reg_check}{Regression diagnostics for linear models; otherwise a message.}
#'   \item{approach}{The regression approach used.}
#'   \item{format}{The output format used.}
#'   \item{source}{Function identifier (\code{"multi_reg"}).}
#' }
#'
#' @details
#' In default mode (\code{adjust_for = NULL}), all exposures are included in a
#' single multivariable model. In adjusted mode, one model is fitted per exposure,
#' adjusting for the variables specified in \code{adjust_for}.
#'
#' Interaction terms specified via \code{interaction} are included in the model
#' using standard formula expansion (e.g. \code{bmi*sex}). Interaction effects are
#' displayed as additional rows beneath the corresponding exposure.
#'
#' @importFrom stats qnorm residuals nobs
#' @export
multi_reg <- function(data,
                      outcome,
                      exposures,
                      adjust_for = NULL,
                      interaction = NULL,
                      approach = "logit",
                      format = c("gt", "flextable"),
                      theme = c("minimal")) {

  format <- match.arg(format)
  theme <- .resolve_theme(theme)

  fmt_class <- if (format == "gt") "gt_multi" else "ft_multi"
  effect_label_adj <- paste("Adjusted", .get_effect_label(approach))

  core <- .run_multi_core(
    data = data,
    outcome = outcome,
    exposures = exposures,
    approach = approach,
    adjust_for = adjust_for,
    interaction = interaction
  )

  footnotes <- if (!core$adjusted_mode) {
    c(
      .abbrev_note(approach),
      if (!is.null(interaction)) .interaction_note(interaction) else NULL,
      if (!is.na(core$n_used)) {
        paste0(
          "N = ", core$n_used,
          " complete observations included in the multivariable model"
        )
      } else {
        "N reflects complete observations included in the multivariable model"
      }
    )
  } else {
    c(
      .abbrev_note(approach),
      .adjustment_note(adjust_for),
      if (!is.null(interaction)) .interaction_note(interaction) else NULL,
      paste0(
        "N = ", core$n_used,
        " complete observations included across outcome, exposure, and adjustment variables"
      )
    )
  }

  display_df <- .make_display_multi(
    core$table_body,
    core$data_clean,
    outcome,
    effect_label_adj
  )
  .must_be_display_df_multi(display_df)

  tbl <- if (format == "gt") {
    .build_gt_multi(display_df, effect_label_adj, footnotes, theme)
  } else {
    .build_flextable_multi(display_df, effect_label_adj, footnotes, theme)
  }

  res <- list(
    table = tbl,
    table_body = core$table_body,
    table_display = display_df,
    models = core$models,
    model_summaries = core$model_summaries,
    reg_check = core$reg_check,
    approach = approach,
    format = format,
    source = "multi_reg"
  )

  class(res) <- c("gtregression", "multi_reg", fmt_class, class(res))
  res
}
