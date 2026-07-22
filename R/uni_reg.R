#' Univariate regression
#'
#' Create a publication-ready univariate regression table using either
#' \pkg{gt} or \pkg{flextable}
#'
#' @param data data.frame
#' @param outcome Character scalar; outcome column name. Quoted and bare names
#'   are accepted.
#' @param exposures Character vector; exposure column names. Quoted names are
#'   recommended in scripts, and bare names are also accepted.
#' @param approach one of \code{"logit"}, \code{"logbinomial"}, \code{"poisson"}, \code{"linear"}
#' @param format One of \code{"flextable"} (default) or \code{"gt"}.
#' @param theme preset name (e.g. \code{"minimal"}, \code{"striped"}, \code{"clinical"},
#'   \code{"shaded"}, \code{"jama"}) or primitives
#'   \code{c("plain","zebra","lines","labels_bold","compact","header_shaded")}
#'
#' @return A list of class \code{c("gtregression","uni_reg", ...)} with elements:
#' \describe{
#'   \item{table}{A \code{flextable} (when \code{format="flextable"}) or
#'   \code{gt_tbl} (when \code{format="gt"}).}
#'   \item{table_body}{Data frame of numeric estimates and CIs.}
#'   \item{table_display}{Data frame for display (headers + levels).}
#'   \item{models}{List of fitted univariate models.}
#'   \item{model_summaries}{Per-model \code{summary()} results.}
#'   \item{reg_check}{Diagnostics for linear models; message otherwise.}
#'   \item{approach, format, source}{Metadata fields.}
#' }
#'
#' @examples
#' d <- mtcars
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   uni_reg(d, "am", c("mpg","cyl"), approach = "logit", format = "gt")$table
#' }
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   uni_reg(d, "am", c("mpg","cyl"), approach = "logit", format = "flextable")$table
#' }
#'
#' @importFrom stats qnorm residuals
#' @export
uni_reg <- function(data,
                    outcome,
                    exposures,
                    approach = "logit",
                    format = c("flextable","gt"),
                    theme = c("minimal")) {

  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
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

  # ---- validate inputs (package helper) -------------------------------------
  .validate_uni_inputs(data, outcome, exposures, approach)

  # engine tag for S3 class chain
  fmt_class <- if (format == "gt") "gt_uni" else "ft_uni"

  # ---- fit, tidy, stack ------------------------------------------------------
  model_list <- lapply(exposures, function(x) .fit_uni_model(data, outcome, x, approach))
  names(model_list) <- exposures
  model_list <- Filter(Negate(is.null), model_list)
  if (!length(model_list))
    stop("All models failed. Check data and exposure specifications.", call. = FALSE)

  tidy_list <- Map(function(fit, var) .tidy_uni(fit, var, approach), model_list, names(model_list))
  tidy_list <- Filter(Negate(is.null), tidy_list)
  if (!length(tidy_list))
    stop("No estimable coefficients for supplied exposures.", call. = FALSE)

  td <- do.call(rbind, tidy_list)

  # ---- display + build table -------------------------------------------------
  effect_label <- .get_effect_label(approach)
  display_df   <- .make_display(td, data, outcome, approach, effect_label)
  .must_be_display_df(display_df)

  source_note  <- .abbrev_note(approach)

  tbl <- if (format == "gt") {
    .build_gt(display_df, effect_label, source_note, theme)
  } else {
    .build_flextable(display_df, effect_label, source_note, theme)
  }

  # ---- summaries & diagnostics ----------------------------------------------
  model_summaries <- lapply(model_list, summary)

  # Always define a default first (belt-and-braces)
  reg_diagnostics <- list(message = "Regression diagnostics available only for 'linear' models.")

  if (identical(approach, "linear")) {
    reg_diagnostics <- lapply(names(model_list), function(v) {
      .reg_check_linear(model_list[[v]], exposure = v)
    })
    names(reg_diagnostics) <- names(model_list)
  }


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
    source          = "uni_reg"
  )
  class(res) <- c("gtregression", "uni_reg", fmt_class, class(res))
  res
}
