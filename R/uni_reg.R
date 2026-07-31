#' Univariate regression
#'
#' Fit one model per exposure and return a clean regression table in
#' \pkg{flextable} or \pkg{gt} format.
#'
#' @param data A data frame containing the outcome and exposure variables.
#' @param outcome Character scalar; outcome column name. Quoted and bare names
#'   are accepted.
#' @param exposures Character vector; exposure column names. Quoted names are
#'   recommended in scripts, and bare names are also accepted.
#' @param approach Regression approach. One of \code{"logit"}, \code{"firth"},
#'   \code{"logbinomial"}, \code{"poisson"}, \code{"robpoisson"},
#'   \code{"linear"}, or \code{"negbin"}. Use \code{"firth"} for Firth
#'   penalized logistic regression, especially with sparse cells or separation.
#' @param format One of \code{"flextable"} (default) or \code{"gt"}.
#' @param theme Preset name (e.g. \code{"minimal"}, \code{"striped"}, \code{"clinical"},
#'   \code{"shaded"}, \code{"jama"}) or primitives
#'   \code{c("plain","zebra","lines","labels_bold","compact","header_shaded")}
#' @param model_stats Logical; if \code{TRUE}, extract model-fit statistics
#'   such as AIC, BIC, log-likelihood, deviance, pseudo R-squared for
#'   non-linear models, and R-squared for linear models. Statistics are stored
#'   in the returned object's \code{model_stats} element and are not added to
#'   the publication table.
#'
#' @details
#' Use this when you want a quick crude association table before building an
#' adjusted model. The fitted models are kept in the returned object, so the
#' formatted table does not hide the underlying analysis.
#'
#' If exposure variables have a \code{"label"} attribute, for example from
#' \code{labelled::var_label()}, those labels are used automatically in the
#' displayed table and plots. Internal matching still uses the original column
#' names.
#'
#' @return A list of class \code{c("gtregression","uni_reg", ...)} with elements:
#' \describe{
#'   \item{table}{A \code{flextable} (when \code{format="flextable"}) or
#'   \code{gt_tbl} (when \code{format="gt"}).}
#'   \item{table_body}{Data frame of numeric estimates and CIs.}
#'   \item{table_display}{Data frame for display (headers + levels).}
#'   \item{models}{List of fitted univariate models.}
#'   \item{model_summaries}{Per-model \code{summary()} results.}
#'   \item{model_stats}{Model-fit statistics when \code{model_stats = TRUE};
#'   otherwise \code{NULL}.}
#'   \item{variable_labels}{Named character vector of display labels used for
#'   exposure variables.}
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
#'   ft <- uni_reg(d, "am", c("mpg","cyl"), approach = "logit", format = "flextable")
#'   class(ft$table)
#' }
#'
#' endometrial_data <- data_endometrial
#' endometrial_data$HG <- factor(endometrial_data$HG, levels = c(0, 1))
#' endometrial_data$NV <- factor(endometrial_data$NV, levels = c(0, 1))
#' uni_reg(endometrial_data, HG, c(NV, PI, EH), approach = firth, format = gt)$table
#'
#' @importFrom stats qnorm residuals
#' @export
uni_reg <- function(data,
                    outcome,
                    exposures,
                    approach = "logit",
                    format = c("flextable","gt"),
                    theme = c("minimal"),
                    model_stats = FALSE) {

  outcome <- .vars_arg(substitute(outcome), env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  approach <- .choice_arg(
    substitute(approach),
    env = parent.frame(),
    choices = c("logit","firth","logbinomial","poisson","robpoisson","linear","negbin")
  )
  approach <- .normalize_approach(approach)
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("flextable","gt"))
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  if (!is.logical(model_stats) || length(model_stats) != 1L || is.na(model_stats)) {
    stop("`model_stats` must be TRUE or FALSE.", call. = FALSE)
  }

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

  source_note  <- c(
    .abbrev_note(approach),
    if (any(td$ref %in% TRUE)) .ref_note() else NULL
  )

  tbl <- if (format == "gt") {
    .build_gt(display_df, effect_label, source_note, theme)
  } else {
    .build_flextable(display_df, effect_label, source_note, theme)
  }

  # ---- summaries & diagnostics ----------------------------------------------
  model_summaries <- lapply(model_list, .model_summary)
  fit_stats <- if (isTRUE(model_stats)) .model_stats_table(model_list, approach) else NULL

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
    model_stats     = fit_stats,
    variable_labels = attr(display_df, "variable_labels", exact = TRUE),
    reg_check       = reg_diagnostics,
    approach        = approach,
    format          = format,
    source          = "uni_reg"
  )
  class(res) <- c("gtregression", "uni_reg", fmt_class, class(res))
  res
}
