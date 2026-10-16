#' Univariable regression
#'
#' Fit one model per exposure and return a clean regression table in
#' \pkg{flextable} or \pkg{gt} format.
#'
#' @param data A data frame containing the outcome and exposure variables.
#' @param outcome Character scalar; outcome column name. Quoted and bare names
#'   are accepted.
#'   For binary-outcome approaches (\code{"logit"}, \code{"firth"},
#'   \code{"logbinomial"}, \code{"robpoisson"}) the outcome may be numeric
#'   0/1 or 1/2 (2 is the event; integer or double), logical (\code{TRUE} is
#'   the event), or a two-level factor or character variable (the second
#'   level is the event, which is reported in a message). The same factor,
#'   character and logical coding is accepted by \code{"poisson"}.
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
#' @param show_ref Logical; if \code{TRUE} (default), display reference-category
#'   rows as \code{"Ref."}. If \code{FALSE}, hide reference rows; a message
#'   reminds users to use \code{show_ref = TRUE} when reference rows are needed.
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
#' Confidence intervals are Wald intervals; for \code{approach = "linear"}
#' they use the t distribution with the residual degrees of freedom (as
#' \code{confint()} for \code{lm}), consistent with the t-based p-values.
#' Factor exposures are always fitted with treatment contrasts (each level
#' versus the reference), whatever \code{options(contrasts = )} or a
#' \code{contrasts} attribute says. An exposure whose model cannot be fitted
#' is kept in the table with a dash and named in a footnote and in
#' \code{failed_exposures}.
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
#'   \item{reg_check}{Diagnostics for linear models. Printing this element
#'   renders a publication-ready table; individual diagnostic rows remain
#'   available for programmatic use. Other approaches return a message.}
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
                    model_stats = FALSE,
                    show_ref = TRUE) {

  outcome <- .vars_arg(
    substitute(outcome),
    env = parent.frame(),
    data_names = names(data)
  )
  exposures <- .vars_arg(
    substitute(exposures),
    env = parent.frame(),
    data_names = names(data)
  )
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
  .validate_show_ref(show_ref)

  format <- match.arg(format, c("flextable","gt"))
  .validate_theme_arg(theme)
  theme  <- .resolve_theme(theme)

  # ---- validate inputs (package helper) -------------------------------------
  .validate_uni_inputs(data, outcome, exposures, approach)
  # Binary outcomes are recoded to 0/1 once, on the full data (factor and
  # character: second level = event; logical: TRUE = event; numeric 1/2: 2).
  data <- .normalize_outcome_once(data, outcome, approach)

  # engine tag for S3 class chain
  fmt_class <- if (format == "gt") "gt_uni" else "ft_uni"

  # ---- fit, tidy, stack ------------------------------------------------------
  fit_errors <- character(0)
  model_list <- lapply(exposures, function(x) {
    withCallingHandlers(
      .fit_uni_model(data, outcome, x, approach),
      warning = function(w) {
        if (startsWith(conditionMessage(w), "Model failed for exposure")) {
          fit_errors[[x]] <<- conditionMessage(w)
        }
      }
    )
  })
  names(model_list) <- exposures
  model_list <- Filter(Negate(is.null), model_list)
  if (!length(model_list)) {
    stop(
      "All models failed. Check data and exposure specifications.",
      if (length(fit_errors)) paste0("\n", paste(unique(fit_errors), collapse = "\n")),
      call. = FALSE
    )
  }

  tidy_list <- Map(function(fit, var) .tidy_uni(fit, var, approach), model_list, names(model_list))
  tidy_list <- Filter(Negate(is.null), tidy_list)
  if (!length(tidy_list))
    stop("No estimable coefficients for supplied exposures.", call. = FALSE)

  td <- do.call(rbind, tidy_list)

  # Exposures whose model could not be fitted or estimated are kept visible
  # in the table (dash) instead of disappearing.
  failed_exposures <- setdiff(exposures, names(tidy_list))

  # ---- display + build table -------------------------------------------------
  effect_label <- .get_effect_label(approach)
  .message_hidden_ref_rows("uni_reg", td, show_ref)
  display_df   <- .make_display_uni(
    td, data, outcome, effect_label,
    show_ref = show_ref,
    exposure_order = exposures,
    failed_exposures = failed_exposures
  )
  .must_be_display_df(display_df)

  source_note  <- c(
    .abbrev_note(approach),
    if (.has_visible_reference(td, show_ref)) .ref_note() else NULL,
    if (length(failed_exposures)) {
      paste0(
        "\u2014 = model could not be fitted for: ",
        paste(vapply(failed_exposures, .label_var, character(1),
                     label_map = attr(display_df, "variable_labels",
                                      exact = TRUE)),
              collapse = ", "),
        "."
      )
    } else {
      NULL
    }
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
    footnotes       = source_note,
    failed_exposures = failed_exposures,
    reg_check       = .as_reg_check_result(reg_diagnostics, format = format),
    approach        = approach,
    format          = format,
    source          = "uni_reg",
    show_ref        = isTRUE(show_ref)
  )
  class(res) <- c("gtregression", "uni_reg", fmt_class, class(res))
  res
}


#' Display frame for univariable regression tables
#'
#' Same layout as the multivariable tables: `Characteristic`, `N`, effect,
#' `p-value`, with an en dash between the confidence limits.
#'
#' @keywords internal
#' @noRd
.make_display_uni <- function(td, data, outcome, effect_label, show_ref = TRUE,
                              exposure_order = NULL,
                              failed_exposures = character(0)) {
  stopifnot(all(c("exposure", "level", "estimate", "conf.low", "conf.high",
                  "p.value", "ref") %in% names(td)))
  shown <- unique(td$exposure)
  if (is.null(exposure_order)) exposure_order <- shown
  exposure_order <- unique(exposure_order[exposure_order %in%
                                            c(shown, failed_exposures)])
  label_map <- .var_label_map(data, exposure_order)
  fmt_est_ci <- function(est, lo, hi, digits = 2) {
    f <- function(x) formatC(x, digits = digits, format = "f", big.mark = ",")
    paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
  }
  complete_n <- function(x) {
    sum(stats::complete.cases(data[, c(outcome, x), drop = FALSE]))
  }
  columns <- c("Characteristic", "N", effect_label, "p-value", "is_header")
  header_row <- function(exp_nm, effect = "", p = "") {
    header <- data.frame(Characteristic = .label_var(exp_nm, label_map),
                         stringsAsFactors = FALSE)
    header$N <- complete_n(exp_nm)
    header[[effect_label]] <- effect
    header[["p-value"]] <- p
    header$is_header <- TRUE
    header[, columns, drop = FALSE]
  }

  blocks <- lapply(exposure_order, function(exp_nm) {
    if (!exp_nm %in% shown) {
      return(header_row(exp_nm, effect = "\u2014"))
    }
    df <- td[td$exposure == exp_nm, , drop = FALSE]
    is_fact <- any(df$ref)

    if (!is_fact) {
      return(header_row(
        exp_nm,
        fmt_est_ci(df$estimate[1], df$conf.low[1], df$conf.high[1]),
        .fmt_p(df$p.value[1])
      ))
    }
    if (!isTRUE(show_ref) && nrow(df) == 2L && sum(df$ref %in% TRUE) == 1L) {
      row <- df[!(df$ref %in% TRUE), , drop = FALSE][1, ]
      return(header_row(
        exp_nm,
        fmt_est_ci(row$estimate, row$conf.low, row$conf.high),
        .fmt_p(row$p.value)
      ))
    }

    lev <- df
    lev$Characteristic <- paste0("  ", lev$level)
    lev$N <- NA_integer_
    lev[[effect_label]] <- ifelse(
      lev$ref, "Ref.", fmt_est_ci(lev$estimate, lev$conf.low, lev$conf.high)
    )
    lev[["p-value"]] <- ifelse(lev$ref, "", .fmt_p(lev$p.value))
    lev$is_header <- FALSE
    rbind(header_row(exp_nm), lev[, columns, drop = FALSE],
          make.row.names = FALSE)
  })
  names(blocks) <- exposure_order
  out <- do.call(rbind, blocks)
  rownames(out) <- NULL
  row_exposure <- rep(names(blocks), vapply(blocks, nrow, integer(1)))
  .attach_display_metadata(out, row_exposure = row_exposure,
                           variable_labels = label_map)
}
