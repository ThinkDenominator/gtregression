#' Compare parametric survival model distributions
#'
#' Fit the same parametric survival regression model using multiple
#' distributions and compare model-fit statistics such as AIC and BIC.
#'
#' @param data A \code{data.frame} containing survival time, event status, and
#'   predictor variables.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param exposures Character vector of main exposure variable names. Quoted
#'   names are recommended in scripts, and bare names are also accepted.
#' @param adjust_for Optional character vector of adjustment variables. These
#'   variables are included in every candidate model.
#' @param distributions Parametric survival distributions to compare. Defaults
#'   to \code{c("weibull", "exponential", "lognormal", "loglogistic")}.
#'   Quoted and bare values are accepted. Common spellings such as
#'   \code{"log-normal"} and \code{"log-logistic"} are also accepted.
#' @param digits Number of digits for displayed model statistics.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","surv_model_compare", ...)}
#' with elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with model-fit statistics.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{models}}{Named list of fitted \code{survreg} models.}
#'   \item{\code{time,event,exposures,adjust_for,distributions,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' The same model formula is fitted for each candidate distribution using
#' \code{survival::survreg()}. Lower AIC or BIC values indicate better relative
#' model fit among the compared distributions. These statistics should be used
#' with clinical judgment and visual checks; they do not prove that a
#' distribution is scientifically correct.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#' lung_data$prior <- factor(lung_data$prior, levels = c(0, 10),
#'                           labels = c("No", "Yes"))
#'
#' surv_model_compare(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c("trt", "celltype", "prior"),
#'   adjust_for = c("age", "karno")
#' )
#'
#' surv_model_compare(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   exposures = c(trt, prior),
#'   distributions = c(weibull, "log-logistic"),
#'   format = tibble
#' )
#'
#' @importFrom survival survreg
#' @export
surv_model_compare <- function(data,
                               time,
                               event,
                               exposures,
                               adjust_for = NULL,
                               distributions = c("weibull", "exponential", "lognormal", "loglogistic"),
                               digits = 2,
                               format = c("flextable", "gt", "tibble"),
                               theme = c("minimal")) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  exposures <- .vars_arg(substitute(exposures), env = parent.frame())
  adjust_for <- .vars_arg(substitute(adjust_for), env = parent.frame(), allow_null = TRUE)
  distributions <- .surv_distribution_arg(
    substitute(distributions),
    env = parent.frame(),
    multiple = TRUE,
    arg = "distributions"
  )
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format, c("flextable", "gt", "tibble"))
  theme <- .resolve_theme(theme)

  data_clean <- .validate_surv_model_compare_inputs(
    data = data,
    time = time,
    event = event,
    exposures = exposures,
    adjust_for = adjust_for,
    distributions = distributions,
    digits = digits
  )

  predictors <- unique(c(exposures, adjust_for))
  models <- vector("list", length(distributions))
  names(models) <- distributions

  for (dist in distributions) {
    fit <- .fit_surv_model(data_clean, time, event, predictors, dist)
    if (is.null(fit)) {
      stop("Parametric survival model fitting failed for distribution '", dist, "'.", call. = FALSE)
    }
    models[[dist]] <- fit
  }

  table_body <- .surv_model_compare_body(models)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .surv_model_compare_display(table_body, digits = digits)
  tbl <- .build_surv_model_compare_table(
    table_display,
    format = format,
    theme = theme
  )

  res <- list(
    table = tbl,
    table_body = tibble::as_tibble(table_body),
    table_display = table_display,
    models = models,
    time = time,
    event = event,
    exposures = unique(exposures),
    adjust_for = if (!is.null(adjust_for) && length(adjust_for)) unique(adjust_for) else NULL,
    distributions = distributions,
    format = format,
    source = "surv_model_compare"
  )

  class(res) <- c("gtregression", "surv_model_compare", class(res))
  res
}

#' @keywords internal
#' @noRd
.validate_surv_model_compare_inputs <- function(data,
                                                time,
                                                event,
                                                exposures,
                                                adjust_for,
                                                distributions,
                                                digits) {
  choices <- c("weibull", "exponential", "lognormal", "loglogistic")
  if (!is.character(distributions) || length(distributions) < 1L ||
      anyNA(distributions) || any(!distributions %in% choices)) {
    stop(
      "`distributions` must contain one or more of: ",
      paste(choices, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }

  .validate_cox_inputs(
    data = data,
    time = time,
    event = event,
    exposures = unique(exposures),
    adjust_for = adjust_for
  )
}

#' @keywords internal
#' @noRd
.surv_model_compare_body <- function(models) {
  out <- lapply(names(models), function(dist) {
    fit <- models[[dist]]
    data.frame(
      Distribution = dist,
      AIC = .safe_numeric(stats::AIC(fit)),
      BIC = .safe_numeric(stats::BIC(fit)),
      logLik = .safe_numeric(as.numeric(stats::logLik(fit))),
      Scale = .safe_numeric(fit$scale),
      N = .safe_numeric(stats::nobs(fit)),
      Events = .safe_numeric(attr(fit, "gtregression_events", exact = TRUE)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)
  out$Best.AIC <- out$AIC == min(out$AIC, na.rm = TRUE)
  out$Best.BIC <- out$BIC == min(out$BIC, na.rm = TRUE)
  out <- out[order(out$AIC), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @keywords internal
#' @noRd
.surv_model_compare_display <- function(table_body, digits = 2) {
  fmt_num <- function(x) {
    ifelse(
      is.na(x),
      "NA",
      formatC(x, digits = digits, format = "f", big.mark = ",")
    )
  }

  data.frame(
    Distribution = table_body$Distribution,
    AIC = fmt_num(table_body$AIC),
    BIC = fmt_num(table_body$BIC),
    `Log-likelihood` = fmt_num(table_body$logLik),
    Scale = fmt_num(table_body$Scale),
    N = as.character(table_body$N),
    Events = as.character(table_body$Events),
    `Best AIC` = ifelse(table_body$Best.AIC, "Yes", "No"),
    `Best BIC` = ifelse(table_body$Best.BIC, "Yes", "No"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_surv_model_compare_table <- function(display,
                                            format = c("flextable", "gt"),
                                            theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Lower AIC or BIC indicates better relative fit among the compared distributions.",
    "Use model fit statistics with clinical judgment and visual checks."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Parametric survival model comparison") |>
      gt::cols_align(align = "left", columns = "Distribution") |>
      gt::cols_align(
        align = "center",
        columns = c("AIC", "BIC", "Log-likelihood", "Scale", "N", "Events", "Best AIC", "Best BIC")
      ) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_source_note(gt::md(note)) |>
      .compact_gt_source_notes()

    if ("header_shaded" %in% theme) {
      tbl <- gt::tab_options(tbl, column_labels.background.color = "#f6f8fa")
    }
    if ("zebra" %in% theme) {
      tbl <- gt::opt_row_striping(tbl)
    }
    if ("compact" %in% theme) {
      tbl <- gt::tab_options(tbl, data_row.padding = gt::px(2))
    }
    return(tbl)
  }

  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Parametric survival model comparison")
  ft <- flextable::align(ft, j = "Distribution", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("AIC", "BIC", "Log-likelihood", "Scale", "N", "Events", "Best AIC", "Best BIC"),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  if ("header_shaded" %in% theme) {
    ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  }
  if ("zebra" %in% theme && nrow(display) > 1L) {
    ft <- flextable::bg(ft, i = seq(1, nrow(display), by = 2), bg = "#f6f8fa", part = "body")
  }
  if ("compact" %in% theme) {
    ft <- flextable::padding(ft, padding = 2, part = "body")
  }
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- .compact_flex_footer(ft)
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
