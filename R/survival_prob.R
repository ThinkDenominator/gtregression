#' Kaplan-Meier survival probability table
#'
#' Estimate Kaplan-Meier survival probabilities at user-specified follow-up
#' times, such as 30-day, 6-month, or 1-year survival.
#'
#' @param data A \code{data.frame} containing survival time and event status.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for separate Kaplan-Meier survival
#'   probabilities. Quoted and bare names are accepted.
#' @param times Numeric vector of follow-up times at which survival probability
#'   should be estimated.
#' @param digits Number of digits for percentages and survival probabilities.
#' @param extend Logical. If \code{TRUE}, requested times beyond the observed
#'   follow-up range are retained using the last available Kaplan-Meier estimate.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","survival_prob", ...)}
#' with elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with Kaplan-Meier survival probabilities.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{fit}}{Fitted \code{survfit} object.}
#'   \item{\code{time,event,by,times,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' Survival probabilities are estimated from \code{survival::survfit()} at the
#' requested follow-up times. Events and censored counts are interval counts up
#' to each requested time point as returned by \code{summary.survfit()}.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' survival_prob(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt,
#'   times = c(90, 180, 365)
#' )
#'
#' survival_prob(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   times = c(90, 180),
#'   format = tibble
#' )
#'
#' @importFrom survival survfit
#' @importFrom stats complete.cases
#' @export
survival_prob <- function(data,
                          time,
                          event,
                          by = NULL,
                          times,
                          digits = 1,
                          extend = TRUE,
                          format = c("flextable", "gt", "tibble"),
                          theme = c("minimal")) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  by <- .vars_arg(substitute(by), env = parent.frame(), allow_null = TRUE)
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format, c("flextable", "gt", "tibble"))
  theme <- .resolve_theme(theme)

  data_clean <- .validate_survival_prob_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    times = times,
    digits = digits,
    extend = extend
  )

  if (!is.null(by)) {
    data_clean[[by]] <- factor(data_clean[[by]])
  }

  fit <- survival::survfit(.km_formula(time, event, by), data = data_clean)
  sm <- summary(fit, times = times, extend = extend)
  table_body <- .survival_prob_table_body(sm, by = by)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .survival_prob_display(table_body, digits = digits)
  tbl <- .build_survival_prob_table(
    table_display,
    format = format,
    theme = theme
  )

  res <- list(
    table = tbl,
    table_body = tibble::as_tibble(table_body),
    table_display = table_display,
    fit = fit,
    time = time,
    event = event,
    by = by,
    times = times,
    extend = extend,
    format = format,
    source = "survival_prob"
  )

  class(res) <- c("gtregression", "survival_prob", class(res))
  res
}

#' @keywords internal
#' @noRd
.validate_survival_prob_inputs <- function(data, time, event, by, times, digits, extend) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1L || !time %in% names(data)) {
    stop("`time` must be a single survival time variable in `data`.", call. = FALSE)
  }
  if (!is.character(event) || length(event) != 1L || !event %in% names(data)) {
    stop("`event` must be a single event indicator variable in `data`.", call. = FALSE)
  }
  if (!is.null(by) && (!is.character(by) || length(by) != 1L || !by %in% names(data))) {
    stop("`by` must be NULL or a single grouping variable in `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[time]])) {
    stop("`time` must be numeric.", call. = FALSE)
  }
  if (any(data[[time]] < 0, na.rm = TRUE)) {
    stop("`time` must contain non-negative follow-up times.", call. = FALSE)
  }
  if (missing(times) || !is.numeric(times) || length(times) < 1L ||
      anyNA(times) || any(times < 0)) {
    stop("`times` must be a numeric vector of non-negative follow-up times.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }
  if (!is.logical(extend) || length(extend) != 1L || is.na(extend)) {
    stop("`extend` must be TRUE or FALSE.", call. = FALSE)
  }

  vars_needed <- unique(c(time, event, by))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  if (nrow(data_clean) == 0) {
    stop("No complete cases available for survival probabilities.", call. = FALSE)
  }

  data_clean[[event]] <- .cox_event01(data_clean[[event]])
  if (sum(data_clean[[event]] == 1) == 0) {
    stop("`event` must include at least one event.", call. = FALSE)
  }
  if (!is.null(by) && length(unique(data_clean[[by]])) < 2L) {
    stop("`by` must contain at least two non-missing groups.", call. = FALSE)
  }

  data_clean
}

#' @keywords internal
#' @noRd
.survival_prob_table_body <- function(sm, by = NULL) {
  group <- if (is.null(by)) {
    rep("Overall", length(sm$time))
  } else {
    sub("^.*=", "", sm$strata)
  }

  data.frame(
    Group = group,
    Time = as.numeric(sm$time),
    N.risk = as.numeric(sm$n.risk),
    Events = as.numeric(sm$n.event),
    Censored = as.numeric(sm$n.censor),
    Survival.probability = as.numeric(sm$surv),
    CI.lower = as.numeric(sm$lower),
    CI.upper = as.numeric(sm$upper),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.survival_prob_display <- function(table_body, digits = 1) {
  fmt_time <- function(x) {
    formatC(x, digits = digits, format = "f", big.mark = ",")
  }
  fmt_pct <- function(x) {
    ifelse(
      is.na(x),
      "NA",
      paste0(formatC(100 * x, digits = digits, format = "f"), "%")
    )
  }

  surv_txt <- paste0(
    fmt_pct(table_body$Survival.probability),
    " (",
    fmt_pct(table_body$CI.lower),
    "-",
    fmt_pct(table_body$CI.upper),
    ")"
  )

  data.frame(
    Group = table_body$Group,
    Time = fmt_time(table_body$Time),
    `At risk` = as.character(table_body$N.risk),
    Events = as.character(table_body$Events),
    Censored = as.character(table_body$Censored),
    `Survival probability (95% CI)` = surv_txt,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_survival_prob_table <- function(display,
                                       format = c("flextable", "gt"),
                                       theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Survival probabilities are estimated using Kaplan-Meier methods.",
    "Events and censored counts are interval counts up to each requested time point."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Kaplan-Meier survival probabilities") |>
      gt::cols_align(align = "left", columns = "Group") |>
      gt::cols_align(
        align = "center",
        columns = c("Time", "At risk", "Events", "Censored", "Survival probability (95% CI)")
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
  ft <- flextable::set_caption(ft, caption = "Kaplan-Meier survival probabilities")
  ft <- flextable::align(ft, j = "Group", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("Time", "At risk", "Events", "Censored", "Survival probability (95% CI)"),
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
