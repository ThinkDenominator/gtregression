#' Kaplan-Meier risk table
#'
#' Tabulate the number at risk at selected follow-up times.
#'
#' @param data A \code{data.frame} containing survival time and event status.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for separate Kaplan-Meier risk tables.
#'   Quoted and bare names are accepted.
#' @param times Numeric vector of follow-up times for the risk table.
#' @param digits Number of digits for displayed follow-up times.
#' @param extend Logical. If \code{TRUE}, requested times beyond the observed
#'   follow-up range are retained using the last available Kaplan-Meier risk set.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","km_risk_table", ...)}
#' with elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with Kaplan-Meier risk table counts.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{fit}}{Fitted \code{survfit} object.}
#'   \item{\code{time,event,by,times,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' The \code{At risk} column gives the number still under observation at each
#' requested time. The \code{Events} and \code{Censored} columns are interval
#' counts up to each requested time point as returned by
#' \code{summary.survfit()}.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' km_risk_table(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt,
#'   times = c(0, 90, 180, 365)
#' )
#'
#' km_risk_table(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   times = c(0, 90, 180),
#'   format = tibble
#' )
#'
#' @importFrom survival survfit
#' @importFrom stats complete.cases
#' @export
km_risk_table <- function(data,
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
  table_body <- .km_risk_table_body(sm, by = by)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .km_risk_table_display(table_body, digits = digits)
  tbl <- .build_km_risk_table(
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
    source = "km_risk_table"
  )

  class(res) <- c("gtregression", "km_risk_table", class(res))
  res
}

#' @keywords internal
#' @noRd
.km_risk_table_body <- function(sm, by = NULL) {
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
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.km_risk_table_display <- function(table_body, digits = 1) {
  fmt_time <- function(x) {
    formatC(x, digits = digits, format = "f", big.mark = ",")
  }

  data.frame(
    Group = table_body$Group,
    Time = fmt_time(table_body$Time),
    `At risk` = as.character(table_body$N.risk),
    Events = as.character(table_body$Events),
    Censored = as.character(table_body$Censored),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_km_risk_table <- function(display,
                                 format = c("flextable", "gt"),
                                 theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "At risk is the number still under observation at the requested time.",
    "Events and censored counts are interval counts up to each requested time point."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Kaplan-Meier risk table") |>
      gt::cols_align(align = "left", columns = "Group") |>
      gt::cols_align(
        align = "center",
        columns = c("Time", "At risk", "Events", "Censored")
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
  ft <- flextable::set_caption(ft, caption = "Kaplan-Meier risk table")
  ft <- flextable::align(ft, j = "Group", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("Time", "At risk", "Events", "Censored"),
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
