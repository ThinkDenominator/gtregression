#' Kaplan-Meier survival summary table
#'
#' Create a publication-ready Kaplan-Meier summary table with total N, events,
#' censored observations, and median survival with 95% confidence interval.
#'
#' @param data A \code{data.frame} containing survival time and event status.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for separate Kaplan-Meier summaries.
#'   Quoted and bare names are accepted.
#' @param digits Number of digits for survival time summaries.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","survival_summary", ...)}
#' with elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with numeric Kaplan-Meier summaries.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{fit}}{Fitted \code{survfit} object.}
#'   \item{\code{time,event,by,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' Median survival is estimated from \code{survival::survfit()}. If the median
#' survival time is not reached during follow-up, the display table shows
#' \code{"Not reached"}.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' survival_summary(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt
#' )
#'
#' survival_summary(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   format = tibble
#' )
#'
#' @importFrom survival Surv survfit
#' @export
survival_summary <- function(data,
                             time,
                             event,
                             by = NULL,
                             digits = 1,
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

  data_clean <- .validate_survival_summary_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    digits = digits
  )

  if (!is.null(by)) {
    data_clean[[by]] <- factor(data_clean[[by]])
  }

  fml <- .km_formula(time, event, by)
  fit <- survival::survfit(fml, data = data_clean)
  table_body <- .survival_summary_table_body(fit, by = by)
  table_display <- .survival_summary_display(table_body, digits = digits)

  tbl <- NULL
  if (format != "tibble") {
    tbl <- .build_survival_summary_table(
      table_display,
      format = format,
      theme = theme
    )
  }

  res <- list(
    table = tbl,
    table_body = tibble::as_tibble(table_body),
    table_display = table_display,
    fit = fit,
    time = time,
    event = event,
    by = by,
    format = format,
    source = "survival_summary"
  )

  class(res) <- c("gtregression", "survival_summary", class(res))
  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }
  res
}

#' @keywords internal
#' @noRd
.validate_survival_summary_inputs <- function(data, time, event, by, digits) {
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
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }

  vars_needed <- unique(c(time, event, by))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  if (nrow(data_clean) == 0) {
    stop("No complete cases available for survival summary.", call. = FALSE)
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
.survival_summary_table_body <- function(fit, by = NULL) {
  sm <- summary(fit)
  tab <- as.data.frame(sm$table)
  if (is.null(dim(sm$table))) {
    tab <- as.data.frame(t(sm$table))
    rownames(tab) <- "Overall"
  }

  group <- if (is.null(by)) {
    "Overall"
  } else {
    sub("^.*=", "", rownames(tab))
  }

  records <- as.numeric(tab$records)
  events <- as.numeric(tab$events)

  data.frame(
    Group = group,
    N = records,
    Events = events,
    Censored = records - events,
    Median = as.numeric(tab$median),
    CI.lower = as.numeric(tab[["0.95LCL"]]),
    CI.upper = as.numeric(tab[["0.95UCL"]]),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.survival_summary_display <- function(table_body, digits = 1) {
  fmt_num <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, digits = digits, format = "f", big.mark = ",")
    )
  }

  median_txt <- ifelse(
    is.na(table_body$Median),
    "Not reached",
    paste0(
      fmt_num(table_body$Median),
      " (",
      ifelse(is.na(table_body$CI.lower), "NA", fmt_num(table_body$CI.lower)),
      "-",
      ifelse(is.na(table_body$CI.upper), "NA", fmt_num(table_body$CI.upper)),
      ")"
    )
  )

  data.frame(
    Group = table_body$Group,
    N = as.character(table_body$N),
    Events = as.character(table_body$Events),
    Censored = as.character(table_body$Censored),
    `Median survival (95% CI)` = median_txt,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_survival_summary_table <- function(display,
                                          format = c("flextable", "gt"),
                                          theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Median survival is estimated using Kaplan-Meier methods.",
    "Not reached means survival did not fall to 50% during observed follow-up."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Kaplan-Meier survival summary") |>
      gt::cols_align(align = "left", columns = "Group") |>
      gt::cols_align(
        align = "center",
        columns = c("N", "Events", "Censored", "Median survival (95% CI)")
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
  ft <- flextable::set_caption(ft, caption = "Kaplan-Meier survival summary")
  ft <- flextable::align(ft, j = "Group", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("N", "Events", "Censored", "Median survival (95% CI)"),
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
