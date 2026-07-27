#' Log-rank test for Kaplan-Meier survival curves
#'
#' Compare survival curves between groups using the log-rank test.
#'
#' @param data A \code{data.frame} containing survival time, event status, and
#'   grouping variable.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Grouping variable used to compare survival curves. Quoted and bare
#'   names are accepted.
#' @param digits Number of digits for the chi-square statistic and expected
#'   events.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","logrank_test", ...)} with
#' elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with group-level log-rank components and
#'   overall test statistics.}
#'   \item{\code{test}}{One-row tibble with chi-square statistic, degrees of
#'   freedom, and p-value.}
#'   \item{\code{fit}}{The \code{survdiff} object.}
#'   \item{\code{time,event,by,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' The log-rank test compares the observed number of events with the expected
#' number of events in each group under the null hypothesis that the survival
#' curves are the same. It is a group comparison, not an effect-size estimate;
#' use \code{cox_reg()} when a hazard ratio is needed.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' logrank_test(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt
#' )
#'
#' logrank_test(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   by = "trt",
#'   format = tibble
#' )
#'
#' @importFrom survival survdiff
#' @importFrom stats pchisq complete.cases
#' @export
logrank_test <- function(data,
                         time,
                         event,
                         by,
                         digits = 2,
                         format = c("flextable", "gt", "tibble"),
                         theme = c("minimal")) {

  time <- .cox_single_var_arg(substitute(time), data = data, env = parent.frame())
  event <- .cox_single_var_arg(substitute(event), data = data, env = parent.frame())
  by <- .cox_single_var_arg(substitute(by), data = data, env = parent.frame())
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format, c("flextable", "gt", "tibble"))
  theme <- .resolve_theme(theme)

  data_clean <- .validate_logrank_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    digits = digits
  )
  data_clean[[by]] <- factor(data_clean[[by]])

  fit <- survival::survdiff(.km_formula(time, event, by), data = data_clean)
  test <- .logrank_test_summary(fit)
  table_body <- .logrank_table_body(fit, test)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .logrank_table_display(table_body, digits = digits)
  tbl <- .build_logrank_table(
    table_display,
    test = test,
    digits = digits,
    format = format,
    theme = theme
  )

  res <- list(
    table = tbl,
    table_body = tibble::as_tibble(table_body),
    table_display = table_display,
    test = tibble::as_tibble(test),
    fit = fit,
    time = time,
    event = event,
    by = by,
    format = format,
    source = "logrank_test"
  )

  class(res) <- c("gtregression", "logrank_test", class(res))
  res
}

#' @keywords internal
#' @noRd
.validate_logrank_inputs <- function(data, time, event, by, digits) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1L || !time %in% names(data)) {
    stop("`time` must be a single survival time variable in `data`.", call. = FALSE)
  }
  if (!is.character(event) || length(event) != 1L || !event %in% names(data)) {
    stop("`event` must be a single event indicator variable in `data`.", call. = FALSE)
  }
  if (!is.character(by) || length(by) != 1L || !by %in% names(data)) {
    stop("`by` must be a single grouping variable in `data`.", call. = FALSE)
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
    stop("No complete cases available for log-rank test.", call. = FALSE)
  }

  data_clean[[event]] <- .cox_event01(data_clean[[event]])
  if (sum(data_clean[[event]] == 1) == 0) {
    stop("`event` must include at least one event.", call. = FALSE)
  }
  if (length(unique(data_clean[[by]])) < 2L) {
    stop("`by` must contain at least two non-missing groups.", call. = FALSE)
  }

  data_clean
}

#' @keywords internal
#' @noRd
.logrank_test_summary <- function(fit) {
  chisq <- as.numeric(fit$chisq)
  df <- length(fit$n) - 1L
  data.frame(
    Chi.square = chisq,
    df = df,
    p.value = stats::pchisq(chisq, df = df, lower.tail = FALSE),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.logrank_table_body <- function(fit, test) {
  data.frame(
    Group = sub("^.*=", "", names(fit$n)),
    N = as.numeric(fit$n),
    Observed = as.numeric(fit$obs),
    Expected = as.numeric(fit$exp),
    Chi.square = test$Chi.square,
    df = test$df,
    p.value = test$p.value,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.logrank_table_display <- function(table_body, digits = 2) {
  data.frame(
    Group = table_body$Group,
    N = as.character(table_body$N),
    `Observed events` = as.character(table_body$Observed),
    `Expected events` = formatC(table_body$Expected, digits = digits, format = "f"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_logrank_table <- function(display,
                                 test,
                                 digits = 2,
                                 format = c("flextable", "gt"),
                                 theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  p_txt <- if (is.na(test$p.value)) {
    "NA"
  } else if (test$p.value < 0.001) {
    "<0.001"
  } else {
    formatC(test$p.value, digits = 3, format = "f")
  }
  note <- paste0(
    "Log-rank test: chi-square = ",
    formatC(test$Chi.square, digits = digits, format = "f"),
    ", df = ",
    test$df,
    ", p-value = ",
    p_txt,
    ". This compares survival curves; use cox_reg() when a hazard ratio is needed."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Log-rank test") |>
      gt::cols_align(align = "left", columns = "Group") |>
      gt::cols_align(
        align = "center",
        columns = c("N", "Observed events", "Expected events")
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
  ft <- flextable::set_caption(ft, caption = "Log-rank test")
  ft <- flextable::align(ft, j = "Group", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("N", "Observed events", "Expected events"),
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
