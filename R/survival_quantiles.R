#' Kaplan-Meier survival quantile table
#'
#' Estimate Kaplan-Meier survival time quantiles, such as the 25th percentile,
#' median, and 75th percentile survival times.
#'
#' @param data A \code{data.frame} containing survival time and event status.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for separate Kaplan-Meier quantiles.
#'   Quoted and bare names are accepted.
#' @param probs Numeric vector of event-time quantiles to estimate. The default
#'   \code{c(0.25, 0.5, 0.75)} reports the 25th percentile, median, and 75th
#'   percentile event times.
#' @param digits Number of digits for survival time summaries.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","survival_quantiles", ...)}
#' with elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with Kaplan-Meier quantiles.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{fit}}{Fitted \code{survfit} object.}
#'   \item{\code{time,event,by,probs,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' A probability of \code{0.50} is the median event time: the estimated time by
#' which 50% of participants have had the event, corresponding to 50% survival.
#' A probability of \code{0.25} is the time by which 25% have had the event,
#' corresponding to 75% survival. If a quantile is not reached during observed
#' follow-up, the display table shows \code{"Not reached"}.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' survival_quantiles(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt
#' )
#'
#' survival_quantiles(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   probs = c(0.25, 0.5),
#'   format = tibble
#' )
#'
#' @importFrom survival survfit
#' @importFrom stats complete.cases quantile
#' @export
survival_quantiles <- function(data,
                               time,
                               event,
                               by = NULL,
                               probs = c(0.25, 0.5, 0.75),
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

  data_clean <- .validate_survival_quantiles_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    probs = probs,
    digits = digits
  )

  if (!is.null(by)) {
    data_clean[[by]] <- factor(data_clean[[by]])
  }

  fit <- survival::survfit(.km_formula(time, event, by), data = data_clean)
  q <- stats::quantile(fit, probs = probs)
  table_body <- .survival_quantiles_table_body(q, probs = probs, by = by)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .survival_quantiles_display(table_body, digits = digits)
  tbl <- .build_survival_quantiles_table(
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
    probs = probs,
    format = format,
    source = "survival_quantiles"
  )

  class(res) <- c("gtregression", "survival_quantiles", class(res))
  res
}

#' @keywords internal
#' @noRd
.validate_survival_quantiles_inputs <- function(data, time, event, by, probs, digits) {
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
  if (!is.numeric(probs) || length(probs) < 1L || anyNA(probs) ||
      any(probs <= 0 | probs >= 1)) {
    stop("`probs` must be a numeric vector with values between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }

  vars_needed <- unique(c(time, event, by))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  if (nrow(data_clean) == 0) {
    stop("No complete cases available for survival quantiles.", call. = FALSE)
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
.survival_quantiles_table_body <- function(q, probs, by = NULL) {
  if (is.null(dim(q$quantile))) {
    out <- data.frame(
      Group = "Overall",
      Probability = probs,
      Survival.probability = 1 - probs,
      Time = as.numeric(q$quantile),
      CI.lower = as.numeric(q$lower),
      CI.upper = as.numeric(q$upper),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  groups <- sub("^.*=", "", rownames(q$quantile))
  do.call(
    rbind,
    lapply(seq_along(groups), function(i) {
      data.frame(
        Group = groups[[i]],
        Probability = probs,
        Survival.probability = 1 - probs,
        Time = as.numeric(q$quantile[i, ]),
        CI.lower = as.numeric(q$lower[i, ]),
        CI.upper = as.numeric(q$upper[i, ]),
        stringsAsFactors = FALSE
      )
    })
  )
}

#' @keywords internal
#' @noRd
.survival_quantiles_display <- function(table_body, digits = 1) {
  pct <- function(x) paste0(formatC(100 * x, digits = 0, format = "f"), "%")
  fmt_num <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, digits = digits, format = "f", big.mark = ",")
    )
  }
  time_txt <- ifelse(
    is.na(table_body$Time),
    "Not reached",
    paste0(
      fmt_num(table_body$Time),
      " (",
      ifelse(is.na(table_body$CI.lower), "NA", fmt_num(table_body$CI.lower)),
      "-",
      ifelse(is.na(table_body$CI.upper), "NA", fmt_num(table_body$CI.upper)),
      ")"
    )
  )

  data.frame(
    Group = table_body$Group,
    `Event percentile` = pct(table_body$Probability),
    `Survival probability` = pct(table_body$Survival.probability),
    `Time (95% CI)` = time_txt,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_survival_quantiles_table <- function(display,
                                            format = c("flextable", "gt"),
                                            theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Kaplan-Meier quantiles estimate the time by which the event percentile has occurred.",
    "For example, the 50% event percentile is the median survival time."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Kaplan-Meier survival quantiles") |>
      gt::cols_align(align = "left", columns = "Group") |>
      gt::cols_align(
        align = "center",
        columns = c("Event percentile", "Survival probability", "Time (95% CI)")
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
  ft <- flextable::set_caption(ft, caption = "Kaplan-Meier survival quantiles")
  ft <- flextable::align(ft, j = "Group", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("Event percentile", "Survival probability", "Time (95% CI)"),
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
