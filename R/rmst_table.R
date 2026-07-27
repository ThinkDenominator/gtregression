#' Restricted mean survival time table
#'
#' Estimate restricted mean survival time (RMST) up to a user-specified follow-up
#' time. RMST is the average survival time observed within a fixed time window,
#' such as 365 days.
#'
#' @param data A \code{data.frame} containing survival time and event status.
#' @param time Survival follow-up time. Quoted and bare names are accepted.
#' @param event Event indicator. Quoted and bare names are accepted. Numeric
#'   \code{0/1}, numeric \code{1/2}, logical, character, and factor variables
#'   are accepted. For two-level character or factor variables, the second level
#'   is treated as the event.
#' @param by Optional grouping variable for separate RMST estimates. Quoted and
#'   bare names are accepted.
#' @param tau Restriction time for RMST. For example, \code{tau = 365} reports
#'   mean survival time restricted to the first 365 days of follow-up.
#' @param digits Number of digits for displayed survival time summaries.
#' @param conf.level Confidence level for RMST confidence intervals.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","rmst_table", ...)} with
#' elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with numeric RMST summaries.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{fit}}{Fitted \code{survfit} object.}
#'   \item{\code{time,event,by,tau,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' RMST is estimated from \code{survival::survfit()} using Kaplan-Meier methods
#' and \code{summary.survfit(rmean = tau)}. When \code{by} has exactly two
#' groups, the table includes the RMST difference as the second group minus the
#' first group, with a Wald confidence interval and p-value based on the
#' reported RMST standard errors.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' rmst_table(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   by = trt,
#'   tau = 365
#' )
#'
#' rmst_table(
#'   data = lung_data,
#'   time = "time",
#'   event = "status",
#'   tau = 180,
#'   format = tibble
#' )
#'
#' @importFrom survival survfit
#' @importFrom stats complete.cases pnorm qnorm
#' @export
rmst_table <- function(data,
                       time,
                       event,
                       by = NULL,
                       tau,
                       digits = 1,
                       conf.level = 0.95,
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

  data_clean <- .validate_rmst_inputs(
    data = data,
    time = time,
    event = event,
    by = by,
    tau = tau,
    digits = digits,
    conf.level = conf.level
  )

  if (!is.null(by)) {
    data_clean[[by]] <- factor(data_clean[[by]])
  }

  fit <- survival::survfit(.km_formula(time, event, by), data = data_clean)
  sm <- summary(fit, rmean = tau)
  table_body <- .rmst_table_body(sm, by = by, tau = tau, conf.level = conf.level)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .rmst_table_display(table_body, digits = digits)
  tbl <- .build_rmst_table(
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
    tau = tau,
    conf.level = conf.level,
    format = format,
    source = "rmst_table"
  )

  class(res) <- c("gtregression", "rmst_table", class(res))
  res
}

#' @keywords internal
#' @noRd
.validate_rmst_inputs <- function(data, time, event, by, tau, digits, conf.level) {
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
  if (missing(tau) || !is.numeric(tau) || length(tau) != 1L ||
      is.na(tau) || tau <= 0) {
    stop("`tau` must be a single positive follow-up time.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1) {
    stop("`conf.level` must be a number between 0 and 1.", call. = FALSE)
  }

  vars_needed <- unique(c(time, event, by))
  data_clean <- data[stats::complete.cases(data[, vars_needed, drop = FALSE]), , drop = FALSE]
  if (nrow(data_clean) == 0) {
    stop("No complete cases available for RMST estimation.", call. = FALSE)
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
.rmst_table_body <- function(sm, by = NULL, tau, conf.level = 0.95) {
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

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  rmst <- as.numeric(tab$rmean)
  se <- as.numeric(tab[["se(rmean)"]])

  out <- data.frame(
    Type = "Group",
    Group = group,
    Tau = tau,
    N = as.numeric(tab$records),
    Events = as.numeric(tab$events),
    RMST = rmst,
    SE = se,
    CI.lower = rmst - z * se,
    CI.upper = rmst + z * se,
    Difference = NA_real_,
    Difference.SE = NA_real_,
    Difference.CI.lower = NA_real_,
    Difference.CI.upper = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )

  if (!is.null(by) && nrow(out) == 2L) {
    diff <- out$RMST[2] - out$RMST[1]
    diff_se <- sqrt(sum(out$SE^2))
    diff_p <- if (diff_se > 0) {
      2 * stats::pnorm(abs(diff / diff_se), lower.tail = FALSE)
    } else {
      NA_real_
    }

    out <- rbind(
      out,
      data.frame(
        Type = "Difference",
        Group = paste0("Difference (", out$Group[2], " - ", out$Group[1], ")"),
        Tau = tau,
        N = NA_real_,
        Events = NA_real_,
        RMST = NA_real_,
        SE = diff_se,
        CI.lower = NA_real_,
        CI.upper = NA_real_,
        Difference = diff,
        Difference.SE = diff_se,
        Difference.CI.lower = diff - z * diff_se,
        Difference.CI.upper = diff + z * diff_se,
        p.value = diff_p,
        stringsAsFactors = FALSE
      )
    )
  }

  out
}

#' @keywords internal
#' @noRd
.rmst_table_display <- function(table_body, digits = 1) {
  fmt_num <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, digits = digits, format = "f", big.mark = ",")
    )
  }
  fmt_p <- function(x) {
    ifelse(
      is.na(x),
      "",
      ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format = "f"))
    )
  }

  rmst_txt <- ifelse(
    is.na(table_body$RMST),
    "",
    paste0(
      fmt_num(table_body$RMST),
      " (",
      fmt_num(table_body$CI.lower),
      "-",
      fmt_num(table_body$CI.upper),
      ")"
    )
  )

  diff_txt <- ifelse(
    is.na(table_body$Difference),
    "",
    paste0(
      fmt_num(table_body$Difference),
      " (",
      fmt_num(table_body$Difference.CI.lower),
      "-",
      fmt_num(table_body$Difference.CI.upper),
      ")"
    )
  )

  data.frame(
    Group = table_body$Group,
    Tau = fmt_num(table_body$Tau),
    N = ifelse(is.na(table_body$N), "", as.character(table_body$N)),
    Events = ifelse(is.na(table_body$Events), "", as.character(table_body$Events)),
    `RMST (95% CI)` = rmst_txt,
    `RMST difference (95% CI)` = diff_txt,
    `p-value` = fmt_p(table_body$p.value),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
.build_rmst_table <- function(display,
                              format = c("flextable", "gt"),
                              theme = c("minimal")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "RMST is restricted mean survival time up to tau.",
    "For two groups, the difference is the second group minus the first group."
  )

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Restricted mean survival time") |>
      gt::cols_align(align = "left", columns = "Group") |>
      gt::cols_align(
        align = "center",
        columns = c("Tau", "N", "Events", "RMST (95% CI)", "RMST difference (95% CI)", "p-value")
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
  ft <- flextable::set_caption(ft, caption = "Restricted mean survival time")
  ft <- flextable::align(ft, j = "Group", align = "left", part = "all")
  ft <- flextable::align(
    ft,
    j = c("Tau", "N", "Events", "RMST (95% CI)", "RMST difference (95% CI)", "p-value"),
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
