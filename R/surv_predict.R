#' Predict survival probabilities from a parametric survival model
#'
#' Estimate model-based survival probabilities at user-specified follow-up
#' times from a fitted \code{survival::survreg()} model.
#'
#' @param model A fitted \code{survreg} model, or a \code{surv_reg()} object
#'   containing exactly one fitted model.
#' @param newdata Optional \code{data.frame} of profiles for prediction. If
#'   \code{NULL}, one typical profile is built from the model data using medians
#'   for numeric variables and the most common level for categorical variables.
#' @param times Numeric vector of follow-up times at which survival probability
#'   should be predicted.
#' @param digits Number of digits for displayed follow-up times and
#'   probabilities.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#' @param theme Table styling preset.
#'
#' @return A list of class \code{c("gtregression","surv_predict", ...)}
#' with elements:
#' \describe{
#'   \item{\code{table}}{A \code{flextable}, \code{gt_tbl}, or \code{NULL}
#'   when \code{format = "tibble"}.}
#'   \item{\code{table_body}}{Tibble with predicted survival probabilities.}
#'   \item{\code{table_display}}{Display data frame used to render the table.}
#'   \item{\code{model}}{The fitted \code{survreg} model used for prediction.}
#'   \item{\code{newdata,times,distribution,format,source}}{Metadata fields.}
#' }
#'
#' @details
#' \code{surv_predict()} is for parametric survival regression models fitted by
#' \code{surv_reg()} or \code{survival::survreg()}. It is not a Kaplan-Meier
#' estimate and it is not a Cox prediction helper.
#'
#' Supported distributions are \code{"weibull"}, \code{"exponential"},
#' \code{"lognormal"}, and \code{"loglogistic"}, matching
#' \code{surv_reg()}. Predictions are conditional on the supplied profile and
#' the chosen parametric distribution.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' fit <- surv_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = "trt",
#'   adjust_for = c("age", "karno"),
#'   distribution = weibull
#' )
#'
#' surv_predict(
#'   model = fit$models$trt,
#'   newdata = data.frame(
#'     trt = factor("Test", levels = levels(lung_data$trt)),
#'     age = 60,
#'     karno = 70
#'   ),
#'   times = c(90, 180, 365)
#' )
#'
#' surv_predict(
#'   model = fit,
#'   times = c(90, 180),
#'   format = tibble
#' )
#'
#' @importFrom stats median predict terms delete.response model.frame
#' @export
surv_predict <- function(model,
                         newdata = NULL,
                         times,
                         digits = 1,
                         format = c("flextable", "gt", "tibble"),
                         theme = c("minimal")) {

  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  theme <- .choice_arg(substitute(theme), env = parent.frame())

  format <- match.arg(format, c("flextable", "gt", "tibble"))
  theme <- .resolve_theme(theme)

  fit <- .surv_predict_model(model)
  .validate_surv_predict_inputs(fit, newdata = newdata, times = times, digits = digits)

  if (is.null(newdata)) {
    newdata <- .surv_predict_typical_newdata(fit)
  }

  table_body <- .surv_predict_table_body(fit, newdata = newdata, times = times)

  if (format == "tibble") {
    return(tibble::as_tibble(table_body))
  }

  table_display <- .surv_predict_display(table_body, digits = digits)
  tbl <- .build_surv_predict_table(
    table_display,
    format = format,
    theme = theme,
    distribution = fit$dist
  )

  res <- list(
    table = tbl,
    table_body = tibble::as_tibble(table_body),
    table_display = table_display,
    model = fit,
    newdata = newdata,
    times = times,
    distribution = fit$dist,
    format = format,
    source = "surv_predict"
  )

  class(res) <- c("gtregression", "surv_predict", class(res))
  res
}

#' @keywords internal
#' @noRd
.surv_predict_model <- function(model) {
  if (inherits(model, "survreg")) {
    return(model)
  }

  if (inherits(model, "surv_reg")) {
    if (is.null(model$models) || length(model$models) != 1L) {
      stop(
        "`model` is a surv_reg() object with multiple fitted models. ",
        "Pass one model, for example `model$models$trt`.",
        call. = FALSE
      )
    }
    return(model$models[[1L]])
  }

  stop("`model` must be a survreg model or a single-model surv_reg() object.", call. = FALSE)
}

#' @keywords internal
#' @noRd
.validate_surv_predict_inputs <- function(model, newdata, times, digits) {
  supported <- c("weibull", "exponential", "lognormal", "loglogistic")
  if (is.null(model$dist) || !model$dist %in% supported) {
    stop(
      "`model` must use one of these survreg distributions: ",
      paste(supported, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(newdata) && !is.data.frame(newdata)) {
    stop("`newdata` must be NULL or a data.frame.", call. = FALSE)
  }
  if (missing(times) || !is.numeric(times) || length(times) < 1L ||
      anyNA(times) || any(times < 0)) {
    stop("`times` must be a numeric vector of non-negative follow-up times.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("`digits` must be a non-negative whole number.", call. = FALSE)
  }
  if (is.null(newdata) && is.null(model$model)) {
    stop("`model` must be fitted with `model = TRUE` to support default profiles.", call. = FALSE)
  }

  if (!is.null(newdata)) {
    tryCatch(
      stats::predict(model, newdata = newdata, type = "lp"),
      error = function(e) {
        stop("`newdata` is not compatible with `model`: ", e$message, call. = FALSE)
      }
    )
  }

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.surv_predict_typical_newdata <- function(model) {
  mf <- model$model
  term_labels <- attr(stats::terms(model), "term.labels")
  vars <- unique(gsub("`", "", term_labels, fixed = TRUE))
  vars <- vars[vars %in% names(mf)]

  if (!length(vars)) {
    return(data.frame(.profile = "Overall", stringsAsFactors = FALSE))
  }

  out <- data.frame(.profile = "Typical profile", stringsAsFactors = FALSE)
  for (var in vars) {
    out[[var]] <- .surv_predict_typical_value(mf[[var]])
  }
  out
}

#' @keywords internal
#' @noRd
.surv_predict_typical_value <- function(x) {
  if (is.numeric(x)) {
    return(stats::median(x, na.rm = TRUE))
  }
  if (is.factor(x)) {
    tab <- table(x)
    val <- names(tab)[which.max(tab)]
    return(factor(val, levels = levels(x), ordered = is.ordered(x)))
  }
  if (is.logical(x)) {
    tab <- table(x)
    return(as.logical(names(tab)[which.max(tab)]))
  }
  tab <- table(x)
  names(tab)[which.max(tab)]
}

#' @keywords internal
#' @noRd
.surv_predict_table_body <- function(model, newdata, times) {
  lp <- as.numeric(stats::predict(model, newdata = newdata, type = "lp"))
  profiles <- seq_len(nrow(newdata))
  grid <- expand.grid(
    Profile = profiles,
    Time = times,
    KEEP.OUT.ATTRS = FALSE
  )

  eta <- lp[grid$Profile]
  surv <- .surv_predict_probability(
    time = grid$Time,
    eta = eta,
    scale = model$scale,
    distribution = model$dist
  )

  out <- data.frame(
    Profile = grid$Profile,
    Time = as.numeric(grid$Time),
    Survival.probability = as.numeric(surv),
    Distribution = model$dist,
    stringsAsFactors = FALSE
  )

  profile_cols <- newdata[grid$Profile, , drop = FALSE]
  rownames(profile_cols) <- NULL
  cbind(
    out[, "Profile", drop = FALSE],
    profile_cols,
    out[, c("Time", "Survival.probability", "Distribution"), drop = FALSE]
  )
}

#' @keywords internal
#' @noRd
.surv_predict_probability <- function(time, eta, scale, distribution) {
  out <- rep(NA_real_, length(time))
  zero <- time == 0
  out[zero] <- 1

  idx <- !zero
  z <- (log(time[idx]) - eta[idx]) / scale

  out[idx] <- switch(
    distribution,
    weibull = exp(-exp(z)),
    exponential = exp(-exp(z)),
    lognormal = stats::pnorm(z, lower.tail = FALSE),
    loglogistic = 1 / (1 + exp(z))
  )
  pmin(pmax(out, 0), 1)
}

#' @keywords internal
#' @noRd
.surv_predict_display <- function(table_body, digits = 1) {
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

  out <- table_body
  out$Time <- fmt_time(out$Time)
  out$Survival.probability <- fmt_pct(out$Survival.probability)
  names(out)[names(out) == "Survival.probability"] <- "Predicted survival"
  names(out)[names(out) == "Distribution"] <- "Model distribution"
  out
}

#' @keywords internal
#' @noRd
.build_surv_predict_table <- function(display,
                                      format = c("flextable", "gt"),
                                      theme = c("minimal"),
                                      distribution = NULL) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Model-based predictions from a parametric survival regression model.",
    if (!is.null(distribution)) paste0("Distribution: ", distribution, ".") else NULL,
    "Predictions depend on the supplied profile and model specification."
  )

  center_cols <- setdiff(names(display), names(display)[names(display) %in% c("Profile", ".profile")])

  if (format == "gt") {
    tbl <- gt::gt(display) |>
      gt::tab_header(title = "Predicted survival probabilities") |>
      gt::cols_align(align = "center", columns = center_cols) |>
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
  ft <- flextable::set_caption(ft, caption = "Predicted survival probabilities")
  ft <- flextable::align(ft, j = center_cols, align = "center", part = "all")
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
