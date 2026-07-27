#' Check proportional hazards assumption for Cox models
#'
#' Test the proportional hazards assumption for fitted Cox proportional hazards
#' models using Schoenfeld residuals.
#'
#' @param model A fitted \code{coxph} model or a \code{cox_reg()} object.
#' @param transform Time transformation passed to \code{survival::cox.zph()}.
#'   One of \code{"km"}, \code{"rank"}, or \code{"identity"}.
#' @param alpha Significance level used for the simple interpretation column.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}.
#'
#' @return A tibble, \code{gt_tbl}, or \code{flextable} with Schoenfeld
#' residual proportional hazards tests. The table contains:
#' \describe{
#'   \item{\code{Model}}{Model name. For a direct \code{coxph} object, this is
#'   \code{"cox_model"}.}
#'   \item{\code{Term}}{Model term or \code{"GLOBAL"}.}
#'   \item{\code{Chi.square}}{Chi-square statistic.}
#'   \item{\code{df}}{Degrees of freedom.}
#'   \item{\code{p.value}}{Test p-value.}
#'   \item{\code{Interpretation}}{Simple screening interpretation using
#'   \code{alpha}.}
#' }
#'
#' @details
#' \code{check_ph()} is a diagnostic aid for Cox models. A small p-value
#' suggests possible evidence against the proportional hazards assumption for a
#' term or for the global model test. This should be interpreted with plots,
#' clinical knowledge, follow-up patterns, and modelling purpose.
#'
#' @examples
#' lung_data <- data_lungcancer
#' lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
#'                         labels = c("Standard", "Test"))
#'
#' cox_fit <- cox_reg(
#'   data = lung_data,
#'   time = time,
#'   event = status,
#'   exposures = c(trt, celltype, age)
#' )
#'
#' check_ph(cox_fit)
#' check_ph(cox_fit, format = tibble)
#'
#' @importFrom survival cox.zph
#' @export
check_ph <- function(model,
                     transform = c("km", "rank", "identity"),
                     alpha = 0.05,
                     format = c("flextable", "gt", "tibble")) {

  transform <- .choice_arg(
    substitute(transform),
    env = parent.frame(),
    choices = c("km", "rank", "identity")
  )
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )

  transform <- match.arg(transform, c("km", "rank", "identity"))
  format <- match.arg(format, c("flextable", "gt", "tibble"))

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a number between 0 and 1.", call. = FALSE)
  }

  model_list <- .check_ph_model_list(model)
  out <- do.call(
    rbind,
    lapply(names(model_list), function(nm) {
      .check_ph_one_model(model_list[[nm]], model_name = nm,
                          transform = transform, alpha = alpha)
    })
  )
  rownames(out) <- NULL

  if (format == "tibble") {
    return(tibble::as_tibble(out))
  }

  .build_check_ph_table(out, format = format, alpha = alpha,
                        transform = transform)
}

#' @keywords internal
#' @noRd
.check_ph_model_list <- function(model) {
  if (inherits(model, "coxph")) {
    return(list(cox_model = model))
  }

  if (!inherits(model, "gtregression") || !identical(model$source, "cox_reg")) {
    stop("`model` must be a fitted coxph model or cox_reg() object.", call. = FALSE)
  }

  if (is.null(model$models) || !length(model$models)) {
    stop("No Cox models found in `model`.", call. = FALSE)
  }

  if (!all(vapply(model$models, inherits, logical(1), what = "coxph"))) {
    stop("All models must be fitted coxph objects.", call. = FALSE)
  }

  model$models
}

#' @keywords internal
#' @noRd
.check_ph_one_model <- function(fit, model_name, transform, alpha) {
  zph <- tryCatch(
    survival::cox.zph(fit, transform = transform),
    error = function(e) {
      stop(
        "Could not compute proportional hazards test for model '",
        model_name,
        "': ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  tab <- as.data.frame(zph$table)
  tab$Term <- rownames(tab)
  rownames(tab) <- NULL

  chisq_col <- intersect(c("chisq", "Chisq"), names(tab))[1]
  df_col <- intersect(c("df", "Df"), names(tab))[1]
  p_col <- intersect(c("p", "P"), names(tab))[1]

  out <- data.frame(
    Model = model_name,
    Term = tab$Term,
    Chi.square = as.numeric(tab[[chisq_col]]),
    df = as.numeric(tab[[df_col]]),
    p.value = as.numeric(tab[[p_col]]),
    stringsAsFactors = FALSE
  )

  out$Test <- ifelse(out$Term == "GLOBAL", "Global", "Term")
  out$Interpretation <- ifelse(
    is.na(out$p.value),
    NA_character_,
    ifelse(
      out$p.value < alpha,
      "Possible PH violation",
      "No evidence of PH violation"
    )
  )

  out[, c("Model", "Term", "Test", "Chi.square", "df", "p.value", "Interpretation")]
}

#' @keywords internal
#' @noRd
.build_check_ph_table <- function(result,
                                  format = c("flextable", "gt"),
                                  alpha = 0.05,
                                  transform = "km") {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste0(
    "Screening aid only. Small p-values suggest possible non-proportional ",
    "hazards; interpret with Schoenfeld residual plots, follow-up pattern, ",
    "clinical context, and model purpose. alpha = ", alpha,
    "; transform = ", transform, "."
  )

  display <- result |>
    dplyr::mutate(
      Chi.square = dplyr::if_else(
        is.na(.data$Chi.square),
        "",
        formatC(.data$Chi.square, digits = 2, format = "f")
      ),
      df = dplyr::if_else(
        is.na(.data$df),
        "",
        formatC(.data$df, digits = 0, format = "f")
      ),
      p.value = dplyr::case_when(
        is.na(.data$p.value) ~ "",
        .data$p.value < 0.001 ~ "<0.001",
        TRUE ~ formatC(.data$p.value, digits = 3, format = "f")
      )
    )

  if (format == "gt") {
    return(
      gt::gt(display) |>
        gt::tab_header(title = "Proportional hazards check") |>
        gt::cols_label(
          Chi.square = "Chi-square",
          p.value = "p-value"
        ) |>
        gt::cols_align(align = "left", columns = c("Model", "Term", "Test", "Interpretation")) |>
        gt::cols_align(align = "center", columns = c("Chi.square", "df", "p.value")) |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#e7f5ec"),
          locations = gt::cells_body(rows = .data$Interpretation == "No evidence of PH violation")
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fde2e2"),
          locations = gt::cells_body(rows = .data$Interpretation == "Possible PH violation")
        ) |>
        gt::tab_source_note(gt::md(note)) |>
        .compact_gt_source_notes()
    )
  }

  ft <- flextable::flextable(display)
  ft <- flextable::set_caption(ft, caption = "Proportional hazards check")
  ft <- flextable::set_header_labels(
    ft,
    Chi.square = "Chi-square",
    p.value = "p-value"
  )
  ft <- flextable::align(
    ft,
    j = c("Model", "Term", "Test", "Interpretation"),
    align = "left",
    part = "all"
  )
  ft <- flextable::align(
    ft,
    j = c("Chi.square", "df", "p.value"),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::bg(
    ft,
    i = which(display$Interpretation == "No evidence of PH violation"),
    bg = "#e7f5ec",
    part = "body"
  )
  ft <- flextable::bg(
    ft,
    i = which(display$Interpretation == "Possible PH violation"),
    bg = "#fde2e2",
    part = "body"
  )
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- .compact_flex_footer(ft)
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
