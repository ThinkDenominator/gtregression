#' Dissect a dataset before regression
#'
#' Returns a tidy summary of each variable's structure, missingness, uniqueness,
#' and suitability for use in regression models.
#'
#' @param data A data frame.
#' @param verbose Logical; if \code{TRUE}, print the summary and interpretation
#'   notes. The tibble is returned invisibly only when printed by the console.
#' @param format Output format. One of \code{"flextable"} (default),
#'   \code{"gt"}, or \code{"tibble"}. Use \code{format = "tibble"} for
#'   pipeline-friendly raw output.
#'
#' @return A tibble, \code{gt_tbl}, or \code{flextable}, depending on
#'   \code{format}. The tibble has columns: Variable, Type, Missing (%), Unique,
#'   Levels, Compatibility, and Hint.
#' @export
#'
#' @examples
#' dissect(data_birthwt)
#' dissect(data_birthwt, format = "gt")
#'
#' # Print interpretation notes for beginners
#' dissect(data_birthwt, verbose = TRUE)
dissect <- function(data, verbose = FALSE, format = c("flextable", "gt", "tibble")) {
  if (!is.data.frame(data)) {
    stop("The input to `dissect()` must be a data frame. ",
         "You provided an object of class: ", class(data)[1], call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt", "tibble")
  )
  format <- match.arg(format, c("flextable", "gt", "tibble"))


  summarize_column <- function(x, name) {
    x_non_na <- x[!is.na(x)]
    type <- class(x)[1]
    unique_vals <- length(unique(x_non_na))
    pct_missing <- round(mean(is.na(x)) * 100, 1)

    # Levels or sample values
    if (is.factor(x)) {
      lvls <- levels(x)
      levels_out <- paste(head(lvls, 5), collapse = ", ")
      if (length(lvls) > 5) levels_out <- paste0(levels_out, ", ...")
    } else if (is.character(x) || is.logical(x)) {
      lvls <- unique(x_non_na)
      levels_out <- paste(head(lvls, 5), collapse = ", ")
      if (length(lvls) > 5) levels_out <- paste0(levels_out, ", ...")
    } else {
      levels_out <- "-"
    }

    # Compatibility tag and beginner-friendly hint
    if (all(is.na(x))) {
      comp <- "incompatible"
      hint <- "All values are missing; do not use as-is."
    } else if (unique_vals == 1) {
      comp <- "incompatible"
      hint <- "Only one observed value; no variation for regression."
    } else if (is.numeric(x)) {
      if (unique_vals > 2) {
        comp <- "compatible"
        hint <- "Numeric variable can be used as continuous."
      } else {
        comp <- "maybe"
        hint <- "Two numeric values; confirm binary coding or convert to factor."
      }
    } else if (is.factor(x)) {
      if (unique_vals > 5) {
        comp <- "maybe"
        hint <- "Many factor levels; check sparse categories before modeling."
      } else {
        comp <- "compatible"
        hint <- "Factor variable can be used as categorical."
      }
    } else if (is.character(x)) {
      if (unique_vals > 5) {
        comp <- "maybe"
        hint <- "Character variable has many values; consider factor grouping."
      } else {
        comp <- "compatible"
        hint <- "Consider converting character values to factor before modeling."
      }
    } else if (inherits(x, "Date")) {
      comp <- "maybe"
      hint <- "Date variable usually needs transformation before modeling."
    } else if (is.logical(x)) {
      comp <- "compatible"
      hint <- "Logical variable can be used as binary."
    } else {
      comp <- "incompatible"
      hint <- "Unsupported variable type; transform before modeling."
    }

    tibble::tibble(
      Variable = name,
      Type = type,
      `Missing (%)` = paste0(pct_missing, "%"),
      Unique = unique_vals,
      Levels = levels_out,
      Compatibility = comp,
      Hint = hint
    )
  }

  result <- purrr::map_dfr(names(data), ~ summarize_column(data[[.x]], .x))

  if (isTRUE(verbose)) {
    print(result)

    message("\nInterpretation notes:\n",
        "- compatible: ready to use in regression or with minimal preparation\n",
        "- maybe: check coding, sparse levels, or whether transformation is needed\n",
        "- incompatible: not usable as-is (e.g., all NA or no variation)\n")
  }

  if (format == "tibble") {
    return(result)
  }

  .build_dissect_table(result, format = format)
}

#' Build formatted dissect table
#' @keywords internal
#' @noRd
.build_dissect_table <- function(result,
                                 format = c("flextable", "gt")) {
  format <- match.arg(format, c("flextable", "gt"))
  note <- paste(
    "Screening aid only; review coding, missingness, sparse levels,",
    "and study context before modeling."
  )

  if (format == "gt") {
    return(
      gt::gt(result) |>
        gt::tab_header(title = "Dataset dissection before regression") |>
        gt::cols_align(
          align = "left",
          columns = c("Variable", "Type", "Levels", "Compatibility", "Hint")
        ) |>
        gt::cols_align(
          align = "center",
          columns = c("Missing (%)", "Unique")
        ) |>
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#e7f5ec"),
          locations = gt::cells_body(
            columns = "Compatibility",
            rows = .data$Compatibility == "compatible"
          )
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fff4d6"),
          locations = gt::cells_body(
            columns = "Compatibility",
            rows = .data$Compatibility == "maybe"
          )
        ) |>
        gt::tab_style(
          style = gt::cell_fill(color = "#fde2e2"),
          locations = gt::cells_body(
            columns = "Compatibility",
            rows = .data$Compatibility == "incompatible"
          )
        ) |>
        gt::tab_source_note(gt::md(note))
    )
  }

  ft <- flextable::flextable(result)
  ft <- flextable::set_caption(ft, caption = "Dataset dissection before regression")
  ft <- flextable::align(
    ft,
    j = c("Variable", "Type", "Levels", "Compatibility", "Hint"),
    align = "left",
    part = "all"
  )
  ft <- flextable::align(
    ft,
    j = c("Missing (%)", "Unique"),
    align = "center",
    part = "all"
  )
  ft <- flextable::bold(ft, part = "header", bold = TRUE)

  ft <- flextable::bg(
    ft,
    i = which(result$Compatibility == "compatible"),
    j = "Compatibility",
    bg = "#e7f5ec",
    part = "body"
  )
  ft <- flextable::bg(
    ft,
    i = which(result$Compatibility == "maybe"),
    j = "Compatibility",
    bg = "#fff4d6",
    part = "body"
  )
  ft <- flextable::bg(
    ft,
    i = which(result$Compatibility == "incompatible"),
    j = "Compatibility",
    bg = "#fde2e2",
    part = "body"
  )
  ft <- flextable::add_footer_lines(ft, values = note)
  ft <- flextable::fontsize(ft, size = 8, part = "footer")
  ft <- flextable::italic(ft, italic = TRUE, part = "footer")
  flextable::autofit(ft)
}
