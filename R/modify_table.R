#' Modify Regression Table Labels and Layout
#'
#' Easily customize variable labels, level labels, headers, and caption of regression tables.
#'
#' @param gt_table A gtsummary table (e.g., from `uni_reg()`, `multi_reg()`).
#' @param variable_labels A named vector to relabel variable names. Names should match variables in the model.
#' @param level_labels A named vector to relabel levels of categorical variables.
#' @param header_labels A named vector to relabel column headers. Names must match internal column names (e.g., `"estimate"`, `"p.value"`).
#' @param caption A character string to set the table caption (title).
#' @param bold_labels Logical. If `TRUE`, bold the variable labels. Default is `FALSE`.
#' @param bold_levels Logical. If `TRUE`, bold the levels of categorical variables. Default is `FALSE`.
#'
#' @return A customized `gtsummary` table object.
#'
#' @examples
#' \dontrun{
#' library(gtregression)
#' tbl_custom <- modify_table(
#'uni_rr,
#'variable_labels = c(age_cat = "Age", bmi = "BMI"),
#'level_labels = c(`Young` = "Young Adults", `Old` = "Older Adults"),
#'header_labels = c(estimate = "**Risk Ratio**", `p.value` = "***P*-value**"),
#'caption = "Table 1: Univariate Regression",
#'bold_labels = TRUE
#')
#' }
#'
#' @importFrom gtsummary modify_table_body modify_header modify_caption bold_labels bold_levels
#' @importFrom dplyr mutate case_when
#' @export
modify_table <- function(gt_table,
                         variable_labels = NULL,
                         level_labels = NULL,
                         header_labels = NULL,
                         caption = NULL,
                         bold_labels = FALSE,
                         bold_levels = FALSE) {
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop("Package 'gtsummary' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  tbl <- gt_table

  # 1. Variable labels
  if (!is.null(variable_labels)) {
    tbl <- gtsummary::modify_table_body(
      tbl,
      ~ dplyr::mutate(
        .x,
        label = dplyr::case_when(
          row_type == "label" & variable %in% names(variable_labels) ~ variable_labels[variable],
          TRUE ~ label
        )
      )
    )
  }

  # 2. Level labels
  if (!is.null(level_labels)) {
    tbl <- gtsummary::modify_table_body(
      tbl,
      ~ dplyr::mutate(
        .x,
        label = dplyr::case_when(
          row_type == "level" & label %in% names(level_labels) ~ level_labels[label],
          TRUE ~ label
        )
      )
    )
  }

  # 3. Header labels
  if (!is.null(header_labels)) {
    tbl <- gtsummary::modify_header(tbl, update = header_labels)
  }

  # 4. Caption
  if (!is.null(caption)) {
    tbl <- gtsummary::modify_caption(tbl, caption)
  }

  # 5. Bold labels
  if (isTRUE(bold_labels)) {
    tbl <- gtsummary::bold_labels(tbl)
  }

  # 6. Bold levels
  if (isTRUE(bold_levels)) {
    tbl <- gtsummary::bold_levels(tbl)
  }

  return(tbl)
}
