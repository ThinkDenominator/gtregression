#' Modify Regression Table Labels and Layout
#'
#' Allows customization of labels, headers, and layout of regression tables
#' created using `gtsummary`. Designed for tables from functions like
#' `uni_reg()`, `multi_reg()`, etc.
#' @param gt_table A `gtsummary` table object.
#' @param variable_labels A named vector for relabeling variable names.
#' @param level_labels A named list for relabeling levels of variables.
#'   Should be structured as `list(var1 = c(old1 = new1, old2 = new2), ...)`.
#' @param header_labels A named vector for relabeling column headers. Names
#'   should match internal column names (e.g., `"estimate"`, `"p.value"`).
#' @param caption A character string used to set the table title.
#' @param bold_labels Logical. If `TRUE`, bolds variable labels.
#' @param bold_levels Logical. If `TRUE`, bolds factor level labels.
#' @param remove_N Logical. If `TRUE`, hides the `N` column in univariate
#'   regression tables (`uni_reg`, `uni_reg_nbin`). Ignored for other tables.
#' @param remove_N_obs Logical. If `TRUE`, removes the source note showing the
#'   no of observations in multivariable models (`multi_reg`, `multi_reg_nbin`).
#' @param remove_abbreviations Logical. If `TRUE`, removes default footnotes
#'   for estimate abbreviations.
#' @param caveat A character string to add as a footnote (source note) below
#'   the table, e.g., "N may vary due to missing data."
#' @return A customized `gtsummary` table object with modified labels,
#' layout, and options.
#'
#' @examples
#' \dontrun{
#' library(gtregression)
#' tbl_custom <- modify_table(
#'   uni_rr,
#'   variable_labels = c(age_cat = "Age", bmi = "BMI"),
#'   level_labels = list(age_cat = c(`Young` = "Young Adults",
#'   `Old` = "Older Adults")),
#'   header_labels = c(estimate = "**Risk Ratio**",
#'   `p.value` = "***P*-value**"),
#'   caption = "Table 1: Univariate Regression",
#'   bold_labels = TRUE,
#'   remove_N = TRUE,
#'   caveat = "N may vary due to missing data."
#' )
#' }
#'
#' @importFrom gtsummary modify_table_body modify_header modify_caption
#' @importFrom gtsummary bold_labels bold_levels modify_column_hide
#' @importFrom gtsummary remove_source_note modify_source_note
#' @importFrom dplyr mutate case_when
#' @importFrom dplyr mutate case_when all_of
#' @export
modify_table <- function(gt_table,
                         variable_labels = NULL,
                         level_labels = NULL,
                         header_labels = NULL,
                         caption = NULL,
                         bold_labels = FALSE,
                         bold_levels = FALSE,
                         remove_N = FALSE,
                         remove_N_obs = FALSE,
                         remove_abbreviations = FALSE,
                         caveat = NULL) {
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("Package 'gtsummary' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required.")

  tbl <- gt_table

  # Detect source type or fallback for merged/stacked tables
  source_type <- attr(tbl, "source")
  if (inherits(tbl, "tbl_merge") || inherits(tbl, "tbl_stack")) {
    source_type <- "merged"
  }

  # 1. Edit Variable labels
  if (!is.null(variable_labels)) {
    tbl <- gtsummary::modify_table_body(tbl, ~ dplyr::mutate(.x,
      label = dplyr::case_when(
        row_type == "label" &
          variable %in% names(variable_labels) ~ variable_labels[variable],
        TRUE ~ label
      )
    ))
  }

  # 2. Edit Level labels - mapping by variable and level
  if (!is.null(level_labels)) {
    tbl <- gtsummary::modify_table_body(tbl, ~ dplyr::mutate(.x,
      label = dplyr::case_when(
        row_type == "level" &
          variable %in% names(level_labels) &
          label %in% unlist(purrr::map(level_labels, names)) ~
          purrr::map2_chr(variable, label, function(var, lev) {
            out <- tryCatch(
              {
                if (!is.null(level_labels[[var]]) &&
                    lev %in% names(level_labels[[var]])) {
                  level_labels[[var]][[lev]]
                } else {
                  lev
                }
              },
              error = function(e) lev
            )
            out
          }),
        TRUE ~ label
      )
    ))
  }

  # 3. Edit Header labels
  if (!is.null(header_labels)) {
    tbl <- gtsummary::modify_header(tbl, !!!header_labels)
  }

  # 4. Edit Caption
  if (!is.null(caption)) {
    tbl <- gtsummary::modify_caption(tbl, caption)
  }

  # 5. Bold variable labels if you're feeling fancy
  if (isTRUE(bold_labels)) {
    tbl <- gtsummary::bold_labels(tbl)
  }

  # 6. Bold factor levels if you're jobless
  if (isTRUE(bold_levels)) {
    tbl <- gtsummary::bold_levels(tbl)
  }

  # 7. Remove N columns for univariate or merged tables if you really want to
  if (isTRUE(remove_N)) {
    if (source_type %in% c("uni_reg", "uni_reg_nbin")) {
      tbl <- gtsummary::modify_column_hide(tbl, stat_n)
    } else if (inherits(tbl, "tbl_merge")) {
      n_cols <- grep("^stat_n", names(tbl$table_body), value = TRUE)
      if (length(n_cols) > 0) {
        tbl <- gtsummary::modify_column_hide(tbl, all_of(n_cols))
      } else {
        warning("No stat_n columns found in tbl_merge object.")
      }
    } else {
      warning("`remove_N` is ignored because the table is not a univariate or
              merged regression table.")
    }
  }


  # 8. Remove sample size source note (for multi_reg or merged tables)
  if (isTRUE(remove_N_obs)) {
    if (source_type %in% c("multi_reg", "multi_reg_nbin")) {
      tbl <- gtsummary::remove_source_note(tbl)
    } else if (inherits(tbl, "tbl_merge")) {
      tbl <- gtsummary::remove_source_note(tbl)
    } else {
      warning("`remove_N_obs` is ignored because the table is not multivariable
              or merged.")
    }
  }


  # 9. Edit Caveat (always applied) if you want to be politically correct
  if (!is.null(caveat)) {
    tbl <- gtsummary::modify_source_note(tbl, caveat)
  }

  # 10. Remove abbreviations if you want to go minimalist
  if (isTRUE(remove_abbreviations)) {
    tbl <- gtsummary::remove_abbreviation(tbl)
  }

  return(tbl)
}
