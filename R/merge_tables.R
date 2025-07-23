#' Merge Multiple gtsummary Tables (Descriptive, Univariate, Multivariable)
#'
#' Flexibly merges any 2 or more `gtsummary` tables (e.g., from `descriptive_table()`,
#' `uni_reg()`, `multi_reg()`) into a single table using `tbl_merge()`.
#' Automatically applies column spanners based on the order of inputs.
#'
#' @param ... Two or more `gtsummary` table objects to merge.
#' @param spanners Optional character vector of column header titles. If not supplied,
#'   defaults to `"Table 1"`, `"Univariate"`, `"Multivariable"` etc.
#'
#' @return A merged `gtsummary::tbl_merge` object.
#' @export
#'
#' @examples
#' \dontrun{
#' merge_tables(desc_tbl, uni_tbl)
#' merge_tables(desc_tbl, multi_tbl)
#' merge_tables(uni_tbl, multi_tbl)
#' merge_tables(desc_tbl, uni_tbl, multi_tbl)
#' merge_tables(desc_tbl, uni_tbl, spanners = c("T1", "Crude"))
#' }

merge_tables <- function(..., spanners = NULL) {
  tbls <- list(...)

  if (length(tbls) < 2) {
    stop("At least two gtsummary tables are required to merge.", call. = FALSE)
  }

  # Auto-generate spanners if not provided
  if (is.null(spanners)) {
    default_labels <- c("Table 1", "Table 2", "Table 3", "Table 4", "Table 5")
    spanners <- default_labels[seq_along(tbls)]
  }

  if (length(spanners) != length(tbls)) {
    stop("The number of `spanners` must match the number of tables provided.",
         call. = FALSE)
  }

  # Validate all are gtsummary-compatible
  are_valid <- vapply(tbls, inherits, logical(1), what = "gtsummary")
  if (!all(are_valid)) {
    stop("All inputs must be gtsummary table objects.",
         "(e.g., tbl_summary, tbl_stack, tbl_regression).", call. = FALSE)
  }

  # Merge with spanners
  gtsummary::tbl_merge(
    tbls = tbls,
    tab_spanner = spanners
  )
}
