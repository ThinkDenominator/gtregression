#' Descriptive Summary Table for Study Characteristics (User-Friendly)
#'
#' Creates a clean, publication-ready summary table using
#' `gtsummary::tbl_summary()`. Designed for beginner analysts, this function
#' applies sensible defaults and flexible options to display categorical and
#' continuous variables with or without stratification. It supports one-line
#' summaries of dichotomous variables, handles missing data gracefully, and
#' includes an optional "Overall" column for comparison.
#'
#' @param data A data frame containing your study dataset.
#'
#' @param exposures A character vector specifying the variable names (columns)
#'   in `data` that should be included in the summary table. These can be
#'   categorical or continuous.
#'
#' @param by Optional. A single character string giving the name of a grouping
#'   variable (e.g., outcome). If supplied, the table will show stratified
#'   summaries by this variable.
#'
#' @param percent Character. Either `"column"` (default) or `"row"`.
#'   - `"column"` calculates percentages within each group defined by `by`
#'     (i.e., denominator = column total).
#'   - `"row"` calculates percentages across `by` groups (i.e., denominator =
#'     row total).
#'   If `by` is not specified, `"column"` is used and `"row"` is ignored.
#'
#' @param digits Integer. Controls how many decimal places are shown for
#'   percentages and means. Defaults to 1.
#'
#' @param show_missing Character. One of `"ifany"` (default) or `"no"`.
#'   - `"ifany"` shows missing value counts only when missing values exist.
#'   - `"no"` hides missing counts entirely.
#'
#' @param show_dichotomous Character. One of `"all_levels"` (default) or
#'   `"single_row"`.
#'   - `"all_levels"` displays all levels of binary (dichotomous) variables.
#'   - `"single_row"` shows only one row (typically "Yes", "Present", or a
#'     user-defined level), making the table more compact.
#'
#' @param show_overall Character. One of `"no"` (default), `"first"`, or
#'   `"last"`.
#'   If `by` is supplied:
#'   - `"first"` includes a column for overall summaries before the stratified
#'     columns.
#'   - `"last"` includes the overall column at the end.
#'   - `"no"` disables the overall column.
#'
#' @param statistic Optional named vector of summary types for specific
#'   variables.
#'   For example, use `statistic = c(age = "mean", bmi = "median")` to override
#'   default summaries. Accepted values: `"mean"`, `"median"`, `"mode"`,
#'   `"count"`.
#'
#' @param value Optional. A list of formulas specifying which level of a binary
#'   variable to show when `show_dichotomous = "single_row"`.
#'   For example, `value = list(sex ~ "Female")` will report only the "Female"
#'   row.
#'
#' @return A `gtsummary::tbl_summary` object with additional class
#'   `"descriptive_table"`. Can be printed, customized, merged, or exported.
#' @importFrom gtsummary tbl_summary all_continuous all_categorical

#' @examples
#' \dontrun{
#' # Basic summary by outcome
#' descriptive_table(Pima, exposures = c("age", "mass", "bmi"),
#'                   by = "diabetes_cat")
#'
#' # Compact one-row output for binary vars
#' descriptive_table(Pima, exposures = c("smoke", "alcohol"),
#'                   show_dichotomous = "single_row")
#'
#' # Summary without stratification
#' descriptive_table(Pima, exposures = c("age", "bmi"))
#' }
#'
#' @export

descriptive_table <- function(data,
                              exposures,
                              by = NULL,
                              percent = c("column", "row"),
                              digits = 1,
                              show_missing = c("ifany", "no"),
                              show_dichotomous = c("all_levels", "single_row"),
                              show_overall = c("no", "first", "last"),
                              statistic = NULL,
                              value = NULL) {
  # Match arguments
  percent <- match.arg(percent)
  show_missing <- match.arg(show_missing)
  show_dichotomous <- match.arg(show_dichotomous)
  show_overall <- match.arg(show_overall)

  # Validate inputs
  stopifnot("exposures must be a character vector" = is.character(exposures))
  data <- as.data.frame(data)

  # Validate variable presence
  missing_vars <- setdiff(c(exposures, by), names(data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not found in the data: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Use internal helper to validate exposures
  .validate_exposures(data, exposures)

  # Warn if using row percentages without grouping
  if (percent == "row" && is.null(by)) {
    warning("Row percent are only meaningful when a `by` variable is provided.",
            "Defaulting to column-wise %.")
  }

  # Auto-detect dichotomous exposures
  binary_exposures <- exposures[
    purrr::map_lgl(data[exposures], function(x) {
      x <- x[!is.na(x)]
      length(unique(x)) == 2 &&
        (is.factor(x) ||
         is.character(x) ||
         is.numeric(x))
    })
  ]

  # Set display types
  type_arg <- NULL
  if (length(binary_exposures) > 0) {
    type_val <- if (show_dichotomous == "all_levels") "categorical"
    else "dichotomous"
    type_arg <- purrr::map(binary_exposures,
                           ~ rlang::new_formula(as.name(.x), type_val))
  }

  # Merge user-defined summary types (e.g., mean vs median)
  if (!is.null(statistic)) {
    user_defined <- purrr::map(names(statistic), ~ {
      rlang::new_formula(
        as.name(.x),
        match.arg(statistic[[.x]], c("mean", "median", "mode", "count"))
      )
    })
    type_arg <- c(type_arg, user_defined)
  }

  # Default statistic display
  statistic_arg <- list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  )

  # Build base table
  tbl <- gtsummary::tbl_summary(
    data = data,
    include = unique(c(exposures, by)),
    by = by,
    type = type_arg,
    value = value,
    statistic = statistic_arg,
    missing = show_missing,
    digits = list(
      all_continuous() ~ digits,
      all_categorical() ~ c(0, digits)
    ),
    percent = percent
  )

  # Add overall column if applicable
  if (show_overall != "no" && !is.null(by)) {
    if (percent == "row") {
      tbl <- gtsummary::add_overall(
        tbl,
        last = (show_overall == "last"),
        statistic = list(all_categorical() ~ "{n}"),
        digits = ~ 0
      )
    } else {
      tbl <- gtsummary::add_overall(tbl, last = (show_overall == "last"))
    }
  }

  # Tag output
  class(tbl) <- c("descriptive_table", class(tbl))
  attr(tbl, "source") <- "descriptive_table"
  return(tbl)
}
