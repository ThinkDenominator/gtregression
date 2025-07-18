#' Descriptive Summary Table for Study Characteristics (No p-values)
#'
#' Generates a tidy, publication-ready summary table of study characteristics.
#' Supports stratification via `by`, auto-detects summary types, and formats output
#' with optional overall column and dichotomous display mode.
#'
#' @param data A data.frame containing the dataset.
#' @param exposures Character vector of variables to summarize.
#' @param by Optional. Character string of grouping variable (like outcome).
#' @param statistic Optional named vector specifying summary type for each variable.
#'        Acceptable values: "mean", "median", "mode", "count".
#' @param percent Character. One of "column" or "row". Default is "column".
#' @param digits Integer. Number of decimal places to use. Default is 1.
#' @param show_missing Character or logical. Either "ifany" or FALSE.
#' @param overall Character. One of "no", "first", or "last". Default is "no".
#' @param show_dichotomous Character. One of "all_levels" (default) or "single_row".
#'   Controls whether to display both levels or just one row for dichotomous variables.
#' @param value Optional. A list of formulas specifying which level to display
#'   for dichotomous variables when `show_dichotomous = "single_row"`.
#'
#' @return A gtsummary object with class 'tbl_summary' and 'descriptive_table'.
#' @export

descriptive_table <- function(data,
                              exposures,
                              by = NULL,
                              statistic = NULL,
                              percent = "column",
                              digits = 1,
                              show_missing = c("ifany", "no"),
                              show_overall = c("no", "last", "first"),
                              show_dichotomous = c("all_levels", "single_row"),
                              value = NULL) {
  # Argument matching
  percent <- match.arg(percent)

  # warning for row percent
  if (percent == "row" && is.null(by)) {
    warning(
      "You are using `percent = 'row'` without specifying a `by` variable.\n",
      "Row percentages are meaningless in this context — did you mean 'column'?"
    )
  }
  show_overall <- match.arg(show_overall)
  show_missing <- match.arg(as.character(show_missing), choices = c("ifany", "no"))
  show_dichotomous <- match.arg(show_dichotomous)


  data <- as.data.frame(data)
  stopifnot("exposures must be character vector" = is.character(exposures))

  # Check that all variables are in the dataset
  missing_vars <- setdiff(c(exposures, by), names(data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not found in the data: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Auto-detect dichotomous exposures
  binary_exposures <- exposures[
    purrr::map_lgl(data[exposures], ~ {
      x <- .x[!is.na(.x)]
      length(unique(x)) == 2 && (is.factor(x) || is.character(x) || is.numeric(x))
    })
  ]

  # Build default type argument
  type_arg <- NULL
  if (length(binary_exposures) > 0) {
    type_val <- if (show_dichotomous == "all_levels") "categorical" else "dichotomous"
    type_arg <- purrr::map(binary_exposures, ~ rlang::new_formula(as.name(.x), type_val))
  }

  # Merge with user-defined statistic overrides
  if (!is.null(statistic)) {
    user_defined <- purrr::map(names(statistic), ~ {
      rlang::new_formula(
        as.name(.x),
        match.arg(statistic[[.x]], c("mean", "median", "mode", "count"))
      )
    })
    type_arg <- c(type_arg, user_defined)
  }

  # Define statistics
  statistic_arg <- list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  )

  # Build the summary table
  tbl <- gtsummary::tbl_summary(
    data = data[, c(exposures, by), drop = FALSE],
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

  # Add overall column if requested
  if (show_overall != "no" && !is.null(by)) {
    if (percent == "row") {
      tbl <- gtsummary::add_overall(
        tbl,
        last = (show_overall == "last"),
        statistic = ~ "{n}"
      )
    } else {
      tbl <- gtsummary::add_overall(tbl, last = (show_overall == "last"))
    }
  }

  class(tbl) <- c("descriptive_table", class(tbl))
  attr(tbl, "source") <- "descriptive_table"
  return(tbl)
}
