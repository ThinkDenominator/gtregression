% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_table.R
\name{descriptive_table}
\alias{descriptive_table}
\title{Descriptive Summary Table for Study Characteristics (User-Friendly)}
\usage{
descriptive_table(
  data,
  exposures,
  by = NULL,
  percent = c("column", "row"),
  digits = 1,
  show_missing = c("ifany", "no"),
  show_dichotomous = c("all_levels", "single_row"),
  show_overall = c("no", "first", "last"),
  statistic = NULL,
  value = NULL
)
}
\arguments{
\item{data}{A data frame containing your study dataset.}

\item{exposures}{A character vector specifying the variable names (columns)
in `data` that should be included in the summary table. These can be
categorical or continuous.}

\item{by}{Optional. A single character string giving the name of a grouping
variable (e.g., outcome). If supplied, the table will show stratified
summaries by this variable.}

\item{percent}{Character. Either `"column"` (default) or `"row"`.
- `"column"` calculates percentages within each group defined by `by`
  (i.e., denominator = column total).
- `"row"` calculates percentages across `by` groups (i.e., denominator =
  row total).
If `by` is not specified, `"column"` is used and `"row"` is ignored.}

\item{digits}{Integer. Controls how many decimal places are shown for
percentages and means. Defaults to 1.}

\item{show_missing}{Character. One of `"ifany"` (default) or `"no"`.
- `"ifany"` shows missing value counts only when missing values exist.
- `"no"` hides missing counts entirely.}

\item{show_dichotomous}{Character. One of `"all_levels"` (default) or
`"single_row"`.
- `"all_levels"` displays all levels of binary (dichotomous) variables.
- `"single_row"` shows only one row (typically "Yes", "Present", or a
  user-defined level), making the table more compact.}

\item{show_overall}{Character. One of `"no"` (default), `"first"`, or
`"last"`.
If `by` is supplied:
- `"first"` includes a column for overall summaries before the stratified
  columns.
- `"last"` includes the overall column at the end.
- `"no"` disables the overall column.}

\item{statistic}{Optional named vector of summary types for specific
variables.
For example, use `statistic = c(age = "mean", bmi = "median")` to override
default summaries. Accepted values: `"mean"`, `"median"`, `"mode"`,
`"count"`.}

\item{value}{Optional. A list of formulas specifying which level of a binary
variable to show when `show_dichotomous = "single_row"`.
For example, `value = list(sex ~ "Female")` will report only the "Female"
row.}
}
\value{
A `gtsummary::tbl_summary` object with additional class
  `"descriptive_table"`. Can be printed, customized, merged, or exported.
}
\description{
Creates a clean, publication-ready summary table using
`gtsummary::tbl_summary()`. Designed for beginner analysts, this function
applies sensible defaults and flexible options to display categorical and
continuous variables with or without stratification. It supports one-line
summaries of dichotomous variables, handles missing data gracefully, and
includes an optional "Overall" column for comparison.
}
\examples{
\donttest{
if (requireNamespace("mlbench", quietly = TRUE)) {
  data("PimaIndiansDiabetes2", package = "mlbench")
  library(dplyr)
pima <- PimaIndiansDiabetes2 |>
  mutate(
    diabetes = ifelse(diabetes == "pos", 1, 0),

    bmi = cut(
      mass,
      breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
      labels = c("Underweight", "Normal", "Overweight", "Obese")
    )
  )
  descriptive_table(pima, exposures = c("age", "bmi"),
                    by = "diabetes")
}
}

}
