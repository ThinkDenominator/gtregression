# Descriptive Summary Table (no gtsummary) using gt/flextable

Publication-ready summary of categorical and continuous variables
(optionally stratified). Mimics the OG gtsummary style: \* column
headers include N, e.g. "Overall, N=200" \* categorical rows shown as n
(%) \* continuous rows default to Median (IQR) (footnote reflects
summary)

## Usage

``` r
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
  value = NULL,
  format = c("gt", "flextable"),
  theme = c("minimal")
)
```

## Arguments

- data:

  data.frame

- exposures:

  character; variables to summarise

- by:

  optional single grouping variable

- percent:

  "column" (default) or "row"; aliases like "col"/"rows" accepted

- digits:

  integer; decimals for % and continuous stats (default 1)

- show_missing:

  "ifany" (default) or "no"

- show_dichotomous:

  "all_levels" (default) or "single_row"

- show_overall:

  "no" (default), "first", or "last"

- statistic:

  optional named vector per continuous var: values in
  "mean","median","mode","count" (default is "median" = Median (IQR))

- value:

  optional named list for single-row binaries (e.g.,
  list(sex="Female")); formula entries like list(sex ~ "Female") are
  also accepted

- format:

  "gt" (default) or "flextable"

- theme:

  preset or primitives

## Value

A list with class `c("gtregression", "descriptive_table", ...)`
containing:

- `table`:

  A `gt_tbl` or `flextable`.

- `table_display`:

  Display-ready data.

- `table_body`:

  Long audit data with variable, level, and type.

- metadata:

  Additional metadata fields.
