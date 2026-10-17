# Descriptive summary table

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
  format = c("flextable", "gt"),
  theme = c("minimal")
)
```

## Arguments

- data:

  data.frame

- exposures:

  Character vector of variables to summarise. Quoted names are
  recommended in scripts, and bare names are also accepted.

- by:

  Optional single grouping variable. Quoted and bare names are accepted.
  If it is also listed in \`exposures\`, it is left out of the
  summarised variables, with a message.

- percent:

  "column" (default) or "row"; aliases like "col"/"rows" accepted

- digits:

  integer; decimals for % and continuous stats (default 1)

- show_missing:

  "ifany" (default) or "no". With "ifany", a "(Missing)" row (n and % of
  the column N) is added under every variable that has missing values,
  categorical or continuous.

- show_dichotomous:

  "all_levels" (default) or "single_row"

- show_overall:

  "no" (default), "first", or "last"

- statistic:

  Optional summary type for numeric variables. Use a single value such
  as `"mean"` or `mean` for all numeric variables, or a named vector
  such as `c(age = mean, ftv = categorical)`. Supported values are
  `"mean"`, `"median"`, `"mode"`, `"count"`, and `"categorical"`. Use
  `"categorical"` for numeric ordinal variables that should be shown as
  n (%).

- value:

  optional named list for single-row binaries (e.g.,
  list(sex="Female")); formula entries like list(sex ~ "Female") are
  also accepted. The value must be one of the observed levels of the
  variable.

- format:

  "flextable" (default) or "gt"

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

- `variable_labels`:

  Named character vector of display labels used for variables.

- metadata:

  Additional metadata fields.

## Details

If variables have a `"label"` attribute, for example from
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
those labels are used automatically in the displayed table. Internal
matching still uses the original column names.

Variable types: factor, character and logical variables are shown as n
(%) per level (logical: `FALSE`/`TRUE`); numeric variables with exactly
two distinct values are shown as categories; numeric variables with a
single distinct value are shown as one category (n, 100%) unless a
numeric `statistic` is requested for them; other numeric variables are
continuous. Levels of numeric variables shown as categories are sorted
by value. Date and date-time variables are not summarised: they are
excluded and named in a message.
