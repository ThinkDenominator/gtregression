# Merge gtregression tables and preserve structure and notes

Merge gtregression tables and preserve structure and notes

## Usage

``` r
merge_tables(
  ...,
  spanners = NULL,
  theme = "minimal",
  format = c("flextable", "gt")
)
```

## Arguments

- ...:

  Two or more `gtregression` objects containing `$table_display`.

- spanners:

  Character vector of spanner labels, one per table. If `NULL`, defaults
  to `"Table 1"`, `"Table 2"`, etc.

- theme:

  Merge theme preset or vector of primitives.

- format:

  Output table format. One of `"flextable"` (default) or `"gt"`. The
  merged display is rebuilt in this format, independently of the formats
  used by the input tables.

## Value

A merged table object of class `c("gtregression", "merged_table", ...)`.

## Details

Binary variables should use the same row display across all input
tables. For a descriptive, univariable, and multivariable merge, the
clearest publication layout is usually `show_dichotomous = "all_levels"`
in
[`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md)
and `show_ref = TRUE` in each regression function. Mixing these settings
can create additional binary rows; in that situation `merge_tables()`
issues a warning with the compatible settings.

Footnotes created by each input table are retained unchanged. Exact
duplicate notes are shown once in the merged table.

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

uni_tbl <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht"),
  approach = "logit"
)

multi_tbl <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht"),
  adjust_for = c("age", "lwt"),
  approach = "logit"
)

merge_tables(
  uni_tbl,
  multi_tbl,
  spanners = c("Univariable", "Adjusted")
)
```
