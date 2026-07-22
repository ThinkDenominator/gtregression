# Merge gtregression tables and preserve structure and notes

Merge gtregression tables and preserve structure and notes

## Usage

``` r
merge_tables(..., spanners = NULL, theme = "minimal")
```

## Arguments

- ...:

  Two or more `gtregression` objects containing `$table_display`.

- spanners:

  Character vector of spanner labels, one per table. If `NULL`, defaults
  to `"Table 1"`, `"Table 2"`, etc.

- theme:

  Merge theme preset or vector of primitives.

## Value

A merged table object of class `c("gtregression", "merged_table", ...)`.

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
