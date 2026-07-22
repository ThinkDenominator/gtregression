# Modify Regression/Descriptive Tables (labels, headers, caption, notes)

Works with objects created by this package (class `"gtregression"`):
[`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md),
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
[`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md),
and
[`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md).
No gtsummary dependency or fallback.

## Usage

``` r
modify_table(
  gt_table,
  variable_labels = NULL,
  level_labels = NULL,
  header_labels = NULL,
  caption = NULL,
  bold_labels = FALSE,
  bold_levels = FALSE,
  remove_N = FALSE,
  remove_N_obs = FALSE,
  remove_abbreviations = FALSE,
  caveat = NULL
)
```

## Arguments

- gt_table:

  Table object produced by this package (must contain `$table_display`).

- variable_labels:

  Named character vector, for example `c(old_var = "New label", ...)`.

- level_labels:

  Named list for factor levels:
  `list(var1 = c(old = "New", ...), var2 = c(...))`.

- header_labels:

  Named character vector to rename visible headers, e.g.
  `c("OR (95% CI)" = "Crude OR", "p-value" = "P")`. Common aliases such
  as `estimate`, `p.value`, and `N` are also accepted.

- caption:

  Optional caption/title.

- bold_labels:

  Logical; bold variable (header) rows in the body.

- bold_levels:

  Logical; bold factor level rows in the body.

- remove_N:

  Logical; if `TRUE`, drops the `N` column for univariate package
  tables.

- remove_N_obs:

  Logical; if `TRUE`, suppresses multivariable complete-case footnote.

- remove_abbreviations:

  Logical; if `TRUE`, removes the Abbreviations footnote line.

- caveat:

  Optional extra footnote.

## Value

The modified table object (same class as input).

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

tbl <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "smoke", "ht"),
  approach = logit
)

modify_table(
  tbl,
  variable_labels = c(age = "Maternal age", smoke = "Smoking"),
  level_labels = list(smoke = c(Yes = "Smoker")),
  header_labels = c(estimate = "Crude OR", p.value = "P"),
  caption = "Univariable regression for low birth weight"
)$table


  
Univariable regression for low birth weight

  
Characteristic
```
