# Build a compatible data frame for forest plots

Creates the tabular input used by
[`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md)
from
[`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md),
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
and optionally
[`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md)
objects. This function is useful when users want to inspect, edit, or
reuse the exact data that will be passed to the forest plot.

## Usage

``` r
forest_df(uni, multi = NULL, desc = NULL, digits = 2)
```

## Arguments

- uni:

  A `gtregression` object from
  [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md).
  If `multi` is supplied without `uni`, the multivariable object is
  plotted as a single effect column.

- multi:

  Optional `gtregression` object from
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md).

- desc:

  Optional descriptive table object from
  [`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md).

- digits:

  Number of digits used when formatting confidence intervals.

## Value

A data frame with display columns, formatted effect-size columns,
standard-error columns, and plotting attributes used by
[`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md)
to draw confidence intervals.

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

uni_or <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht"),
  approach = logit
)
multi_or <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht"),
  adjust_for = c("age", "lwt"),
  approach = logit
)

forest_data <- forest_df(uni_or, multi_or)
head(forest_data)
#>   Characteristic        OR (95% CI)    Adjusted OR (95% CI)      se_uni
#> 1            age   0.95 (0.89-1.01)                         0.031513184
#> 2            lwt   0.99 (0.97-1.00)                         0.006169475
#> 3          smoke                                                     NA
#> 4             No                 --                      --          NA
#> 5            Yes   2.02 (1.08-3.78)        1.96 (1.03-3.70) 0.319636414
#> 6             ht                                                     NA
#>      se_adj
#> 1        NA
#> 2        NA
#> 3        NA
#> 4        NA
#> 5 0.3258718
#> 6        NA
```
