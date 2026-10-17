# Build a compatible data frame for forest plots

Creates the tabular input used by
[`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
from
[`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md),
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
and optionally
[`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md)
objects. This function is useful when users want to inspect, edit, or
reuse the exact data that will be passed to the forest plot. Stratified
regression objects are also supported when supplied one at a time; the
returned data frame keeps the characteristic rows once and places strata
side by side as separate effect columns for
[`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md).

## Usage

``` r
forest_df(uni = NULL, multi = NULL, desc = NULL, digits = 2)
```

## Arguments

- uni:

  A `gtregression` regression object, usually the crude results (for
  example
  [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md),
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
  [`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md)
  or
  [`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md)).
  If `multi` is supplied without `uni`, the adjusted object is plotted
  as a single effect column.

- multi:

  Optional adjusted object of the same model family, for example
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  or a model fitted with `multivariable = TRUE`. Exposure-specific
  adjusted results (`adjust_for`) and the single multivariable model can
  also be shown together; the second column is then labelled "Mutually
  adjusted".

- desc:

  Optional descriptive table object from
  [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md).

- digits:

  Number of digits used when formatting confidence intervals.

## Value

A data frame with display columns, formatted effect-size columns,
standard-error columns, and plotting attributes used by
[`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
to draw confidence intervals.

## Details

Effect columns use the same header as the source tables, for example
"Matched OR (95% CI)" for
[`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md)
and "Common OR (95% CI)" for
[`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md),
and the estimates and intervals are taken unchanged from each result's
`table_body`.

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
  approach = "logit"
)
#> Outcome `low`: modelling the event 'Low BW' (second level); reference level is 'Normal BW'.
multi_or <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht"),
  adjust_for = c("age", "lwt"),
  approach = "logit"
)
#> Outcome `low`: modelling the event 'Low BW' (second level); reference level is 'Normal BW'.

forest_data <- forest_df(uni_or, multi_or)
head(forest_data)
#>   Characteristic                                              OR (95% CI)
#> 1            age                                         0.95 (0.89–1.01)
#> 2            lwt                                         0.99 (0.97–1.00)
#> 3          smoke                                                         
#> 4             No                                                     Ref.
#> 5            Yes                                         2.02 (1.08–3.78)
#> 6             ht                                                         
#>                                           Adjusted OR (95% CI)      se_uni
#> 1                                                              0.031513763
#> 2                                                              0.006169588
#> 3                                                                       NA
#> 4                                                         Ref.          NA
#> 5                                             1.96 (1.03–3.70) 0.319642287
#> 6                                                                       NA
#>      se_adj
#> 1        NA
#> 2        NA
#> 3        NA
#> 4        NA
#> 5 0.3258778
#> 6        NA
```
