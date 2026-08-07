# Log-rank test for Kaplan-Meier survival curves

Compare survival curves between groups using the log-rank test.

## Usage

``` r
logrank_test(
  data,
  time,
  event,
  by,
  digits = 2,
  format = c("flextable", "gt", "tibble"),
  theme = c("minimal")
)
```

## Arguments

- data:

  A `data.frame` containing survival time, event status, and grouping
  variable.

- time:

  Survival follow-up time. Quoted and bare names are accepted.

- event:

  Event indicator. Quoted and bare names are accepted. Numeric `0/1`,
  numeric `1/2`, logical, character, and factor variables are accepted.
  For two-level character or factor variables, the second level is
  treated as the event.

- by:

  Grouping variable used to compare survival curves. Quoted and bare
  names are accepted.

- digits:

  Number of digits for the chi-square statistic and expected events.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","logrank_test", ...)` with elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with group-level log-rank components and overall test
  statistics.

- `test`:

  One-row tibble with chi-square statistic, degrees of freedom, and
  p-value.

- `fit`:

  The `survdiff` object.

- `time,event,by,format,source`:

  Metadata fields.

## Details

The log-rank test compares the observed number of events with the
expected number of events in each group under the null hypothesis that
the survival curves are the same. It is a group comparison, not an
effect-size estimate; use
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
when a hazard ratio is needed.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

logrank_test(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

logrank_test(
  data = lung_data,
  time = "time",
  event = "status",
  by = "trt",
  format = tibble
)
#> # A tibble: 2 × 7
#>   Group        N Observed Expected Chi.square    df p.value
#>   <chr>    <dbl>    <dbl>    <dbl>      <dbl> <int>   <dbl>
#> 1 Standard    69       64     64.5    0.00823     1   0.928
#> 2 Test        68       64     63.5    0.00823     1   0.928
```
