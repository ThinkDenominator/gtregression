# Kaplan-Meier survival quantile table

Estimate Kaplan-Meier survival time quantiles, such as the 25th
percentile, median, and 75th percentile survival times.

## Usage

``` r
survival_quantiles(
  data,
  time,
  event,
  by = NULL,
  probs = c(0.25, 0.5, 0.75),
  digits = 1,
  format = c("flextable", "gt", "tibble"),
  theme = c("minimal")
)
```

## Arguments

- data:

  A `data.frame` containing survival time and event status.

- time:

  Survival follow-up time. Quoted and bare names are accepted.

- event:

  Event indicator. Quoted and bare names are accepted. Numeric `0/1`,
  numeric `1/2`, logical, character, and factor variables are accepted.
  For two-level character or factor variables, the second level is
  treated as the event.

- by:

  Optional grouping variable for separate Kaplan-Meier quantiles. Quoted
  and bare names are accepted.

- probs:

  Numeric vector of event-time quantiles to estimate. The default
  `c(0.25, 0.5, 0.75)` reports the 25th percentile, median, and 75th
  percentile event times.

- digits:

  Number of digits for survival time summaries.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","survival_quantiles", ...)` with
elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with Kaplan-Meier quantiles.

- `table_display`:

  Display data frame used to render the table.

- `fit`:

  Fitted `survfit` object.

- `time,event,by,probs,format,source`:

  Metadata fields.

## Details

A probability of `0.50` is the median event time: the estimated time by
which 50 A probability of `0.25` is the time by which 25 corresponding
to 75 follow-up, the display table shows `"Not reached"`.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

survival_quantiles(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

survival_quantiles(
  data = lung_data,
  time = "time",
  event = "status",
  probs = c(0.25, 0.5),
  format = tibble
)
#> # A tibble: 2 × 6
#>   Group   Probability Survival.probability  Time CI.lower CI.upper
#>   <chr>         <dbl>                <dbl> <dbl>    <dbl>    <dbl>
#> 1 Overall        0.25                 0.75    25       19       36
#> 2 Overall        0.5                  0.5     80       52      105
```
