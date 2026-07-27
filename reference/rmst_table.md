# Restricted mean survival time table

Estimate restricted mean survival time (RMST) up to a user-specified
follow-up time. RMST is the average survival time observed within a
fixed time window, such as 365 days.

## Usage

``` r
rmst_table(
  data,
  time,
  event,
  by = NULL,
  tau,
  digits = 1,
  conf.level = 0.95,
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

  Optional grouping variable for separate RMST estimates. Quoted and
  bare names are accepted.

- tau:

  Restriction time for RMST. For example, `tau = 365` reports mean
  survival time restricted to the first 365 days of follow-up.

- digits:

  Number of digits for displayed survival time summaries.

- conf.level:

  Confidence level for RMST confidence intervals.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","rmst_table", ...)` with elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with numeric RMST summaries.

- `table_display`:

  Display data frame used to render the table.

- `fit`:

  Fitted `survfit` object.

- `time,event,by,tau,format,source`:

  Metadata fields.

## Details

RMST is estimated from
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
using Kaplan-Meier methods and `summary.survfit(rmean = tau)`. When `by`
has exactly two groups, the table includes the RMST difference as the
second group minus the first group, with a Wald confidence interval and
p-value based on the reported RMST standard errors.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

rmst_table(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  tau = 365
)

rmst_table(
  data = lung_data,
  time = "time",
  event = "status",
  tau = 180,
  format = tibble
)
#> # A tibble: 1 × 14
#>   Type  Group     Tau     N Events  RMST    SE CI.lower CI.upper Difference
#>   <chr> <chr>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>      <dbl>
#> 1 Group Overall   180   137    128  88.5  5.64     77.5     99.6         NA
#> # ℹ 4 more variables: Difference.SE <dbl>, Difference.CI.lower <dbl>,
#> #   Difference.CI.upper <dbl>, p.value <dbl>
```
