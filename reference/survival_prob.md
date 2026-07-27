# Kaplan-Meier survival probability table

Estimate Kaplan-Meier survival probabilities at user-specified follow-up
times, such as 30-day, 6-month, or 1-year survival.

## Usage

``` r
survival_prob(
  data,
  time,
  event,
  by = NULL,
  times,
  digits = 1,
  extend = TRUE,
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

  Optional grouping variable for separate Kaplan-Meier survival
  probabilities. Quoted and bare names are accepted.

- times:

  Numeric vector of follow-up times at which survival probability should
  be estimated.

- digits:

  Number of digits for percentages and survival probabilities.

- extend:

  Logical. If `TRUE`, requested times beyond the observed follow-up
  range are retained using the last available Kaplan-Meier estimate.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","survival_prob", ...)` with elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with Kaplan-Meier survival probabilities.

- `table_display`:

  Display data frame used to render the table.

- `fit`:

  Fitted `survfit` object.

- `time,event,by,times,format,source`:

  Metadata fields.

## Details

Survival probabilities are estimated from
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
at the requested follow-up times. Events and censored counts are
interval counts up to each requested time point as returned by
`summary.survfit()`.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

survival_prob(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(90, 180, 365)
)

survival_prob(
  data = lung_data,
  time = "time",
  event = "status",
  times = c(90, 180),
  format = tibble
)
#> # A tibble: 2 × 8
#>   Group    Time N.risk Events Censored Survival.probability CI.lower CI.upper
#>   <chr>   <dbl>  <dbl>  <dbl>    <dbl>                <dbl>    <dbl>    <dbl>
#> 1 Overall    90     62     73        3                0.464    0.387    0.556
#> 2 Overall   180     27     30        4                0.222    0.161    0.308
```
