# Kaplan-Meier risk table

Tabulate the number at risk at selected follow-up times.

## Usage

``` r
km_risk_table(
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

  Optional grouping variable for separate Kaplan-Meier risk tables.
  Quoted and bare names are accepted.

- times:

  Numeric vector of follow-up times for the risk table.

- digits:

  Number of digits for displayed follow-up times.

- extend:

  Logical. If `TRUE`, requested times beyond the observed follow-up
  range are retained using the last available Kaplan-Meier risk set.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","km_risk_table", ...)` with elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with Kaplan-Meier risk table counts.

- `table_display`:

  Display data frame used to render the table.

- `fit`:

  Fitted `survfit` object.

- `time,event,by,times,format,source`:

  Metadata fields.

## Details

The `At risk` column gives the number still under observation at each
requested time. The `Events` and `Censored` columns are interval counts
up to each requested time point as returned by `summary.survfit()`.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

km_risk_table(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(0, 90, 180, 365)
)

km_risk_table(
  data = lung_data,
  time = "time",
  event = "status",
  times = c(0, 90, 180),
  format = tibble
)
#> # A tibble: 3 × 5
#>   Group    Time N.risk Events Censored
#>   <chr>   <dbl>  <dbl>  <dbl>    <dbl>
#> 1 Overall     0    137      0        0
#> 2 Overall    90     62     73        3
#> 3 Overall   180     27     30        4
```
