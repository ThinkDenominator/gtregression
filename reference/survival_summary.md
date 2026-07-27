# Kaplan-Meier survival summary table

Create a publication-ready Kaplan-Meier summary table with total N,
events, censored observations, and median survival with 95

## Usage

``` r
survival_summary(
  data,
  time,
  event,
  by = NULL,
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

  Optional grouping variable for separate Kaplan-Meier summaries. Quoted
  and bare names are accepted.

- digits:

  Number of digits for survival time summaries.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","survival_summary", ...)` with
elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with numeric Kaplan-Meier summaries.

- `table_display`:

  Display data frame used to render the table.

- `fit`:

  Fitted `survfit` object.

- `time,event,by,format,source`:

  Metadata fields.

## Details

Median survival is estimated from
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).
If the median survival time is not reached during follow-up, the display
table shows `"Not reached"`.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

survival_summary(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

survival_summary(
  data = lung_data,
  time = "time",
  event = "status",
  format = tibble
)
#> # A tibble: 1 × 7
#>   Group       N Events Censored Median CI.lower CI.upper
#>   <chr>   <dbl>  <dbl>    <dbl>  <dbl>    <dbl>    <dbl>
#> 1 Overall   137    128        9     80       52      105
```
