# Kaplan-Meier survival plot

Create a Kaplan-Meier survival curve with optional confidence interval,
censoring marks, log-rank p-value, and risk table.

## Usage

``` r
km_plot(
  data,
  time,
  event,
  by = NULL,
  conf.int = TRUE,
  risk_table = TRUE,
  p_value = TRUE,
  censor = TRUE,
  break_time_by = NULL,
  xlim = NULL,
  xlab = "Time",
  ylab = "Survival probability",
  title = NULL,
  legend_title = NULL,
  palette = NULL,
  base_size = 13
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

  Optional grouping variable for separate Kaplan-Meier curves. Quoted
  and bare names are accepted.

- conf.int:

  Logical; if `TRUE`, show confidence limits.

- risk_table:

  Logical; if `TRUE`, add a number-at-risk table below the curve.

- p_value:

  Logical; if `TRUE`, show the log-rank p-value when `by` is supplied.

- censor:

  Logical; if `TRUE`, show censoring marks.

- break_time_by:

  Optional numeric interval for x-axis and risk-table time breaks. If
  `NULL`, breaks are chosen automatically.

- xlim:

  Optional numeric vector of length 2 specifying x-axis limits.

- xlab, ylab:

  Axis labels.

- title:

  Optional plot title.

- legend_title:

  Optional legend title. If `NULL`, the labelled `by` variable name is
  used.

- palette:

  Optional character vector of colors for grouped curves.

- base_size:

  Base font size.

## Value

A `ggplot2` object when `risk_table = FALSE`; otherwise a `patchwork`
object combining the survival curve and risk table. The returned object
has attributes `fit`, `plot_data`, `risk_table`, and `logrank_p`.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

km_plot(
  data = lung_data,
  time = time,
  event = status
)


km_plot(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  break_time_by = 200
)

```
