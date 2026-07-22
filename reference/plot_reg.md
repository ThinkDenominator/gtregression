# Visualize a regression model as a forest plot

Creates a forest plot from a fitted `gtregression` object produced by
functions such as
[`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md)
or
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md).

## Usage

``` r
plot_reg(
  tbl,
  title = NULL,
  caption = NULL,
  ref_line = NULL,
  order_y = NULL,
  log_x = FALSE,
  xlim = NULL,
  breaks = NULL,
  point_color = "#1F77B4",
  errorbar_color = "#4C4C4C",
  base_size = 14,
  show_ref = TRUE,
  sig_color = NULL,
  sig_errorbar_color = NULL,
  alpha = 0.05,
  show_adjustment_note = TRUE
)
```

## Arguments

- tbl:

  A fitted `gtregression` object.

- title:

  Optional plot title.

- caption:

  Optional plot caption. If `NULL`, an adjustment note is added
  automatically for adjusted
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
  objects when `show_adjustment_note = TRUE`.

- ref_line:

  Optional numeric value for the reference line. Defaults to 0 for
  linear models and 1 otherwise.

- order_y:

  Optional character vector specifying exposure order.

- log_x:

  Logical; if `TRUE`, use a log-scaled x-axis for non-linear models.

- xlim:

  Optional numeric vector of length 2 specifying x-axis limits.

- breaks:

  Optional numeric vector of x-axis tick breaks.

- point_color:

  Fill color for points.

- errorbar_color:

  Color for error bars.

- base_size:

  Base font size.

- show_ref:

  Logical; if `TRUE`, reference rows are shown.

- sig_color:

  Optional fill color for significant points.

- sig_errorbar_color:

  Optional color for significant error bars.

- alpha:

  Significance level for linear models when `p.value` is available.

- show_adjustment_note:

  Logical; if `TRUE`, add a default caption describing `adjust_for`
  variables when available.

## Value

A `ggplot2` object.
