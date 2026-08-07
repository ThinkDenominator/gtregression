# Side-by-side forest plots: univariate vs multivariable

Creates two aligned forest plots from compatible `gtregression`
regression objects such as crude and adjusted outputs from
[`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md),
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
or
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md).

## Usage

``` r
plot_reg_combine(
  tbl_uni,
  tbl_multi,
  title_uni = NULL,
  title_multi = NULL,
  caption = NULL,
  ref_line = NULL,
  order_y = NULL,
  log_x = FALSE,
  point_color = "#1F77B4",
  errorbar_color = "#4C4C4C",
  base_size = 14,
  show_ref = TRUE,
  sig_color = NULL,
  sig_errorbar_color = NULL,
  xlim_uni = NULL,
  breaks_uni = NULL,
  xlim_multi = NULL,
  breaks_multi = NULL,
  alpha = 0.05,
  show_adjustment_note = TRUE
)
```

## Arguments

- tbl_uni:

  A univariate `gtregression` object.

- tbl_multi:

  A multivariable `gtregression` object.

- title_uni, title_multi:

  Optional panel titles.

- caption:

  Optional combined plot caption. If `NULL`, notes are added
  automatically for adjusted
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  objects and displayed reference categories.

- ref_line:

  Optional numeric reference line. If `NULL`, uses 0 for linear models
  and 1 otherwise.

- order_y:

  Optional character vector to customize exposure ordering.

- log_x:

  Logical; if `TRUE`, uses log x-axis for non-linear models.

- point_color, errorbar_color:

  Base colors for non-significant rows.

- base_size:

  Base font size for `theme_minimal()`.

- show_ref:

  Logical; if `TRUE`, include reference levels as `(Ref.)`. If `FALSE`,
  binary exposures are shown as compact rows for the estimated
  non-reference category; affirmative levels such as `Yes` or `1` are
  displayed using the variable name.

- sig_color, sig_errorbar_color:

  Optional colors for significant rows. If `NULL`, base colors are
  reused.

- xlim_uni, breaks_uni:

  Optional x-axis limits and breaks for the univariate panel.

- xlim_multi, breaks_multi:

  Optional x-axis limits and breaks for the multivariable panel.

- alpha:

  Significance level for linear models when `p.value` is available.

- show_adjustment_note:

  Logical; if `TRUE`, add a default caption describing `adjust_for`
  variables from `tbl_multi` when available.

## Value

A `patchwork` object with two `ggplot2` panels.
