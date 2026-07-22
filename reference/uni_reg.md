# Univariate regression

Create a publication-ready univariate regression table using either gt
or flextable

## Usage

``` r
uni_reg(
  data,
  outcome,
  exposures,
  approach = "logit",
  format = c("gt", "flextable"),
  theme = c("minimal")
)
```

## Arguments

- data:

  data.frame

- outcome:

  character scalar; outcome column name

- exposures:

  character vector; exposure column names

- approach:

  one of `"logit"`, `"logbinomial"`, `"poisson"`, `"linear"`

- format:

  one of `"gt"` (default) or `"flextable"`

- theme:

  preset name (e.g. `"minimal"`, `"striped"`, `"clinical"`, `"shaded"`,
  `"jama"`) or primitives
  `c("plain","zebra","lines","labels_bold","compact","header_shaded")`

## Value

A list of class `c("gtregression","uni_reg", ...)` with elements:

- table:

  A `gt_tbl` (when `format="gt"`) or `flextable` (when
  `format="flextable"`).

- table_body:

  Data frame of numeric estimates and CIs.

- table_display:

  Data frame for display (headers + levels).

- models:

  List of fitted univariate models.

- model_summaries:

  Per-model [`summary()`](https://rdrr.io/r/base/summary.html) results.

- reg_check:

  Diagnostics for linear models; message otherwise.

- approach, format, source:

  Metadata fields.

## Examples

``` r
d <- mtcars
if (requireNamespace("gt", quietly = TRUE)) {
  uni_reg(d, "am", c("mpg","cyl"), approach = "logit", format = "gt")$table
}


  

Characteristic
```
