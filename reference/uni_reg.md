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
  format = c("flextable", "gt"),
  theme = c("minimal")
)
```

## Arguments

- data:

  data.frame

- outcome:

  Character scalar; outcome column name. Quoted and bare names are
  accepted.

- exposures:

  Character vector; exposure column names. Quoted names are recommended
  in scripts, and bare names are also accepted.

- approach:

  one of `"logit"`, `"logbinomial"`, `"poisson"`, `"linear"`

- format:

  One of `"flextable"` (default) or `"gt"`.

- theme:

  preset name (e.g. `"minimal"`, `"striped"`, `"clinical"`, `"shaded"`,
  `"jama"`) or primitives
  `c("plain","zebra","lines","labels_bold","compact","header_shaded")`

## Value

A list of class `c("gtregression","uni_reg", ...)` with elements:

- table:

  A `flextable` (when `format="flextable"`) or `gt_tbl` (when
  `format="gt"`).

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
