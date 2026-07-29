# Univariate regression

Fit one model per exposure and return a clean regression table in
flextable or gt format.

## Usage

``` r
uni_reg(
  data,
  outcome,
  exposures,
  approach = "logit",
  format = c("flextable", "gt"),
  theme = c("minimal"),
  model_stats = FALSE
)
```

## Arguments

- data:

  A data frame containing the outcome and exposure variables.

- outcome:

  Character scalar; outcome column name. Quoted and bare names are
  accepted.

- exposures:

  Character vector; exposure column names. Quoted names are recommended
  in scripts, and bare names are also accepted.

- approach:

  Regression approach. One of `"logit"`, `"firth"`, `"logbinomial"`,
  `"poisson"`, `"robpoisson"`, `"linear"`, or `"negbin"`. Use `"firth"`
  for Firth penalized logistic regression, especially with sparse cells
  or separation.

- format:

  One of `"flextable"` (default) or `"gt"`.

- theme:

  Preset name (e.g. `"minimal"`, `"striped"`, `"clinical"`, `"shaded"`,
  `"jama"`) or primitives
  `c("plain","zebra","lines","labels_bold","compact","header_shaded")`

- model_stats:

  Logical; if `TRUE`, extract model-fit statistics such as AIC, BIC,
  log-likelihood, deviance, pseudo R-squared for non-linear models, and
  R-squared for linear models. Statistics are stored in the returned
  object's `model_stats` element and are not added to the publication
  table.

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

- model_stats:

  Model-fit statistics when `model_stats = TRUE`; otherwise `NULL`.

- variable_labels:

  Named character vector of display labels used for exposure variables.

- reg_check:

  Diagnostics for linear models; message otherwise.

- approach, format, source:

  Metadata fields.

## Details

Use this when you want a quick crude association table before building
an adjusted model. The fitted models are kept in the returned object, so
the formatted table does not hide the underlying analysis.

If exposure variables have a `"label"` attribute, for example from
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
those labels are used automatically in the displayed table and plots.
Internal matching still uses the original column names.

## Examples

``` r
d <- mtcars
if (requireNamespace("gt", quietly = TRUE)) {
  uni_reg(d, "am", c("mpg","cyl"), approach = "logit", format = "gt")$table
}


  

Characteristic
```
