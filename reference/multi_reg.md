# Multivariable regression

Fit adjusted or multivariable regression models and return a
manuscript-style table in flextable or gt format.

## Usage

``` r
multi_reg(
  data,
  outcome,
  exposures,
  adjust_for = NULL,
  interaction = NULL,
  approach = "logit",
  format = c("flextable", "gt"),
  theme = c("minimal"),
  model_stats = FALSE,
  show_ref = TRUE
)
```

## Arguments

- data:

  A `data.frame` containing the variables of interest.

- outcome:

  Character scalar; name of the outcome variable. Quoted and bare names
  are accepted.

- exposures:

  Character vector; exposure variable(s) to report. Quoted names are
  recommended in scripts, and bare names are also accepted. If
  `adjust_for = NULL`, all exposures are included in a single
  multivariable model. If `adjust_for` is supplied, one adjusted model
  is fitted per exposure and only exposure-specific adjusted estimate(s)
  are displayed.

- adjust_for:

  Optional character vector of adjustment variables. Quoted and bare
  names are accepted. Must not overlap with `exposures`.

- interaction:

  Optional character scalar specifying one interaction term using
  standard formula syntax, e.g. `"bmi*sex"`. When used with
  `adjust_for`, only a single exposure should be supplied.

- approach:

  Character scalar specifying the regression approach. One of `"logit"`,
  `"firth"`, `"logbinomial"`, `"poisson"`, `"linear"`, `"robpoisson"`,
  or `"negbin"`. Use `"firth"` for Firth penalized logistic regression,
  especially with sparse cells or separation.

- format:

  Output table format; one of `"flextable"` (default) or `"gt"`.

- theme:

  Table styling preset (e.g. `"minimal"`, `"striped"`, `"clinical"`,
  `"shaded"`, `"jama"`) or a character vector of primitives such as
  `c("plain","zebra","lines","labels_bold","compact","header_shaded")`.

- model_stats:

  Logical; if `TRUE`, extract model-fit statistics such as AIC, BIC,
  log-likelihood, deviance, pseudo R-squared for non-linear models, and
  R-squared for linear models. Statistics are stored in the returned
  object's `model_stats` element and are not added to the publication
  table.

- show_ref:

  Logical; if `TRUE` (default), display reference-category rows as
  `"Ref."`. If `FALSE`, hide reference rows; a message reminds users to
  use `show_ref = TRUE` when reference rows are needed.

## Value

A list of class `c("gtregression","multi_reg", ...)` with elements:

- table:

  A `flextable` (if `format="flextable"`) or `gt_tbl` (if
  `format="gt"`).

- table_body:

  A data frame of adjusted estimates and confidence intervals for each
  exposure and level.

- table_display:

  A data frame used for rendering the final table, including header and
  level rows.

- models:

  A list of fitted model(s).

- model_summaries:

  [`summary()`](https://rdrr.io/r/base/summary.html) output for the
  fitted model(s).

- model_stats:

  Model-fit statistics when `model_stats = TRUE`; otherwise `NULL`.

- variable_labels:

  Named character vector of display labels used for exposure variables.

- reg_check:

  Regression diagnostics for linear models; otherwise a message.

- approach:

  The regression approach used.

- format:

  The output format used.

- source:

  Function identifier (`"multi_reg"`).

- adjusted_mode:

  Whether one adjusted model per exposure was fitted.

- adjust_for:

  Adjustment variables supplied by the user, if any.

- exposures:

  Exposure variables supplied by the user.

- interaction:

  Interaction term supplied by the user, if any.

## Details

Use the default mode when you want all exposures in one model. Use
`adjust_for` when you want one adjusted estimate per reported exposure,
with the same adjustment set used repeatedly.

If exposure variables have a `"label"` attribute, for example from
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
those labels are used automatically in the displayed table and plots.
Internal matching still uses the original column names.

In default mode (`adjust_for = NULL`), all exposures are included in a
single multivariable model. In adjusted mode, one model is fitted per
exposure, adjusting for the variables specified in `adjust_for`.

Interaction terms specified via `interaction` are included in the model
using standard formula expansion (e.g. `bmi*sex`). Interaction effects
are displayed as additional rows beneath the corresponding exposure.

## Examples

``` r
endometrial_data <- data_endometrial
endometrial_data$HG <- factor(endometrial_data$HG, levels = c(0, 1))
endometrial_data$NV <- factor(endometrial_data$NV, levels = c(0, 1))
multi_reg(endometrial_data, HG, c(NV, PI, EH), approach = firth, format = gt)$table
#> Registered S3 method overwritten by 'lme4':
#>   method           from
#>   na.action.merMod car 


  

Characteristic
```
