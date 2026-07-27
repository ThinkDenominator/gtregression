# Multivariable regression

Create a publication-ready multivariable regression table using either
gt or flextable, without a gtsummary dependency.

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
  model_stats = FALSE
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
multi_reg(endometrial_data, HG, c(NV, PI, EH), approach = firth)$table
#> Registered S3 method overwritten by 'lme4':
#>   method           from
#>   na.action.merMod car 


.cl-e94be674{}.cl-e9447f60{font-family:'DejaVu Sans';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e9447f74{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e9447f7e{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e947a8e8{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e947a8f2{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:14pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e947a8fc{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 0.9;background-color:transparent;}.cl-e947cf94{width:1.489in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cf9e{width:2.132in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfa8{width:0.925in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfa9{width:1.489in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfaa{width:2.132in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfb2{width:0.925in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfb3{width:1.489in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfbc{width:2.132in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfbd{width:0.925in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 1pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfc6{width:1.489in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfd0{width:2.132in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfd1{width:0.925in;background-color:transparent;vertical-align: middle;border-bottom: 0.75pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfd2{width:1.489in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfda{width:2.132in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfdb{width:0.925in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(102, 102, 102, 1.00);border-top: 0.75pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfe4{width:1.489in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfe5{width:2.132in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e947cfee{width:0.925in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Characteristic
```
