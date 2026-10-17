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
  are accepted. For binary-outcome approaches (`"logit"`, `"firth"`,
  `"logbinomial"`, `"robpoisson"`) the outcome may be numeric 0/1 or 1/2
  (2 is the event; integer or double), logical (`TRUE` is the event), or
  a two-level factor or character variable (the second level is the
  event, which is reported in a message). The same factor, character and
  logical coding is accepted by `"poisson"`.

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

  Regression diagnostics for linear models. Printing this element
  renders a publication-ready table; individual diagnostic rows remain
  available for programmatic use. Other approaches return a message.

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

Confidence intervals are Wald intervals; for `approach = "linear"` they
use the t distribution with the residual degrees of freedom (as
[`confint()`](https://rdrr.io/r/stats/confint.html) for `lm`). Factor
exposures are always fitted with treatment contrasts. Log-binomial
models that fail with the default starting values are refitted from the
intercept `log(mean(y))`; if the model still cannot be fitted the error
names the approach and suggests `approach = "robpoisson"`.

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
#> logistf::logistf(formula = formula, data = data)
#> 
#> Model fitted by Penalized ML
#> Coefficients:
#>                    coef   se(coef) lower 0.95  upper 0.95      Chisq
#> (Intercept)  3.77455951 1.43900672  1.0825371  7.20928050  8.1980136
#> NV1          2.92927330 1.46497415  0.6097244  7.85463171  6.7984572
#> PI          -0.03475175 0.03789237 -0.1244587  0.04045547  0.7468285
#> EH          -2.60416387 0.75362838 -4.3651832 -1.23272106 17.7593175
#>                        p method
#> (Intercept) 4.193628e-03      2
#> NV1         9.123668e-03      2
#> PI          3.874822e-01      2
#> EH          2.506867e-05      2
#> 
#> Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
#> 
#> Likelihood ratio test=43.65582 on 3 df, p=1.78586e-09, n=79
#> Wald test = 21.66965 on 3 df, p = 7.641345e-05


  

Characteristic
```
