# Parametric survival regression

Fit parametric survival models and report time ratios.

## Usage

``` r
surv_reg(
  data,
  time,
  event,
  exposures,
  adjust_for = NULL,
  multivariable = FALSE,
  multivariate = NULL,
  distribution = "weibull",
  format = c("flextable", "gt"),
  theme = c("minimal"),
  model_stats = FALSE
)
```

## Arguments

- data:

  A `data.frame` containing survival time, event status, and exposure
  variables.

- time:

  Survival follow-up time. Quoted and bare names are accepted.

- event:

  Event indicator. Quoted and bare names are accepted. Numeric `0/1`,
  numeric `1/2`, logical, character, and factor variables are accepted.
  For two-level character or factor variables, the second level is
  treated as the event.

- exposures:

  Character vector of exposure variable names. Quoted names are
  recommended in scripts, and bare names are also accepted.

- adjust_for:

  Optional character vector of adjustment variables. When supplied, one
  adjusted model is fitted per exposure.

- multivariable:

  Logical; if `FALSE` (default), the current exposure-by-exposure
  workflow is used. If `TRUE`, one multivariable parametric survival
  model is fitted using all variables in `exposures`, and all exposure
  coefficients are reported.

- multivariate:

  Optional logical alias for `multivariable`. This is accepted for
  convenience; `multivariable` is used internally.

- distribution:

  Parametric survival distribution. One of `"weibull"`, `"exponential"`,
  `"lognormal"`, or `"loglogistic"`. Quoted and bare values are
  accepted. Common spellings such as `"log-normal"` and `"log-logistic"`
  are also accepted.

- format:

  Output table format; one of `"flextable"` (default) or `"gt"`.

- theme:

  Table styling preset.

- model_stats:

  Logical; if `TRUE`, extract model-fit statistics including AIC, BIC,
  log-likelihood, scale, number of events, and N.

## Value

A list of class `c("gtregression","surv_reg", ...)` with elements:

- table:

  A `flextable` or `gt_tbl`.

- table_body:

  Data frame of time ratios and confidence intervals.

- table_display:

  Data frame used to render the publication table.

- models:

  List of fitted `survreg` models.

- model_summaries:

  Summary output for the fitted models.

- model_stats:

  Model-fit statistics when `model_stats = TRUE`; otherwise `NULL`.

- variable_labels:

  Named character vector of display labels.

- time,event,distribution,approach,format,source,adjust_for,exposures:

  Metadata fields.

## Details

`surv_reg()` fits accelerated failure time style parametric survival
models using
[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html).
The exponentiated coefficient is displayed as a time ratio. A time ratio
above 1 suggests longer survival time; a time ratio below 1 suggests
shorter survival time, conditional on the chosen distribution.

By default, `surv_reg()` keeps the exposure-by-exposure workflow:
without `adjust_for`, one crude model is fitted per exposure; with
`adjust_for`, one adjusted model is fitted per exposure and only the
exposure estimate is reported.

With `multivariable = TRUE`, all variables in `exposures` are included
in one parametric survival model and all coefficients are reported.
Since these estimates are adjusted for the other variables in the same
model, the table reports `Adjusted Time Ratio (95% CI)`. The
`adjust_for` argument is not used in this mode; include every variable
that belongs in the model inside `exposures`.

If exposure variables have a `"label"` attribute, for example from
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
those labels are used automatically in the displayed table.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))
lung_data$prior <- factor(lung_data$prior, levels = c(0, 10),
                          labels = c("No", "Yes"))

surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "karno", "age")
)

surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior),
  adjust_for = c(age, karno),
  distribution = lognormal
)

surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior, age, karno),
  distribution = weibull,
  multivariable = TRUE
)

# multivariate is accepted as an alias
surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  multivariate = TRUE
)
```
