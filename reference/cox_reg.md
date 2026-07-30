# Cox proportional hazards regression

Fit Cox proportional hazards models and report hazard ratios.

## Usage

``` r
cox_reg(
  data,
  time,
  event,
  exposures,
  adjust_for = NULL,
  stratifier = NULL,
  interaction = NULL,
  multivariable = FALSE,
  multivariate = NULL,
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
  adjusted Cox model is fitted per exposure.

- stratifier:

  Optional single stratifying variable. When supplied, stratum-specific
  Cox tables are produced using the same crude, adjusted, or
  multivariable workflow requested by the other arguments. The
  stratifier cannot also be used as the time, event, exposure,
  adjustment, or interaction variable.

- interaction:

  Optional character scalar specifying one interaction term using
  standard formula syntax, e.g. `"trt*prior"`. Quoted and bare
  interaction syntax are accepted. In exposure-by-exposure mode, supply
  a single exposure; in `multivariable = TRUE` mode, the interaction is
  added to the single multivariable model.

- multivariable:

  Logical; if `FALSE` (default), the current exposure-by-exposure
  workflow is used. If `TRUE`, one multivariable Cox model is fitted
  using all variables in `exposures`, and all exposure coefficients are
  reported.

- multivariate:

  Optional logical alias for `multivariable`. This is accepted for
  convenience; `multivariable` is used internally.

- format:

  Output table format; one of `"flextable"` (default) or `"gt"`.

- theme:

  Table styling preset.

- model_stats:

  Logical; if `TRUE`, extract model-fit statistics including AIC, BIC,
  log-likelihood, concordance, number of events, and N.

## Value

A list of class `c("gtregression","cox_reg", ...)` with elements:

- table:

  A `flextable` or `gt_tbl`.

- table_body:

  Data frame of hazard ratios and confidence intervals.

- table_display:

  Data frame used to render the publication table.

- models:

  List of fitted `coxph` models.

- model_summaries:

  Summary output for the fitted models.

- model_stats:

  Model-fit statistics when `model_stats = TRUE`; otherwise `NULL`.

- variable_labels:

  Named character vector of display labels.

- time,event,approach,format,source,adjust_for,exposures,interaction:

  Metadata fields.

## Details

By default, `cox_reg()` keeps the exposure-by-exposure workflow: without
`adjust_for`, one crude Cox model is fitted per exposure; with
`adjust_for`, one adjusted Cox model is fitted per exposure and only the
exposure estimate is reported. This is useful for screening or for
reporting several adjusted exposure effects.

With `multivariable = TRUE`, all variables in `exposures` are included
in a single Cox model and all coefficients are reported. This mirrors
the multivariable workflow used by
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md).
The `adjust_for` argument is not used in this mode; include every
variable that belongs in the model inside `exposures`. Since these
estimates are adjusted for the other variables in the same model, the
table reports `Adjusted HR (95% CI)`.

Interaction terms specified via `interaction` are included using
standard formula expansion (for example, `trt*prior`). Interaction
effects are displayed as additional rows beneath the corresponding
exposure.

The proportional hazards assumption should be assessed separately, for
example with
[`check_ph()`](https://thinkdenominator.github.io/gtregression/reference/check_ph.md).

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

cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "karno", "age")
)

cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior),
  adjust_for = c(age, karno)
)

# Interaction in an adjusted exposure model
cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno),
  interaction = trt*prior
)

cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior, age, karno),
  multivariable = TRUE
)

# multivariate is accepted as an alias
cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  multivariate = TRUE
)
```
