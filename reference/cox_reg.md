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
  show_sample = "both",
  model_stats = FALSE,
  show_ref = TRUE,
  baseline_strata = NULL,
  time_start = NULL,
  cluster = NULL,
  weights = NULL,
  robust = NULL,
  ties = c("efron", "breslow", "exact"),
  control = NULL
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

- show_sample:

  For stratified Cox tables (`stratifier` supplied), controls which
  sample-size columns are shown in the publication table. One of
  `"both"` (default; N and events), `"n"`, `"events"`, or `"none"`.
  Model statistics, when requested, still retain both N and event
  counts. This argument is ignored when `stratifier` is not supplied:
  non-stratified tables always show the model N column and report the
  number of events in a footnote.

- model_stats:

  Logical; if `TRUE`, extract model-fit statistics including AIC, BIC,
  log-likelihood, concordance, number of events, and N.

- show_ref:

  Logical; if `TRUE` (default), display reference-category rows as
  `"Ref."`. If `FALSE`, hide reference rows; a message reminds users to
  use `show_ref = TRUE` when reference rows are needed.

- baseline_strata:

  Optional character vector of categorical variables used to stratify
  the baseline hazard within one Cox model. Common hazard ratios are
  estimated while each stratum has a separate baseline hazard. No hazard
  ratio is estimated for these variables. This cannot be combined with
  `stratifier`, which fits separate models.

- time_start:

  Optional numeric variable containing entry or interval-start times.
  When supplied, a counting-process response
  `Surv(time_start, time, event)` is fitted, enabling delayed entry and
  start-stop data.

- cluster:

  Optional variable identifying correlated observations. Cox models use
  a clustered sandwich variance estimate for this variable.

- weights:

  Optional numeric case-weight variable.

- robust:

  Optional logical passed to
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).
  The default `NULL` lets `coxph()` choose its standard behaviour;
  clustered models use robust variance automatically.

- ties:

  Method for handling tied event times: `"efron"` (default),
  `"breslow"`, or `"exact"`.

- control:

  Optional named list of arguments for
  [`survival::coxph.control()`](https://rdrr.io/pkg/survival/man/coxph.control.html),
  or a completed control list returned by that function.

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

- footnotes:

  Character vector used by the rendered table. Adjustment notes use the
  same display labels as the table.

- time,event,approach,format,source,adjust_for,baseline_strata,
  exposures,interaction:

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
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md).
The `adjust_for` argument is not used in this mode; include every
variable that belongs in the model inside `exposures`. Since these
estimates are adjusted for the other variables in the same model, the
table reports `Adjusted HR (95% CI)`.

Interaction terms specified via `interaction` are included using
standard formula expansion (for example, `trt*prior`). Interaction
effects are displayed as additional rows beneath the corresponding
exposure.

`baseline_strata` fits one Cox model with separate baseline hazards for
the observed combinations of the selected categorical variables. It
reports common hazard ratios for the predictors and no coefficient for
the strata. This differs from `stratifier`, which fits separate models
and reports stratum-specific results.

The proportional hazards assumption should be assessed separately, for
example with
[`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md).

Stratified Cox tables show both N and event counts by default
(`show_sample = "both"`). Use `show_sample = "n"`,
`show_sample = "events"`, or `show_sample = "none"` to control the
displayed sample columns. All counts are taken from the fitted model
within each stratum, so rows dropped for missing values in any model
variable (including `time_start`, `cluster`, and `weights`) are not
counted. When models within a stratum use different numbers of
observations, the footnote reports the range. Rows with a missing
`stratifier` value are excluded and counted in a footnote. `show_sample`
has no effect without `stratifier`.

Complete-case analysis is used: each model uses the rows that are
complete for its own variables. Unused factor levels are dropped for
each fitted model, so the reference category shown as `"Ref."` is the
first level present in the data used by that model. Ordered factors are
analysed with treatment (category-versus-reference) contrasts. Data in
which every observation is an event are accepted.

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

# One model with separate baseline hazards by cell type
cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  baseline_strata = celltype,
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
