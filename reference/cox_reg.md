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

- time,event,approach,format,source,adjust_for,exposures:

  Metadata fields.

## Details

Without `adjust_for`, `cox_reg()` fits one crude Cox model per exposure
and reports `HR (95% CI)`. With `adjust_for`, it fits one adjusted Cox
model per exposure and reports `Adjusted HR (95% CI)`. The proportional
hazards assumption should be assessed separately, for example with
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
```
