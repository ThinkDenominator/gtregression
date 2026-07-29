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

Without `adjust_for`, `surv_reg()` fits one crude model per exposure and
reports `Time Ratio (95% CI)`. With `adjust_for`, it fits one adjusted
model per exposure and reports `Adjusted Time Ratio (95% CI)`.

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
```
