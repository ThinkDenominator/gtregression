# Plot mediation paths

Draw the exposure, mediator, and outcome path from a
[`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
result.

## Usage

``` r
plot_mediation(mediation_object, show_estimates = TRUE, base_size = 13)
```

## Arguments

- mediation_object:

  Object returned by
  [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md).

- show_estimates:

  Logical; if `TRUE`, show direct and indirect effect estimates on the
  plot.

- base_size:

  Base font size.

## Value

A `ggplot2` object.

## Examples

``` r
med <- mediation_analysis(
  data = data_diabetes_mediation,
  exposure = obesity,
  mediator = glucose,
  outcome = diabetes,
  covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
  outcome_approach = logit,
  sims = 50,
  seed = 123
)
plot_mediation(med)

plot_mediation(med, show_estimates = FALSE)

```
