# Proportional-odds ordinal logistic regression

Fit crude, exposure-specific adjusted, or joint multivariable
proportional-odds models and return publication-ready common odds
ratios.

## Usage

``` r
ordinal_reg(
  data,
  outcome,
  exposures,
  adjust_for = NULL,
  multivariable = FALSE,
  multivariate = NULL,
  assumption_test = TRUE,
  model_stats = FALSE,
  format = c("flextable", "gt"),
  theme = "minimal",
  show_ref = TRUE
)
```

## Arguments

- data:

  A data frame containing the outcome and predictors.

- outcome:

  An ordered-factor outcome. Its level order defines movement from lower
  to higher categories.

- exposures:

  Predictor variables to report.

- adjust_for:

  Optional covariates included but not reported.

- multivariable:

  Logical; fit all exposures in one model.

- multivariate:

  Optional alias for \`multivariable\`.

- assumption_test:

  Logical; perform term-level likelihood-ratio tests of the
  proportional-odds assumption.

- model_stats:

  Logical; return model-fit and sample-size statistics.

- format:

  Output table format: \`"flextable"\` or \`"gt"\`.

- theme:

  Table styling preset.

- show_ref:

  Logical; display categorical reference rows.

## Value

A \`gtregression\` object containing the publication table, fitted
models, common odds ratios, proportional-odds tests, and metadata.

## Details

An odds ratio above one indicates greater odds of being in a higher
outcome category, assuming the proportional-odds assumption holds. The
outcome must be an ordered factor so category direction is never
inferred silently.

## Examples

``` r
ordinal_data <- data.frame(
  recovery = ordered(
    c("Poor", "Fair", "Good", "Excellent", "Fair", "Good"),
    levels = c("Poor", "Fair", "Good", "Excellent")
  ),
  treatment = factor(c("Control", "Control", "New", "New", "Control", "New")),
  age = c(70, 64, 58, 51, 62, 55)
)
ordinal_reg(
  ordinal_data,
  outcome = recovery,
  exposures = treatment,
  adjust_for = age,
  assumption_test = FALSE
)
#> Warning: (1) Hessian is numerically singular: parameters are not uniquely determined 
#> In addition: Absolute convergence criterion was met, but relative criterion was not met
```
