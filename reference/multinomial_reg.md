# Multinomial logistic regression

Fit crude, exposure-specific adjusted, or joint multivariable
multinomial logistic models for an unordered outcome with three or more
categories.

## Usage

``` r
multinomial_reg(
  data,
  outcome,
  exposures,
  adjust_for = NULL,
  multivariable = FALSE,
  multivariate = NULL,
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

  An unordered factor outcome with at least three levels. The first
  level is the reference outcome category.

- exposures:

  Predictor variables to report.

- adjust_for:

  Optional covariates included but not reported.

- multivariable:

  Logical; fit all exposures in one model.

- multivariate:

  Optional alias for \`multivariable\`.

- model_stats:

  Logical; return model-fit and sample-size statistics.

- format:

  Output table format: \`"flextable"\` or \`"gt"\`.

- theme:

  Table styling preset.

- show_ref:

  Logical; display categorical predictor reference rows.

## Value

A \`gtregression\` object containing a side-by-side table of outcome
comparisons, fitted models, statistics, and one standard regression
object per comparison in \`\$per_outcome\`.

## Details

Each odds ratio compares one non-reference outcome category with the
first outcome level. Outcome categories are nominal: their order has no
numeric or severity interpretation. Use \`relevel()\` before fitting to
choose another reference category.

## Examples

``` r
set.seed(1501)
example_data <- data.frame(
  destination = factor(sample(c("Home", "Rehabilitation", "Hospital"), 120,
                              replace = TRUE),
                       levels = c("Home", "Rehabilitation", "Hospital")),
  treatment = factor(sample(c("Control", "Intervention"), 120, TRUE)),
  age = rnorm(120, 65, 10)
)
multinomial_reg(
  example_data,
  outcome = destination,
  exposures = treatment,
  adjust_for = age
)
```
