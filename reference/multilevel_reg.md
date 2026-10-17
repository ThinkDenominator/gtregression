# Random-intercept multilevel regression

Fit a linear or logistic mixed-effects model with one grouping variable
and return publication-ready fixed-effect estimates.

## Usage

``` r
multilevel_reg(
  data,
  outcome,
  exposures,
  group,
  adjust_for = NULL,
  approach = c("linear", "logit"),
  format = c("flextable", "gt"),
  theme = c("minimal"),
  model_stats = FALSE,
  show_ref = TRUE
)
```

## Arguments

- data:

  A data frame containing the outcome, predictors, and group.

- outcome:

  Continuous outcome for \`approach = "linear"\` or binary outcome for
  \`approach = "logit"\`.

- exposures:

  Predictor variables to report.

- group:

  Grouping variable for the random intercept.

- adjust_for:

  Optional covariates included in the model but not reported.

- approach:

  \`"linear"\` or \`"logit"\`.

- format:

  Output table format: \`"flextable"\` or \`"gt"\`.

- theme:

  Table styling preset.

- model_stats:

  Logical; return model fit, clustering, random-effect, and ICC
  statistics.

- show_ref:

  Logical; display categorical reference rows.

## Value

A \`gtregression\` object containing the publication table, numeric
estimates, fitted mixed model, model statistics, and analysis metadata.

## Details

This intentionally focused workflow supports one random intercept.
Random slopes, crossed effects, and nested random-effects formulas are
outside this function's scope and should be fitted directly with
specialist software.

For linear models, the ICC is the random-intercept variance divided by
total random-intercept plus residual variance. For logistic models, a
latent-scale ICC is calculated using residual variance pi-squared over
three.

## Examples

``` r
set.seed(71)
multilevel_data <- data.frame(
  clinic = factor(rep(seq_len(20), each = 12)),
  treatment = factor(rep(c("Control", "Intervention"), 120)),
  age = rnorm(240, 58, 9)
)
clinic_effect <- rnorm(20, 0, 3)
multilevel_data$score <- 50 + 2 * (multilevel_data$treatment == "Intervention") +
  0.2 * multilevel_data$age + clinic_effect[multilevel_data$clinic] + rnorm(240)

multilevel_reg(
  multilevel_data,
  outcome = score,
  exposures = c(treatment, age),
  group = clinic,
  approach = linear,
  model_stats = TRUE
)
```
