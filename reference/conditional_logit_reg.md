# Conditional logistic regression for matched case-control studies

Fits conditional logistic regression models for individually or
frequency matched case-control data and returns publication-ready
matched odds ratios.

## Usage

``` r
conditional_logit_reg(
  data,
  outcome,
  exposures,
  matched_set,
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

  A data frame containing the outcome, predictors, and matched-set
  identifier.

- outcome:

  Binary case-control outcome. The second factor/character level is
  treated as the case; numeric outcomes must use 0/1 or 1/2 coding.

- exposures:

  Predictor names. Quoted and bare names are accepted.

- matched_set:

  Variable identifying each matched case-control set.

- adjust_for:

  Optional covariates. In exposure-by-exposure mode, each exposure is
  fitted with these covariates.

- multivariable:

  Logical. If `TRUE`, one model includes all exposures and optional
  adjustment variables.

- multivariate:

  Optional alias for `multivariable`.

- model_stats:

  Logical. If `TRUE`, return model N, case and control counts,
  matched-set count, AIC, and log-likelihood.

- format:

  Output format: `"flextable"` (default) or `"gt"`.

- theme:

  Table theme preset.

- show_ref:

  Logical. Display reference rows for categorical predictors.

## Value

A `gtregression` object containing the publication table, underlying
estimates, fitted `clogit` models, summaries, model statistics, and
matching metadata.

## Details

Matching is handled by conditioning on `matched_set`; no odds ratio is
estimated for that variable. Matched sets without both a case and a
control are not informative for conditional logistic regression and are
excluded with a message. This function is intended for matched
case-control studies, not complex survey analysis.

## Examples

``` r
matched_data <- data.frame(
  set = rep(1:8, each = 3),
  case = rep(c(1, 0, 0), 8),
  exposed = factor(rep(c("Yes", "No", "No"), 8),
                   levels = c("No", "Yes")),
  age = rep(40:47, each = 3) + rep(c(0, -1, 1), 8)
)

conditional_logit_reg(
  matched_data,
  outcome = case,
  exposures = exposed,
  matched_set = set
)
#> Warning: Ran out of iterations and did not converge
```
