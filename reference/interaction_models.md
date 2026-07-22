# Compare Models With and Without an Interaction Term

Fits two models, one with and one without an interaction term between an
exposure and a potential effect modifier. The models are compared using
a likelihood ratio test or Wald test to assess statistical evidence of
interaction.

## Usage

``` r
interaction_models(
  data,
  outcome,
  exposure,
  covariates = NULL,
  effect_modifier,
  approach = "logit",
  test = c("LRT", "Wald"),
  alpha = 0.05,
  verbose = FALSE,
  format = c("flextable", "gt", "tibble")
)
```

## Arguments

- data:

  A data frame containing all required variables.

- outcome:

  Outcome variable name. Quoted and bare names are accepted.

- exposure:

  Main exposure variable name. Quoted and bare names are accepted.

- covariates:

  Optional character vector of additional covariates. Quoted names are
  recommended in scripts, and bare names are also accepted.

- effect_modifier:

  Variable name for the potential effect modifier. Quoted and bare names
  are accepted.

- approach:

  Regression approach. One of `"logit"`, `"logbinomial"`, `"poisson"`,
  `"robpoisson"`, `"negbin"`, or `"linear"`.

- test:

  Statistical test for model comparison. One of `"LRT"` or `"Wald"`.

- alpha:

  Significance threshold used to classify the interaction result.

- verbose:

  Logical; if `TRUE`, prints a short interpretation.

- format:

  Output format for the viewing table. One of `"flextable"` (default),
  `"gt"`, or `"tibble"`. Use `format = "tibble"` to keep only the
  original list structure.

## Value

A list with model objects, formulas, p-value, decision, and a one-row
summary tibble. When `format` is `"gt"` or `"flextable"`, the list also
includes `table`.

## Details

Use this function when the interaction is planned or clinically/causally
motivated and you want a focused model comparison. Mantel-Haenszel
estimation is not used here because this function tests an explicit
interaction term in a regression model. For broader screening of
candidate confounders or effect modifiers, including
Mantel-Haenszel-supported checks when appropriate, use
[`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md).

## See also

[`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md)
for broader confounding and effect-modification screening.
