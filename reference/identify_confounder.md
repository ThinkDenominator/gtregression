# Identify confounders and effect modifiers

Review whether one or more candidate variables may act as confounders or
effect modifiers for one or more exposures.

## Usage

``` r
identify_confounder(
  data,
  outcome = NULL,
  exposure,
  potential_confounder,
  approach = "logit",
  time = NULL,
  event = NULL,
  distribution = "weibull",
  method = "change",
  threshold = 10,
  emm_threshold = 10,
  emm_test = c("interaction", "both", "estimate"),
  interaction_alpha = 0.05,
  format = c("flextable", "gt"),
  theme = c("minimal")
)
```

## Arguments

- data:

  A data frame.

- outcome:

  Outcome variable name. Quoted and bare names are accepted. Required
  for ordinary regression approaches. Leave unset for Cox and parametric
  survival approaches and supply `time` and `event` instead.

- exposure:

  Exposure variable name(s). Can be a character scalar or vector. Quoted
  names are recommended in scripts, and bare names are also accepted.

- potential_confounder:

  Candidate confounder/effect-modifier variable name(s). Can be a
  character scalar or vector. Quoted names are recommended in scripts,
  and bare names are also accepted.

- approach:

  Regression approach. One of `"logit"`, `"logbinomial"`, `"poisson"`,
  `"robpoisson"`, `"linear"`, `"negbin"`, `"cox"`, or `"survreg"`.

- time:

  Survival time variable name for `approach = "cox"` or
  `approach = "survreg"`. Quoted and bare names are accepted.

- event:

  Event indicator variable name for survival approaches. Values may be
  coded as 0/1, 1/2, logical, or a two-level factor/character variable.

- distribution:

  Parametric survival distribution for `approach = "survreg"`. One of
  `"weibull"`, `"exponential"`, `"lognormal"`, or `"loglogistic"`.

- method:

  Confounding assessment method. One of `"change"`, `"mh"`, or `"both"`.
  `"change"` compares crude and adjusted model estimates. `"mh"`
  compares crude and Mantel-Haenszel pooled estimates and is available
  for binary outcome, binary exposure, and categorical strata. `"both"`
  uses either method.

- threshold:

  Percent change threshold for confounding assessment.

- emm_threshold:

  Threshold for relative spread in stratum-specific estimates when using
  estimate-based effect-modification screening.

- emm_test:

  One of `"interaction"`, `"both"`, or `"estimate"`.

- interaction_alpha:

  Alpha threshold for interaction p-values.

- format:

  Output table format. One of `"flextable"` (default) or `"gt"`.

- theme:

  Table theme preset or primitives.

## Value

If a single exposure-candidate pair is supplied, returns a detailed
list.

If multiple combinations are supplied, returns a list with:

- summary:

  A tibble with one row per exposure-candidate combination.

- details:

  A named list of detailed results for each combination.

## Details

The function first assesses possible effect modification using
stratum-specific estimates and/or an interaction test. If no important
effect modification is detected, it then assesses confounding using the
selected method.

This is a screening aid for viewing and organising results. Confounding
and effect modification should be interpreted using subject-matter
knowledge, study design, and causal diagrams such as DAGs. Automated
change-in-estimate and interaction checks should not be used as the sole
basis for model adjustment.

For each exposure-candidate pair, the interaction screen fits
`outcome ~ exposure + potential_confounder + exposure:potential_confounder`.
It does not adjust for other covariates. Consequently, the reported
interaction p-value is a candidate-only screening result, not a final
adjusted interaction analysis. Use
`interaction_models(covariates = ...)` when testing a planned,
covariate-adjusted interaction.

Use this function when you want to screen one or more candidate
variables and organise crude, adjusted, Mantel-Haenszel, and
effect-modification signals in one place. For a focused comparison of
models with and without a planned exposure-by-modifier interaction term,
use
[`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md).

## See also

[`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md)
for focused model comparison of a planned interaction term.

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other"))
  )

identify_confounder(
  data = birthwt_data,
  outcome = low,
  exposure = smoke,
  potential_confounder = race,
  approach = logit
)

lung_data <- data_lungcancer |>
  dplyr::mutate(
    trt = factor(trt, levels = c(1, 2),
                 labels = c("Standard treatment", "Test treatment")),
    prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
  )

identify_confounder(
  data = lung_data,
  time = time,
  event = status,
  exposure = trt,
  potential_confounder = prior,
  approach = cox
)
```
