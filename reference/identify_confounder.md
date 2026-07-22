# Identify confounders and effect modifiers

Assesses whether one or more candidate variables act as confounders or
effect modifiers for one or more exposures.

## Usage

``` r
identify_confounder(
  data,
  outcome,
  exposure,
  potential_confounder,
  approach = "logit",
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

  Outcome variable name. Quoted and bare names are accepted.

- exposure:

  Exposure variable name(s). Can be a character scalar or vector. Quoted
  names are recommended in scripts, and bare names are also accepted.

- potential_confounder:

  Candidate confounder/effect-modifier variable name(s). Can be a
  character scalar or vector. Quoted names are recommended in scripts,
  and bare names are also accepted.

- approach:

  Regression approach. One of `"logit"`, `"logbinomial"`, `"poisson"`,
  `"robpoisson"`, `"linear"`, or `"negbin"`.

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

Use this function when you want to screen one or more candidate
variables and organise crude, adjusted, Mantel-Haenszel, and
effect-modification signals in one place. For a focused comparison of
models with and without a planned exposure-by-modifier interaction term,
use
[`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md).

## See also

[`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md)
for focused model comparison of a planned interaction term.
