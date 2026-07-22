# Confounding and Interaction

## Confounding and Interaction

Confounding asks whether adjustment changes the exposure effect.
Interaction asks whether the exposure effect differs across another
variable.

These checks are screening aids for viewing and organising results. Use
DAGs, subject-matter knowledge, and study design to decide which
variables are confounders or effect modifiers. Automated
change-in-estimate and interaction checks should not be used as the sole
basis for model adjustment.

### Which Function Should I Use?

| Question | Use | Why |
|----|----|----|
| Could these candidate variables be confounders or effect modifiers? | [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md) | Screens crude, adjusted, Mantel-Haenszel, and interaction signals together. |
| Does this planned interaction term improve the model? | [`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md) | Compares models with and without `exposure:effect_modifier` using LRT or Wald tests. |
| Do I need a Mantel-Haenszel estimate? | `identify_confounder(method = mh)` or `identify_confounder(method = both)` | MH is a stratified pooled estimate for eligible binary/categorical settings, not a formal interaction-term test. |

A practical workflow is:

1.  Use DAGs, prior literature, and study design to list important
    variables.
2.  Use
    [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md)
    to organise screening evidence for candidate confounders or effect
    modifiers.
3.  Use
    [`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md)
    when you have a planned interaction hypothesis.
4.  If interaction is important, consider stratified reporting with
    [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md)
    or
    [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md).

``` r

library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")

birthwt_data <- data_birthwt |>
  mutate(
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )
```

### Identify Confounders

Use `method = change` for the model-based change-in-estimate method. Use
`method = mh` or `method = both` when Mantel-Haenszel is appropriate.
The output is intentionally tidy rather than publication-first.

``` r

confounder_check <- identify_confounder(
  data = birthwt_data,
  outcome = "low",
  exposure = "smoke",
  potential_confounder = c("race", "ht"),
  approach = logit,
  method = both
)

confounder_check$summary
```

    ## # A tibble: 2 × 13
    ##   exposure candidate crude_est adjusted_est mh_est percent_change
    ##   <chr>    <chr>         <dbl>        <dbl>  <dbl>          <dbl>
    ## 1 smoke    race           2.02         3.05   3.09          51.0 
    ## 2 smoke    ht             2.02         2.04   2.03           0.78
    ## # ℹ 7 more variables: percent_change_model <dbl>, percent_change_mh <dbl>,
    ## #   is_confounder <lgl>, interaction_p <dbl>, is_effect_modifier <lgl>,
    ## #   decision <chr>, recommendation <chr>

### Test Interaction

[`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md)
compares models with and without the interaction term. It is
deliberately model-based and uses `LRT` or `Wald`, not Mantel-Haenszel.
Use it when the interaction term is planned or supported by clinical,
causal, or subject-matter reasoning.

``` r

interaction_check <- interaction_models(
  data = birthwt_data,
  outcome = "low",
  exposure = "smoke",
  effect_modifier = "race",
  covariates = c("age", "lwt"),
  approach = logit,
  test = LRT,
  format = gt
)

interaction_check$table
```

| Interaction screening |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| Outcome | Exposure | Effect modifier | Approach | Test | p-value | Alpha | Interaction? | Decision | Interpretation |
| low | smoke | race | logit | Likelihood Ratio Test | 0.319 | 0.050 | No | No interaction | No statistical evidence of interaction between smoke and race at alpha = 0.05. |
| Screening aid only; interaction decisions should be interpreted with subject-matter knowledge, study design, and stratum-specific estimates. |  |  |  |  |  |  |  |  |  |

### What To Inspect

- [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md):
  `$summary`, `$table`, and `$details`.
- [`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md):
  `$summary`, `$table`, `$p_value`, `$decision`, and fitted model
  objects.
- Use subject-matter knowledge with these outputs. The functions support
  interpretation; they do not replace the study design.
