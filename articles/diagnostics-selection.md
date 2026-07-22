# Diagnostics and Model Selection

## Diagnostics and Model Selection

Before the final table goes into a manuscript, check the model. These
helpers keep diagnostics close to the regression workflow.

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
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes"))
  )

exposures <- c("age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat")
```

### Convergence

``` r

check_convergence(
  data = birthwt_data,
  exposures = exposures,
  outcome = "low",
  approach = logit,
  multivariate = TRUE,
  format = gt
)
```

| Convergence check |  |  |  |
|----|----|----|----|
| Exposure | Model | Converged | Max fitted value |
| age + lwt + race + smoke + ht + ui + ptl_cat | logit | Yes | 0.880 |
| Screening aid only; inspect non-convergence, impossible fitted values, and model specification before interpreting estimates. |  |  |  |

### Collinearity

``` r

birthwt_multi <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = logit
)

check_collinearity(birthwt_multi, format = gt)
```

| Collinearity check |  |  |
|----|----|----|
| Variable | VIF | Interpretation |
| age | 1.04 | No collinearity |
| lwt | 1.14 | No collinearity |
| race | 1.11 | No collinearity |
| smoke | 1.16 | No collinearity |
| ht | 1.08 | No collinearity |
| ui | 1.02 | No collinearity |
| ptl_cat | 1.05 | No collinearity |
| Screening aid only; interpret VIF with model purpose, coding choices, sample size, and subject-matter knowledge. |  |  |

### Stepwise Model Selection

``` r

selected <- select_models(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = logit,
  direction = forward,
  format = gt
)

selected$table
```

| Stepwise model selection |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|
| Model | Formula | Predictors | AIC | BIC | Log-likelihood | deviance | Selected variables | Best AIC |
| 1 | low ~ 1 | 0 | 236.67 | 239.91 | -117.34 | 234.67 |  | No |
| 2 | low ~ ptl_cat | 1 | 225.90 | 232.38 | -110.95 | 221.90 | ptl_cat | No |
| 3 | low ~ ptl_cat + age | 2 | 223.30 | 233.02 | -108.65 | 217.30 | ptl_cat + age | No |
| 4 | low ~ ptl_cat + age + ht | 3 | 221.12 | 234.09 | -106.56 | 213.12 | ptl_cat + age + ht | No |
| 5 | low ~ ptl_cat + age + ht + lwt | 4 | 217.43 | 233.64 | -103.72 | 207.43 | ptl_cat + age + ht + lwt | No |
| 6 | low ~ ptl_cat + age + ht + lwt + ui | 5 | 217.15 | 236.60 | -102.58 | 205.15 | ptl_cat + age + ht + lwt + ui | Yes |
| Screening aid only; compare candidate models with study design, clinical or subject-matter judgement, and model diagnostics. |  |  |  |  |  |  |  |  |

### What To Inspect

- [`check_convergence()`](https://thinkdenominator.github.io/gtregression/reference/check_convergence.md):
  convergence status and maximum fitted probabilities. Use `format = gt`
  or `format = flextable` for viewing tables.
- [`check_collinearity()`](https://thinkdenominator.github.io/gtregression/reference/check_collinearity.md):
  VIF and interpretation. Nested model outputs keep their list structure
  when formatted.
- [`select_models()`](https://thinkdenominator.github.io/gtregression/reference/select_models.md):
  `$results_table`, `$best_model`, `$all_models`, and `$table` when
  `format = gt` or `format = flextable`.
