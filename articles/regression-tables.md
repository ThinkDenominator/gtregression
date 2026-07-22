# Regression Tables

## Regression Tables

Go from fitted models to publication-ready tables without
hand-formatting effect estimates. `gtregression` supports logistic,
log-binomial, Poisson, robust Poisson, negative binomial, and linear
regression.

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
    ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes")),
    ftv_cat = factor(case_when(
      ftv == 0 ~ "None",
      ftv == 1 ~ "One",
      ftv >= 2 ~ "Two or more"
    ), levels = c("None", "One", "Two or more"))
  )

birthwt_exposures <- c(
  "age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"
)
```

### Univariable Models

[`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md)
fits one model per exposure and returns a table ready for reports.

``` r

birthwt_uni <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = birthwt_exposures,
  approach = logit,
  theme = clinical
)

birthwt_uni$table
```

[TABLE]

### Adjusted Models

[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
can fit all exposures in one model, or fit one adjusted model per
exposure using the same adjustment set.

``` r

birthwt_multi <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit,
  theme = striped
)

birthwt_multi$table
```

| Characteristic | Adjusted OR (95% CI) | p-value |
|----|----|----|
| ftv_cat |  |  |
| None | — |  |
| One | 0.60 (0.26–1.38) | 0.230 |
| Two or more | 0.86 (0.38–1.96) | 0.717 |
| ht |  |  |
| No | — |  |
| Yes | 5.99 (1.51–23.79) | 0.011 |
| ptl_cat |  |  |
| No | — |  |
| Yes | 4.49 (1.90–10.58) | \<0.001 |
| smoke |  |  |
| No | — |  |
| Yes | 2.87 (1.36–6.04) | 0.006 |
| ui |  |  |
| No | — |  |
| Yes | 2.27 (0.98–5.24) | 0.055 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |
| Adjusted for age, lwt, and race |  |  |
| N = 189 complete observations included across outcome, exposure, and adjustment variables |  |  |

The adjustment variables are recorded in the table footnote so the
result is ready for manuscript-style reporting.

### Other Effect Measures

Switch the `approach` to change the estimand.

``` r

uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui", "ptl_cat"),
  approach = logbinomial
)$table
```

[TABLE]

### Continuous Outcomes

Linear regression outputs beta coefficients and keeps diagnostics under
`$reg_check`.

``` r

birthwt_linear <- multi_reg(
  data = birthwt_data,
  outcome = "bwt",
  exposures = c("age", "lwt", "race", "smoke", "ht", "ui"),
  approach = linear
)

birthwt_linear$table
```

| Characteristic | Adjusted Beta (95% CI) | p-value |
|----|----|----|
| age | -4.67 (-22.97–13.63) | 0.617 |
| ht |  |  |
| No | — |  |
| Yes | -590.03 (-982.59–-197.47) | 0.004 |
| lwt | 4.40 (1.05–7.74) | 0.011 |
| race |  |  |
| White | — |  |
| Black | -490.64 (-783.04–-198.23) | 0.001 |
| Other | -356.61 (-579.77–-133.46) | 0.002 |
| smoke |  |  |
| No | — |  |
| Yes | -360.71 (-564.60–-156.82) | \<0.001 |
| ui |  |  |
| No | — |  |
| Yes | -528.53 (-793.30–-263.77) | \<0.001 |
| Abbreviations: Beta = Linear regression coefficient; CI = Confidence Interval. |  |  |
| N = 189 complete observations included in the multivariable model |  |  |

### What To Inspect

- `$table`: publication-ready table.
- `$table_body`: numeric estimates behind the display.
- `$models`: fitted model objects.
- `$model_summaries`: model-level summaries.
- `$reg_check`: diagnostics for linear models.
