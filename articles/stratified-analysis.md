# Stratified Analysis

## Stratified Analysis

Stratified regression repeats the analysis inside each subgroup and
places the results side by side. It is useful when the same association
may look different across groups.

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
```

### Univariable by Stratum

``` r

strata_uni <- stratified_uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht", "ui", "ptl_cat"),
  stratifier = "race",
  approach = "logit",
  theme = clinical
)

strata_uni$table
```

|  | race = White |  |  | race = Black |  |  | race = Other |  |  |
|----|----|----|----|----|----|----|----|----|----|
| Characteristic | N | OR (95% CI) | p-value | N | OR (95% CI) | p-value | N | OR (95% CI) | p-value |
| age | 96 | 0.95 (0.86–1.04) | 0.226 | 26 | 1.05 (0.90–1.23) | 0.526 | 67 | 0.94 (0.84–1.05) | 0.297 |
| lwt | 96 | 0.98 (0.97–1.00) | 0.123 | 26 | 0.99 (0.97–1.01) | 0.517 | 67 | 0.97 (0.95–1.00) | 0.056 |
| smoke | 96 |  |  | 26 |  |  | 67 |  |  |
|  No |  | — |  |  | — |  |  | — |  |
|  Yes |  | 5.76 (1.78–18.60) | 0.003 |  | 3.30 (0.63–17.16) | 0.156 |  | 1.25 (0.35–4.46) | 0.731 |
| ht | 96 |  |  | 26 |  |  | 67 |  |  |
|  No |  | — |  |  | — |  |  | — |  |
|  Yes |  | 2.22 (0.35–14.20) | 0.399 |  | 3.11 (0.24–39.54) | 0.382 |  | 5.59 (0.55–56.99) | 0.146 |
| ui | 96 |  |  | 26 |  |  | 67 |  |  |
|  No |  | — |  |  | — |  |  | — |  |
|  Yes |  | 2.26 (0.66–7.75) | 0.196 |  | 3.11 (0.24–39.54) | 0.382 |  | 2.88 (0.80–10.33) | 0.105 |
| ptl_cat | 96 |  |  | 26 |  |  | 67 |  |  |
|  No |  | — |  |  | — |  |  | — |  |
|  Yes |  | 5.96 (1.80–19.72) | 0.003 |  | 1.44 (0.17–12.23) | 0.736 |  | 4.47 (1.18–16.90) | 0.027 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |  |  |

### Adjusted by Stratum

Use `adjust_for` when each exposure should be adjusted for the same
variables within each stratum.

``` r

strata_multi <- stratified_multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui", "ptl_cat"),
  stratifier = "race",
  adjust_for = c("age", "lwt"),
  approach = "logit",
  theme = striped
)

strata_multi$table
```

|  | race = White |  | race = Black |  | race = Other |  |
|----|----|----|----|----|----|----|
| Characteristic | Adjusted OR (95% CI) | p-value | Adjusted OR (95% CI) | p-value | Adjusted OR (95% CI) | p-value |
| smoke |  |  |  |  |  |  |
|  No | — |  | — |  | — |  |
|  Yes | 4.97 (1.47–16.80) | 0.010 | 2.96 (0.48–18.32) | 0.243 | 1.23 (0.32–4.80) | 0.762 |
| ht |  |  |  |  |  |  |
|  No | — |  | — |  | — |  |
|  Yes | 3.72 (0.45–30.67) | 0.222 | 5.71 (0.27–121.78) | 0.264 | 7.93 (0.66–95.10) | 0.102 |
| ui |  |  |  |  |  |  |
|  No | — |  | — |  | — |  |
|  Yes | 1.59 (0.43–5.96) | 0.488 | 4.49 (0.28–72.28) | 0.289 | 2.68 (0.72–10.05) | 0.143 |
| ptl_cat |  |  |  |  |  |  |
|  No | — |  | — |  | — |  |
|  Yes | 6.26 (1.77–22.16) | 0.004 | 0.96 (0.09–9.85) | 0.973 | 5.55 (1.31–23.56) | 0.020 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |
| Adjusted for age and lwt |  |  |  |  |  |  |
| Complete observations included by race stratum: White: N = 96; Black: N = 26; Other: N = 67 |  |  |  |  |  |  |

### What To Inspect

- `$table`: rendered side-by-side table.
- `$table_display`: wide data used to build the table.
- `$models`: fitted models by stratum.
- `$reg_check`: diagnostics for linear models.
