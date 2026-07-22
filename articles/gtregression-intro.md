# Start Here: Model to Manuscript

![](../reference/figures/gtregression_hex.png)

## gtregression

**Publication-ready regression tables and plots for real-world health
data.**

`gtregression` helps you fit, adjust, stratify, visualise, and export
regression results with beginner-friendly R syntax. It supports
logistic, log-binomial, Poisson, robust Poisson, negative binomial, and
linear regression.

### What You Can Make

- Clean descriptive tables.
- Univariable and multivariable regression tables.
- Adjusted models with clear footnotes.
- Stratified regression outputs.
- Forest plots and publication-style forest tables.
- Model diagnostics, model selection, confounding, and interaction
  checks.
- HTML, PDF, PNG, and Word outputs.

### Install

``` r

install.packages("gtregression")

# Development version
devtools::install_github("ThinkDenominator/gtregression")
```

### Prepare Example Data

The articles use `data_birthwt`, a small built-in dataset that is easy
to learn with.

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
    ptl_cat = ifelse(ptl > 0, "Yes", "No"),
    ftv_cat = case_when(
      ftv == 0 ~ "None",
      ftv == 1 ~ "One",
      ftv >= 2 ~ "Two or more"
    )
  ) |>
  mutate(
    ptl_cat = factor(ptl_cat, levels = c("No", "Yes")),
    ftv_cat = factor(ftv_cat, levels = c("None", "One", "Two or more"))
  )

birthwt_exposures <- c(
  "age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"
)
```

### Five-Minute Workflow

#### Describe

``` r

birthwt_summary <- descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = "column",
  show_overall = "last",
  theme = clinical
)

birthwt_summary$table
```

| Characteristic | Normal BW, N=130 | Low BW, N=59 | Overall, N=189 |
|----|----|----|----|
| age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 23.0 (19.0-26.0) |
| ftv_cat |  |  |  |
|  None | 64 (49.2%) | 36 (61.0%) | 100 (52.9%) |
|  One | 36 (27.7%) | 11 (18.6%) | 47 (24.9%) |
|  Two or more | 30 (23.1%) | 12 (20.3%) | 42 (22.2%) |
| ht |  |  |  |
|  No | 125 (96.2%) | 52 (88.1%) | 177 (93.7%) |
|  Yes | 5 (3.8%) | 7 (11.9%) | 12 (6.3%) |
| lwt | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 121.0 (110.0-140.0) |
| ptl_cat |  |  |  |
|  No | 118 (90.8%) | 41 (69.5%) | 159 (84.1%) |
|  Yes | 12 (9.2%) | 18 (30.5%) | 30 (15.9%) |
| race |  |  |  |
|  White | 73 (56.2%) | 23 (39.0%) | 96 (50.8%) |
|  Black | 15 (11.5%) | 11 (18.6%) | 26 (13.8%) |
|  Other | 42 (32.3%) | 25 (42.4%) | 67 (35.4%) |
| smoke |  |  |  |
|  No | 86 (66.2%) | 29 (49.2%) | 115 (60.8%) |
|  Yes | 44 (33.8%) | 30 (50.8%) | 74 (39.2%) |
| ui |  |  |  |
|  No | 116 (89.2%) | 45 (76.3%) | 161 (85.2%) |
|  Yes | 14 (10.8%) | 14 (23.7%) | 28 (14.8%) |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |

#### Model

``` r

birthwt_uni <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = birthwt_exposures,
  approach = "logit",
  theme = clinical
)

birthwt_multi <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  adjust_for = c("age", "lwt", "race"),
  approach = "logit",
  theme = striped
)

birthwt_multi$table
```

| Characteristic | Adjusted OR (95% CI) | p-value |
|----|----|----|
| ftv_cat |  |  |
| None | — |  |
|  One | 0.60 (0.26–1.38) | 0.230 |
|  Two or more | 0.86 (0.38–1.96) | 0.717 |
| ht |  |  |
| No | — |  |
|  Yes | 5.99 (1.51–23.79) | 0.011 |
| ptl_cat |  |  |
| No | — |  |
|  Yes | 4.49 (1.90–10.58) | \<0.001 |
| smoke |  |  |
| No | — |  |
|  Yes | 2.87 (1.36–6.04) | 0.006 |
| ui |  |  |
| No | — |  |
|  Yes | 2.27 (0.98–5.24) | 0.055 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |
| Adjusted for age, lwt, and race |  |  |
| N = 189 complete observations included across outcome, exposure, and adjustment variables |  |  |

#### Visualise

``` r

plot_reg(
  birthwt_multi,
  title = "Adjusted Regression for Low Birth Weight"
)
```

![](gtregression-intro_files/figure-html/quick-plot-1.png)

### Where To Go Next

- **Descriptive Tables**: build baseline tables users can read.
- **Regression Tables**: create crude and adjusted publication-ready
  outputs.
- **Visualise Results**: plot regression estimates and forest tables.
- **Stratified Analysis**: repeat models across subgroups.
- **Diagnostics**: check convergence, collinearity, and model selection.
- **Confounding & Interaction**: support interpretation and model
  decisions.
- **Customize & Export**: polish and save tables, plots, and reports.
