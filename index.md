# gtregression

Publication-ready regression tables and forest plots for real-world
health data. Fit models, compare estimates, visualise results, and
export manuscript-ready outputs without hand-formatting every
coefficient.

[Start the
workflow](https://thinkdenominator.github.io/gtregression/articles/gtregression-intro.md)[Explore
functions](https://thinkdenominator.github.io/gtregression/reference/index.md)

![gtregression logo](reference/figures/gtregression_hex.png)

## Publication-Ready Regression Tables and Plots

`gtregression` helps you move from model to manuscript: fit regression
models, produce clean tables, visualise estimates, merge outputs, and
export results without hand-formatting every coefficient.

It supports logistic, log-binomial, Poisson, robust Poisson, negative
binomial, linear, adjusted, and stratified regression workflows.

| Build | What you get |
|----|----|
| Descriptive tables | Grouped summaries with row or column percentages |
| Regression tables | Crude, adjusted, stratified, and linear model outputs |
| Visualisations | Forest plots and publication-style forest tables |
| Interpretation helpers | Confounding, interaction, convergence, and collinearity checks |
| Exports | HTML, PDF, PNG, and Word-ready outputs |

**Describe**Build baseline tables with grouped summaries.

**Model**Fit crude, adjusted, and stratified regressions.

**Visualise**Create plots and forest tables for estimates.

**Export**Save polished tables, figures, and Word outputs.

## Why It Exists

Many students, researchers, and public health analysts need regression
outputs that are readable, reproducible, and report-ready.
`gtregression` keeps the R syntax beginner-friendly while preserving
transparent model objects underneath.

## Install

``` r

install.packages("gtregression")

# Development version
devtools::install_github("ThinkDenominator/gtregression")
```

## Five-Minute Workflow

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
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

exposures <- c("age", "lwt", "race", "smoke", "ht", "ui")

desc <- descriptive_table(
  birthwt_data,
  exposures = exposures,
  by = "low",
  percent = column,
  show_overall = last
)

uni <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = logit
)

multi <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit
)

plot_reg(multi, title = "Adjusted Regression for Low Birth Weight")
```

Objects stay inspectable:

``` r

desc$table
uni$table
multi$table
multi$models
```

## Browse by Task

| Task | Start here |
|----|----|
| First workflow | [Start Here](https://thinkdenominator.github.io/gtregression/articles/gtregression-intro.html) |
| Descriptive summaries | [Descriptive Tables](https://thinkdenominator.github.io/gtregression/articles/descriptive-tables.html) |
| Regression tables | [Regression Tables](https://thinkdenominator.github.io/gtregression/articles/regression-tables.html) |
| Visualise estimates | [Visualise Results](https://thinkdenominator.github.io/gtregression/articles/visualise-results.html) |
| Stratified models | [Stratified Analysis](https://thinkdenominator.github.io/gtregression/articles/stratified-analysis.html) |
| Diagnostics and selection | [Diagnostics](https://thinkdenominator.github.io/gtregression/articles/diagnostics-selection.html) |
| Confounding and interaction | [Interpret](https://thinkdenominator.github.io/gtregression/articles/confounding-interaction.html) |
| Merge and export | [Customize and Export](https://thinkdenominator.github.io/gtregression/articles/customize-export.html) |

## Function Map

| Workflow | Functions |
|----|----|
| Describe | [`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md), [`dissect()`](https://thinkdenominator.github.io/gtregression/reference/dissect.md) |
| Model | [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md), [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md) |
| Stratify | [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md), [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md) |
| Visualise | [`plot_reg()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg.md), [`plot_reg_combine()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg_combine.md), [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md), [`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md) |
| Diagnose | [`check_convergence()`](https://thinkdenominator.github.io/gtregression/reference/check_convergence.md), [`check_collinearity()`](https://thinkdenominator.github.io/gtregression/reference/check_collinearity.md), [`select_models()`](https://thinkdenominator.github.io/gtregression/reference/select_models.md) |
| Interpret | [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md), [`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md) |
| Polish and export | [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md), [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md), [`save_table()`](https://thinkdenominator.github.io/gtregression/reference/save_table.md), [`save_plot()`](https://thinkdenominator.github.io/gtregression/reference/save_plot.md), [`save_docx()`](https://thinkdenominator.github.io/gtregression/reference/save_docx.md) |

## Citation

If you use `gtregression` in your work, please cite it as:

Polani R, Eliyas SK, Sakthivel M, Krishnamoorthy Y, Majella MG.
*gtregression: Tools for Creating Publication-Ready Regression Tables.*
Zenodo. <https://doi.org/10.5281/zenodo.16905350>

## Acknowledgements

`gtregression` builds on the R ecosystem, including `gtsummary`, `gt`,
`flextable`, `MASS`, `risks`, `dplyr`, `ggplot2`, and related packages.
