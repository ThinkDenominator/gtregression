# gtregression

Publication-ready regression and survival analysis tables, plots, and
forest plots for real-world health data. Fit models, compare estimates,
visualise results, and export manuscript-ready outputs without
hand-formatting every coefficient.

[Start the
workflow](https://gtregression.thinkdenominator.com/articles/gtregression-intro.html)[Explore
functions](https://gtregression.thinkdenominator.com/reference/)

![gtregression logo](reference/figures/gtregression_hex.png)

## Publication-Ready Regression, Survival, and Mediation Outputs

`gtregression` helps you move from model to manuscript: fit regression
models, produce clean tables, visualise estimates, merge outputs, and
export results without hand-formatting every coefficient.

It supports logistic, log-binomial, Poisson, robust Poisson, negative
binomial, linear, Cox, parametric survival, and causal mediation
workflows, including adjusted and stratified models.

| Build | What you get |
|----|----|
| Descriptive tables | Grouped summaries with row or column percentages |
| Regression tables | Crude, adjusted, stratified, linear, Cox, and parametric survival outputs |
| Survival analysis | Kaplan-Meier curves, survival summaries, RMST, log-rank tests, Cox PH checks, and survival predictions |
| Mediation analysis | Direct, indirect, total, and proportion mediated effects with causal caveats |
| Visualisations | Regression plots, survival curves, fitted survival curves, and forest tables |
| Interpretation helpers | Confounding, interaction, mediation, convergence, collinearity, model selection, and survival diagnostics |
| Exports | HTML, PDF, PNG, and Word-ready outputs |

## From Data to Manuscript

One connected workflow

### Start with the question. Finish with a result you can use.

Each step leaves an inspectable object behind, so beginners have a clear
path and experienced analysts retain full control.

01

**Prepare**

Check variables, labels, levels, and missing data.

`dissect(data)` Analysis-ready data

02

**Describe**

Build a clear baseline table before modelling.

`descriptive_table(...)` Table 1

03

**Model**

Fit crude, adjusted, stratified, or survival models.

`uni_reg() + multi_reg()` Effect estimates

04

**Interpret**

Review assumptions, confounding, interaction, and fit.

`check_*() + compare_models()` Defensible model

05

**Publish**

Merge, visualise, and export polished outputs.

`forest_reg() + save_table()` Manuscript-ready output

## Why It Exists

Many students, researchers, and public health analysts need regression
outputs that are readable, reproducible, and report-ready.
`gtregression` keeps the R syntax approachable while preserving
transparent model objects underneath.

## Built on Trusted R Packages

`gtregression` is intentionally a readable interface over established R
packages. The package uses widely trusted modelling, tidying, plotting,
and reporting tools so users can inspect fitted models and understand
the statistical engines behind each output.

| Area | Core packages used |
|----|----|
| Data handling and tidy workflows | `dplyr`, `purrr`, `tibble`, `rlang` |
| Model fitting | `stats`, `MASS`, `survival`, `risks`, `logistf` |
| Robust and diagnostic inference | `sandwich`, `lmtest`, `broom`, `broom.helpers` |
| Tables and Word-ready reporting | `flextable`, `officer`, `gt` |
| Figures and forest plots | `ggplot2`, `patchwork`, `forestploter`, `scales` |
| Optional development and checking tools | `testthat`, `knitr`, `rmarkdown`, `pkgdown`, `car`, `forcats`, `ggtext` |

The user-facing functions return objects with fitted models, table
bodies, and display metadata that advanced users can audit, modify, or
reuse.

## Install

``` r

# CRAN
install.packages("gtregression")

# Development version
remotes::install_github("ThinkDenominator/gtregression")
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

attr(birthwt_data$age, "label") <- "Maternal age"
attr(birthwt_data$lwt, "label") <- "Maternal weight"
attr(birthwt_data$smoke, "label") <- "Smoking during pregnancy"

desc <- descriptive_table(
  birthwt_data,
  exposures = exposures,
  by = "low",
  percent = "column",
  show_overall = "last"
)

uni <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit"
)

multi <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = "logit"
)

plot_reg(multi, title = "Adjusted Regression for Low Birth Weight")

forest_reg = forest_reg(forest_df(uni, multi)) 

merge_tables(desc, uni, multi)
```

Variable labels set with `attr(x, "label")` or
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
are used automatically in display tables and plots, while original
column names remain available internally for merging, modification, and
testing.

Objects stay inspectable:

``` r

desc$table
uni$table
multi$table
multi$models
```

Optional model-fit statistics can be requested without changing the
publication table:

``` r

uni_stats <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  model_stats = TRUE
)

uni_stats$model_stats
```

## Browse by Task

| Task | Start here |
|----|----|
| First workflow | [Start Here](https://gtregression.thinkdenominator.com/articles/gtregression-intro.html) |
| Descriptive summaries | [Descriptive Tables](https://gtregression.thinkdenominator.com/articles/descriptive-tables.html) |
| Regression tables | [Regression Tables](https://gtregression.thinkdenominator.com/articles/regression-tables.html) |
| Survival analysis | [Survival Analysis](https://gtregression.thinkdenominator.com/articles/survival-analysis.html) |
| Causal mediation | [Causal Mediation](https://gtregression.thinkdenominator.com/articles/causal-mediation.html) |
| Visualise estimates | [Visualise Results](https://gtregression.thinkdenominator.com/articles/visualise-results.html) |
| Stratified models | [Stratified Analysis](https://gtregression.thinkdenominator.com/articles/stratified-analysis.html) |
| Diagnostics and selection | [Diagnostics](https://gtregression.thinkdenominator.com/articles/diagnostics-selection.html) |
| Confounding and interaction | [Confounding & Interaction](https://gtregression.thinkdenominator.com/articles/confounding-interaction.html) |
| Merge and export | [Customize and Export](https://gtregression.thinkdenominator.com/articles/customize-export.html) |

## Function Map

| Workflow | Functions |
|----|----|
| Describe | [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md), [`dissect()`](https://gtregression.thinkdenominator.com/reference/dissect.md) |
| Model | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md), [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md), [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md) |
| Survival | [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md), [`km_risk_table()`](https://gtregression.thinkdenominator.com/reference/km_risk_table.md), [`survival_summary()`](https://gtregression.thinkdenominator.com/reference/survival_summary.md), [`survival_quantiles()`](https://gtregression.thinkdenominator.com/reference/survival_quantiles.md), [`survival_prob()`](https://gtregression.thinkdenominator.com/reference/survival_prob.md), [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md), [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md), [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md), [`surv_model_compare()`](https://gtregression.thinkdenominator.com/reference/surv_model_compare.md), [`plot_surv_fit()`](https://gtregression.thinkdenominator.com/reference/plot_surv_fit.md), [`surv_predict()`](https://gtregression.thinkdenominator.com/reference/surv_predict.md) |
| Stratify | [`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md), [`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md) |
| Visualise | [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md), [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md), [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md), [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md) |
| Diagnose | [`check_convergence()`](https://gtregression.thinkdenominator.com/reference/check_convergence.md), [`check_collinearity()`](https://gtregression.thinkdenominator.com/reference/check_collinearity.md), [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md), [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md) |
| Interpret | [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md), [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md), [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md), [`plot_mediation()`](https://gtregression.thinkdenominator.com/reference/plot_mediation.md) |
| Polish and export | [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md), [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md), [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md), [`save_plot()`](https://gtregression.thinkdenominator.com/reference/save_plot.md), [`save_docx()`](https://gtregression.thinkdenominator.com/reference/save_docx.md) |

## Citation

If you use `gtregression` in your work, please cite it as:

Polani R, Eliyas SK, Sakthivel M, Kaviprawin M, Krishnamoorthy Y,
Majella MG. *gtregression: Tools for Creating Publication-Ready
Regression Tables.* Zenodo. <https://doi.org/10.5281/zenodo.16905350>

## Acknowledgements

`gtregression` builds on the R ecosystem, especially `stats`,
`survival`, `MASS`, `risks`, `logistf`, `broom`, `broom.helpers`,
`sandwich`, `lmtest`, `dplyr`, `purrr`, `tibble`, `rlang`, `flextable`,
`officer`, `gt`, `ggplot2`, `patchwork`, `forestploter`, and `scales`.
