# gtregression

R package for health research

From model to manuscript. Fit regression and survival models, and get
tables, forest plots and survival curves ready for your paper, without
hand-formatting a single coefficient.

[Get
started](https://gtregression.thinkdenominator.com/articles/gtregression-intro.html)[Which
function?](https://gtregression.thinkdenominator.com/articles/choose-model.html)

`install.packages("gtregression")`

![gtregression logo](reference/figures/gtregression_hex.png)

## What you can make

[![Merged descriptive, crude and adjusted regression
table](reference/figures/home-table.png)Manuscript tables Descriptive,
crude and adjusted results merged into one table, with every footnote
kept.](https://gtregression.thinkdenominator.com/articles/merge-tables.html)

[![Forest plot of crude and adjusted odds
ratios](reference/figures/home-forest.png)Forest plots Crude and
adjusted estimates side by side, straight from the fitted
models.](https://gtregression.thinkdenominator.com/articles/visualise-results.html)

[![Kaplan-Meier curves with a number-at-risk
table](reference/figures/home-km.png)Survival analysis Kaplan-Meier
curves, Cox and parametric models, competing risks and
RMST.](https://gtregression.thinkdenominator.com/articles/survival-analysis.html)

[![Adjusted risk difference table with number needed to
harm](reference/figures/home-rd.png)Absolute effects Risk differences
and the number needed to treat, crude or
adjusted.](https://gtregression.thinkdenominator.com/articles/risk-difference.html)

## New in 1.2.0

**Risk difference and NNT**
[`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md)
reports absolute risks, the risk difference and NNTB/NNTH, crude or
adjusted.

**More study designs** Matched case-control, multilevel, ordinal,
multinomial and competing-risk regression tables.

**Richer survival tools** Proportional-hazards plots, delayed entry,
clustering, weights and adjusted survival curves.

**Better merging** Merge any number of tables, keep each block’s notes,
and choose a table theme.

[Read the full list of
changes](https://gtregression.thinkdenominator.com/news/index.html).

## From data to manuscript

01

**Prepare**

Check variables, labels, levels and missing data.

[`dissect()`](https://gtregression.thinkdenominator.com/reference/dissect.md)
Analysis-ready data

02

**Describe**

Build a clear baseline table before modelling.

[`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md)
Table 1

03

**Model**

Fit crude, adjusted, stratified or survival models.

[`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
Effect estimates

04

**Interpret**

Check assumptions, confounding, interaction and fit.

[`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md)
[`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md)
A defensible model

05

**Publish**

Merge, visualise and export.

[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
[`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
Manuscript-ready output

## A five-minute example

``` r

library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")

birthwt <- data_birthwt |>
  mutate(
    race = factor(race, levels = c(1, 2, 3), labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

attr(birthwt$smoke, "label") <- "Smoking during pregnancy"
exposures <- c("age", "lwt", "race", "smoke", "ht")

# Baseline characteristics by outcome
desc <- descriptive_table(birthwt, exposures = exposures, by = "low",
                          show_dichotomous = "all_levels")

# One model per exposure, then one adjusted model
crude <- uni_reg(birthwt, outcome = "low", exposures = exposures, approach = "logit")
adjusted <- multi_reg(birthwt, outcome = "low", exposures = exposures, approach = "logit")

# One table for the paper, and a forest plot
table_2 <- merge_tables(desc, crude, adjusted,
                        spanners = c("Low birth weight", "Crude", "Adjusted"))
forest <- forest_reg(forest_df(crude, adjusted))

# Save
save_table(table_2, filename = "table-2", format = "docx")
save_forest(forest, filename = "forest-plot", format = "png")
```

Variable labels set with `attr(x, "label")` or
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
appear in every table and plot. Each result keeps its fitted models
(`adjusted$models`) and table data (`adjusted$table_body`), so nothing
is hidden.

## Find your way

[**Describe data**Baseline tables with row or column
percentages.](https://gtregression.thinkdenominator.com/articles/descriptive-tables.html)

[**Regression tables**Logistic, risk ratio, count, linear and Firth
models.](https://gtregression.thinkdenominator.com/articles/regression-tables.html)

[**Special designs**Matched, multilevel, ordinal, multinomial and
competing
risks.](https://gtregression.thinkdenominator.com/articles/special-models.html)

[**Survival analysis**Kaplan-Meier, Cox, parametric models and
predictions.](https://gtregression.thinkdenominator.com/articles/survival-analysis.html)

[**Risk difference and NNT**Absolute effects for two-group
comparisons.](https://gtregression.thinkdenominator.com/articles/risk-difference.html)

[**Stratified analysis**The same model within each
subgroup.](https://gtregression.thinkdenominator.com/articles/stratified-analysis.html)

[**Confounding and interaction**Screen confounders and test effect
modification.](https://gtregression.thinkdenominator.com/articles/confounding-interaction.html)

[**Causal mediation**Direct and indirect effects with clear
caveats.](https://gtregression.thinkdenominator.com/articles/causal-mediation.html)

[**Diagnostics**Convergence, collinearity, fit and model
comparison.](https://gtregression.thinkdenominator.com/articles/diagnostics-selection.html)

[**Visualise**Regression plots, forest plots and survival
curves.](https://gtregression.thinkdenominator.com/articles/visualise-results.html)

[**Merge and customise**Combine tables, apply themes, style with
flextable.](https://gtregression.thinkdenominator.com/articles/merge-tables.html)

[**Export**Word, HTML, PDF and image
output.](https://gtregression.thinkdenominator.com/articles/customize-export.html)

## Built on trusted R packages

`gtregression` is a readable interface over established packages:
`stats`, `survival`, `MASS`, `risks` and `logistf` fit the models;
`broom`, `sandwich` and `lmtest` handle inference; `flextable`,
`officer` and `gt` build the tables; `ggplot2`, `patchwork` and
`forestploter` draw the figures. The fitted models stay inside every
result, so each number can be traced back to its source.

## Install

``` r

# CRAN
install.packages("gtregression")

# Development version
remotes::install_github("ThinkDenominator/gtregression")
```

## Citation

If you use `gtregression` in your work, please cite it as:

Polani R, Eliyas SK, Sakthivel M, Kaviprawin M, Krishnamoorthy Y,
Majella MG. *gtregression: Tools for Creating Publication-Ready
Regression Tables.* Zenodo. <https://doi.org/10.5281/zenodo.16905349>
