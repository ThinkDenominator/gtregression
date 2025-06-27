---
title: Welcome to gtregression
---

## Function Overview

The `gtregression` package provides regression modeling, diagnostics, plots, and reporting tools designed for usability and accessibility. Here's a summary of its core functions:

### Regression Modeling

| Function Name             | Purpose                                        |
|---------------------------|------------------------------------------------|
| `uni_reg()`               | Univariable regression (OR, RR, IRR, or β)     |
| `multi_reg()`             | Multivariable regression                       |
| `uni_reg_nbin()`          | Univariable negative binomial regression       |
| `multi_reg_nbin()`        | Multivariable negative binomial regression     |
| `stratified_uni_reg()`    | Stratified univariable regression              |
| `stratified_multi_reg()`  | Stratified multivariable regression            |
| `stratified_uni_nbin()`   | Stratified univariable NB regression           |
| `stratified_multi_nbin()` | Stratified multivariable NB regression         |

### Model Diagnostics and Variable Assessment

| Function Name             | Purpose                                        |
|---------------------------|------------------------------------------------|
| `select_models()`         | Stepwise model selection (AIC/BIC/logLik)      |
| `identify_confounder()`   | Confounding assessment via % change or MH      |
| `check_convergence()`     | Evaluate model convergence and max probability |
| `interaction_models()`    | Compare models with and without interactions   |
| `check_collinearity()`    | Check multicollinearity via VIF and pairwise   |

## Plots and Exports

| Function Name        | Purpose                                             |
|----------------------|-----------------------------------------------------|
| `modify_table()`     | Modify `gtsummary` tables (e.g., labels, p-values)  |
| `plot_reg()`         | Forest plot of regression model estimates           |
| `plot_reg_combine()` | Side-by-side plot of univariable and multivariable models |
| `save_table()`       | Save regression tables to Word/HTML                  |
| `save_plot()`        | Save regression plots to file                        |
| `save_docx()`        | Save tables and plots in one report                  |

