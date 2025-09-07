---
title: Welcome to gtregression
---

## Function Overview

The `gtregression` package provides regression modeling, diagnostics, plots, and
reporting tools designed for usability and accessibility. 
Here's a summary of its core functions:

### Descriptive & Compatibility Tools

| Function Name        | Purpose                               |
|----------------------|---------------------------------------|
| `descriptive_table()`| Summarise exposures by outcome groups |
| `dissect()`          | Check outcome-exposure compatibility  |

### Regression Functions - Fit univariate and multivariable models

| Function Name | Purpose                              |
|---------------|--------------------------------------|
| `uni_reg()`   | Univariable regression (OR/RR/IRR/β) |
| `multi_reg()` | Multivariable regression             |

### Regression Functions by stratifier

| Function Name            | Purpose                             |
|--------------------------|-------------------------------------|
| `stratified_uni_reg()`   | Stratified univariable regression   |
| `stratified_multi_reg()` | Stratified multivariable regression |

### Model Diagnostics & Selection

| Function Name         | Purpose                                          |
|-----------------------|--------------------------------------------------|
| `check_convergence()` | Evaluate model convergence and max fitted values |
| `select_models()`     | Stepwise model selection (AIC/BIC/adjusted R²)   |

### Confounding & Interaction

| Function Name           | Purpose                                           |
|------------------------|------------------------------------------------|
| `identify_confounder()` | Confounding assessment via % change or MH method  |
| `interaction_models()`  | Compare models with and without interaction terms |

### Plots & Exports

| Function Name        | Purpose                                        |
|----------------------|------------------------------------------------|
| `plot_reg()`         | Forest plot for a single regression model      |
| `plot_reg_combine()` | Side-by-side forest plots for uni/multi models |
| `modify_table()`     | Customize column labels or output structure    |
| `save_table()`       | Export table to `.html`, `.csv`, `.docx`       |
| `save_docx()`        | Save table as Word document (`.docx`)          |
| `save_plot()`        | Save plot as `.png`, `.pdf`, etc.              |
| `merge_tables()`     | Combine descriptive and regression tables      |


