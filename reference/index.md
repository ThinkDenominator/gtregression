# Package index

## Describe and Check Data

Create baseline descriptive tables and inspect variable compatibility
before modeling.

- [`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md)
  : Descriptive Summary Table (no gtsummary) using gt/flextable
- [`dissect()`](https://thinkdenominator.github.io/gtregression/reference/dissect.md)
  : Dissect a dataset before regression

## Regression Tables

Create publication-ready crude, adjusted, and multivariable regression
tables.

- [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md)
  : Univariate regression
- [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
  : Multivariable regression
- [`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md)
  : Cox proportional hazards regression
- [`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md)
  : Parametric survival regression

## Stratified Regression Tables

Repeat univariable or adjusted models within levels of a stratifier.

- [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md)
  : Stratified univariable regression
- [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md)
  : Stratified multivariable regression

## Visualise Regression Results

Turn regression and survival outputs into curves, forest plots, and
publication-style forest tables.

- [`km_plot()`](https://thinkdenominator.github.io/gtregression/reference/km_plot.md)
  : Kaplan-Meier survival plot
- [`km_risk_table()`](https://thinkdenominator.github.io/gtregression/reference/km_risk_table.md)
  : Kaplan-Meier risk table
- [`rmst_table()`](https://thinkdenominator.github.io/gtregression/reference/rmst_table.md)
  : Restricted mean survival time table
- [`survival_summary()`](https://thinkdenominator.github.io/gtregression/reference/survival_summary.md)
  : Kaplan-Meier survival summary table
- [`survival_quantiles()`](https://thinkdenominator.github.io/gtregression/reference/survival_quantiles.md)
  : Kaplan-Meier survival quantile table
- [`survival_prob()`](https://thinkdenominator.github.io/gtregression/reference/survival_prob.md)
  : Kaplan-Meier survival probability table
- [`logrank_test()`](https://thinkdenominator.github.io/gtregression/reference/logrank_test.md)
  : Log-rank test for Kaplan-Meier survival curves
- [`surv_model_compare()`](https://thinkdenominator.github.io/gtregression/reference/surv_model_compare.md)
  : Compare parametric survival model distributions
- [`plot_surv_fit()`](https://thinkdenominator.github.io/gtregression/reference/plot_surv_fit.md)
  : Plot observed and fitted parametric survival curves
- [`surv_predict()`](https://thinkdenominator.github.io/gtregression/reference/surv_predict.md)
  : Predict survival probabilities from a parametric survival model
- [`plot_reg()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg.md)
  : Visualize a regression model as a forest plot
- [`plot_reg_combine()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg_combine.md)
  : Side-by-side forest plots: univariate vs multivariable
- [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md)
  : Build a compatible data frame for forest plots
- [`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md)
  : Draw a publication-ready forest plot

## Diagnostics and Model Selection

Check convergence, collinearity, model fit, and candidate model paths.

- [`check_convergence()`](https://thinkdenominator.github.io/gtregression/reference/check_convergence.md)
  : Check Convergence for a Regression Model
- [`check_collinearity()`](https://thinkdenominator.github.io/gtregression/reference/check_collinearity.md)
  : Check collinearity using VIF for fitted models
- [`check_ph()`](https://thinkdenominator.github.io/gtregression/reference/check_ph.md)
  : Check proportional hazards assumption for Cox models
- [`plot_model_fit()`](https://thinkdenominator.github.io/gtregression/reference/plot_model_fit.md)
  : Plot Model Fit Diagnostics
- [`select_models()`](https://thinkdenominator.github.io/gtregression/reference/select_models.md)
  : Stepwise Model Selection with Evaluation Metrics

## Confounding and Interaction

Support interpretation with confounding and interaction checks.

- [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md)
  : Identify confounders and effect modifiers
- [`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md)
  : Compare Models With and Without an Interaction Term

## Modify, Merge, and Export

Polish tables, combine outputs, and save tables, plots, or Word reports.

- [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md)
  : Modify Regression/Descriptive Tables (labels, headers, caption,
  notes)
- [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md)
  : Merge gtregression tables and preserve structure and notes
- [`save_table()`](https://thinkdenominator.github.io/gtregression/reference/save_table.md)
  : Save a single regression or summary table
- [`save_docx()`](https://thinkdenominator.github.io/gtregression/reference/save_docx.md)
  : Save multiple tables and plots to a Word document
- [`save_plot()`](https://thinkdenominator.github.io/gtregression/reference/save_plot.md)
  : Save a single plot

## Object Helpers

Inspect and print objects returned by gtregression functions.

- [`` `$`( ``*`<gtregression>`*`)`](https://thinkdenominator.github.io/gtregression/reference/cash-.gtregression.md)
  : Access fields on gtregression objects with \`\$\`
- [`print(`*`<gtregression>`*`)`](https://thinkdenominator.github.io/gtregression/reference/print.gtregression.md)
  : Print gtregression objects (unified)

## Example Datasets

Built-in datasets for examples, teaching, and tests.

- [`data_birthwt`](https://thinkdenominator.github.io/gtregression/reference/data_birthwt.md)
  : Birth Weight Data
- [`data_PimaIndiansDiabetes`](https://thinkdenominator.github.io/gtregression/reference/data_PimaIndiansDiabetes.md)
  : PimaIndians2 Diabetes Dataset
- [`data_gt_quin`](https://thinkdenominator.github.io/gtregression/reference/data_gt_quin.md)
  : Student Absenteeism in Rural Schools
- [`data_epilepsy`](https://thinkdenominator.github.io/gtregression/reference/data_epilepsy.md)
  : Epilepsy Treatment and Seizure Counts
- [`data_endometrial`](https://thinkdenominator.github.io/gtregression/reference/data_endometrial.md)
  : Endometrial Cancer Histology Grade Data
- [`data_infertility`](https://thinkdenominator.github.io/gtregression/reference/data_infertility.md)
  : Infertility Matched Case-Control Study
- [`data_lungcancer`](https://thinkdenominator.github.io/gtregression/reference/data_lungcancer.md)
  : Lung Cancer Trial Data
