# Package index

## Prepare and describe data

Import data, check variables before modelling, and build baseline
tables.

- [`import_data()`](https://gtregression.thinkdenominator.com/reference/import_data.md)
  : Import an analysis dataset
- [`dissect()`](https://gtregression.thinkdenominator.com/reference/dissect.md)
  : Dissect a dataset before regression
- [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md)
  : Descriptive summary table

## Regression tables

Crude and adjusted estimates for binary, count and continuous outcomes.

- [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
  : Univariable regression
- [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  : Multivariable regression

## Special study designs

Matched case-control, clustered, ordered and unordered outcomes.

- [`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md)
  : Conditional logistic regression for matched case-control studies
- [`multilevel_reg()`](https://gtregression.thinkdenominator.com/reference/multilevel_reg.md)
  : Random-intercept multilevel regression
- [`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md)
  : Proportional-odds ordinal logistic regression
- [`multinomial_reg()`](https://gtregression.thinkdenominator.com/reference/multinomial_reg.md)
  : Multinomial logistic regression

## Absolute effects

Absolute risks, risk difference and NNTB/NNTH for two-group comparisons.

- [`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md)
  : Risk difference between two groups

## Stratified analysis

The same model repeated within each level of a stratifier.

- [`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md)
  : Stratified univariable regression
- [`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md)
  : Stratified multivariable regression

## Survival: describe and compare

Kaplan-Meier and cumulative incidence curves, summaries and tests.

- [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md)
  : Kaplan-Meier survival plot
- [`km_risk_table()`](https://gtregression.thinkdenominator.com/reference/km_risk_table.md)
  : Kaplan-Meier risk table
- [`survival_summary()`](https://gtregression.thinkdenominator.com/reference/survival_summary.md)
  : Kaplan-Meier survival summary table
- [`survival_quantiles()`](https://gtregression.thinkdenominator.com/reference/survival_quantiles.md)
  : Kaplan-Meier survival quantile table
- [`survival_prob()`](https://gtregression.thinkdenominator.com/reference/survival_prob.md)
  : Kaplan-Meier survival probability table
- [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md)
  : Restricted mean survival time table
- [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md)
  : Log-rank test for Kaplan-Meier survival curves
- [`cuminc_plot()`](https://gtregression.thinkdenominator.com/reference/cuminc_plot.md)
  : Cumulative incidence plot for competing risks

## Survival: models

Cox, parametric and competing-risk regression, with checks and
predictions.

- [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  : Cox proportional hazards regression
- [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  : Parametric survival regression
- [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md)
  : Fine-Gray competing-risk regression
- [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md)
  : Check proportional hazards assumption for Cox models
- [`plot(`*`<gtregression_ph>`*`)`](https://gtregression.thinkdenominator.com/reference/plot.gtregression_ph.md)
  : Plot proportional hazards diagnostics
- [`plot_cox_survival()`](https://gtregression.thinkdenominator.com/reference/plot_cox_survival.md)
  : Plot adjusted survival curves from a Cox model
- [`surv_model_compare()`](https://gtregression.thinkdenominator.com/reference/surv_model_compare.md)
  : Compare parametric survival model distributions
- [`plot_surv_fit()`](https://gtregression.thinkdenominator.com/reference/plot_surv_fit.md)
  : Plot observed and fitted parametric survival curves
- [`surv_predict()`](https://gtregression.thinkdenominator.com/reference/surv_predict.md)
  : Predict survival probabilities from a parametric survival model

## Plots and forest plots

Visualise estimates from one or two regression results.

- [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md)
  : Plot regression estimates
- [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md)
  : Plot crude and adjusted estimates side by side
- [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  : Build a compatible data frame for forest plots
- [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  : Draw a forest table from regression outputs

## Diagnostics and model choice

Convergence, collinearity, fit, and comparison of candidate models.

- [`check_convergence()`](https://gtregression.thinkdenominator.com/reference/check_convergence.md)
  : Check regression model convergence
- [`check_collinearity()`](https://gtregression.thinkdenominator.com/reference/check_collinearity.md)
  : Check collinearity using VIF for fitted models
- [`plot_model_fit()`](https://gtregression.thinkdenominator.com/reference/plot_model_fit.md)
  : Plot model-fit diagnostics
- [`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md)
  : Compare prespecified regression models
- [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md)
  : Stepwise model selection with fit metrics

## Confounding, interaction and mediation

Support causal interpretation of an exposure-outcome association.

- [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md)
  : Identify confounders and effect modifiers
- [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md)
  : Compare models with and without an interaction term
- [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
  : Causal mediation analysis
- [`plot_mediation()`](https://gtregression.thinkdenominator.com/reference/plot_mediation.md)
  : Plot mediation paths

## Customise, merge and export

Relabel tables, combine them, and save tables, plots and Word reports.

- [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  : Modify table labels, headers, captions and notes
- [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
  : Merge two or more tables side by side
- [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
  : Save a single regression or summary table
- [`save_docx()`](https://gtregression.thinkdenominator.com/reference/save_docx.md)
  : Save multiple tables and plots to a Word document
- [`save_plot()`](https://gtregression.thinkdenominator.com/reference/save_plot.md)
  : Save a single plot
- [`save_forest()`](https://gtregression.thinkdenominator.com/reference/save_forest.md)
  : Save a forest_reg() output

## Graphical interface

Run the same analyses from menus.

- [`gtregression_app()`](https://gtregression.thinkdenominator.com/reference/gtregression_app.md)
  : Launch the gtregression app
- [`gtregression_addin()`](https://gtregression.thinkdenominator.com/reference/gtregression_addin.md)
  : Launch the gtregression app from the RStudio Addins menu

## Working with results

Inspect and print the objects returned by gtregression functions.

- [`` `$`( ``*`<gtregression>`*`)`](https://gtregression.thinkdenominator.com/reference/cash-.gtregression.md)
  : Access fields on gtregression objects with \`\$\`
- [`print(`*`<gtregression>`*`)`](https://gtregression.thinkdenominator.com/reference/print.gtregression.md)
  : Print gtregression objects

## Example datasets

Datasets used in the examples and guides.

- [`data_birthwt`](https://gtregression.thinkdenominator.com/reference/data_birthwt.md)
  : Birth Weight Data
- [`data_SynthDiabetes`](https://gtregression.thinkdenominator.com/reference/data_SynthDiabetes.md)
  : Synthetic Diabetes Dataset
- [`data_gt_quin`](https://gtregression.thinkdenominator.com/reference/data_gt_quin.md)
  : Student Absenteeism in Rural Schools
- [`data_epilepsy`](https://gtregression.thinkdenominator.com/reference/data_epilepsy.md)
  : Epilepsy Treatment and Seizure Counts
- [`data_endometrial`](https://gtregression.thinkdenominator.com/reference/data_endometrial.md)
  : Endometrial Cancer Histology Grade Data
- [`data_diabetes_mediation`](https://gtregression.thinkdenominator.com/reference/data_diabetes_mediation.md)
  : Diabetes Mediation Teaching Dataset
- [`data_competing_risk`](https://gtregression.thinkdenominator.com/reference/data_competing_risk.md)
  : Synthetic Competing-Risk Oncology Dataset
- [`data_infertility`](https://gtregression.thinkdenominator.com/reference/data_infertility.md)
  : Infertility Matched Case-Control Study
- [`data_lungcancer`](https://gtregression.thinkdenominator.com/reference/data_lungcancer.md)
  : Lung Cancer Trial Data
