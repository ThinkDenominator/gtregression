# Package index

## Describe and Check Data

Create baseline descriptive tables and inspect variable compatibility
before modeling.

- [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md)
  : Descriptive Summary Table (no gtsummary) using gt/flextable
- [`dissect()`](https://gtregression.thinkdenominator.com/reference/dissect.md)
  : Dissect a dataset before regression

## Regression Tables

Create publication-ready crude, adjusted, and multivariable regression
tables.

- [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
  : Univariate regression
- [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  : Multivariable regression
- [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  : Cox proportional hazards regression
- [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  : Parametric survival regression

## Stratified Regression Tables

Repeat univariable or adjusted models within levels of a stratifier.

- [`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md)
  : Stratified univariable regression
- [`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md)
  : Stratified multivariable regression

## Visualise Regression Results

Turn regression and survival outputs into curves, forest plots, and
publication-style forest tables.

- [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md)
  : Kaplan-Meier survival plot
- [`km_risk_table()`](https://gtregression.thinkdenominator.com/reference/km_risk_table.md)
  : Kaplan-Meier risk table
- [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md)
  : Restricted mean survival time table
- [`survival_summary()`](https://gtregression.thinkdenominator.com/reference/survival_summary.md)
  : Kaplan-Meier survival summary table
- [`survival_quantiles()`](https://gtregression.thinkdenominator.com/reference/survival_quantiles.md)
  : Kaplan-Meier survival quantile table
- [`survival_prob()`](https://gtregression.thinkdenominator.com/reference/survival_prob.md)
  : Kaplan-Meier survival probability table
- [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md)
  : Log-rank test for Kaplan-Meier survival curves
- [`surv_model_compare()`](https://gtregression.thinkdenominator.com/reference/surv_model_compare.md)
  : Compare parametric survival model distributions
- [`plot_surv_fit()`](https://gtregression.thinkdenominator.com/reference/plot_surv_fit.md)
  : Plot observed and fitted parametric survival curves
- [`surv_predict()`](https://gtregression.thinkdenominator.com/reference/surv_predict.md)
  : Predict survival probabilities from a parametric survival model
- [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md)
  : Plot regression estimates
- [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md)
  : Side-by-side forest plots: univariate vs multivariable
- [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  : Build a compatible data frame for forest plots
- [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  : Draw a forest table from regression outputs

## Diagnostics and Model Selection

Check convergence, collinearity, model fit, and candidate model paths.

- [`check_convergence()`](https://gtregression.thinkdenominator.com/reference/check_convergence.md)
  : Check regression model convergence
- [`check_collinearity()`](https://gtregression.thinkdenominator.com/reference/check_collinearity.md)
  : Check collinearity using VIF for fitted models
- [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md)
  : Check proportional hazards assumption for Cox models
- [`plot_model_fit()`](https://gtregression.thinkdenominator.com/reference/plot_model_fit.md)
  : Plot model-fit diagnostics
- [`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md)
  : Compare Prespecified Regression Models
- [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md)
  : Stepwise model selection with fit metrics

## Confounding and Interaction

Support interpretation with confounding, interaction, and mediation
workflows.

- [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md)
  : Identify confounders and effect modifiers
- [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md)
  : Compare Models With and Without an Interaction Term
- [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
  : Causal mediation analysis
- [`plot_mediation()`](https://gtregression.thinkdenominator.com/reference/plot_mediation.md)
  : Plot mediation paths

## Modify, Merge, and Export

Polish tables, combine outputs, launch the app, and save tables, plots,
or Word reports.

- [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  : Modify Regression/Descriptive Tables (labels, headers, caption,
  notes)
- [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
  : Merge gtregression tables and preserve structure and notes
- [`gtregression_app()`](https://gtregression.thinkdenominator.com/reference/gtregression_app.md)
  : Launch the gtregression app
- [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
  : Save a single regression or summary table
- [`save_docx()`](https://gtregression.thinkdenominator.com/reference/save_docx.md)
  : Save multiple tables and plots to a Word document
- [`save_plot()`](https://gtregression.thinkdenominator.com/reference/save_plot.md)
  : Save a single plot
- [`save_forest()`](https://gtregression.thinkdenominator.com/reference/save_forest.md)
  : Save a forest_reg() output

## Object Helpers

Inspect and print objects returned by gtregression functions.

- [`` `$`( ``*`<gtregression>`*`)`](https://gtregression.thinkdenominator.com/reference/cash-.gtregression.md)
  : Access fields on gtregression objects with \`\$\`
- [`print(`*`<gtregression>`*`)`](https://gtregression.thinkdenominator.com/reference/print.gtregression.md)
  : Print gtregression objects (unified)

## Example Datasets

Built-in datasets for examples, teaching, and tests.

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
- [`data_infertility`](https://gtregression.thinkdenominator.com/reference/data_infertility.md)
  : Infertility Matched Case-Control Study
- [`data_lungcancer`](https://gtregression.thinkdenominator.com/reference/data_lungcancer.md)
  : Lung Cancer Trial Data
