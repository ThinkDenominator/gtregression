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

## Stratified Regression Tables

Repeat univariable or adjusted models within levels of a stratifier.

- [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md)
  : Stratified univariable regression
- [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md)
  : Stratified multivariable regression

## Visualise Regression Results

Turn regression tables into forest plots and publication-style forest
tables.

- [`plot_reg()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg.md)
  : Visualize a regression model as a forest plot
- [`plot_reg_combine()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg_combine.md)
  : Side-by-side forest plots: univariate vs multivariable
- [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md)
  : Build a compatible data frame for forest plots
- [`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md)
  : Draw a publication-ready forest plot

## Diagnostics and Model Selection

Check convergence, collinearity, and candidate model paths.

- [`check_convergence()`](https://thinkdenominator.github.io/gtregression/reference/check_convergence.md)
  : Check Convergence for a Regression Model
- [`check_collinearity()`](https://thinkdenominator.github.io/gtregression/reference/check_collinearity.md)
  : Check collinearity using VIF for fitted models
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
- [`data_infertility`](https://thinkdenominator.github.io/gtregression/reference/data_infertility.md)
  : Infertility Matched Case-Control Study
- [`data_lungcancer`](https://thinkdenominator.github.io/gtregression/reference/data_lungcancer.md)
  : Lung Cancer Trial Data
