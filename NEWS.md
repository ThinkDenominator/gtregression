# gtregression 1.1.0

## Added

* Added an internal Data Prep workflow to the gtregression app, with explicit
  original/prepared analysis-data selection, quick starts, undo/redo, confirmed
  reset, prepared-data download, and reproducible preparation code. This code is
  included directly in gtregression and does not add gtstats as a dependency.
* Added observed-level reference category controls for categorical regression
  predictors in the app. Selected baselines are applied with reproducible factor
  releveling and included in generated and exported R scripts.
* Added publication-ready `flextable` output as the default table format across
  package table functions, while retaining `gt` and tibble-style outputs where
  appropriate.
* Added support for unquoted option values in common arguments, including model
  approaches and output formats, so users can write calls such as
  `approach = logit` and `format = flextable`.
* Added `adjust_for` support for adjusted multivariable workflows, including
  downstream regression plots and combined regression plots.
* Added adjusted-variable notes to regression plots and combined regression
  plots.
* Added optional model-fit statistics for `uni_reg()` and `multi_reg()` via
  `model_stats = TRUE`, with AIC, BIC, log-likelihood, deviance, pseudo
  R-squared, linear-model R-squared, and model N stored in `$model_stats`.
* Added `approach = "firth"` / `approach = firth` for Firth penalized logistic
  regression in `uni_reg()`, `multi_reg()`, `stratified_uni_reg()`, and
  `stratified_multi_reg()`, with OR tables and compatibility with regression
  plots and forest plot helpers.
* Added `data_endometrial`, a classic endometrial cancer dataset with a
  separation pattern, for teaching and testing Firth logistic regression.
* Added `cox_reg()` for Cox proportional hazards regression using `time`,
  `event`, `exposures`, and optional `adjust_for`, returning HR and adjusted HR
  tables.
* Added `surv_reg()` for parametric survival regression using
  `time`, `event`, `exposures`, optional `adjust_for`, and a selectable survival
  distribution, returning time-ratio tables.
* Added stratified Cox and parametric survival workflows through the
  `stratifier` argument in `cox_reg()` and `surv_reg()`, with stratum-specific
  N, event counts, and formatted regression tables.
* Added `km_plot()` for Kaplan-Meier survival curves with optional confidence
  intervals, censoring marks, log-rank p-values, and number-at-risk tables.
* Added more publication controls to `km_plot()`, including y-axis limits,
  percentage-scale display, optional grid removal, theme selection, confidence
  interval styling, and title/subtitle sizing for patchwork-style figure panels.
* Added `km_risk_table()` for standalone Kaplan-Meier risk tables at requested
  follow-up times, with at-risk, event, and censored counts.
* Added `rmst_table()` for restricted mean survival time summaries up to a
  chosen follow-up time, with optional two-group RMST difference.
* Added `surv_model_compare()` for comparing candidate parametric survival
  distributions by AIC, BIC, log-likelihood, scale, events, and N.
* Added `plot_surv_fit()` for visually comparing observed Kaplan-Meier curves
  with fitted parametric survival curves.
* Added `surv_predict()` for model-based survival probability predictions from
  fitted parametric survival regression models.
* Added `survival_summary()` for Kaplan-Meier median survival summaries with
  total N, events, censored counts, and publication-style table outputs.
* Added `survival_quantiles()` for Kaplan-Meier survival time quantiles,
  including event percentiles, corresponding survival probabilities, and
  confidence intervals.
* Added `survival_prob()` for Kaplan-Meier survival probabilities at fixed
  follow-up times, with at-risk, event, censored, and confidence interval
  columns.
* Added `logrank_test()` for formal comparison of Kaplan-Meier survival curves,
  with observed and expected events plus formatted p-value output.
* Added `check_ph()` for proportional hazards screening of Cox models using
  Schoenfeld residual tests, with flextable, gt, and tibble outputs.
* Added `plot_model_fit()` for visual model diagnostics from fitted `lm`/`glm`
  models and from models stored in `uni_reg()` and `multi_reg()` outputs.
* Added `mediation_analysis()` for regression-based mediation analysis with
  formatted direct, indirect, total, and proportion mediated tables.
* Added `plot_mediation()` for drawing a mediation path diagram from
  `mediation_analysis()` outputs.
* Added `data_diabetes_mediation`, a health-related diabetes teaching dataset
  for practicing obesity, glucose, and diabetes mediation workflows.
* Added `compare_models()` for publication-ready comparison of fitted
  gtregression model outputs, including AIC, BIC, log-likelihood, likelihood
  ratio statistics, primary exposure estimates, percent change, analysis-sample
  checks, and highlighted best-fit summaries.
* Added a guided candidate-model builder to the app's Compare Models workflow.
  Users can name and fit two to six logistic, linear, count, Cox, or parametric
  survival candidates with model-specific exposures, adjustment variables, and
  optional interactions before producing a reproducible comparison table and
  complete copyable R code.
* Added `save_forest()` for exporting `forest_reg()` outputs with reproducible
  sizing across graphics devices and operating systems.
* Added automatic support for variable label attributes, including labels set
  with `labelled::var_label()`, across descriptive, regression, stratified,
  merged, plotted, and forest-style outputs.
* Added formatted display outputs for diagnostic and review functions
  including `dissect()`, `select_models()`, `interaction_models()`, and
  `identify_confounder()`.
* Added Mantel-Haenszel comparison support to `identify_confounder()` to support
  confounding assessment alongside crude and adjusted model comparisons.
* Added manual case-study scripts under `dev/manual-tests/` for real-time testing
  of logistic, linear, log-binomial, robust Poisson, Poisson, negative binomial,
  Cox, parametric survival, Firth, and mediation workflows.

## Changed

* Improved `descriptive_table()`, `uni_reg()`, `multi_reg()`,
  `stratified_uni_reg()`, and `stratified_multi_reg()` documentation with clearer
  examples using package datasets.
* Improved pkgdown organization, homepage content, reference grouping, and
  rendered examples to better show publication-ready tables and plots.
* Improved README and pkgdown documentation with an explicit dependency overview
  showing the trusted R packages used for modelling, tidying, tables, Word
  export, figures, diagnostics, and forest plots.
* Improved `merge_tables()` so descriptive, crude, and adjusted tables can be
  combined more reliably, including when visible variable labels differ between
  input tables.
* Improved `modify_table()` so merged tables retain clean headers, spanners, and
  footnotes after relabelling.
* Improved `forest_df()` and `forest_reg()` support for descriptive summaries
  combined with univariable and multivariable regression outputs.
* Improved `forest_df()` and `forest_reg()` support for stratified regression
  outputs, including one-object stratified forest plots with highlighted stratum
  headers and preserved row order.
* Improved downstream survival support so `cox_reg()` and `surv_reg()` outputs
  work with `plot_reg()`, `plot_reg_combine()`, `forest_df()`, `forest_reg()`,
  `merge_tables()`, `modify_table()`, and `select_models()`.
* Improved `cox_reg()` and `surv_reg()` consistency with the rest of the package:
  both now support single multivariable models, adjusted exposure workflows,
  interaction terms, stratified workflows, and coherent table labels.
* Improved `compare_models()` output so user-supplied or object-derived model
  names are displayed instead of generic model labels, and context-aware warnings
  distinguish same-sample comparisons from different-sample comparisons.
* Improved `save_table()` handling for wide Word tables by preferring landscape
  orientation before reducing font size, respecting minimum font sizes, and
  allowing users to turn width fitting off.
* Improved table footer and source-note spacing across formatted
  tables so abbreviations, adjustment notes, and caveats render more compactly
  in flextable, gt, Word, and pkgdown outputs.
* Improved Word export so flextable outputs are fitted to a standard Word page
  width by default, with `save_docx(table_width = ...)` available for custom
  document layouts.
* Improved `select_models()` output so formatted tables clearly report the model
  selection direction used.
* Improved repository organization for CRAN readiness, including excluding
  development-only manual scripts from package builds.

## Fixed

* Fixed `merge_tables()` flextable headers so internal merge suffixes such as
  `_p1`, `_p2`, and `_p3` are not shown in rendered tables.
* Fixed `merge_tables()` row alignment when descriptive and regression tables
  contain the same variables but different visible labels.
* Fixed `modify_table()` handling of merged flextable outputs so clean subheaders
  and group spanners are preserved.
* Fixed `check_convergence()` so failed multivariable model fits return a clear
  non-converged table rather than failing during table formatting.
* Fixed pkgdown reference metadata for internal print methods.
* Fixed save functions so files are written to a temporary directory when users do
  not provide an explicit destination, supporting CRAN-safe examples and tests.
* Fixed univariable Cox regression preprocessing so each exposure is fitted on
  complete cases for `time`, `event`, and the current exposure rather than using
  a single complete-case dataset across all exposures.
* Fixed Cox and parametric survival validation so zero follow-up times are
  allowed when accepted by the underlying `survival` model functions, while
  negative follow-up times remain invalid.
* Fixed regression and publication-ready outputs so user-supplied variable order
  is preserved rather than alphabetically reordered.
* Fixed forest plot merging so adjusted reference rows are left blank for
  variables that were not included in the adjusted model.
* Fixed `forest_df()` row-order handling after joins and merges so forest plots
  follow the same display order as the source regression table.

# gtregression 1.0.0

* Initial release of gtregression.
* Added support for logit, log-binomial, linear, Poisson, robust Poisson, and
  negative binomial models.
* Added functions for confounder identification, model selection, diagnostics,
  plotting, and saving output.
* Added forest plot functions (`plot_reg()`, `plot_reg_combine()`).
* Added initial documentation and pkgdown homepage.
