# Changelog

## gtregression 1.1.0

### Added

- Added publication-ready `flextable` output as the default table format
  across package table functions, while retaining `gt` and tibble-style
  outputs where appropriate.
- Added support for unquoted option values in common arguments,
  including model approaches and output formats, so users can write
  calls such as `approach = logit` and `format = flextable`.
- Added `adjust_for` support for adjusted multivariable workflows,
  including downstream regression plots and combined regression plots.
- Added adjusted-variable notes to regression plots and combined
  regression plots.
- Added optional model-fit statistics for
  [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md)
  and
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
  via `model_stats = TRUE`, with AIC, BIC, log-likelihood, deviance,
  pseudo R-squared, linear-model R-squared, and model N stored in
  `$model_stats`.
- Added `approach = "firth"` / `approach = firth` for Firth penalized
  logistic regression in
  [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md),
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
  [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md),
  and
  [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md),
  with OR tables and compatibility with regression plots and forest plot
  helpers.
- Added `data_endometrial`, a classic endometrial cancer dataset with a
  separation pattern, for teaching and testing Firth logistic
  regression.
- Added
  [`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md)
  for Cox proportional hazards regression using `time`, `event`,
  `exposures`, and optional `adjust_for`, returning HR and adjusted HR
  tables.
- Added
  [`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md)
  for parametric survival regression using `time`, `event`, `exposures`,
  optional `adjust_for`, and a selectable survival distribution,
  returning time-ratio tables.
- Added
  [`km_plot()`](https://thinkdenominator.github.io/gtregression/reference/km_plot.md)
  for Kaplan-Meier survival curves with optional confidence intervals,
  censoring marks, log-rank p-values, and number-at-risk tables.
- Added
  [`km_risk_table()`](https://thinkdenominator.github.io/gtregression/reference/km_risk_table.md)
  for standalone Kaplan-Meier risk tables at requested follow-up times,
  with at-risk, event, and censored counts.
- Added
  [`rmst_table()`](https://thinkdenominator.github.io/gtregression/reference/rmst_table.md)
  for restricted mean survival time summaries up to a chosen follow-up
  time, with optional two-group RMST difference.
- Added
  [`surv_model_compare()`](https://thinkdenominator.github.io/gtregression/reference/surv_model_compare.md)
  for comparing candidate parametric survival distributions by AIC, BIC,
  log-likelihood, scale, events, and N.
- Added
  [`plot_surv_fit()`](https://thinkdenominator.github.io/gtregression/reference/plot_surv_fit.md)
  for visually comparing observed Kaplan-Meier curves with fitted
  parametric survival curves.
- Added
  [`surv_predict()`](https://thinkdenominator.github.io/gtregression/reference/surv_predict.md)
  for model-based survival probability predictions from fitted
  parametric survival regression models.
- Added
  [`survival_summary()`](https://thinkdenominator.github.io/gtregression/reference/survival_summary.md)
  for Kaplan-Meier median survival summaries with total N, events,
  censored counts, and publication-style table outputs.
- Added
  [`survival_quantiles()`](https://thinkdenominator.github.io/gtregression/reference/survival_quantiles.md)
  for Kaplan-Meier survival time quantiles, including event percentiles,
  corresponding survival probabilities, and confidence intervals.
- Added
  [`survival_prob()`](https://thinkdenominator.github.io/gtregression/reference/survival_prob.md)
  for Kaplan-Meier survival probabilities at fixed follow-up times, with
  at-risk, event, censored, and confidence interval columns.
- Added
  [`logrank_test()`](https://thinkdenominator.github.io/gtregression/reference/logrank_test.md)
  for formal comparison of Kaplan-Meier survival curves, with observed
  and expected events plus formatted p-value output.
- Added
  [`check_ph()`](https://thinkdenominator.github.io/gtregression/reference/check_ph.md)
  for proportional hazards screening of Cox models using Schoenfeld
  residual tests, with flextable, gt, and tibble outputs.
- Added
  [`plot_model_fit()`](https://thinkdenominator.github.io/gtregression/reference/plot_model_fit.md)
  for visual model diagnostics from fitted `lm`/`glm` models and from
  models stored in
  [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md)
  and
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
  outputs.
- Added
  [`mediation_analysis()`](https://thinkdenominator.github.io/gtregression/reference/mediation_analysis.md)
  for regression-based mediation analysis with formatted direct,
  indirect, total, and proportion mediated tables.
- Added
  [`plot_mediation()`](https://thinkdenominator.github.io/gtregression/reference/plot_mediation.md)
  for drawing a mediation path diagram from
  [`mediation_analysis()`](https://thinkdenominator.github.io/gtregression/reference/mediation_analysis.md)
  outputs.
- Added `data_diabetes_mediation`, a health-related diabetes teaching
  dataset for practicing obesity, glucose, and diabetes mediation
  workflows.
- Added automatic support for variable label attributes, including
  labels set with
  [`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
  across descriptive, regression, stratified, merged, plotted, and
  forest-style outputs.
- Added formatted display outputs for diagnostic and review functions
  including
  [`dissect()`](https://thinkdenominator.github.io/gtregression/reference/dissect.md),
  [`select_models()`](https://thinkdenominator.github.io/gtregression/reference/select_models.md),
  [`interaction_models()`](https://thinkdenominator.github.io/gtregression/reference/interaction_models.md),
  and
  [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md).
- Added Mantel-Haenszel comparison support to
  [`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md)
  to support confounding assessment alongside crude and adjusted model
  comparisons.
- Added manual case-study scripts under `dev/manual-tests/` for
  real-time testing of logistic, linear, log-binomial, robust Poisson,
  Poisson, negative binomial, Cox, parametric survival, Firth, and
  mediation workflows.

### Changed

- Improved
  [`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md),
  [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md),
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
  [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md),
  and
  [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md)
  documentation with clearer examples using package datasets.
- Improved pkgdown organization, homepage content, reference grouping,
  and rendered examples to better show publication-ready tables and
  plots.
- Improved README and pkgdown documentation with an explicit dependency
  overview showing the trusted R packages used for modelling, tidying,
  tables, Word export, figures, diagnostics, and forest plots.
- Improved
  [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md)
  so descriptive, crude, and adjusted tables can be combined more
  reliably, including when visible variable labels differ between input
  tables.
- Improved
  [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md)
  so merged tables retain clean headers, spanners, and footnotes after
  relabelling.
- Improved
  [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md)
  and
  [`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md)
  support for descriptive summaries combined with univariable and
  multivariable regression outputs.
- Improved downstream survival support so
  [`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md)
  and
  [`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md)
  outputs work with
  [`plot_reg()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg.md),
  [`plot_reg_combine()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg_combine.md),
  [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md),
  [`forest_reg()`](https://thinkdenominator.github.io/gtregression/reference/forest_reg.md),
  [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md),
  [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md),
  and
  [`select_models()`](https://thinkdenominator.github.io/gtregression/reference/select_models.md).
- Improved table footer and source-note spacing across formatted tables
  so abbreviations, adjustment notes, and caveats render more compactly
  in flextable, gt, Word, and pkgdown outputs.
- Improved Word export so flextable outputs are fitted to a standard
  Word page width by default, with `save_docx(table_width = ...)`
  available for custom document layouts.
- Improved
  [`select_models()`](https://thinkdenominator.github.io/gtregression/reference/select_models.md)
  output so formatted tables clearly report the model selection
  direction used.
- Improved repository organization for CRAN readiness, including
  excluding development-only manual scripts from package builds.

### Fixed

- Fixed
  [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md)
  flextable headers so internal merge suffixes such as `_p1`, `_p2`, and
  `_p3` are not shown in rendered tables.
- Fixed
  [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md)
  row alignment when descriptive and regression tables contain the same
  variables but different visible labels.
- Fixed
  [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md)
  handling of merged flextable outputs so clean subheaders and group
  spanners are preserved.
- Fixed
  [`check_convergence()`](https://thinkdenominator.github.io/gtregression/reference/check_convergence.md)
  so failed multivariable model fits return a clear non-converged table
  rather than failing during table formatting.
- Fixed pkgdown reference metadata for internal print methods.
- Fixed save functions so files are written to a temporary directory
  when users do not provide an explicit destination, supporting
  CRAN-safe examples and tests.

## gtregression 1.0.0

CRAN release: 2025-08-18

- Initial release of gtregression.
- Added support for logit, log-binomial, linear, Poisson, robust
  Poisson, and negative binomial models.
- Added functions for confounder identification, model selection,
  diagnostics, plotting, and saving output.
- Added forest plot functions
  ([`plot_reg()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg.md),
  [`plot_reg_combine()`](https://thinkdenominator.github.io/gtregression/reference/plot_reg_combine.md)).
- Added initial documentation and pkgdown homepage.
