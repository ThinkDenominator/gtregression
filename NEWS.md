# gtregression 1.1.0

## Added

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
* Added `cox_reg()` for beginner-friendly Cox proportional hazards regression
  using `time`, `event`, `exposures`, and optional `adjust_for`, returning
  publication-ready HR and adjusted HR tables.
* Added `surv_reg()` for beginner-friendly parametric survival regression using
  `time`, `event`, `exposures`, optional `adjust_for`, and a selectable survival
  distribution, returning publication-ready time-ratio tables.
* Added `km_plot()` for Kaplan-Meier survival curves with optional confidence
  intervals, censoring marks, log-rank p-values, and number-at-risk tables.
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
* Added automatic support for variable label attributes, including labels set
  with `labelled::var_label()`, across descriptive, regression, stratified,
  merged, plotted, and forest-style outputs.
* Added publication-style display outputs for diagnostic and helper functions
  including `dissect()`, `select_models()`, `interaction_models()`, and
  `identify_confounder()`.
* Added Mantel-Haenszel comparison support to `identify_confounder()` to support
  confounding assessment alongside crude and adjusted model comparisons.
* Added manual case-study scripts under `dev/manual-tests/` for real-time testing
  of logistic, linear, log-binomial, robust Poisson, Poisson, negative binomial,
  Cox, and parametric survival workflows.

## Changed

* Improved `descriptive_table()`, `uni_reg()`, `multi_reg()`,
  `stratified_uni_reg()`, and `stratified_multi_reg()` documentation with clearer
  beginner-friendly examples using package datasets.
* Improved pkgdown organization, homepage content, reference grouping, and
  rendered examples to better show publication-ready tables and plots.
* Improved `merge_tables()` so descriptive, crude, and adjusted tables can be
  combined more reliably, including when visible variable labels differ between
  input tables.
* Improved `modify_table()` so merged tables retain clean headers, spanners, and
  footnotes after relabelling.
* Improved `forest_df()` and `forest_reg()` support for descriptive summaries
  combined with univariable and multivariable regression outputs.
* Improved downstream survival support so `cox_reg()` and `surv_reg()` outputs
  work with `plot_reg()`, `plot_reg_combine()`, `forest_df()`, `forest_reg()`,
  `merge_tables()`, `modify_table()`, and `select_models()`.
* Improved table footer and source-note spacing across publication and helper
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
* Fixed save helpers so files are written to a temporary directory when users do
  not provide an explicit destination, supporting CRAN-safe examples and tests.

# gtregression 1.0.0

* Initial release of gtregression.
* Added support for logit, log-binomial, linear, Poisson, robust Poisson, and
  negative binomial models.
* Added functions for confounder identification, model selection, diagnostics,
  plotting, and saving output.
* Added forest plot functions (`plot_reg()`, `plot_reg_combine()`).
* Added initial documentation and pkgdown homepage.
