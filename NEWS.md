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
* Added publication-style display outputs for diagnostic and helper functions
  including `dissect()`, `select_models()`, `interaction_models()`, and
  `identify_confounder()`.
* Added Mantel-Haenszel comparison support to `identify_confounder()` to support
  confounding assessment alongside crude and adjusted model comparisons.
* Added manual case-study scripts under `dev/manual-tests/` for real-time testing
  of logistic, linear, log-binomial, robust Poisson, Poisson, and negative
  binomial workflows.

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
