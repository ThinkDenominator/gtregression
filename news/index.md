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
- Added publication-style display outputs for diagnostic and helper
  functions including
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
  Poisson, and negative binomial workflows.

### Changed

- Improved
  [`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md),
  [`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md),
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
  [`stratified_uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_uni_reg.md),
  and
  [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md)
  documentation with clearer beginner-friendly examples using package
  datasets.
- Improved pkgdown organization, homepage content, reference grouping,
  and rendered examples to better show publication-ready tables and
  plots.
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
- Fixed save helpers so files are written to a temporary directory when
  users do not provide an explicit destination, supporting CRAN-safe
  examples and tests.

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
