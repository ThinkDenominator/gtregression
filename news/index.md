# Changelog

## gtregression 1.2.0

### Changes that can alter numbers

- Linear models: confidence intervals in
  [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md),
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  and the stratified functions are now t-based (as
  [`confint.lm()`](https://rdrr.io/r/stats/confint.html)), consistent
  with the reported p-values. Intervals are slightly wider in small
  samples.
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  recovers the standard error with the same t quantile.
- Binary outcomes are handled the same way whether stored as integer,
  double, logical, factor or character: 0/1 and 1/2 codes are accepted,
  `TRUE` or the second level is the event, and a message names the event
  that was modelled. In stratified analyses the coding is decided once
  on the full data.
- [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  and
  [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md):
  unused factor levels are dropped per fitted model, so `Ref.` is the
  level the model actually used and unused levels no longer appear as
  `NA` rows. Ordered factors are reported as level-versus-reference
  effects; estimates for ordered-factor exposures therefore change from
  polynomial to treatment contrasts. Treatment contrasts are also used
  for display in the other regression functions.
- `cox_reg(baseline_strata = )` builds the model with `strata()` so that
  the baseline hazard is stratified on every supported version of
  survival.
- Stratified survival tables: crude N and Events are taken from the
  fitted models (they could be overstated with missing `time_start`,
  `weights` or `cluster` values).
  [`surv_model_compare()`](https://gtregression.thinkdenominator.com/reference/surv_model_compare.md)
  and `surv_reg(model_stats = TRUE)` count events on the rows used by
  the model.
- [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md)
  fits every candidate on one complete-case sample, returns `n_used` and
  `n_excluded`, and backward selection can reach the intercept-only
  model.
- [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md):
  the interaction p-value is a likelihood-ratio test for all approaches;
  numeric-coded candidates with at most 10 distinct values are treated
  as categories; unstable strata are ignored; the recommendation can be
  `"not_estimable"`.
- [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md)
  with `approach = "robpoisson"` uses a robust Wald test.
- [`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md):
  the change from the first model is computed on the ratio scale for
  ratio measures.
- [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md):
  the confidence-interval label follows `conf_level`, invalid exposure
  values are an error, failed bootstrap replicates give a warning, and a
  footnote states the logit approximation.
- [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md):
  Date and date-time variables are excluded with a message; logical and
  constant variables are shown as categories; continuous variables with
  missing values gain a `(Missing)` row; numeric levels are sorted
  numerically; an unknown `value` is an error.
- Log-binomial models use starting values and fail with an informative
  message when they cannot be fitted.

### Changes to table wording and layout

- [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
  now writes confidence intervals with an en dash, as all other tables
  already did, and the position of the `N` column is the same in all
  regression tables.
- [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
  keeps exposures whose model could not be fitted: the row shows a dash,
  a footnote names them, and the result gains `failed_exposures`.
- Non-estimable coefficients are shown as blank cells (previously
  `NA (NA-NA)`) in single, stratified and merged tables.
- Interaction rows are labelled from the fitted model, so factor levels
  that contain `:` and duplicated coefficient names are displayed
  correctly.
- Numeric stratifiers are ordered by value. The `Stratified by:`
  footnote shows the variable label when the stratifier has one.
- [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  and
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  with `stratifier`: `show_sample` now defaults to `"both"` (N and
  events); it has no effect without `stratifier`. The N footnote shows a
  range with an en dash when models differ, uses “analysis intervals”
  with `time_start`, and the adjustment footnote uses variable labels.
  New footnotes report rows excluded for a missing stratifier and
  reference categories that differ between strata.
- Non-stratified
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  and
  [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md)
  tables end with a `Number of events:` footnote. New footnotes, when
  applicable, name the event level of a factor or character `event` and
  state that robust variance was used with case weights.
- Multivariable tables of the newer model families (ordinal,
  multinomial, conditional logit, multilevel) show an `N` column and its
  footnote.
- [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md):
  confidence-interval headers follow `conf.level`; a warning and a
  footnote are given when `tau` exceeds the follow-up of a group.
- [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md):
  a variable shown as a single row in one table and with level rows in
  another is aligned on one block (with `Ref.` marked where the
  reference level differs); a continuous variable with a `(Missing)` row
  stays on one row; column labels, spanners, column order and footnotes
  are taken from the source tables (source spanners become an inner
  spanner row); abbreviation notes are combined into one line.
- [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  starts from the notes, labels, spanners and column order of the source
  table, so calling it without arguments leaves a table unchanged.
  `remove_N_obs = TRUE` removes every wording of the
  complete-observation footnote; `remove_N = TRUE` removes the `N`
  column and the notes that only explain that column.
- Single-table flextables centre their value columns, as gt,
  descriptive, stratified and merged tables do. Compact themes
  (`"clinical"`, `"striped"`, `"jama"`) keep the indentation of level
  rows.
- [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md)
  and
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md)
  place every estimate on the row of its own variable and level,
  including relabelled levels, adjusted-only levels and interaction
  rows. Without the ggtext package the axis labels are plain text.
- [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md):
  the curve and the risk table share the same x limits.
  [`cuminc_plot()`](https://gtregression.thinkdenominator.com/reference/cuminc_plot.md):
  `xlim`/`ylim` zoom without dropping data and groups follow the factor
  level order.
  [`plot_cox_survival()`](https://gtregression.thinkdenominator.com/reference/plot_cox_survival.md)
  supports models with `baseline_strata`.

### New messages and errors

- An invalid `theme` is an error in every table function and in
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md).
- Survival data without censored observations are accepted. `weights`
  must be greater than zero;
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  requires `time` greater than zero; `ties = "exact"` cannot be combined
  with `time_start`.
- Clear errors for
  [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md)
  without `by`,
  [`cuminc_plot()`](https://gtregression.thinkdenominator.com/reference/cuminc_plot.md)
  limits that are not increasing,
  [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md)
  /
  [`cuminc_plot()`](https://gtregression.thinkdenominator.com/reference/cuminc_plot.md)
  without `event_of_interest`, `plot_cox_survival(by = )` naming a
  baseline-strata variable, and
  [`surv_predict()`](https://gtregression.thinkdenominator.com/reference/surv_predict.md)
  on stratified input.
- [`multinomial_reg()`](https://gtregression.thinkdenominator.com/reference/multinomial_reg.md)
  names the exposure and level in the error raised when a level is lost
  in the fitted sample.
- [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
  explains the alternatives when a flextable is saved as PDF.
- Duplicated exposures are de-duplicated in the survival functions.
- A variable can no longer take two roles unnoticed.
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  and
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  refuse the time or event variable as an exposure or adjustment
  variable;
  [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md),
  [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md)
  and
  [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md)
  refuse the outcome as a predictor, and
  [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md)
  refuses an effect modifier equal to the exposure or a covariate
  repeating either;
  [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
  refuses covariates that repeat the exposure, mediator or outcome;
  [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md),
  [`survival_summary()`](https://gtregression.thinkdenominator.com/reference/survival_summary.md)
  and
  [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md)
  refuse `by` equal to the time or event variable.
  [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md)
  leaves the `by` variable out of `exposures`, with a message.
- [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  gives a clear error for
  [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
  results.

### New result fields

- [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md):
  `failed_exposures`.
  [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md):
  `n_used`, `n_excluded`.
- Stratified results
  ([`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md),
  [`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md),
  stratified
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  /
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md))
  and
  [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md):
  `footnotes`.
- [`multinomial_reg()`](https://gtregression.thinkdenominator.com/reference/multinomial_reg.md):
  a single `format`, plus `column_labels`, `spanners` and `theme`.
  Merged tables: `inner_spanners` and `theme`.
- [`plot_cox_survival()`](https://gtregression.thinkdenominator.com/reference/plot_cox_survival.md):
  `plot_data` gains `profile` and `stratum`.

### Variable-name arguments

- A bare name that is a column of `data` always refers to that column,
  even when an object of the same name exists in the workspace or in a
  package (for example `time`, `rank`, `class`, `by`).
- The one exception is a wrapper function that forwards its own
  argument, for example
  `function(d, by) descriptive_table(d, "x", by = by)`: the value passed
  by the caller is used. This applies to all variable arguments,
  including `time`, `event`, `stratifier`, `weights` and `by` in the
  survival functions.
- [`import_data()`](https://gtregression.thinkdenominator.com/reference/import_data.md)
  falls back to Latin-1 when a text file is not valid UTF-8, and no
  longer treats `'` as a quote character.

### Other changes

- [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md)
  now retains its Schoenfeld-residual diagnostics and supports the
  familiar [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  interface, including model and term selection.

- [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  now supports delayed entry/start-stop data, clustered robust variance,
  case weights, tie handling, and Cox fitting controls.

- [`plot_cox_survival()`](https://gtregression.thinkdenominator.com/reference/plot_cox_survival.md)
  produces adjusted survival curves and confidence bands from `coxph` or
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  models.

- Merged flextables now use visible publication-style header and outer
  rules while avoiding spurious rules between exposure rows.

- Align reference and comparison labels at the same indentation level in
  all publication-ready regression tables. This fixes binary rows such
  as `No` and `Yes` appearing at different horizontal positions.

- Updated
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  for forestploter 1.2.0: axis settings now use `set_xaxis()`, labels
  use `set_labs()`, and themes are passed through `style`, while
  retaining compatibility with forestploter 1.1.x. The ineffective
  `effects` argument is no longer forwarded and is retained only as a
  deprecated compatibility argument.

### New features and improvements

- New function
  [`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md):
  absolute risks and the risk difference between two groups for a binary
  outcome, as a `gt` or `flextable` table. The crude analysis uses the
  Miettinen-Nurminen score interval and score test. With `adjust_for`,
  risks are standardised over the analysis population from a
  main-effects logistic regression and the Wald interval and p-value use
  the influence-function standard error. `event_type = "harmful"` or
  `"beneficial"` adds NNTB / NNTH with an Altman interval that runs
  through infinity when the risk-difference interval includes zero. The
  event and the direction of the contrast are always stated in the table
  footnote.
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  supports captions, caveats, labels and headers for these tables;
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md),
  [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md),
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md),
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  and
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  reject them with a clear message.

- [`gtregression_app()`](https://gtregression.thinkdenominator.com/reference/gtregression_app.md):
  new **Trials** task for
  [`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md),
  beside Regression and Survival. Choose the outcome and which value is
  the event, the treatment variable and its reference group, optional
  adjustment variables, and whether the number needed to treat is
  reported. The table, DOCX / RTF / HTML export, R code and Session Code
  come from
  [`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md)
  itself.

- Documentation: new articles “Risk Difference and NNT”, “Merge and
  Customise Tables” (three or more tables, themes and flextable styling)
  and “Which Function Should I Use?”. The website has a new home page,
  grouped guides menu and a function reference organised by task.
  Reference titles now use consistent wording (for example “Univariable
  regression”).

- [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  /
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md):
  effect columns use the same header as the source table, so
  [`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md)
  shows “Matched OR” and
  [`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md)
  “Common OR” (previously a generic “OR”). Exposure-specific adjusted
  results (`adjust_for`) can now be shown next to the single
  multivariable model of the same family in
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  and
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md);
  the second column is labelled “Mutually adjusted”.
  `forest_df(multi = )` works without `uni`. Default forest-plot axes
  now use rounded ticks spaced so that labels do not overlap; `xlim` and
  `ticks_at` still override them.

- Make DOCX flextable width fitting consistent across flextable versions
  and operating systems by synchronizing column geometry across header,
  body, and footer parts. Strengthen count-model tests with genuinely
  overdispersed data so negative-binomial checks remain stable on
  R-devel platforms.

- Add
  [`import_data()`](https://gtregression.thinkdenominator.com/reference/import_data.md)
  as a single validated interface for CSV, TSV, TXT, Excel, RDS, Stata,
  SPSS, and SAS datasets. Preserve labelled columns, keep optional
  readers conditional, support extensionless Shiny upload paths, and use
  the same importer in GT Regression.

- Add
  [`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md)
  for matched case-control studies, with crude, adjusted, and joint
  multivariable matched odds ratios; explicit matched-set accounting;
  informative-set, case, and control counts; publication-ready tables;
  and downstream merge, modification, plot, forest, and export support.

- Add
  [`multilevel_reg()`](https://gtregression.thinkdenominator.com/reference/multilevel_reg.md),
  a focused multilevel workflow for continuous and binary outcomes: one
  grouping variable with a random intercept, publication-ready
  fixed-effect estimates, cluster and observation counts,
  random-intercept variance, intraclass correlation where appropriate,
  and convergence or singular-fit guidance. Random slopes and nested
  structures are outside its scope.

- Add
  [`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md)
  for ordered outcomes with three or more categories. It provides crude,
  adjusted, and joint multivariable proportional-odds models,
  publication-ready common odds ratios, explicit outcome ordering,
  model-fit statistics, and term-level proportional-odds assumption
  checks.

- Add
  [`multinomial_reg()`](https://gtregression.thinkdenominator.com/reference/multinomial_reg.md)
  for unordered outcomes with three or more categories. The publication
  table places each non-reference outcome comparison in a separate
  column and retains a standard regression object per comparison for
  focused plots and forest tables.

- Fix `show_ref = FALSE` so binary predictors may continue to display
  only the comparison level, while categorical predictors with more than
  two levels retain their reference category for interpretable output.
  Apply and test the same behaviour across regression tables, stratified
  analyses, merged tables, regression plots, and forest plots.

- Allow
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  and
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  to retain and display infinite confidence limits. Publication text
  uses the infinity symbol, original estimates remain unchanged, and
  only drawing coordinates are clipped to a finite plot boundary.

- Harden
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
  for sensitivity-analysis layouts, including multiple univariate or
  multiple multivariable tables with user-defined scenario headings,
  stable row alignment, and preserved model-specific notes.

- Add
  [`cuminc_plot()`](https://gtregression.thinkdenominator.com/reference/cuminc_plot.md)
  for cumulative incidence functions in the presence of competing
  events, with explicit censoring and event-of-interest coding,
  confidence intervals, optional grouping, and Gray’s test.

- Add
  [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md)
  for Fine-Gray regression, supporting crude and adjusted analyses with
  publication-ready `SHR` and `Adjusted SHR` tables.

- Add a Cox-specific `baseline_strata` argument to
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  for models that allow separate baseline hazards across one or more
  categorical strata while estimating common hazard ratios for the
  reported predictors. Keep this distinct from the existing `stratifier`
  argument, which produces stratum-specific analyses and outputs.

- Do not report hazard ratios for `baseline_strata` variables. Record
  them in model metadata and add a concise table footnote explaining
  that the baseline hazard was stratified and no coefficient was
  estimated for those variables. Validate overlap with `time`, `event`,
  `exposures`, `adjust_for`, and `stratifier`, and warn about strata
  with very small samples or no events.

- Make competing-risk outputs compatible with
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md),
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md),
  [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md),
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md),
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md),
  and package export helpers where statistically appropriate.

- Add `data_competing_risk`, a fully synthetic oncology teaching dataset
  for competing-risk analysis, together with beginner-friendly examples
  and a dedicated manual test case study.

- Document the distinction between cumulative incidence, Fine-Gray
  subdistribution hazards, cause-specific Cox hazards, and Kaplan-Meier
  estimates. Include interpretation caveats so the methods are not
  presented as interchangeable.

- Add focused validation and tests for event coding, competing-event
  coding, missing data, zero follow-up times, absent events, sparse
  strata, model convergence, output labels, and downstream table and
  plot compatibility.

- Extend
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md)
  to accept compatible crude and adjusted outputs from Fine-Gray,
  conditional logistic, ordinal, and individual multinomial outcome
  models, with model-specific effect-axis labels.

- Keep adjustment footnotes synchronized with labels changed through
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  for all new adjusted model families, and allow the note to be removed
  consistently before adding a custom caveat.

- Expose completed Special Models in the app’s merge-table workflow. For
  multinomial regression, each non-reference outcome comparison is
  offered as a separate mergeable table and reproduced correctly in
  Session Code.

- Retain Special Models exposure-model and multivariable results
  separately in the app, matching the Cox and parametric-survival
  workflow. Both tables and their run-time code snapshots remain
  available for modification, merging, supported plots, downloads, and
  whole-session script export.

- Offer compatible retained Special Models as combined
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md)
  and
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  choices in the app. Multinomial models are paired by shared outcome
  comparison, preventing estimates for different outcomes from being
  combined.

- Add an explicit retained-result selector to the Special Models
  diagnostics. Model statistics and ordinal assumption checks now
  identify whether the exposure or multivariable result is being
  inspected and explain unavailable output instead of showing an
  ambiguous blank panel.

### GT Regression app

- A variable chosen for two roles (for example a stratifier that is also
  an exposure) is explained in a dialog with a one-click fix that
  removes the extra selection and runs the analysis again; the Stratify
  panel warns as soon as the stratifier is also a model variable. Any
  error in the app is shown as a short notification and no longer closes
  the app or opens the debugger; failed downloads are explained the same
  way.
- Easier to use:
  - each Run button opens the tab that shows its result;
  - a variable chosen as the outcome, time, event, grouping or treatment
    variable cannot be ticked again as a predictor in that workspace,
    and is removed from the list with a short note if it was already
    ticked; Select all skips the stratifier;
  - model warnings and analysis messages (for example separation,
    non-convergence, or which outcome level was modelled) are shown in
    plain language above the table instead of only in the R console;
  - a result is marked as out of date when its selections or data
    change;
  - before a model is run, the ready line lists things worth checking:
    rows lost to missing values, an outcome that does not suit the
    approach, variables with one value or many categories, numeric codes
    that may be categories, categories with all or no events, and few
    events per model term;
  - a “Working…” indicator with elapsed time appears during longer runs,
    and Run buttons wait until the current run finishes;
  - import problems (separator, encoding, empty file, sheet, missing
    reader package) are explained, and imports note empty columns,
    adjusted column names and unread Excel sheets;
  - Session Code has Save session and Open a saved session, which keep
    the analysis data and selections in a `.gtrsession` file.
- The app keeps every table produced in a session. Earlier runs stay
  available to Merge, Modify, regression plots and forest plots
  (labelled with their run time), and stratified tables and
  risk-difference tables now appear in Modify (stratified tables also in
  Merge). Session Code recreates every kept result from the selections
  used to run it, each under its own name (for example `multi_result_1`,
  `multi_result`). A session keeps up to 30 results in the desktop app
  and 15 on a web server; when fitted models exceed 500 MB (200 MB on a
  web server) the oldest results keep their table but release their
  models. Session Code lists the kept results and can remove earlier
  ones.
- The generated merge code was missing a line break before
  [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md).

## gtregression 1.1.0

CRAN release: 2026-08-21

Archived release: <https://doi.org/10.5281/zenodo.22049943>

### Added

- Added an internal Data Prep workflow to the gtregression app, with
  explicit original/prepared analysis-data selection, quick starts,
  undo/redo, confirmed reset, prepared-data download, and reproducible
  preparation code. This code is included directly in gtregression and
  does not add gtstats as a dependency.
- Added observed-level reference category controls for categorical
  regression predictors in the app. Selected baselines are applied with
  reproducible factor releveling and included in generated and exported
  R scripts.
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
  [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
  and
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  via `model_stats = TRUE`, with AIC, BIC, log-likelihood, deviance,
  pseudo R-squared, linear-model R-squared, and model N stored in
  `$model_stats`.
- Added `approach = "firth"` / `approach = firth` for Firth penalized
  logistic regression in
  [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md),
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
  [`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md),
  and
  [`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md),
  with OR tables and compatibility with regression plots and forest plot
  helpers.
- Added `data_endometrial`, a classic endometrial cancer dataset with a
  separation pattern, for teaching and testing Firth logistic
  regression.
- Added
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  for Cox proportional hazards regression using `time`, `event`,
  `exposures`, and optional `adjust_for`, returning HR and adjusted HR
  tables.
- Added
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  for parametric survival regression using `time`, `event`, `exposures`,
  optional `adjust_for`, and a selectable survival distribution,
  returning time-ratio tables.
- Added stratified Cox and parametric survival workflows through the
  `stratifier` argument in
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  and
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md),
  with stratum-specific N, event counts, and formatted regression
  tables.
- Added
  [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md)
  for Kaplan-Meier survival curves with optional confidence intervals,
  censoring marks, log-rank p-values, and number-at-risk tables.
- Added more publication controls to
  [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md),
  including y-axis limits, percentage-scale display, optional grid
  removal, theme selection, confidence interval styling, and
  title/subtitle sizing for patchwork-style figure panels.
- Added
  [`km_risk_table()`](https://gtregression.thinkdenominator.com/reference/km_risk_table.md)
  for standalone Kaplan-Meier risk tables at requested follow-up times,
  with at-risk, event, and censored counts.
- Added
  [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md)
  for restricted mean survival time summaries up to a chosen follow-up
  time, with optional two-group RMST difference.
- Added
  [`surv_model_compare()`](https://gtregression.thinkdenominator.com/reference/surv_model_compare.md)
  for comparing candidate parametric survival distributions by AIC, BIC,
  log-likelihood, scale, events, and N.
- Added
  [`plot_surv_fit()`](https://gtregression.thinkdenominator.com/reference/plot_surv_fit.md)
  for visually comparing observed Kaplan-Meier curves with fitted
  parametric survival curves.
- Added
  [`surv_predict()`](https://gtregression.thinkdenominator.com/reference/surv_predict.md)
  for model-based survival probability predictions from fitted
  parametric survival regression models.
- Added
  [`survival_summary()`](https://gtregression.thinkdenominator.com/reference/survival_summary.md)
  for Kaplan-Meier median survival summaries with total N, events,
  censored counts, and publication-style table outputs.
- Added
  [`survival_quantiles()`](https://gtregression.thinkdenominator.com/reference/survival_quantiles.md)
  for Kaplan-Meier survival time quantiles, including event percentiles,
  corresponding survival probabilities, and confidence intervals.
- Added
  [`survival_prob()`](https://gtregression.thinkdenominator.com/reference/survival_prob.md)
  for Kaplan-Meier survival probabilities at fixed follow-up times, with
  at-risk, event, censored, and confidence interval columns.
- Added
  [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md)
  for formal comparison of Kaplan-Meier survival curves, with observed
  and expected events plus formatted p-value output.
- Added
  [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md)
  for proportional hazards screening of Cox models using Schoenfeld
  residual tests, with flextable, gt, and tibble outputs.
- Added
  [`plot_model_fit()`](https://gtregression.thinkdenominator.com/reference/plot_model_fit.md)
  for visual model diagnostics from fitted `lm`/`glm` models and from
  models stored in
  [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
  and
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
  outputs.
- Added
  [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
  for regression-based mediation analysis with formatted direct,
  indirect, total, and proportion mediated tables.
- Added
  [`plot_mediation()`](https://gtregression.thinkdenominator.com/reference/plot_mediation.md)
  for drawing a mediation path diagram from
  [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md)
  outputs.
- Added `data_diabetes_mediation`, a health-related diabetes teaching
  dataset for practicing obesity, glucose, and diabetes mediation
  workflows.
- Added
  [`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md)
  for publication-ready comparison of fitted gtregression model outputs,
  including AIC, BIC, log-likelihood, likelihood ratio statistics,
  primary exposure estimates, percent change, analysis-sample checks,
  and highlighted best-fit summaries.
- Added a guided candidate-model builder to the app’s Compare Models
  workflow. Users can name and fit two to six logistic, linear, count,
  Cox, or parametric survival candidates with model-specific exposures,
  adjustment variables, and optional interactions before producing a
  reproducible comparison table and complete copyable R code.
- Added
  [`save_forest()`](https://gtregression.thinkdenominator.com/reference/save_forest.md)
  for exporting
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  outputs with reproducible sizing across graphics devices and operating
  systems.
- Added automatic support for variable label attributes, including
  labels set with
  [`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
  across descriptive, regression, stratified, merged, plotted, and
  forest-style outputs.
- Added formatted display outputs for diagnostic and review functions
  including
  [`dissect()`](https://gtregression.thinkdenominator.com/reference/dissect.md),
  [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md),
  [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md),
  and
  [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md).
- Added Mantel-Haenszel comparison support to
  [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md)
  to support confounding assessment alongside crude and adjusted model
  comparisons.
- Added manual case-study scripts under `dev/manual-tests/` for
  real-time testing of logistic, linear, log-binomial, robust Poisson,
  Poisson, negative binomial, Cox, parametric survival, Firth, and
  mediation workflows.

### Changed

- Improved
  [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md),
  [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md),
  [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
  [`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md),
  and
  [`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md)
  documentation with clearer examples using package datasets.
- Improved pkgdown organization, homepage content, reference grouping,
  and rendered examples to better show publication-ready tables and
  plots.
- Improved README and pkgdown documentation with an explicit dependency
  overview showing the trusted R packages used for modelling, tidying,
  tables, Word export, figures, diagnostics, and forest plots.
- Improved
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
  so descriptive, crude, and adjusted tables can be combined more
  reliably, including when visible variable labels differ between input
  tables.
- Improved
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  so merged tables retain clean headers, spanners, and footnotes after
  relabelling.
- Improved
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  and
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  support for descriptive summaries combined with univariable and
  multivariable regression outputs.
- Improved
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  and
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  support for stratified regression outputs, including one-object
  stratified forest plots with highlighted stratum headers and preserved
  row order.
- Improved downstream survival support so
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  and
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  outputs work with
  [`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md),
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md),
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md),
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md),
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md),
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md),
  and
  [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md).
- Improved
  [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
  and
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  consistency with the rest of the package: both now support single
  multivariable models, adjusted exposure workflows, interaction terms,
  stratified workflows, and coherent table labels.
- Improved
  [`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md)
  output so user-supplied or object-derived model names are displayed
  instead of generic model labels, and context-aware warnings
  distinguish same-sample comparisons from different-sample comparisons.
- Improved
  [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
  handling for wide Word tables by preferring landscape orientation
  before reducing font size, respecting minimum font sizes, and allowing
  users to turn width fitting off.
- Improved table footer and source-note spacing across formatted tables
  so abbreviations, adjustment notes, and caveats render more compactly
  in flextable, gt, Word, and pkgdown outputs.
- Improved Word export so flextable outputs are fitted to a standard
  Word page width by default, with `save_docx(table_width = ...)`
  available for custom document layouts.
- Improved
  [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md)
  output so formatted tables clearly report the model selection
  direction used.
- Improved repository organization for CRAN readiness, including
  excluding development-only manual scripts from package builds.

### Fixed

- Fixed
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
  flextable headers so internal merge suffixes such as `_p1`, `_p2`, and
  `_p3` are not shown in rendered tables.
- Fixed
  [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
  row alignment when descriptive and regression tables contain the same
  variables but different visible labels.
- Fixed
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
  handling of merged flextable outputs so clean subheaders and group
  spanners are preserved.
- Fixed
  [`check_convergence()`](https://gtregression.thinkdenominator.com/reference/check_convergence.md)
  so failed multivariable model fits return a clear non-converged table
  rather than failing during table formatting.
- Fixed pkgdown reference metadata for internal print methods.
- Fixed save functions so files are written to a temporary directory
  when users do not provide an explicit destination, supporting
  CRAN-safe examples and tests.
- Fixed univariable Cox regression preprocessing so each exposure is
  fitted on complete cases for `time`, `event`, and the current exposure
  rather than using a single complete-case dataset across all exposures.
- Fixed Cox and parametric survival validation so zero follow-up times
  are allowed when accepted by the underlying `survival` model
  functions, while negative follow-up times remain invalid.
- Fixed regression and publication-ready outputs so user-supplied
  variable order is preserved rather than alphabetically reordered.
- Fixed forest plot merging so adjusted reference rows are left blank
  for variables that were not included in the adjusted model.
- Fixed
  [`forest_df()`](https://gtregression.thinkdenominator.com/reference/forest_df.md)
  row-order handling after joins and merges so forest plots follow the
  same display order as the source regression table.
- Replaced CRAN-incompatible diabetes demo data that depended on removed
  mlbench Pima data with `data_SynthDiabetes`, a synthetic replacement
  based on `mlbench::SynthDiabetes2` (mlbench \>= 2.1-11), and
  regenerated all mediation teaching fixtures that depended on the prior
  dataset.

## gtregression 1.0.0

CRAN release: 2025-08-18

- Initial release of gtregression.
- Added support for logit, log-binomial, linear, Poisson, robust
  Poisson, and negative binomial models.
- Added functions for confounder identification, model selection,
  diagnostics, plotting, and saving output.
- Added forest plot functions
  ([`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md),
  [`plot_reg_combine()`](https://gtregression.thinkdenominator.com/reference/plot_reg_combine.md)).
- Added initial documentation and pkgdown homepage.
