# gtregression manual API verification

These scripts complement the automated test suite. They are intended for
checking complete, user-facing workflows: printed tables, footnotes, plots,
forest plots, merged outputs, and generated model metadata.

## Before running

From the package root, install the development dependencies and load the local
source:

```r
devtools::install_dev_deps()
devtools::load_all()
```

Run scripts from the package root. Work through each script one section at a
time when visually reviewing output, or run the 1.2.0 suite with:

```r
source("dev/manual-tests/00-run-1.2.0-api-verification.R")
```

The runner stops at the first execution error. For the intended release review,
open each script and run one numbered section at a time. Plot windows and table
viewers may open; this is intentional.

## Version 1.2.0 checklist

| Script | API and behaviour to inspect |
|---|---|
| `08-competing-risk-case-study.R` | `cuminc_plot()`, Gray's test, `competing_risk_reg()`, merge, plot, and forest compatibility |
| `12-conditional-logit-matched-case-control.R` | `conditional_logit_reg()` crude, adjusted, and multivariable matched models |
| `13-multilevel-clinic-case-study.R` | `multilevel_reg()` for continuous and binary outcomes, cluster summaries, and ICC |
| `14-ordinal-recovery-case-study.R` | `ordinal_reg()`, outcome ordering, model statistics, and proportional-odds checks |
| `15-multinomial-destination-case-study.R` | `multinomial_reg()`, outcome-specific models, plots, and reference outcome changes |
| `16-import-data-formats.R` | `import_data()` format detection, explicit formats, RDS, delimiters, and name repair |
| `17-cox-advanced-survival-api.R` | PH diagnostic plots, delayed entry/start-stop Cox models, clustering, weights, ties, adjusted curves, and merged-table rules |
| `18-risk-difference-api.R` | `risk_difference()`: crude Miettinen-Nurminen and adjusted (standardised) risk differences, event and reference choice, NNTB/NNTH, precision, export, `modify_table()`, and refusals. Each section lists the expected numbers |
| `11-gtregression-app-manual-test.R` | App import, Special Models, Competing Risks, Trials (risk difference), retained results, code, and downloads |

## What success means

1. Each numbered section runs without an unexpected error.
2. Tables use readable labels and identify reference categories or outcomes.
3. Adjustment, matching, clustering, and competing-risk notes are visible.
4. `plot_reg()`, `plot_reg_combine()`, and `forest_reg()` render without errors
   for supported outputs.
5. Merged tables preserve model-specific headings and notes.
6. App-generated code reproduces the selected analysis outside the app.

The scripts create temporary import files only in `tempdir()`. They do not
write analysis outputs into the package source tree.
