## Submission

This is an update of gtregression, from 1.1.0 to 1.2.0. <!-- maintainer: confirm the version currently on CRAN -->

The release adds model families (conditional logistic, ordinal, multinomial,
multilevel and Fine-Gray regression), extends the Cox and Kaplan-Meier tools,
and corrects a number of defects in estimates, table layout and merging. All
user-visible changes are listed in NEWS.md.

## Test environments

To be completed after the win-builder / R-hub runs. <!-- maintainer: fill in -->

* local: <OS>, R <version>
* win-builder: R-devel, R-release, R-oldrelease
* R-hub: <platforms>

## R CMD check results

To be completed after the win-builder / R-hub runs. <!-- maintainer: paste the
status line of each environment; do not submit with this placeholder -->

Expected NOTE:

* `checking package dependencies ... NOTE: Imports includes 23 non-default
  packages.`
  The package is a single interface to several regression families, each
  fitted by its established reference implementation (survival, cmprsk, lme4,
  ordinal, nnet, MASS, logistf, risks, sandwich, lmtest), and it renders
  results as tables and figures (gt, flextable, officer, ggplot2, patchwork,
  forestploter). These are used by exported functions, not only by examples,
  so they cannot be moved to Suggests without making core functions fail at
  run time. Packages needed only for optional features (the Shiny app, file
  import, ggtext labels) are in Suggests and are used conditionally.

## Tests

The tests of the bundled Shiny application and of its data-preparation
module are skipped on CRAN (`skip_on_cran()`) to keep the check time low; they
run when `NOT_CRAN=true` (for example under `devtools::test()`).

## Reverse dependencies

To be completed. <!-- maintainer: run revdepcheck / tools::package_dependencies(reverse = TRUE) and report -->
