# Launch the gtregression app

Open a menu-driven Shiny app for common gtregression workflows: data
import, descriptive tables, regression tables, survival analysis,
diagnostics, causal mediation, plots, and exports.

## Usage

``` r
gtregression_app(..., launch.browser = NULL)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- launch.browser:

  Logical; passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). The
  default uses the RStudio Viewer when available, otherwise opens a
  browser only in interactive sessions.

## Value

Invisibly returns the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

The app is intentionally kept out of the core package startup path.
Shiny and other interface packages are suggested dependencies and are
loaded only when `gtregression_app()` is called.

The Advanced tab includes a guided candidate-model builder. Users can
name two to six models, choose model-specific exposures and adjustment
variables, optionally add an interaction, and track a primary exposure.
The app fits compatible
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
or
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
objects before passing them to
[`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md).
Generated code records every fitting call and the final comparison for
reproducible use outside the app.

## See also

[`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md),
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)

## Examples

``` r
if (interactive()) {
  gtregression_app()
}
```
