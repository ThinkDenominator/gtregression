# Launch the gtregression app

Open GT Regression, the graphical interface for gtregression: data
import and preparation, descriptive tables, regression tables, survival
analysis, diagnostics, causal mediation, plots, exports and reproducible
R code.

## Usage

``` r
gtregression_app(..., launch.browser = NULL)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), for
  example `port`.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). The
  default opens the app in your web browser in interactive sessions.
  Supply a function such as
  [`rstudioapi::viewer`](https://rstudio.github.io/rstudioapi/reference/viewer.html)
  to choose another viewer.

## Value

Invisibly returns the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

`gtregression_app()` opens GT Regression in your web browser. The
RStudio Addin (*Addins \> GT Regression app*) opens exactly the same
application in an RStudio dialog window through
[`gtregression_addin()`](https://gtregression.thinkdenominator.com/reference/gtregression_addin.md);
only the window differs.

The app runs in your local R session, so data frames in your global
environment can be chosen from the Data workspace (*R Environment*).
Selected data are copied into the app as read-only snapshots; the source
objects are not modified. Files can also be imported, and example data
are included.

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

[`gtregression_addin`](https://gtregression.thinkdenominator.com/reference/gtregression_addin.md),
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
