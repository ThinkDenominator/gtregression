# Launch the gtregression app from the RStudio Addins menu

The RStudio Addin (*Addins \> GT Regression app*) opens GT Regression in
an RStudio dialog window. It is the same application as
[`gtregression_app()`](https://gtregression.thinkdenominator.com/reference/gtregression_app.md),
which opens it in your web browser.

## Usage

``` r
gtregression_addin()
```

## Value

Invisibly returns the result of
[`shiny::runGadget()`](https://rdrr.io/pkg/shiny/man/runGadget.html).

## Details

There is no separate Addin interface: the interface, analyses, generated
R code and access to data frames in your global environment are
identical to
[`gtregression_app()`](https://gtregression.thinkdenominator.com/reference/gtregression_app.md).
Only the container differs (a 1200 x 800 RStudio dialog instead of a
browser window). The shiny and rstudioapi packages are optional and are
checked only when this function is called. Selected data frames are
copied into the app as read-only snapshots; the source objects are not
modified.

## See also

[`gtregression_app`](https://gtregression.thinkdenominator.com/reference/gtregression_app.md)

## Examples

``` r
if (
  interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
) {
  gtregression_addin()
}
```
