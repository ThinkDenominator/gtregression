# Plot proportional hazards diagnostics

Plot scaled Schoenfeld residual diagnostics retained by
[`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md).

## Usage

``` r
# S3 method for class 'gtregression_ph'
plot(x, model = NULL, term = NULL, ...)
```

## Arguments

- x:

  An object returned by
  [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md).

- model:

  Model to plot. Supply its name or position. This is required when
  [`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md)
  evaluated more than one Cox model.

- term:

  Optional model term to plot. Supply its name or position. When
  omitted, all terms in the selected model are plotted.

- ...:

  Additional arguments passed to `plot.cox.zph()`.

## Value

The input object, invisibly.
