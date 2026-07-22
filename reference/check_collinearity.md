# Check collinearity using VIF for fitted models

Computes Variance Inflation Factors (VIF) for fitted multivariable
models returned by
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
or related functions.

## Usage

``` r
check_collinearity(model, format = c("tibble", "gt", "flextable"))
```

## Arguments

- model:

  A fitted model object returned by
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
  [`stratified_multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/stratified_multi_reg.md),
  or compatible `gtregression` functions. Univariable model objects are
  rejected because VIF is not applicable.

- format:

  Output format. One of `"tibble"`, `"gt"`, or `"flextable"`. The
  default `"tibble"` preserves the original tibble or nested-list
  output.

## Value

For multivariable models, a tibble if a single fitted model is present,
or a named list of tibbles if multiple fitted models are present. With
`format = "gt"` or `format = "flextable"`, leaf tibbles are converted to
formatted tables while preserving any list nesting. The tibble contains:

- `Variable`:

  Model term.

- `VIF`:

  Variance inflation factor. For multi-degree-of-freedom terms, this is
  the adjusted GVIF: `GVIF^(1 / (2 * Df))`.

- `Interpretation`:

  Simple interpretation based on common cut points: no collinearity,
  moderate, or high.

For univariate models, an error is raised indicating that VIF is not
applicable.

## Details

If the car package is installed, `check_collinearity()` uses
[`car::vif()`](https://rdrr.io/pkg/car/man/vif.html). Otherwise, it
computes VIF/GVIF from the fitted model matrix so that diagnostics
remain available without an additional dependency.
