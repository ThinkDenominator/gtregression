# Linear Regression Diagnostic Checks (Internal)

Performs diagnostic checks for fitted linear regression models.

## Usage

``` r
.reg_check_linear(model, exposure)
```

## Arguments

- model:

  A fitted linear model (\`lm\` object).

- exposure:

  Character string giving the name of the exposure variable for
  labeling.

## Value

A data frame with one row per diagnostic test, including:

- Exposure:

  Name of the exposure variable.

- Test:

  Diagnostic test name.

- Statistic:

  Test statistic or summary (e.g., p-values).

- Interpretation:

  Plain-language result interpretation.

## Details

The returned rows cover heteroskedasticity, residual normality, model
specification, and influential observations. If a diagnostic cannot be
computed for a given model, the function returns a clear "not available"
result for that row instead of failing.
