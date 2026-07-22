# Validate Exposure Variable(s) for Regression

Ensures that the exposure variable has at least two non-missing levels
or sufficient numeric variation to support regression modelling.

## Usage

``` r
.validate_exposures(data, exposures)
```

## Arguments

- data:

  A data frame containing the exposure variables.

- exposures:

  Character vector of column names to validate.

## Value

Returns TRUE if valid; otherwise throws an error.
