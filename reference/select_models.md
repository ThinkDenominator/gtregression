# Stepwise Model Selection with Evaluation Metrics

Performs stepwise model selection using forward, backward, or both
directions across different regression approaches. The function returns
a summary table with evaluation metrics and the best model based on the
lowest AIC.

## Usage

``` r
select_models(
  data,
  outcome,
  exposures,
  approach = "logit",
  direction = "forward",
  format = c("flextable", "gt", "tibble")
)
```

## Arguments

- data:

  A data frame containing the outcome and predictor variables.

- outcome:

  A single character string indicating the outcome variable. Quoted and
  bare names are accepted.

- exposures:

  Character vector of predictor variables to consider. Quoted names are
  recommended in scripts, and bare names are also accepted.

- approach:

  Regression method. One of: `"logit"`, `"logbinomial"`, `"poisson"`,
  `"robpoisson"`, `"negbin"`, or `"linear"`.

- direction:

  Stepwise selection direction. One of: `"forward"` (default),
  `"backward"`, or `"both"`.

- format:

  Output format for the viewing table. One of `"flextable"` (default),
  `"gt"`, or `"tibble"`. Use `format = "tibble"` to keep only the
  original list structure.

## Value

A list with the following components:

- `results_table`: A tibble summarising each accepted step's model
  metrics (AIC, BIC, deviance, log-likelihood, and adjusted R-squared
  for linear models).

- `best_model`: The best-fitting model object based on lowest AIC.

- `all_models`: A named list of the accepted stepwise models.

- `direction`: Stepwise selection direction used.

- `table`: A formatted `gt_tbl` or `flextable` when `format` is `"gt"`
  or `"flextable"`.

## Examples

``` r
data <- data_birthwt
stepwise <- select_models(
  data = data,
  outcome = "bwt",
  exposures = c("age", "lwt", "smoke"),
  approach = "linear",
  direction = "forward"
)
stepwise$results_table
#> # A tibble: 3 × 8
#>   model_id formula           n_predictors   AIC   BIC logLik  deviance adj_r2
#>      <int> <chr>                    <int> <dbl> <dbl>  <dbl>     <dbl>  <dbl>
#> 1        1 bwt ~ 1                      0 3031. 3038. -1514. 99969656. 0     
#> 2        2 bwt ~ smoke                  1 3026. 3036. -1510. 96343710. 0.0311
#> 3        3 bwt ~ smoke + lwt            2 3022. 3035. -1507. 93194298. 0.0578
stepwise$best_model
#> 
#> Call:
#> stats::lm(formula = fmla, data = model_data)
#> 
#> Coefficients:
#> (Intercept)        smoke          lwt  
#>    2501.125     -272.081        4.237  
#> 
```
