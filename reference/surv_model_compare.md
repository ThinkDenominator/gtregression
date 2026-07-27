# Compare parametric survival model distributions

Fit the same parametric survival regression model using multiple
distributions and compare model-fit statistics such as AIC and BIC.

## Usage

``` r
surv_model_compare(
  data,
  time,
  event,
  exposures,
  adjust_for = NULL,
  distributions = c("weibull", "exponential", "lognormal", "loglogistic"),
  digits = 2,
  format = c("flextable", "gt", "tibble"),
  theme = c("minimal")
)
```

## Arguments

- data:

  A `data.frame` containing survival time, event status, and predictor
  variables.

- time:

  Survival follow-up time. Quoted and bare names are accepted.

- event:

  Event indicator. Quoted and bare names are accepted. Numeric `0/1`,
  numeric `1/2`, logical, character, and factor variables are accepted.
  For two-level character or factor variables, the second level is
  treated as the event.

- exposures:

  Character vector of main exposure variable names. Quoted names are
  recommended in scripts, and bare names are also accepted.

- adjust_for:

  Optional character vector of adjustment variables. These variables are
  included in every candidate model.

- distributions:

  Parametric survival distributions to compare. Defaults to
  `c("weibull", "exponential", "lognormal", "loglogistic")`. Quoted and
  bare values are accepted. Common spellings such as `"log-normal"` and
  `"log-logistic"` are also accepted.

- digits:

  Number of digits for displayed model statistics.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","surv_model_compare", ...)` with
elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with model-fit statistics.

- `table_display`:

  Display data frame used to render the table.

- `models`:

  Named list of fitted `survreg` models.

- `time,event,exposures,adjust_for,distributions,format,source`:

  Metadata fields.

## Details

The same model formula is fitted for each candidate distribution using
[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html).
Lower AIC or BIC values indicate better relative model fit among the
compared distributions. These statistics should be used with clinical
judgment and visual checks; they do not prove that a distribution is
scientifically correct.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))
lung_data$prior <- factor(lung_data$prior, levels = c(0, 10),
                          labels = c("No", "Yes"))

surv_model_compare(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "prior"),
  adjust_for = c("age", "karno")
)

surv_model_compare(
  data = lung_data,
  time = "time",
  event = "status",
  exposures = c(trt, prior),
  distributions = c(weibull, "log-logistic"),
  format = tibble
)
#> # A tibble: 2 × 9
#>   Distribution   AIC   BIC logLik Scale     N Events Best.AIC Best.BIC
#>   <chr>        <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl> <lgl>    <lgl>   
#> 1 weibull      1503. 1515.  -748. 1.16    137    128 TRUE     TRUE    
#> 2 loglogistic  1508. 1519.  -750. 0.784   137    128 FALSE    FALSE   
```
