# Predict survival probabilities from a parametric survival model

Estimate model-based survival probabilities at user-specified follow-up
times from a fitted
[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html)
model.

## Usage

``` r
surv_predict(
  model,
  newdata = NULL,
  times,
  digits = 1,
  format = c("flextable", "gt", "tibble"),
  theme = c("minimal")
)
```

## Arguments

- model:

  A fitted `survreg` model, or a
  [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
  object containing exactly one fitted model.

- newdata:

  Optional `data.frame` of profiles for prediction. If `NULL`, one
  typical profile is built from the model data using medians for numeric
  variables and the most common level for categorical variables.

- times:

  Numeric vector of follow-up times at which survival probability should
  be predicted.

- digits:

  Number of digits for displayed follow-up times and probabilities.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.

- theme:

  Table styling preset.

## Value

A list of class `c("gtregression","surv_predict", ...)` with elements:

- `table`:

  A `flextable`, `gt_tbl`, or `NULL` when `format = "tibble"`.

- `table_body`:

  Tibble with predicted survival probabilities.

- `table_display`:

  Display data frame used to render the table.

- `model`:

  The fitted `survreg` model used for prediction.

- `newdata,times,distribution,format,source`:

  Metadata fields.

## Details

`surv_predict()` is for parametric survival regression models fitted by
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
or
[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html).
It is not a Kaplan-Meier estimate and it is not a Cox prediction helper.

Supported distributions are `"weibull"`, `"exponential"`, `"lognormal"`,
and `"loglogistic"`, matching
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md).
Predictions are conditional on the supplied profile and the chosen
parametric distribution.

## Examples

``` r
lung_data <- data_lungcancer
lung_data$trt <- factor(lung_data$trt, levels = c(1, 2),
                        labels = c("Standard", "Test"))

fit <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = "trt",
  adjust_for = c("age", "karno"),
  distribution = weibull
)

surv_predict(
  model = fit$models$trt,
  newdata = data.frame(
    trt = factor("Test", levels = levels(lung_data$trt)),
    age = 60,
    karno = 70
  ),
  times = c(90, 180, 365)
)

surv_predict(
  model = fit,
  times = c(90, 180),
  format = tibble
)
#> # A tibble: 2 × 8
#>   Profile .profile     trt     age karno  Time Survival.probability Distribution
#>     <int> <chr>        <fct> <dbl> <dbl> <dbl>                <dbl> <chr>       
#> 1       1 Typical pro… Stan…    62    60    90                0.478 weibull     
#> 2       1 Typical pro… Stan…    62    60   180                0.234 weibull     
```
