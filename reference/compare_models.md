# Compare Prespecified Regression Models

Compare gtregression candidate models side by side using model-fit
statistics. This is intended for transparent model comparison after you
have already fitted the candidate models with functions such as
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
[`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md),
or
[`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md).

## Usage

``` r
compare_models(
  ...,
  model_names = NULL,
  nested = TRUE,
  primary_exposure = NULL,
  exponentiate = NULL,
  digits = 2,
  p_digits = 3,
  format = c("flextable", "gt", "tibble"),
  theme = c("minimal")
)
```

## Arguments

- ...:

  Two or more gtregression model objects, or one list containing them.
  Inputs should be outputs from
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
  [`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md),
  or
  [`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md).

- model_names:

  Optional character vector of names to display. If omitted, names
  supplied in `...` are used; otherwise models are labelled `Model 1`,
  `Model 2`, etc.

- nested:

  Logical. If `TRUE`, likelihood-ratio statistics are calculated
  sequentially by comparing each model with the previous model. Use this
  only when models are nested and supplied in the intended order.

- primary_exposure:

  Optional exposure or exact coefficient name to track across models.
  For Cox models this can be used to show the hazard ratio and
  percentage change in the log-effect estimate across candidate models.

- exponentiate:

  Logical. If `NULL`, Cox, logistic, Poisson, negative-binomial, and
  parametric survival models are exponentiated by default, while linear
  models are not.

- digits:

  Number of digits for model statistics and estimates.

- p_digits:

  Number of digits for p-values.

- format:

  Output format. Defaults to `"flextable"`.

- theme:

  Table theme preset.

## Value

A `gtregression` object with:

- `table`: publication-ready table

- `table_body`: raw comparison statistics

- `table_display`: formatted display data

- `models`: fitted models compared

## Details

`compare_models()` does not refit models and does not perform hidden
complete-case filtering. It compares models already fitted by
gtregression and extracts the single fitted model stored in each
object's `models` element. The reported N, event counts, and fit
statistics therefore come from the model already fitted by
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md),
[`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md),
or
[`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md).
This keeps model comparison separate from model selection: compare
candidate models first, then choose the final model using clinical,
epidemiological, and statistical judgement.

Likelihood-ratio p-values are meaningful only for nested models fitted
to the same analysis sample. If the models are not nested, or if model
sample sizes differ, use AIC/BIC and subject-matter reasoning instead.

## Examples

``` r
data("data_lungcancer", package = "gtregression")

lung_data <- data_lungcancer

cox_1 <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt
)

cox_2 <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno)
)

compare_models(
  cox_1,
  cox_2,
  model_names = c("Treatment only", "Treatment + age + performance"),
  primary_exposure = trt
)
```
