# Check Convergence for a Regression Model

Assesses model convergence and provides diagnostics for each exposure
(in univariate mode) or for the full model (in multivariable mode),
depending on the regression approach used.

## Usage

``` r
check_convergence(
  data,
  exposures,
  outcome,
  approach = "logit",
  multivariate = FALSE,
  format = c("tibble", "gt", "flextable")
)
```

## Arguments

- data:

  A data frame containing the dataset.

- exposures:

  A character vector of predictor variable names. If
  `multivariate = FALSE`, each exposure is assessed separately. If
  `multivariate = TRUE`, exposures are included together.

- outcome:

  A character string specifying the outcome variable.

- approach:

  A character string specifying the regression approach. One of:
  `"logit"`, `"logbinomial"`, `"poisson"`, `"robpoisson"`, or
  `"negbin"`.

- multivariate:

  Logical. If `TRUE`, checks convergence for a multivariable model;
  otherwise, performs checks for each univariate model.

- format:

  Output format. One of `"tibble"`, `"gt"`, or `"flextable"`. The
  default `"tibble"` returns the original data-frame style output.

## Value

A data frame, `gt_tbl`, or `flextable` summarizing convergence
diagnostics, including:

- `Exposure`:

  Name of the exposure variable.

- `Model`:

  The regression approach used.

- `Converged`:

  `TRUE` if the model converged successfully; `FALSE` otherwise.

- `Max.prob.`:

  Maximum predicted probability or fitted value in the dataset.

## Details

For `robpoisson`, predicted probabilities (fitted values) may exceed 1,
which is acceptable when estimating risk ratios but should not be
interpreted as actual probabilities.

This function is useful for identifying convergence issues, especially
for `"logbinomial"` models, which often fail to converge.

## See also

\[identify_confounder()\], \[interaction_models()\]

## Examples

``` r
if (requireNamespace("gtregression", quietly = TRUE)) {
  data(data_PimaIndiansDiabetes, package = "gtregression")

  check_convergence(
    data = data_PimaIndiansDiabetes,
    exposures = c("age", "mass"),
    outcome = "diabetes",
    approach = "logit"
  )

  check_convergence(
    data = data_PimaIndiansDiabetes,
    exposures = c("age", "mass"),
    outcome = "diabetes",
    approach = "logit",
    multivariate = TRUE
  )
}
#>                 Exposure Model Converged Max.prob.
#> multivariable age + mass logit      TRUE 0.9359905
```
