# Check regression model convergence

Check whether requested models converge, either one exposure at a time
or as a single multivariable model.

## Usage

``` r
check_convergence(
  data,
  exposures,
  outcome,
  approach = "logit",
  multivariate = FALSE,
  format = c("flextable", "gt", "tibble")
)
```

## Arguments

- data:

  A data frame containing the dataset.

- exposures:

  A character vector of predictor variable names. Quoted names are
  recommended in scripts, and bare names are also accepted. If
  `multivariate = FALSE`, each exposure is assessed separately. If
  `multivariate = TRUE`, exposures are included together.

- outcome:

  A character string specifying the outcome variable. Quoted and bare
  names are accepted.

- approach:

  A character string specifying the regression approach. One of:
  `"logit"`, `"logbinomial"`, `"poisson"`, `"robpoisson"`, or
  `"negbin"`.

- multivariate:

  Logical. If `TRUE`, checks convergence for a multivariable model;
  otherwise, performs checks for each univariate model.

- format:

  Output format. One of `"flextable"` (default), `"gt"`, or `"tibble"`.
  Use `format = "tibble"` for the original data-frame style output.

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

This is especially useful for `"logbinomial"` models, where convergence
problems are common and robust Poisson may be a practical alternative.

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


.cl-457e780a{}.cl-457695a4{font-family:'DejaVu Sans';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-457695b8{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-457695c2{font-family:'DejaVu Sans';font-size:8pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4579e330{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4579e33a{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4579e33b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 0.9;background-color:transparent;}.cl-457a1a30{width:1.197in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a3a{width:0.808in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a44{width:1.212in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a45{width:1.691in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a46{width:1.197in;background-color:rgba(231, 245, 236, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a4e{width:0.808in;background-color:rgba(231, 245, 236, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a4f{width:1.212in;background-color:rgba(231, 245, 236, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a58{width:1.691in;background-color:rgba(231, 245, 236, 1.00);vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a59{width:1.197in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a5a{width:0.808in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a5b{width:1.212in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-457a1a62{width:1.691in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}

Convergence check

Exposure
```
