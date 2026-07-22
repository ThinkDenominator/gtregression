# Stratified multivariable regression

Fits multivariable regression models within each stratum and returns a
unified wide table with one "Characteristic" column and, under bold
spanners for each stratum, two columns: "Adjusted \<effect\>" and
"p-value".

## Usage

``` r
stratified_multi_reg(
  data,
  outcome,
  exposures,
  stratifier,
  adjust_for = NULL,
  interaction = NULL,
  approach = "logit",
  format = c("flextable", "gt"),
  theme = c("minimal")
)
```

## Arguments

- data:

  A data frame containing the variables.

- outcome:

  Character scalar; name of the outcome variable. Quoted and bare names
  are accepted.

- exposures:

  Character vector of exposure variables to report. Quoted names are
  recommended in scripts, and bare names are also accepted.

- stratifier:

  Character scalar; name of the stratifying variable. Quoted and bare
  names are accepted.

- adjust_for:

  Optional character vector of adjustment variables. Quoted and bare
  names are accepted. This argument works the same way as in
  [`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md).

- interaction:

  Optional character scalar specifying one interaction term using
  standard formula syntax, e.g. `"bmi*sex"`

- approach:

  One of `"logit"`, `"logbinomial"`, `"poisson"`, `"linear"`,
  `"robpoisson"`, or `"negbin"`

- format:

  One of `"flextable"` (default) or `"gt"`.

- theme:

  Preset name (e.g. `"minimal"`, `"striped"`, `"clinical"`, `"shaded"`,
  `"jama"`) or primitives
  `c("plain","zebra","lines","labels_bold","compact","header_shaded")`

## Value

A list of class `c("gtregression","stratified_multi_reg", ...)` with:

- `table`:

  A `flextable` (format = `"flextable"`) or `gt_tbl` (format = `"gt"`).

- `table_display`:

  Wide data frame used to build the table.

- `per_stratum`:

  Named list of per-stratum regression results.

- `models`:

  Named list of fitted models by stratum.

- `model_summaries`:

  Named list of model summaries by stratum.

- `reg_check`:

  Named list of diagnostics by stratum.

- `by`, `levels`, `approach`, `format`, `source`:

  Metadata fields.

## Details

If `adjust_for = NULL`, all `exposures` are included in one
multivariable model within each stratum. If `adjust_for` is supplied,
one adjusted model is fitted per exposure within each stratum.

## Examples

``` r
birthwt_data <- data_birthwt |>
  transform(
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1),
                 labels = c("Normal BW", "Low BW"))
  )

stratified_multi <- stratified_multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht"),
  stratifier = "race",
  approach = "logit"
)
#> Running stratified multivariable regression by: race
#>   > Stratum: race = White
#>   > Stratum: race = Black
#>   > Stratum: race = Other

stratified_adjusted <- stratified_multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  stratifier = "race",
  adjust_for = c("age", "lwt"),
  approach = "logit"
)
#> Running stratified multivariable regression by: race
#>   > Stratum: race = White
#>   > Stratum: race = Black
#>   > Stratum: race = Other
```
