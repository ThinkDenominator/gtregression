# Fine-Gray competing-risk regression

Fit proportional subdistribution hazards models and return
publication-ready subdistribution hazard ratios.

## Usage

``` r
competing_risk_reg(
  data,
  time,
  event,
  event_of_interest,
  exposures,
  censor_code = 0,
  adjust_for = NULL,
  multivariable = FALSE,
  multivariate = NULL,
  format = c("flextable", "gt"),
  theme = c("minimal"),
  model_stats = FALSE,
  show_ref = TRUE
)
```

## Arguments

- data:

  A data frame containing follow-up time, event status, and model
  variables.

- time:

  Numeric follow-up-time variable. Quoted and bare names work.

- event:

  Event-status variable with one censoring code and distinct codes for
  the event of interest and competing events.

- event_of_interest:

  Observed code or label for the event being modelled.

- exposures:

  Variables whose effects are reported.

- censor_code:

  Observed code or label for censoring. Defaults to \`0\`.

- adjust_for:

  Optional adjustment variables. One adjusted model is fitted per
  exposure and only that exposure is reported.

- multivariable:

  Logical; fit one model containing and reporting all variables in
  \`exposures\`.

- multivariate:

  Optional alias for \`multivariable\`.

- format:

  Output table format: \`"flextable"\` or \`"gt"\`.

- theme:

  Table styling preset.

- model_stats:

  Logical; return N, event counts, pseudo-log-likelihood, and
  convergence status.

- show_ref:

  Logical; display categorical reference rows.

## Value

A \`gtregression\` object containing the publication table, numeric
estimates, fitted \`crr\` models, model summaries, and analysis
metadata.

## Details

Each model uses the rows that are complete for its own variables. The N
column and the footnote report the observations and the events of
interest used by the fitted models, with the same wording as
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
and
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md).

Fine-Gray regression models the subdistribution hazard for the selected
event. An SHR above 1 indicates a higher cumulative incidence of that
event over follow-up, accounting for the presence of competing events.
It is not a cause-specific Cox hazard ratio and should not be
interpreted as one.

## Examples

``` r
competing_risk_reg(
  data_competing_risk,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  exposures = c(treatment, age, stage)
)
```
