# Risk difference between two groups

Reports the absolute risk of a binary outcome in each of two groups and
the risk difference between them, with a confidence interval and
p-value, as a publication-ready table. The crude analysis uses the
Miettinen-Nurminen score method; the adjusted analysis standardises the
risks from a logistic regression model. Optionally the number needed to
treat (NNTB / NNTH) is added.

## Usage

``` r
risk_difference(
  data,
  outcome,
  exposure,
  adjust_for = NULL,
  reference = NULL,
  event = NULL,
  event_type = NULL,
  conf_level = 0.95,
  format = "gt",
  theme = NULL
)
```

## Arguments

- data:

  A data frame.

- outcome:

  Binary outcome column. Quoted and bare names are accepted. Logical,
  numeric 0/1 or 1/2, two-level factor, two-value character and
  value-labelled (haven) variables are supported.

- exposure:

  Column defining exactly two groups (for example treatment arm or
  exposure status). Quoted and bare names are accepted.

- adjust_for:

  Optional adjustment variables (quoted or bare names). `NULL` (default)
  gives the crude analysis; otherwise the adjusted (standardised)
  analysis is reported.

- reference:

  Group used as the reference. The risk difference is the other
  (comparison) group minus the reference group. `NULL` uses the first
  factor level, `FALSE`, `0`, or the first value in sorted order.

- event:

  Value of `outcome` that counts as the event. `NULL` uses `TRUE`, `1`
  (for 0/1 coding), `2` (for 1/2 coding), the second factor level, or
  the second value in sorted order.

- event_type:

  `NULL` (default), `"harmful"` or `"beneficial"`. When given, a column
  with the number needed to treat is added (see Details). The event
  direction is needed to tell benefit from harm, so no NNT is shown
  without it.

- conf_level:

  Confidence level of the interval; default `0.95`. It changes the
  confidence interval only. The p-value is always the two-sided test of
  a risk difference of zero and does not depend on `conf_level`, so, for
  example, a 99% interval can include zero while the p-value is below
  0.05.

- format:

  `"gt"` (default) or `"flextable"`.

- theme:

  Table theme: a preset name (`"minimal"`, `"clinical"`, `"striped"`,
  `"shaded"`, `"jama"`) or a vector of primitives, as in the regression
  functions. `NULL` (default) uses the package default theme
  (`"minimal"`).

## Value

A list of class `c("risk_difference", "gtregression")` with elements:

- table:

  The rendered `gt_tbl` or `flextable`.

- table_display:

  Data frame of the formatted cells as shown.

- table_body:

  Data frame of numeric results, one row per group (reference first):
  `group`, `events`, `n`, `risk`, `adjusted_risk`, `adjusted_risk_se`,
  `rd`, `rd_se`, `rd_low`, `rd_high`, `p_value`. Risks and differences
  are proportions. `rd_se` is the influence-function standard error and
  is `NA` for the crude analysis, whose score interval is not based on a
  single standard error.

- footnotes:

  Character vector of table footnotes.

- analysis:

  `"crude"` or `"adjusted"`.

- outcome, exposure, adjust_for:

  Variable names.

- event, reference, comparison:

  Resolved event value and groups.

- event_type, conf_level:

  As supplied.

- method:

  List naming the estimate, interval, test and variance methods.

- n:

  List with the numbers of rows `supplied`, `analysed` and `excluded`.

- nnt:

  `NULL`, or a list with `type` (`"NNTB"`, `"NNTH"` or `NA`), `estimate`
  (unrounded, may be `Inf`), `crosses_infinity` and `segments` (data
  frame with `type`, `from`, `to`; two rows ending at `Inf` when the
  interval runs through infinity).

- model:

  The fitted logistic model (adjusted analysis), else `NULL`. In the
  model the outcome is `.rd_y` (1 = event) and the exposure is `.rd_a`
  (1 = comparison group).

- variable_labels, level_labels, theme:

  Labels and theme used for display.

- source, format:

  `"risk_difference"`; `"gt"` or `"flextable"`.

## Details

**Absolute risk and risk difference.** The risk in a group is the
proportion of its members with the event. The risk difference (RD) is
the risk in the comparison group minus the risk in the reference group.
It is stored as a difference of proportions (for example `-0.08`) and
shown in percentage points (`-8.0`). A negative RD means that the event
is less frequent in the comparison group. For a harmful event (for
example death) a negative RD is an absolute risk reduction and a
positive RD an absolute risk increase; for a beneficial event (for
example cure) the reading is reversed. These are readings of the same
number, not separate estimates, and the table reports the signed RD
only. The resolved event, reference and comparison are stored in the
result, are always written in the table footnote, and are announced in a
message whenever they were inferred from anything other than logical or
0/1 coding.

**Analysis population.** One complete-case population is used:
observations with a missing outcome or exposure (and, for the adjusted
analysis, any missing adjustment variable) are excluded. The numbers
supplied, analysed and excluded are stored and the footnote reports
them.

**Crude analysis** (`adjust_for = NULL`). Risks are the observed
proportions. The confidence interval is the Miettinen-Nurminen score
interval (restricted maximum-likelihood variance with the \\N/(N-1)\\
factor, no continuity correction) and the p-value comes from the same
score statistic at a difference of zero (equal to Pearson's chi-square
multiplied by \\(N-1)/N\\). The method is defined when a group has no
events or only events; when no one, or everyone, has the event the
p-value is 1.

**Adjusted analysis.** A main-effects logistic regression of the outcome
on the exposure and the adjustment variables is fitted. Each person's
risk is predicted twice from their own covariates, once as a member of
each group; the predictions are averaged over the analysis population to
give the two adjusted (standardised) risks, and their difference is the
adjusted RD. The table shows the observed events and group sizes next to
the adjusted risks. The standard error is the influence-function
standard error of the stacked estimating equations (logistic score
equations and the two averages), which accounts for the estimation of
the model and for the sampling of the covariates and does not rely on
the logistic model being correct. The confidence interval is a Wald
interval, with limits restricted to the range -1 to 1, and the p-value
is two-sided normal. No small-sample correction is applied; in
simulations with about 80 to 100 participants coverage of the 95%
interval was about 94%. The analysis stops with an error, and returns no
table, when the model does not converge, when fitted probabilities reach
0 or 1 (separation, including a group with no events or only events),
when the exposure or an adjustment variable is aliased, or when the
outcome is constant.

**Interpretation.** The function describes a calculation and makes no
causal claim. In an individually randomised trial the adjusted RD is a
covariate-adjusted estimate of the marginal treatment difference, and it
remains valid when the logistic model is not exactly right (see the FDA
2023 guidance on covariate adjustment). In a non-randomised study it is
a standardised risk difference; it can be read causally only if there is
no unmeasured confounding, the outcome model is correct and the groups
overlap in their covariates.

**NNTB and NNTH.** With `event_type`, the number needed to treat is the
reciprocal of the absolute value of the unrounded RD, shown rounded up
to a whole person. It is labelled NNTB (number needed to treat for one
additional person to benefit) when the comparison group does better and
NNTH (for one additional person to be harmed) when it does worse: for a
harmful event a negative RD gives NNTB and a positive RD NNTH, and the
reverse for a beneficial event. When the confidence interval of the RD
includes zero, the interval of the NNT runs from a finite NNTB through
infinity to a finite NNTH and is shown and stored in that form (Altman
1998), never as a pair of finite limits. The NNT applies to the
follow-up period of the study and assumes that the RD reflects what the
intervention does.

**Limits of this version.** Binary outcome at a fixed follow-up; exactly
two groups; independent observations; main-effects logistic model for
adjustment. Not supported: more than two groups, continuous exposures,
stratified (Mantel-Haenszel) or clustered designs, non-inferiority
margins, time-to-event outcomes. The result can be printed, saved with
[`save_table`](https://gtregression.thinkdenominator.com/reference/save_table.md)
and relabelled with
[`modify_table`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
(caption, caveat, labels and headers only); it is not accepted by
[`merge_tables`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
or the forest-plot functions.

## References

Miettinen O, Nurminen M (1985). Comparative analysis of two rates.
*Statistics in Medicine*, 4(2), 213-226.

Newcombe RG (1998). Interval estimation for the difference between
independent proportions: comparison of eleven methods. *Statistics in
Medicine*, 17(8), 873-890.

Altman DG (1998). Confidence intervals for the number needed to treat.
*BMJ*, 317(7168), 1309-1312.

U.S. Food and Drug Administration (2023). Adjusting for Covariates in
Randomized Clinical Trials for Drugs and Biological Products: Guidance
for Industry.

## See also

[`uni_reg`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
and
[`multi_reg`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
for relative measures (odds ratios, risk ratios).

## Examples

``` r
# Crude: low birth weight by maternal smoking
rd <- risk_difference(data_birthwt, low, smoke)
rd$table_display
#>   Group Events / N  Risk Risk difference (95% CI) p-value
#> 1     0   29 / 115 25.2%                     Ref.        
#> 2     1    30 / 74 40.5%       15.3 (1.7 to 29.0)   0.027
rd$footnotes
#> [1] "Risk = proportion with the event (low = 1). Risk difference = 1 − 0 (groups of smoke), in percentage points."
#> [2] "Confidence interval and p-value: Miettinen–Nurminen score method."                                           
#> [3] "N = 189; no observations excluded."                                                                          
#> [4] "CI = confidence interval; Ref. = reference group."                                                           

# Adjusted (standardised), with the number needed to treat
adj <- risk_difference(
  data_birthwt, "low", "smoke",
  adjust_for = c("age", "lwt"),
  event_type = "harmful",
  format = "flextable"
)
adj$table_display
#>   Group Events / N Adjusted risk Adjusted risk difference (95% CI) p-value
#> 1     0   29 / 115         25.7%                              Ref.        
#> 2     1    30 / 74         39.7%                14.1 (0.3 to 27.8)   0.045
#>        NNT (95% CI)
#> 1                  
#> 2 NNTH 8 (4 to 303)
adj$table_body
#>   group events   n      risk adjusted_risk adjusted_risk_se        rd
#> 1     0     29 115 0.2521739     0.2565853       0.04063451        NA
#> 2     1     30  74 0.4054054     0.3972194       0.05724837 0.1406341
#>        rd_se      rd_low   rd_high    p_value
#> 1         NA          NA        NA         NA
#> 2 0.07006438 0.003310438 0.2779578 0.04472704
adj$nnt
#> $type
#> [1] "NNTH"
#> 
#> $estimate
#> [1] 7.110651
#> 
#> $crosses_infinity
#> [1] FALSE
#> 
#> $segments
#>   type     from       to
#> 1 NNTH 3.597669 302.0748
#> 

# Factor coding: name the event and the reference explicitly
bw <- data_birthwt
bw$smoking <- factor(bw$smoke, levels = c(0, 1), labels = c("No", "Yes"))
bw$weight <- factor(bw$low, levels = c(0, 1), labels = c("Normal", "Low"))
risk_difference(bw, weight, smoking, event = "Low", reference = "No",
                conf_level = 0.90)$table_display
#>   Group Events / N  Risk Risk difference (90% CI) p-value
#> 1    No   29 / 115 25.2%                     Ref.        
#> 2   Yes    30 / 74 40.5%       15.3 (3.9 to 26.8)   0.027
```
