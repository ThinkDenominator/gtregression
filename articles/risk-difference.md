# Risk Difference and NNT

Odds ratios and risk ratios describe how many *times* more likely an
event is. Readers of a trial report, clinicians and guideline panels
also need to know how many *more people out of every hundred* experience
the event, and how many people need to be treated for one of them to
benefit or be harmed.
[`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md)
reports exactly that:

- the absolute risk of the event in each of two groups;
- the risk difference in percentage points, with its confidence interval
  and p-value;
- optionally, the number needed to treat for one additional person to
  benefit (NNTB) or be harmed (NNTH).

It is a separate function from the regression tables because it answers
a different question; the regression functions are unchanged. The
examples below use `format = "flextable"`, which suits Word output;
`format = "gt"` (the default) gives an HTML-first table.

## Example data

The birth-weight data are used to ask a trial-style question: how much
higher is the absolute risk of low birth weight when the mother smokes?

``` r

library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")

bw <- data_birthwt |>
  mutate(
    smoke = factor(smoke, levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    race = factor(race, levels = c(1, 2, 3), labels = c("White", "Black", "Other"))
  )

attr(bw$smoke, "label") <- "Smoking in pregnancy"
attr(bw$low, "label") <- "Low birth weight"
```

## The crude risk difference

Name the outcome, the event and the reference group. Naming them is
optional (the defaults are the second outcome level and the first
exposure level, with a message), but explicit choices make the analysis
easier to read.

``` r

rd <- risk_difference(
  bw,
  outcome = low,
  exposure = smoke,
  event = "Low BW",
  reference = "Non-smoker",
  format = "flextable"
)

rd$table
```

| Smoking in pregnancy | Events / N | Risk | Risk difference (95% CI) | p-value |
|----|----|----|----|----|
| Non-smoker | 29 / 115 | 25.2% | Ref. |  |
| Smoker | 30 / 74 | 40.5% | 15.3 (1.7 to 29.0) | 0.027 |
| Risk = proportion with the event (Low birth weight = Low BW). Risk difference = Smoker − Non-smoker (groups of Smoking in pregnancy), in percentage points. |  |  |  |  |
| Confidence interval and p-value: Miettinen–Nurminen score method. |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |
| CI = confidence interval; Ref. = reference group. |  |  |  |  |

Reading the table:

- **Events / N** and **Risk** are the observed counts and proportions in
  each group.
- **Risk difference** is the comparison group minus the reference group,
  in percentage points. Here the risk of low birth weight is 15.3
  percentage points higher among smokers.
- The footnotes always state the event, the direction of the comparison
  and the method, so the table can be read without the code.

The crude interval and p-value use the Miettinen-Nurminen score method,
which keeps close to its nominal coverage in small samples and when
risks are near 0% or 100%.

The numbers behind the table are available for reporting or further
work:

``` r

rd$table_body
#>        group events   n      risk adjusted_risk adjusted_risk_se        rd
#> 1 Non-smoker     29 115 0.2521739            NA               NA        NA
#> 2     Smoker     30  74 0.4054054            NA               NA 0.1532315
#>   rd_se     rd_low   rd_high    p_value
#> 1    NA         NA        NA         NA
#> 2    NA 0.01722098 0.2897649 0.02689321
```

## Changing the event or the reference group

Swapping the reference group reverses the sign of the difference and
leaves the p-value unchanged.

``` r

rd_swapped <- risk_difference(bw, outcome = low, exposure = smoke,
                              event = "Low BW", reference = "Smoker",
                              format = "flextable")

rd_swapped$table
```

| Smoking in pregnancy | Events / N | Risk | Risk difference (95% CI) | p-value |
|----|----|----|----|----|
| Smoker | 30 / 74 | 40.5% | Ref. |  |
| Non-smoker | 29 / 115 | 25.2% | −15.3 (−29.0 to −1.7) | 0.027 |
| Risk = proportion with the event (Low birth weight = Low BW). Risk difference = Non-smoker − Smoker (groups of Smoking in pregnancy), in percentage points. |  |  |  |  |
| Confidence interval and p-value: Miettinen–Nurminen score method. |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |
| CI = confidence interval; Ref. = reference group. |  |  |  |  |

## Number needed to treat

Add `event_type` to report the number needed to treat. It must say
whether the event is **harmful** (for example death or infection) or
**beneficial** (for example cure), because that decides whether a
difference represents a benefit or a harm.

``` r

rd_nnt <- risk_difference(bw, outcome = low, exposure = smoke,
                          event = "Low BW", reference = "Non-smoker",
                          event_type = "harmful",
                          format = "flextable")

rd_nnt$table
```

| Smoking in pregnancy | Events / N | Risk | Risk difference (95% CI) | p-value | NNT (95% CI) |
|----|----|----|----|----|----|
| Non-smoker | 29 / 115 | 25.2% | Ref. |  |  |
| Smoker | 30 / 74 | 40.5% | 15.3 (1.7 to 29.0) | 0.027 | NNTH 7 (4 to 59) |
| Risk = proportion with the event (Low birth weight = Low BW). Risk difference = Smoker − Non-smoker (groups of Smoking in pregnancy), in percentage points. |  |  |  |  |  |
| Confidence interval and p-value: Miettinen–Nurminen score method. |  |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |  |
| NNTB/NNTH = number needed to treat for one additional person to benefit / be harmed (event classified as harmful). It applies to the follow-up period of the study and assumes the risk difference reflects the effect of the intervention. |  |  |  |  |  |
| CI = confidence interval; Ref. = reference group; NNTB = number needed to treat (benefit); NNTH = number needed to treat (harm). |  |  |  |  |  |

Here about 7 pregnancies exposed to smoking are associated with one
additional low-birth-weight baby (NNTH 7). The interval for the number
needed to treat comes from the interval for the risk difference.

When the risk-difference interval includes zero, the NNT interval passes
through infinity: the data are compatible with benefit, no effect and
harm. The table shows this explicitly instead of giving a misleading
finite range.

``` r

bw$visit <- factor(bw$ftv > 0, levels = c(FALSE, TRUE),
                   labels = c("No visit", "Visit"))

rd_visit <- risk_difference(bw, outcome = low, exposure = visit,
                            event = "Low BW", reference = "No visit",
                            event_type = "harmful",
                            format = "flextable")

rd_visit$table
```

| Group | Events / N | Risk | Risk difference (95% CI) | p-value | NNT (95% CI) |
|----|----|----|----|----|----|
| No visit | 36 / 100 | 36.0% | Ref. |  |  |
| Visit | 23 / 89 | 25.8% | −10.2 (−23.0 to 3.1) | 0.134 | NNTB 10 (NNTB 5 to ∞ to NNTH 32) |
| Risk = proportion with the event (Low birth weight = Low BW). Risk difference = Visit − No visit (groups of visit), in percentage points. |  |  |  |  |  |
| Confidence interval and p-value: Miettinen–Nurminen score method. |  |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |  |
| NNTB/NNTH = number needed to treat for one additional person to benefit / be harmed (event classified as harmful). It applies to the follow-up period of the study and assumes the risk difference reflects the effect of the intervention. |  |  |  |  |  |
| CI = confidence interval; Ref. = reference group; NNTB = number needed to treat (benefit); NNTH = number needed to treat (harm). |  |  |  |  |  |

An NNT applies to the follow-up period of the study and assumes the risk
difference reflects the effect of the intervention; the table footnote
says so. In observational data, interpret it with the same caution as
any other causal claim.

## Adjusted risk difference

`adjust_for` gives an adjusted risk difference. The risks are
standardised: a logistic regression with the exposure and the adjustment
variables is fitted, everyone in the analysis is predicted once as if in
the reference group and once as if in the comparison group, and the
predictions are averaged. The result is a risk difference for the whole
analysed population, on the same percentage-point scale as the crude
result.

``` r

rd_adjusted <- risk_difference(bw, outcome = low, exposure = smoke,
                               event = "Low BW", reference = "Non-smoker",
                               adjust_for = c(age, lwt, race),
                               event_type = "harmful",
                               format = "flextable")

rd_adjusted$table
```

| Smoking in pregnancy | Events / N | Adjusted risk | Adjusted risk difference (95% CI) | p-value | NNT (95% CI) |
|----|----|----|----|----|----|
| Non-smoker | 29 / 115 | 23.4% | Ref. |  |  |
| Smoker | 30 / 74 | 44.3% | 21.0 (6.9 to 35.1) | 0.004 | NNTH 5 (3 to 15) |
| Adjusted risk = standardised proportion with the event (Low birth weight = Low BW). Adjusted risk difference = Smoker − Non-smoker (groups of Smoking in pregnancy), in percentage points. Events / N are the observed counts. |  |  |  |  |  |
| Adjusted risks are standardised to the covariate distribution of the analysis population using logistic regression adjusted for age, lwt, race. |  |  |  |  |  |
| Confidence interval and p-value: Wald, influence-function standard error. |  |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |  |
| NNTB/NNTH = number needed to treat for one additional person to benefit / be harmed (event classified as harmful). It applies to the follow-up period of the study and assumes the risk difference reflects the effect of the intervention. |  |  |  |  |  |
| CI = confidence interval; Ref. = reference group; NNTB = number needed to treat (benefit); NNTH = number needed to treat (harm). |  |  |  |  |  |

**Events / N** are still the observed counts; the risks and their
difference are the adjusted values. The interval and p-value use a Wald
approach with an influence-function standard error, which accounts for
uncertainty in both the model and the covariate distribution.

In randomised trials, adjusting for a few strongly prognostic baseline
variables that were specified in advance usually gives a more precise
estimate of the same treatment effect. In observational studies
adjustment addresses measured confounding only.

## Confidence level

`conf_level` changes the interval only; the p-value always tests a risk
difference of zero.

``` r

rd_90 <- risk_difference(bw, outcome = low, exposure = smoke,
                         event = "Low BW", reference = "Non-smoker",
                         conf_level = 0.90,
                         format = "flextable")

rd_90$table
```

| Smoking in pregnancy | Events / N | Risk | Risk difference (90% CI) | p-value |
|----|----|----|----|----|
| Non-smoker | 29 / 115 | 25.2% | Ref. |  |
| Smoker | 30 / 74 | 40.5% | 15.3 (3.9 to 26.8) | 0.027 |
| Risk = proportion with the event (Low birth weight = Low BW). Risk difference = Smoker − Non-smoker (groups of Smoking in pregnancy), in percentage points. |  |  |  |  |
| Confidence interval and p-value: Miettinen–Nurminen score method. |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |
| CI = confidence interval; Ref. = reference group. |  |  |  |  |

## Missing values

Rows with a missing value in the outcome, exposure or any adjustment
variable are left out, and the footnote reports how many. Counts and
risks always refer to the analysed rows.

## Polish and save

[`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
adds a caption and caveat, and renames variables, groups and headers.
[`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
exports the result.

``` r

rd_final <- risk_difference(bw, outcome = low, exposure = smoke,
                            event = "Low BW", reference = "Non-smoker",
                            event_type = "harmful", format = "flextable") |>
  modify_table(
    level_labels = list(smoke = c(Smoker = "Current smoker")),
    caption = "Table 3. Absolute risk of low birth weight by smoking",
    caveat = "Observational data; differences may reflect confounding."
  )

rd_final$table
```

| Smoking in pregnancy | Events / N | Risk | Risk difference (95% CI) | p-value | NNT (95% CI) |
|----|----|----|----|----|----|
| Non-smoker | 29 / 115 | 25.2% | Ref. |  |  |
| Current smoker | 30 / 74 | 40.5% | 15.3 (1.7 to 29.0) | 0.027 | NNTH 7 (4 to 59) |
| Risk = proportion with the event (Low birth weight = Low BW). Risk difference = Current smoker − Non-smoker (groups of Smoking in pregnancy), in percentage points. |  |  |  |  |  |
| Confidence interval and p-value: Miettinen–Nurminen score method. |  |  |  |  |  |
| N = 189; no observations excluded. |  |  |  |  |  |
| NNTB/NNTH = number needed to treat for one additional person to benefit / be harmed (event classified as harmful). It applies to the follow-up period of the study and assumes the risk difference reflects the effect of the intervention. |  |  |  |  |  |
| CI = confidence interval; Ref. = reference group; NNTB = number needed to treat (benefit); NNTH = number needed to treat (harm). |  |  |  |  |  |
| Observational data; differences may reflect confounding. |  |  |  |  |  |

Table 3. Absolute risk of low birth weight by smoking {.table
.cl-d8bad469 quarto-disable-processing="true"}

``` r

save_table(rd_final, filename = "risk-difference", format = "docx")
```

## Reporting

A sentence for the results section can follow the table directly:

> Low birth weight occurred in 30 of 74 births to smokers (40.5%) and 29
> of 115 births to non-smokers (25.2%), a risk difference of 15.3
> percentage points (95% CI 1.7 to 29.0; Miettinen-Nurminen score
> method). This corresponds to a number needed to harm of 7 (95% CI 4 to
> 59).

## Methods at a glance

|  | Crude | Adjusted (`adjust_for`) |
|----|----|----|
| Risks | Observed proportions | Standardised from a main-effects logistic regression |
| Interval | Miettinen-Nurminen score | Wald, influence-function standard error |
| p-value | Miettinen-Nurminen score test | Wald test |
| Number needed to treat | 1 / risk difference, rounded up; Altman interval | Same |

## Scope

[`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md)
compares exactly two groups on a binary outcome with independent
observations. It does not handle clustered or matched data, time to
event, or more than two groups; restrict the data to two groups, or use
the
[regression](https://gtregression.thinkdenominator.com/articles/regression-tables.md)
and
[survival](https://gtregression.thinkdenominator.com/articles/survival-analysis.md)
functions for those designs. Risk-difference tables stand alone: they
can be relabelled with
[`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
and saved, but are not used in
[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md),
[`plot_reg()`](https://gtregression.thinkdenominator.com/reference/plot_reg.md)
or forest plots.
