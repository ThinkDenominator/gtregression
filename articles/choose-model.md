# Which Function Should I Use?

Start from two questions: **what kind of outcome do you have**, and
**how were the observations collected**? The tables below point to the
function, the `approach` where one is needed, and the effect measure
that appears in the table. Every function shares the same arguments for
exposures, adjustment variables, formatting and export.

## Binary outcomes

An outcome with two values, such as disease or no disease.

| Question | Function | `approach` | Reported as |
|----|----|----|----|
| Odds of the outcome | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) | `"logit"` | Odds ratio |
| Odds with sparse data or separation | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) | `"firth"` | Odds ratio (Firth) |
| Risk of the outcome, relative | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) | `"logbinomial"` or `"robpoisson"` | Risk ratio |
| Risk of the outcome, absolute (two groups) | [`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md) | none | Risk difference, NNTB/NNTH |
| Matched case-control sets | [`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md) | none | Matched odds ratio |
| Clustered observations (clinics, schools) | [`multilevel_reg()`](https://gtregression.thinkdenominator.com/reference/multilevel_reg.md) | `"logit"` | Odds ratio, random intercept |

`"robpoisson"` (Poisson with robust standard errors) is a dependable
alternative when a log-binomial model fails to converge.

## Counts and rates

| Question | Function | `approach` | Reported as |
|----|----|----|----|
| Event counts | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) | `"poisson"` | Incidence rate ratio |
| Over-dispersed counts | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) | `"negbin"` | Incidence rate ratio |

## Continuous outcomes

| Question | Function | `approach` | Reported as |
|----|----|----|----|
| Mean of the outcome | [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) | `"linear"` | Beta (difference in means) |
| Clustered observations | [`multilevel_reg()`](https://gtregression.thinkdenominator.com/reference/multilevel_reg.md) | `"linear"` | Beta, random intercept |

## Outcomes with three or more categories

| Question | Function | Reported as |
|----|----|----|
| Ordered categories (mild, moderate, severe) | [`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md) | Common odds ratio (proportional odds) |
| Unordered categories | [`multinomial_reg()`](https://gtregression.thinkdenominator.com/reference/multinomial_reg.md) | Odds ratio for each category versus the reference category |

## Time to event

| Question | Function | Reported as |
|----|----|----|
| Describe survival over time | [`km_plot()`](https://gtregression.thinkdenominator.com/reference/km_plot.md), [`survival_summary()`](https://gtregression.thinkdenominator.com/reference/survival_summary.md), [`survival_prob()`](https://gtregression.thinkdenominator.com/reference/survival_prob.md) | Kaplan-Meier estimates |
| Compare survival curves | [`logrank_test()`](https://gtregression.thinkdenominator.com/reference/logrank_test.md), [`rmst_table()`](https://gtregression.thinkdenominator.com/reference/rmst_table.md) | Log-rank p-value, restricted mean survival time |
| Hazard of the event | [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md) | Hazard ratio |
| Survival time itself | [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md) | Time ratio (accelerated failure time) |
| One event type in the presence of others | [`cuminc_plot()`](https://gtregression.thinkdenominator.com/reference/cuminc_plot.md), [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md) | Cumulative incidence, subdistribution hazard ratio |

Check the proportional-hazards assumption of a Cox model with
[`check_ph()`](https://gtregression.thinkdenominator.com/reference/check_ph.md).

## Repeating an analysis within groups

[`stratified_uni_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_uni_reg.md)
and
[`stratified_multi_reg()`](https://gtregression.thinkdenominator.com/reference/stratified_multi_reg.md)
fit the same model within each level of a stratifier, for example by sex
or study site.

## Beyond a single estimate

| Question | Function |
|----|----|
| Is a variable a confounder or effect modifier? | [`identify_confounder()`](https://gtregression.thinkdenominator.com/reference/identify_confounder.md), [`interaction_models()`](https://gtregression.thinkdenominator.com/reference/interaction_models.md) |
| How much of an effect acts through a mediator? | [`mediation_analysis()`](https://gtregression.thinkdenominator.com/reference/mediation_analysis.md) |
| Does the model fit and converge? | [`check_convergence()`](https://gtregression.thinkdenominator.com/reference/check_convergence.md), [`check_collinearity()`](https://gtregression.thinkdenominator.com/reference/check_collinearity.md), [`plot_model_fit()`](https://gtregression.thinkdenominator.com/reference/plot_model_fit.md) |
| Which of several prespecified models fits best? | [`compare_models()`](https://gtregression.thinkdenominator.com/reference/compare_models.md), [`select_models()`](https://gtregression.thinkdenominator.com/reference/select_models.md) |

## Next steps

- [Start
  Here](https://gtregression.thinkdenominator.com/articles/gtregression-intro.md)
  walks through a complete analysis.
- [Function
  Options](https://gtregression.thinkdenominator.com/articles/function-options.md)
  lists every argument and default.
- [Merge and Customise
  Tables](https://gtregression.thinkdenominator.com/articles/merge-tables.md)
  combines the results into a manuscript table.
