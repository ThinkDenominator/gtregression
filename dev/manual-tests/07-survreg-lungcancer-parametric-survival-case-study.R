## Manual real-time test: parametric survival case study
## Package: gtregression 1.1
##
## Story:
## The oncology team has already reported Cox hazard ratios. Now they want a
## complementary parametric survival model that speaks in survival time rather
## than hazard. Parametric survival regression reports Time Ratios: values above
## 1 suggest longer survival time and values below 1 suggest shorter survival
## time, conditional on the selected distribution.
##
## How to use:
## Run this script section by section. This is a real-time manual testing script,
## not an automated test file. The comments tell the user what to look for.


## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)

library(gtregression)
library(dplyr)

data("data_lungcancer", package = "gtregression")


## 1. Prepare the lung cancer survival dataset --------------------------------

lung_data <- data_lungcancer |>
  mutate(
    trt = factor(
      trt,
      levels = c(1, 2),
      labels = c("Standard treatment", "Test treatment")
    ),
    prior = factor(
      prior,
      levels = c(0, 10),
      labels = c("No", "Yes")
    ),
    celltype = factor(
      celltype,
      levels = c("squamous", "smallcell", "adeno", "large"),
      labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
    )
  )

time_var <- "time"
event_var <- "status"
survival_exposures <- c("trt", "celltype", "karno", "age", "prior")

attr(lung_data$time, "label") <- "Survival time"
attr(lung_data$status, "label") <- "Death status"
attr(lung_data$trt, "label") <- "Treatment group"
attr(lung_data$celltype, "label") <- "Cancer cell type"
attr(lung_data$karno, "label") <- "Karnofsky performance score"
attr(lung_data$diagtime, "label") <- "Months from diagnosis to randomization"
attr(lung_data$age, "label") <- "Age"
attr(lung_data$prior, "label") <- "Prior therapy"


## 2. Descriptive table before modelling --------------------------------------

## The same descriptive profile can support both Cox and parametric survival
## models. It reminds readers what the survival groups look like clinically.

lung_summary <- descriptive_table(
  data = lung_data,
  exposures = c("time", "status", "celltype", "karno", "diagtime", "age", "prior"),
  by = trt,
  statistic = c(time = median, karno = mean, age = mean),
  percent = column,
  show_overall = last
)

lung_summary


## 3. Kaplan-Meier survival curve --------------------------------------------

## Even when the final model is parametric, the Kaplan-Meier curve is the first
## visual check of the observed survival pattern.

km_by_trt <- km_plot(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  break_time_by = 200,
  title = "Kaplan-Meier survival by treatment"
)

km_by_trt

## Minimal overall survival curve without grouping.
km_overall <- km_plot(
  data = lung_data,
  time = time,
  event = status,
  risk_table = FALSE,
  title = "Overall Kaplan-Meier survival"
)

km_overall

## Quoted column-name objects should work too. This mirrors the regression
## functions and helps when writing reusable analysis scripts.
km_plot(
  data = lung_data,
  time = time_var,
  event = event_var,
  by = "trt",
  break_time_by = 200,
  title = "Kaplan-Meier survival using stored column names"
)

## Useful display options before moving to model-based survival estimates.
km_custom <- km_plot(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  xlim = c(0, 800),
  break_time_by = 200,
  xlab = "Days after randomization",
  ylab = "Survival probability",
  legend_title = "Treatment arm",
  palette = c("#1F77B4", "#D55E00"),
  title = "Kaplan-Meier curve with custom display options"
)

km_custom

## A cleaner manuscript-style curve when the risk table is not needed.
km_plot(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  conf.int = FALSE,
  censor = FALSE,
  risk_table = FALSE,
  title = "Kaplan-Meier survival without confidence bands or censor marks"
)


## 4. Standalone Kaplan-Meier risk table --------------------------------------

## km_plot() can display a risk table underneath the curve. km_risk_table()
## gives the same idea as a standalone formatted table.
##
## What to check:
## - Time 0 should show the starting number at risk.
## - Each requested time should appear within each treatment group.
## - The table should include at-risk, event, and censored counts.

km_risk <- km_risk_table(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(0, 90, 180, 365)
)

km_risk

## Tibble output for checking exact values.
km_risk_table(
  data = lung_data,
  time = time_var,
  event = event_var,
  times = c(0, 90, 180),
  format = tibble
)

## gt output is useful for HTML/pkgdown-style viewing.
km_risk_table(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(0, 90, 180, 365),
  digits = 0,
  format = gt
)


## 5. Restricted mean survival time ------------------------------------------

## RMST is the average survival time up to a fixed time point. It gives an
## absolute survival-time summary that complements model-based time ratios.
##
## What to check:
## - The default output should be a publication-style flextable.
## - tau should appear clearly so users know the time window.
## - With two groups, the table should include the RMST difference.

rmst_365 <- rmst_table(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  tau = 365
)

rmst_365

## Tibble output for checking exact values.
rmst_table(
  data = lung_data,
  time = time_var,
  event = event_var,
  tau = 180,
  format = tibble
)

## gt output works well in HTML/pkgdown-style documents.
rmst_table(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  tau = 365,
  digits = 0,
  format = gt
)


## 6. Kaplan-Meier survival summary table ------------------------------------

## Before interpreting parametric time ratios, show the observed median survival
## from Kaplan-Meier methods. This anchors the model-based estimates.

km_summary <- survival_summary(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

km_summary

## Overall median survival as a tibble for checking exact values.
survival_summary(
  data = lung_data,
  time = time_var,
  event = event_var,
  format = tibble
)

## gt output is useful for HTML/pkgdown-style viewing.
survival_summary(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  digits = 0,
  format = gt
)


## 7. Kaplan-Meier survival quantiles -----------------------------------------

## Quantiles give more detail than the median alone. These observed survival
## time points can help users judge whether a parametric model feels plausible.
##
## What to check:
## - The default output should be a publication-style flextable.
## - Treatment groups should appear clearly.
## - These observed quantiles should be interpreted before model-based
##   time-ratio estimates from surv_reg().

km_quantiles <- survival_quantiles(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

km_quantiles

## Tibble output for checking exact values.
km_quantiles_tibble <- survival_quantiles(
  data = lung_data,
  time = time_var,
  event = event_var,
  probs = c(0.25, 0.5),
  format = tibble
)

km_quantiles_tibble

## Request custom quantiles when the median alone is not enough.
survival_quantiles(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  probs = c(0.1, 0.25, 0.5, 0.75),
  digits = 0
)

## gt output is useful for HTML/pkgdown-style viewing.
survival_quantiles(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  format = gt
)


## 8. Fixed-time Kaplan-Meier survival probabilities --------------------------

## Fixed-time survival is often the easiest summary for readers: for example,
## 90-day, 180-day, and 1-year survival. This helps readers understand observed
## survival before interpreting model-based time ratios.
##
## What to check:
## - The default output should be a publication-style flextable.
## - Each requested time should appear within each treatment group.
## - The table should include at-risk, event, censored, and survival probability
##   with 95% CI columns.

km_prob <- survival_prob(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(90, 180, 365)
)

km_prob

## Tibble output for checking exact values.
km_prob_tibble <- survival_prob(
  data = lung_data,
  time = time_var,
  event = event_var,
  times = c(90, 180),
  format = tibble
)

km_prob_tibble

## If a requested time is beyond observed follow-up, extend = TRUE keeps the
## requested row using the last available Kaplan-Meier estimate. Set
## extend = FALSE when you prefer to omit times beyond the available follow-up.
survival_prob(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(90, 180, 365, 1200),
  extend = FALSE,
  format = tibble
)

## gt output is useful for HTML/pkgdown-style viewing.
survival_prob(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  times = c(90, 180, 365),
  digits = 0,
  format = gt
)


## 9. Log-rank test: compare Kaplan-Meier curves ------------------------------

## The log-rank test is a non-parametric group comparison. It helps describe the
## observed survival difference before moving into parametric survival models.

logrank_trt <- logrank_test(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

logrank_trt

## Tibble output keeps the test statistic and p-value easy to inspect.
logrank_test(
  data = lung_data,
  time = time_var,
  event = event_var,
  by = "trt",
  format = tibble
)

## gt output is useful for HTML/pkgdown-style viewing.
logrank_test(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  format = gt
)


## 10. Compare candidate parametric survival distributions --------------------

## Before choosing a parametric survival model, compare common distributions
## using the same model formula. Lower AIC/BIC suggests better relative fit, but
## users should still use clinical judgement and visual checks.
##
## What to check:
## - The default output should be a publication-style flextable.
## - Best AIC and Best BIC should be easy to spot.
## - Quoted names, unquoted distribution values, and common aliases should work.

surv_dist_compare <- surv_model_compare(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "prior"),
  adjust_for = c("age", "karno")
)

surv_dist_compare

## Tibble output for exact model-fit values.
surv_model_compare(
  data = lung_data,
  time = time_var,
  event = event_var,
  exposures = c(trt, prior),
  distributions = c(weibull, "log-normal", "log-logistic"),
  format = tibble
)

## gt output works well in HTML/pkgdown-style documents.
surv_model_compare(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  distributions = c(weibull, exponential, lognormal, loglogistic),
  digits = 1,
  format = gt
)


## 11. Visual check of parametric survival fit ---------------------------------

## AIC/BIC helps compare distributions numerically. plot_surv_fit() adds the
## visual check: does the fitted parametric curve follow the observed
## Kaplan-Meier curve?
##
## What to check:
## - Observed Kaplan-Meier curves should appear as solid lines.
## - Parametric fitted curves should appear as dashed/alternative line types.
## - If curves differ sharply from KM, reconsider the distribution even when
##   AIC/BIC looks attractive.

plot_surv_fit(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  distributions = c(weibull, "log-logistic"),
  break_time_by = 200,
  title = "Observed Kaplan-Meier and fitted survival curves"
)

## Adjusted fitted curves are predicted at typical adjustment values.
## Numeric adjustment variables use the median; categorical adjustment variables
## use the most common level. This keeps the plot readable.
plot_surv_fit(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  adjust_for = c(age, karno),
  distributions = loglogistic,
  xlim = c(0, 800),
  break_time_by = 200,
  title = "Adjusted fitted log-logistic survival curves"
)

## Quoted column names and stored objects work inside scripts and functions.
plot_surv_fit(
  data = lung_data,
  time = time_var,
  event = event_var,
  by = "trt",
  distributions = c(exp, "log-logistic")
)


## 12. Crude parametric survival regression -----------------------------------

## Default distribution is Weibull.
## Output should show Time Ratio (95% CI), not HR.

surv_weibull <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures
)

surv_weibull
surv_weibull$table_body
surv_weibull$models
surv_weibull$model_summaries

## Explicit distribution can be supplied without quotes.
surv_lognormal <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  distribution = lognormal,
  format = gt
)

surv_lognormal

## Common aliases are accepted too.
surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, prior),
  distribution = "log-logistic"
)


## 13. Compare common parametric distributions with surv_reg() ----------------

## This is not automatic model selection. The user should compare candidate
## distributions with clinical/statistical judgement and diagnostics.

surv_weibull_stats <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  distribution = weibull,
  model_stats = TRUE
)

surv_exponential_stats <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  distribution = exponential,
  model_stats = TRUE
)

surv_lognormal_stats <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  distribution = lognormal,
  model_stats = TRUE
)

surv_loglogistic_stats <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  distribution = loglogistic,
  model_stats = TRUE
)

surv_weibull_stats$model_stats
surv_exponential_stats$model_stats
surv_lognormal_stats$model_stats
surv_loglogistic_stats$model_stats


## 14. Single multivariable parametric survival model -------------------------

## Question:
## What happens if the exposure list itself defines the final AFT model?
##
## Use multivariable = TRUE when you want one parametric survival model
## containing all variables listed in exposures, and you want all coefficients
## reported. The heading should read Adjusted Time Ratio (95% CI), because each
## coefficient is adjusted for the other variables in the model.

surv_full_model <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior, age, karno),
  distribution = weibull,
  multivariable = TRUE,
  model_stats = TRUE
)

surv_full_model
surv_full_model$table_body
surv_full_model$model_stats

## multivariate = TRUE is accepted as a user-friendly alias.
surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  multivariate = TRUE
)


## 14A. Parametric survival interaction term ---------------------------------

## Question:
## Does the treatment time ratio differ according to prior therapy?
##
## This mirrors cox_reg() and multi_reg(): use interaction = exposure*modifier.
## In the exposure-by-exposure workflow, keep exposures to the single exposure
## being interpreted. The table should read Adjusted Time Ratio (95% CI).

surv_interaction <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno),
  interaction = trt*prior,
  distribution = weibull,
  model_stats = TRUE
)

surv_interaction
surv_interaction$table_body
surv_interaction$model_stats

## In a single multivariable parametric survival model, the exposure list
## defines the whole model and the interaction is added to that one model.
surv_full_interaction <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  interaction = "trt*prior",
  distribution = lognormal,
  multivariable = TRUE
)

surv_full_interaction


## 14B. Parametric survival confounding and interaction screening -------------

## These checks mirror the Cox workflow, but the estimates are time-ratio
## style because surv_reg() uses parametric accelerated failure time models.

aft_conf_prior <- identify_confounder(
  data = lung_data,
  time = time,
  event = status,
  exposure = trt,
  potential_confounder = prior,
  approach = surv,
  distribution = weibull,
  method = change,
  format = gt
)

aft_conf_prior
aft_conf_prior$table
aft_conf_prior$summary

aft_interaction_screen <- interaction_models(
  data = lung_data,
  time = time,
  event = status,
  exposure = trt,
  effect_modifier = prior,
  covariates = c(age, karno),
  approach = surv,
  distribution = weibull,
  test = LRT,
  format = gt
)

aft_interaction_screen$table
aft_interaction_screen$p_value
aft_interaction_screen$interaction_terms


## 15. Compare prespecified parametric survival models ------------------------

## surv_model_compare() compares distributions for one formula.
## compare_models() compares fitted candidate gtregression objects from
## surv_reg(). This is useful when the question is whether adding clinical
## predictors improves a chosen parametric model.

aft_m0 <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  distribution = weibull
)

aft_m1 <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno),
  distribution = weibull
)

aft_m2 <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno, celltype, prior),
  multivariable = TRUE,
  distribution = weibull
)

aft_compare <- compare_models(
  aft_m0,
  aft_m1,
  aft_m2,
  model_names = c(
    "Treatment only",
    "Add age and performance",
    "Full clinical model"
  ),
  primary_exposure = trt
)

aft_compare
aft_compare$table_body
aft_compare$table_display

compare_models(
  list(
    "Treatment only" = aft_m0,
    "Adjusted core" = aft_m1,
    "Full clinical model" = aft_m2
  ),
  primary_exposure = "trt",
  format = gt
)


## 15. Adjusted parametric survival regression --------------------------------

## With adjust_for, surv_reg() fits one adjusted model per exposure and reports
## Adjusted Time Ratio (95% CI).

surv_adjusted <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "prior"),
  adjust_for = c("age", "karno"),
  distribution = weibull,
  model_stats = TRUE
)

surv_adjusted
surv_adjusted$table_body
surv_adjusted$model_stats

## Bare and quoted names should both work.
surv_reg(
  data = lung_data,
  time = time_var,
  event = event_var,
  exposures = c(trt, prior),
  adjust_for = c(age, karno),
  distribution = lognormal,
  format = flextable
)


## 16. Predict survival probabilities from parametric models ------------------

## surv_predict() answers a practical clinical question after fitting surv_reg():
## what is the model-based survival probability at selected follow-up times for
## a patient/profile we care about?
##
## Because surv_adjusted contains one model per exposure, pass the specific
## model you want to use, for example surv_adjusted$models$trt.

surv_predict(
  model = surv_adjusted$models$trt,
  newdata = data.frame(
    trt = factor("Test treatment", levels = levels(lung_data$trt)),
    age = 60,
    karno = 70
  ),
  times = c(90, 180, 365)
)

## Compare two treatment profiles at the same age and Karnofsky score.
surv_predict(
  model = surv_adjusted$models$trt,
  newdata = data.frame(
    trt = factor(
      c("Standard treatment", "Test treatment"),
      levels = levels(lung_data$trt)
    ),
    age = c(60, 60),
    karno = c(70, 70)
  ),
  times = c(90, 180),
  format = tibble
)

## If newdata is omitted, surv_predict() uses a typical model profile:
## median values for numeric predictors and the most common level for factors.
surv_predict(
  model = surv_adjusted$models$trt,
  times = c(90, 180, 365),
  format = gt
)


## 17. Polish the tables -------------------------------------------------------

surv_weibull_paper <- modify_table(
  surv_weibull,
  header_labels = c(
    estimate = "Crude Time Ratio",
    p.value = "P value"
  ),
  caption = "Table 1. Weibull parametric survival regression",
  caveat = paste(
    "Time Ratio above 1 suggests longer survival time.",
    "Interpret after checking whether the chosen distribution is reasonable."
  )
)

surv_weibull_paper

surv_adjusted_paper <- modify_table(
  surv_adjusted,
  header_labels = c(
    estimate = "Adjusted Time Ratio",
    p.value = "P value"
  ),
  caption = "Table 2. Adjusted Weibull parametric survival regression",
  caveat = "Adjusted models include age and Karnofsky performance score."
)

surv_adjusted_paper


## 18. Merge descriptive, crude, and adjusted survival tables ------------------

final_surv_table <- merge_tables(
  lung_summary,
  surv_weibull_paper,
  surv_adjusted_paper,
  spanners = c("Baseline profile", "Crude AFT", "Adjusted AFT")
)

final_surv_table

final_surv_table_paper <- modify_table(
  final_surv_table,
  caveat = paste(
    "AFT = accelerated failure time model.",
    "Time ratios are conditional on the selected parametric distribution."
  )
)

final_surv_table_paper


## 19. Visualise parametric survival results ----------------------------------

## plot_reg() works directly with surv_reg() objects. Use log_x = TRUE because
## Time Ratios are ratio measures centred on 1.

plot_surv_crude <- plot_reg(
  surv_weibull,
  show_ref = TRUE,
  title = "Crude parametric survival regression"
)

plot_surv_crude

plot_surv_adjusted <- plot_reg(
  surv_adjusted,
  show_ref = TRUE,
  title = "Adjusted parametric survival regression"
)

plot_surv_adjusted

plot_surv_combined <- plot_reg_combine(
  surv_weibull,
  surv_adjusted,
  title_uni = "Crude Time Ratio",
  title_multi = "Adjusted Time Ratio"
)

plot_surv_combined

## Compact binary display and custom log-axis breaks should also work.
plot_reg(
  surv_weibull,
  show_ref = FALSE,
  xlim = c(0.25, 8),
  breaks = c(0.5, 1, 2, 4, 8),
  title = "Crude parametric survival regression with compact binary rows"
)

plot_reg_combine(
  surv_weibull,
  surv_adjusted,
  show_ref = FALSE,
  xlim_uni = c(0.25, 8),
  breaks_uni = c(0.5, 1, 2, 4, 8),
  xlim_multi = c(0.25, 8),
  breaks_multi = c(0.5, 1, 2, 4, 8),
  title_uni = "Crude Time Ratio",
  title_multi = "Adjusted Time Ratio"
)


## 20. Forest table for parametric survival results ---------------------------

surv_forest_crude <- forest_df(surv_weibull)
surv_forest_crude

surv_forest_adjusted <- forest_df(surv_adjusted)
surv_forest_adjusted

surv_forest_data <- forest_df(surv_weibull, surv_adjusted, desc = lung_summary)
surv_forest_data

forest_reg(surv_forest_crude)
forest_reg(surv_forest_adjusted)

surv_forest <- forest_reg(surv_forest_data)
surv_forest

## Useful option: put the plot on the left of the effect text.
forest_reg(surv_forest_data, side = "left")

## Layout check:
## If x-axis tick labels overlap, control the axis with xlim and ticks_at.
## Parametric survival regression reports Time Ratios, so the no-effect line is 1.
forest_reg(
  surv_forest_data,
  xlim = list(c(0.25, 2), c(0.25, 2)),
  ticks_at = list(c(0.25, 0.5, 1, 2), c(0.25, 0.5, 1, 2))
)

## If the CI plot panel is too narrow or too wide, tune ci_col_width.
forest_reg(surv_forest_data, ci_col_width = c(18, 22))

forest_reg(
  uni = surv_weibull,
  multi = surv_adjusted,
  desc = lung_summary
)


## 21. Model selection for parametric survival regression ----------------------

## select_models() supports approach = survreg. Use the distribution argument to
## choose the parametric survival distribution used during selection.

surv_selection <- select_models(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  approach = survreg,
  distribution = weibull,
  direction = forward,
  format = flextable
)

surv_selection
surv_selection$results_table
surv_selection$best_model

## Check other directions, formats, and distributions.
select_models(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  approach = survreg,
  distribution = lognormal,
  direction = backward,
  format = gt
)

select_models(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  approach = survreg,
  distribution = loglogistic,
  direction = both,
  format = tibble
)


## 22. Export outputs ----------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This keeps examples CRAN-safe and avoids accidental clutter.

save_table(final_surv_table_paper, filename = "lung-parametric-survival-table", format = "docx")
save_plot(km_by_trt, filename = "lung-km-survival-curve", format = "png")
save_plot(plot_surv_combined, filename = "lung-parametric-survival-plot", format = "png")

save_docx(
  tables = list(surv_weibull_paper, surv_adjusted_paper, final_surv_table_paper),
  plots = list(plot_surv_crude, plot_surv_adjusted, plot_surv_combined),
  titles = list(
    "Crude parametric survival regression",
    "Adjusted parametric survival regression",
    "Combined descriptive and parametric survival table",
    "Forest plot - crude parametric survival",
    "Forest plot - adjusted parametric survival",
    "Crude versus adjusted parametric survival plot"
  ),
  filename = "lung-parametric-survival-report",
  table_width = 6.5
)


## 23. Final checklist ---------------------------------------------------------

## Things to confirm manually:
## - surv_reg() displays Time Ratio (95% CI), not HR.
## - km_plot() displays Kaplan-Meier curves with optional risk tables and log-rank p-values.
## - km_risk_table() reports standalone at-risk, event, and censored counts.
## - rmst_table() reports restricted mean survival time and two-group RMST differences.
## - survival_summary() reports Kaplan-Meier median survival with events and censoring.
## - survival_quantiles() reports detailed Kaplan-Meier survival time quantiles.
## - survival_prob() reports fixed-time Kaplan-Meier survival probabilities.
## - logrank_test() compares Kaplan-Meier curves and reports observed/expected events.
## - surv_model_compare() compares candidate AFT distributions before final modelling.
## - plot_surv_fit() overlays fitted AFT curves on observed Kaplan-Meier curves.
## - surv_predict() reports model-based survival probabilities for chosen profiles and times.
## - surv_reg(adjust_for = ...) displays Adjusted Time Ratio (95% CI).
## - compare_models() compares fitted surv_reg() outputs without refitting them.
## - compare_models(primary_exposure = ...) reports the selected Time Ratio and percentage change.
## - distribution accepts unquoted weibull, exponential, lognormal, loglogistic.
## - model_stats = TRUE stores distribution, scale, events, N, AIC, and BIC.
## - Factor reference categories display as Ref.
## - Variable labels set once are used in parametric survival tables.
## - modify_table() can relabel headers and add distribution caveats.
## - merge_tables() combines descriptive, crude, and adjusted survival outputs.
## - plot_reg() and plot_reg_combine() work with Time Ratio outputs.
## - plot_reg() respects show_ref = FALSE, xlim, and breaks for Time Ratio plots.
## - forest_df() and forest_reg() work with Time Ratio outputs.
## - forest_reg(side = "left") works with parametric survival forest tables.
## - select_models() supports approach = survreg with time, event, and distribution.
## - select_models() supports forward, backward, and both directions for survreg.
## - save_docx(table_width = 6.5) keeps wide survival tables fitted to Word.
