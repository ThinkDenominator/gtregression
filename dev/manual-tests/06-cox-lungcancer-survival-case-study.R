## Manual real-time test: Cox survival case study
## Package: gtregression 1.1
##
## Story:
## An oncology team wants to report survival after lung cancer treatment. The
## main question is whether treatment group, cancer cell type, performance
## status, age, and prior therapy are associated with time to death. Cox
## regression gives hazard ratios (HRs), and adjusted Cox regression gives
## adjusted HRs after accounting for core prognostic variables.
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

## The built-in data_lungcancer dataset already contains the two survival
## essentials:
## - time: survival time in days
## - status: 1 = died, 0 = censored
##
## We recode treatment and prior therapy so the table reads naturally.

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

## 1.1-friendly labels:
## Label once, then all gtregression tables use these display names.
attr(lung_data$time, "label") <- "Survival time"
attr(lung_data$status, "label") <- "Death status"
attr(lung_data$trt, "label") <- "Treatment group"
attr(lung_data$celltype, "label") <- "Cancer cell type"
attr(lung_data$karno, "label") <- "Karnofsky performance score"
attr(lung_data$diagtime, "label") <- "Months from diagnosis to randomization"
attr(lung_data$age, "label") <- "Age"
attr(lung_data$prior, "label") <- "Prior therapy"


## 2. Inspect data before survival modelling ----------------------------------

## Default output is a publication-style flextable.
dissect(lung_data)

## Use tibble output for a console-friendly audit.
lung_dissect <- dissect(lung_data, format = "tibble")
lung_dissect


## 3. Kaplan-Meier survival curve --------------------------------------------

## Start with the observed survival curve before fitting Cox models.
## The risk table shows how many participants remain under observation at each
## time point. The p-value is a log-rank test.

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

## Quoted column-name objects should work too. This is helpful inside scripts,
## functions, and Shiny-style workflows.
km_plot(
  data = lung_data,
  time = time_var,
  event = event_var,
  by = "trt",
  break_time_by = 200,
  title = "Kaplan-Meier survival using stored column names"
)

## Useful display options:
## - xlim controls the visible follow-up window.
## - break_time_by controls the x-axis and risk-table time points.
## - palette, xlab, ylab, legend_title, conf.int, and censor polish the figure.
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
## gives the same idea as a standalone publication-ready table.
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

## Console-friendly output for checking exact values.
km_risk_table(
  data = lung_data,
  time = time_var,
  event = event_var,
  times = c(0, 90, 180),
  format = tibble
)

## gt output works well in HTML/pkgdown-style documents.
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

## RMST is the average survival time up to a fixed time point. This is often
## easier to explain than a hazard ratio: "average survival time within 1 year".
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

## Console-friendly output for checking exact values.
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

## The curve is visual. This table gives the numbers users usually want beside
## the curve: N, events, censored observations, and median survival with 95% CI.

km_summary <- survival_summary(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

km_summary

## Tibble output is useful when checking or reporting the exact values.
survival_summary(
  data = lung_data,
  time = time_var,
  event = event_var,
  format = tibble
)

## gt output works for HTML/pkgdown-style viewing.
survival_summary(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  digits = 0,
  format = gt
)


## 7. Kaplan-Meier survival quantiles -----------------------------------------

## Quantiles give more detail than the median alone. The 50% event percentile
## is the median survival time. The 25% event percentile corresponds to 75%
## survival, and the 75% event percentile corresponds to 25% survival.
##
## What to check:
## - The default output should be a publication-style flextable.
## - Treatment groups should appear clearly.
## - "Not reached" should appear when a requested percentile is not estimable.

km_quantiles <- survival_quantiles(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

km_quantiles

## Console-friendly output for checking exact values.
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

## gt output works well in HTML/pkgdown-style documents.
survival_quantiles(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  format = gt
)


## 8. Fixed-time Kaplan-Meier survival probabilities --------------------------

## Fixed-time survival is often the easiest summary for readers: for example,
## 90-day, 180-day, and 1-year survival. This complements the median and
## quantile tables.
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

## Console-friendly output for checking exact values.
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

## gt output works well in HTML/pkgdown-style documents.
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

## The log-rank test formally compares survival curves between treatment arms.
## It gives a p-value for group comparison, not an effect size. Use cox_reg()
## when you want a hazard ratio.

logrank_trt <- logrank_test(
  data = lung_data,
  time = time,
  event = status,
  by = trt
)

logrank_trt

## Tibble output keeps the chi-square statistic, df, and p-value available for
## reporting or additional checks.
logrank_test(
  data = lung_data,
  time = time_var,
  event = event_var,
  by = "trt",
  format = tibble
)

## gt output works well in HTML/pkgdown-style documents.
logrank_test(
  data = lung_data,
  time = time,
  event = status,
  by = trt,
  format = gt
)


## 10. Descriptive table: baseline profile by treatment ------------------------

## Before fitting survival models, describe the treatment groups. This helps the
## reader see whether the clinical profile differs between treatment arms.

lung_summary <- descriptive_table(
  data = lung_data,
  exposures = c("time", "status", "celltype", "karno", "diagtime", "age", "prior"),
  by = trt,
  statistic = c(time = median, karno = mean, age = mean),
  percent = column,
  show_overall = last
)

lung_summary

## Explicit gt output for pkgdown/HTML-style viewing.
descriptive_table(
  data = lung_data,
  exposures = c("time", "status", "celltype", "karno", "diagtime", "age", "prior"),
  by = trt,
  statistic = c(time = median, karno = mean, age = mean),
  percent = column,
  show_overall = last,
  format = gt
)


## 11. Crude Cox proportional hazards regression ------------------------------

## Question:
## What is the crude association between each predictor and time to death?
##
## Output should show HR (95% CI). Factor variables should show Ref. for the
## reference category.

cox_crude <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures
)

cox_crude
cox_crude$table_body
cox_crude$models
cox_crude$model_summaries

## gt output should render the same results in HTML style.
cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  format = gt
)


## 12. Optional model statistics ----------------------------------------------

## model_stats = TRUE keeps fit statistics outside the publication table.
## For Cox models, inspect AIC, BIC, log-likelihood, concordance/C-index,
## number of events, and N.

cox_crude_stats <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  model_stats = TRUE
)

cox_crude_stats$model_stats


## 13. Adjusted Cox regression -------------------------------------------------

## Question:
## What are the adjusted HRs after accounting for age and performance status?
##
## With adjust_for, cox_reg() fits one adjusted Cox model per exposure and shows
## Adjusted HR (95% CI). This matches the existing gtregression pattern used in
## multi_reg(adjust_for = ...).

cox_adjusted <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "prior"),
  adjust_for = c("age", "karno"),
  model_stats = TRUE
)

cox_adjusted
cox_adjusted$table_body
cox_adjusted$model_stats

## Bare and quoted names should both work.
cox_reg(
  data = lung_data,
  time = time_var,
  event = event_var,
  exposures = c(trt, prior),
  adjust_for = c(age, karno),
  format = flextable
)


## 14. Check proportional hazards assumption ----------------------------------

## Cox HRs assume the hazard ratio is reasonably constant over follow-up time.
## check_ph() uses Schoenfeld residual tests via survival::cox.zph().
## A small p-value is a screening signal, not an automatic reason to discard the
## model. Interpret with residual plots, follow-up pattern, and clinical context.

check_ph(cox_crude)

cox_ph_adjusted <- check_ph(cox_adjusted, format = tibble)
cox_ph_adjusted

## Other time transformations supported by cox.zph() can be requested without
## quotes.
check_ph(cox_adjusted, transform = rank, format = gt)

## Direct coxph models are also supported.
check_ph(cox_adjusted$models$trt, format = tibble)


## 15. Polish the Cox tables ---------------------------------------------------

## modify_table() should work with cox_reg() objects just like other regression
## outputs. Use this for publication wording, headers, captions, and caveats.

cox_crude_paper <- modify_table(
  cox_crude,
  header_labels = c(
    estimate = "Crude HR",
    p.value = "P value"
  ),
  caption = "Table 1. Crude Cox regression for lung cancer survival",
  caveat = paste(
    "Cox models estimate hazard ratios.",
    "Assess proportional hazards assumptions before final inference."
  )
)

cox_crude_paper

cox_adjusted_paper <- modify_table(
  cox_adjusted,
  header_labels = c(
    estimate = "Adjusted HR",
    p.value = "P value"
  ),
  caption = "Table 2. Adjusted Cox regression for lung cancer survival",
  caveat = "Adjusted models include age and Karnofsky performance score."
)

cox_adjusted_paper


## 16. Merge descriptive, crude HR, and adjusted HR tables ---------------------

## A common manuscript table places baseline profile, crude HR, and adjusted HR
## side by side. This should preserve clean headers and reference rows.

final_survival_table <- merge_tables(
  lung_summary,
  cox_crude_paper,
  cox_adjusted_paper,
  spanners = c("Baseline profile", "Crude Cox", "Adjusted Cox")
)

final_survival_table

final_survival_table_paper <- modify_table(
  final_survival_table,
  caveat = paste(
    "HR = hazard ratio.",
    "This table is publication-ready, but proportional hazards diagnostics",
    "should be checked before final reporting."
  )
)

final_survival_table_paper


## 17. Visualise Cox results ---------------------------------------------------

## plot_reg() works directly with cox_reg() objects. Use log_x = TRUE because
## HRs are ratio measures centred on 1.

plot_cox_crude <- plot_reg(cox_crude)

plot_cox_crude

plot_cox_adjusted <- plot_reg(
  cox_adjusted,
  show_ref = TRUE,
  title = "Adjusted Cox regression"
)

plot_cox_adjusted

plot_cox_combined <- plot_reg_combine(
  cox_crude,
  cox_adjusted,
  sig_color = "red",
  title_uni = "Crude HR",
  title_multi = "Adjusted HR"
)

plot_cox_combined

## Compact binary display and custom log-axis breaks should also work.
plot_reg(
  cox_crude,
  log_x = TRUE,
  show_ref = FALSE,
  xlim = c(0.25, 8),
  breaks = c(0.5, 1, 2, 4, 8),
  title = "Crude Cox regression with compact binary rows"
)

plot_reg_combine(
  cox_crude,
  cox_adjusted,
  log_x = TRUE,
  show_ref = FALSE,
  xlim_uni = c(0.25, 8),
  breaks_uni = c(0.5, 1, 2, 4, 8),
  xlim_multi = c(0.25, 8),
  breaks_multi = c(0.5, 1, 2, 4, 8),
  title_uni = "Crude HR",
  title_multi = "Adjusted HR"
)


## 18. Forest table for Cox results -------------------------------------------

## forest_df() creates the data frame used by forest_reg(). You can inspect this
## before drawing the publication-style forest table.

cox_forest_crude <- forest_df(cox_crude)
cox_forest_crude

cox_forest_adjusted <- forest_df(cox_adjusted)
cox_forest_adjusted

cox_forest_data <- forest_df(cox_crude, cox_adjusted, desc = lung_summary)
cox_forest_data

forest_reg(cox_forest_crude)
forest_reg(cox_forest_adjusted)

cox_forest <- forest_reg(cox_forest_data)
cox_forest

## Useful option: put the plot on the left of the effect text.
forest_reg(cox_forest_data, side = "left")

## Layout check:
## If x-axis tick labels overlap, control the axis with xlim and ticks_at.
## Cox regression reports HRs, so the no-effect line is 1.
forest_reg(
  cox_forest_data,
  xlim = list(c(0.5, 6), c(0.5, 6)),
  ticks_at = list(c(0.7, 1, 2, 5), c(0.7, 1, 2, 5))
)

## If the CI plot panel is too narrow or too wide, tune ci_col_width.
forest_reg(cox_forest_data, ci_col_width = c(18, 22))

## You can also build and draw in one call.
forest_reg(
  uni = cox_crude,
  multi = cox_adjusted,
  desc = lung_summary
)
## 19. Model selection for Cox regression -------------------------------------

## select_models() supports survival syntax using time and event instead of
## outcome. This is a screening aid only; do not replace clinical judgement or
## proportional hazards diagnostics with stepwise selection.

cox_selection <- select_models(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  approach = cox,
  direction = forward,
  format = flextable
)

cox_selection
cox_selection$results_table
cox_selection$best_model

## Check other directions and output formats.
select_models(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  approach = cox,
  direction = backward,
  format = gt
)

select_models(
  data = lung_data,
  time = time,
  event = status,
  exposures = survival_exposures,
  approach = cox,
  direction = both,
  format = tibble
)


## 20. Export outputs ----------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This is CRAN-friendly and prevents accidental clutter.

save_table(final_survival_table_paper, filename = "lung-cox-table", format = "docx")
save_plot(km_by_trt, filename = "lung-km-survival-curve", format = "png")
save_plot(plot_cox_combined, filename = "lung-cox-combined-plot", format = "png")

save_docx(
  tables = list(cox_crude_paper, cox_adjusted_paper, final_survival_table_paper),
  plots = list(plot_cox_crude, plot_cox_adjusted, plot_cox_combined),
  titles = list(
    "Crude Cox regression",
    "Adjusted Cox regression",
    "Combined descriptive and Cox regression table",
    "Forest plot - crude Cox",
    "Forest plot - adjusted Cox",
    "Crude versus adjusted Cox plot"
  ),
  filename = "lung-cox-survival-report",
  table_width = 6.5
)


## 21. Final checklist ---------------------------------------------------------

## Things to confirm manually:
## - cox_reg() displays HR (95% CI) by default.
## - km_plot() displays Kaplan-Meier curves with optional risk tables and log-rank p-values.
## - km_risk_table() reports standalone at-risk, event, and censored counts.
## - rmst_table() reports restricted mean survival time and two-group RMST differences.
## - survival_summary() reports Kaplan-Meier median survival with events and censoring.
## - survival_quantiles() reports detailed Kaplan-Meier survival time quantiles.
## - survival_prob() reports fixed-time Kaplan-Meier survival probabilities.
## - logrank_test() compares Kaplan-Meier curves and reports observed/expected events.
## - cox_reg(adjust_for = ...) displays Adjusted HR (95% CI).
## - time and event work with both bare names and objects holding column names.
## - Factor reference categories display as Ref.
## - model_stats = TRUE stores concordance/C-index, events, N, AIC, and BIC.
## - check_ph() reports Schoenfeld residual PH tests for Cox models.
## - Variable labels set once are used in Cox tables.
## - modify_table() can relabel headers and add Cox-specific caveats.
## - merge_tables() can combine descriptive, crude HR, and adjusted HR outputs.
## - plot_reg() and plot_reg_combine() work with Cox HR outputs.
## - plot_reg() respects show_ref = FALSE, xlim, and breaks for Cox HR plots.
## - forest_df() and forest_reg() work with Cox HR outputs.
## - forest_reg(side = "left") works with Cox forest tables.
## - select_models() supports approach = cox with time and event.
## - select_models() supports forward, backward, and both directions for Cox.
## - save_docx(table_width = 6.5) keeps wide Cox tables fitted to a Word page.
