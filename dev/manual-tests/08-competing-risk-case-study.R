## Manual real-time test: competing-risk analysis
## Package: gtregression 1.2
##
## Story:
## An oncology team follows patients for relapse. Death before relapse prevents
## relapse from occurring and is therefore a competing event. We will estimate
## cumulative incidence and fit Fine-Gray regression models for relapse.
##
## How to use:
## Run section by section. Compare absolute cumulative incidence from the plot
## with relative subdistribution hazards from the regression table.


## 0. Setup -------------------------------------------------------------------

## During package development: devtools::load_all(".")
## After installation: library(gtregression)

library(gtregression)

data("data_competing_risk", package = "gtregression")
competing_data <- data_competing_risk


## 1. Inspect event coding ----------------------------------------------------

head(competing_data)
str(competing_data)
table(competing_data$outcome, useNA = "ifany")
table(competing_data$treatment, competing_data$outcome)
summary(competing_data$follow_up_months)
dissect(competing_data)

## Confirm three distinct states:
## - Censored
## - Relapse, the event of interest
## - Death without relapse, the competing event


## 2. Overall cumulative incidence -------------------------------------------

relapse_overall <- cuminc_plot(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  title = "Cumulative incidence of relapse"
)

relapse_overall
attr(relapse_overall, "plot_data")
attr(relapse_overall, "event_of_interest")


## 3. Cumulative incidence by treatment --------------------------------------

relapse_by_treatment <- cuminc_plot(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  by = treatment,
  break_time_by = 12,
  xlab = "Months after treatment",
  title = "Cumulative incidence of relapse by treatment"
)

relapse_by_treatment

## Gray's test compares the cumulative-incidence functions between groups.
attr(relapse_by_treatment, "gray_test")


## 4. Plot customisation ------------------------------------------------------

cuminc_plot(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  by = treatment,
  conf.int = FALSE,
  p_value = TRUE,
  break_time_by = 12,
  xlim = c(0, 60),
  ylim = c(0, 0.6),
  y_percent = TRUE,
  palette = c("#0072B2", "#767676"),
  legend_position = "bottom",
  theme = "minimal",
  grid = TRUE,
  title = "Relapse over follow-up",
  caption = "Death without relapse was treated as a competing event."
)


## 5. Crude Fine-Gray regression ---------------------------------------------

crude_shr <- competing_risk_reg(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  exposures = c(treatment, age, stage),
  model_stats = TRUE
)

crude_shr
crude_shr$table_body
crude_shr$models
crude_shr$model_summaries
crude_shr$model_stats


## 6. Exposure-specific adjusted model ---------------------------------------

adjusted_shr <- competing_risk_reg(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  exposures = treatment,
  adjust_for = c(age, stage),
  model_stats = TRUE
)

adjusted_shr
adjusted_shr$model_stats


## 7. One joint multivariable model ------------------------------------------

joint_shr <- competing_risk_reg(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  exposures = c(treatment, age, stage),
  multivariable = TRUE,
  model_stats = TRUE
)

joint_shr
joint_shr$table_body
joint_shr$model_stats


## 8. Reference rows and gt output -------------------------------------------

competing_risk_reg(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  exposures = c(treatment, stage),
  show_ref = FALSE
)

competing_risk_reg(
  data = competing_data,
  time = follow_up_months,
  event = outcome,
  event_of_interest = Relapse,
  censor_code = Censored,
  exposures = treatment,
  format = gt
)


## 9. Merge, modify, plot, and forest -----------------------------------------

fine_gray_merged <- merge_tables(
  crude_shr,
  joint_shr,
  spanners = c("Crude SHR", "Adjusted SHR")
)

fine_gray_merged

fine_gray_for_paper <- modify_table(
  adjusted_shr,
  caption = "Fine-Gray regression for relapse",
  caveat = "Death without relapse was treated as a competing event."
)

fine_gray_for_paper

plot_reg(adjusted_shr)

fine_gray_forest_data <- forest_df(crude_shr, joint_shr)
fine_gray_forest_data
forest_reg(fine_gray_forest_data)


## 10. What to inspect --------------------------------------------------------

## - Cumulative incidence is absolute event probability over time.
## - Gray's test is an unadjusted comparison of cumulative-incidence curves.
## - Fine-Gray regression reports SHR or adjusted SHR, not risk ratios.
## - Event-of-interest and censoring labels are visible in output metadata.
## - Cause-specific Cox, Fine-Gray, cumulative incidence, and Kaplan-Meier
##   estimates answer different questions and are not interchangeable.
