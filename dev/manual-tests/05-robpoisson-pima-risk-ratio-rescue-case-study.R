## Manual real-time test: robust Poisson risk-ratio case study
## Package: gtregression 1.1
##
## Story:
## A diabetes prevention team wants risk ratios for a binary outcome. The first
## statistical choice is log-binomial regression because it estimates risk ratios
## directly. But the larger log-binomial model may not converge. Robust Poisson
## regression is the practical rescue: it keeps the risk-ratio interpretation
## while avoiding many log-binomial convergence problems.
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

data("data_PimaIndiansDiabetes", package = "gtregression")


## 1. Prepare the diabetes risk dataset --------------------------------------

## We use a binary diabetes outcome and clinically readable categories.

pima_data <- data_PimaIndiansDiabetes |>
  mutate(
    diabetes = ifelse(diabetes == "pos", 1, 0),
    diabetes_cat = case_when(
      diabetes == 1 ~ "Diabetes positive",
      TRUE ~ "Diabetes negative"
    ),
    diabetes_cat = factor(
      diabetes_cat,
      levels = c("Diabetes negative", "Diabetes positive")
    ),
    bmi = case_when(
      mass < 25 ~ "Normal",
      mass >= 25 & mass < 30 ~ "Overweight",
      mass >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
    age_cat = case_when(
      age < 30 ~ "Young",
      age >= 30 & age < 50 ~ "Middle-aged",
      age >= 50 ~ "Older",
      TRUE ~ NA_character_
    ),
    age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
    npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
    npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
    glucose_cat = case_when(
      glucose < 140 ~ "Normal",
      glucose >= 140 ~ "High",
      TRUE ~ NA_character_
    ),
    glucose_cat = factor(glucose_cat, levels = c("Normal", "High")),
    bp_cat = case_when(
      pressure < 80 ~ "Normal",
      pressure >= 80 ~ "High",
      TRUE ~ NA_character_
    ),
    bp_cat = factor(bp_cat, levels = c("Normal", "High")),
    triceps_cat = case_when(
      triceps < 23 ~ "Normal",
      triceps >= 23 ~ "High",
      TRUE ~ NA_character_
    ),
    triceps_cat = factor(triceps_cat, levels = c("Normal", "High")),
    insulin_cat = case_when(
      insulin < 30 ~ "Low",
      insulin >= 30 & insulin < 150 ~ "Normal",
      insulin >= 150 ~ "High",
      TRUE ~ NA_character_
    ),
    insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High")),
    dpf_cat = case_when(
      pedigree <= 0.2 ~ "Low genetic risk",
      pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate genetic risk",
      pedigree > 0.5 ~ "High genetic risk",
      TRUE ~ NA_character_
    ),
    dpf_cat = factor(
      dpf_cat,
      levels = c("Low genetic risk", "Moderate genetic risk", "High genetic risk")
    )
  )

outcome <- "diabetes"
risk_factors <- c(
  "bmi", "age_cat", "npreg_cat", "glucose_cat", "bp_cat",
  "triceps_cat", "insulin_cat", "dpf_cat"
)
best_risk_factors <- c("age_cat", "insulin_cat", "bmi", "dpf_cat")


## 2. Inspect data before risk-ratio modelling --------------------------------

## Default output is a publication-style flextable.
dissect(pima_data)

## Use tibble output for a console-friendly data audit.
pima_dissect <- dissect(pima_data, format = "tibble")
pima_dissect

## Optional external overview if skimr is installed.
if (requireNamespace("skimr", quietly = TRUE)) {
  skimr::skim(pima_data)
}


## 3. Descriptive table: who is diabetes positive? ----------------------------

## Column percentages answer:
## "Within diabetes-positive and diabetes-negative groups, what proportion had
## each clinical risk factor?"

pima_summary <- descriptive_table(
  data = pima_data,
  exposures = risk_factors,
  by = "diabetes_cat",
  percent = "column",
  show_missing = "ifany",
  show_overall = "first"
)

pima_summary

## Row percentages answer:
## "Within each risk-factor level, what proportion were diabetes positive?"

pima_summary_row <- descriptive_table(
  data = pima_data,
  exposures = risk_factors,
  by = "diabetes_cat",
  percent = "row",
  show_missing = "no",
  show_overall = "last"
)

pima_summary_row


## 4. Start with log-binomial -------------------------------------------------

## Log-binomial is attractive because it estimates RR directly.
## The problem is convergence: larger or sparse models can fail.

check_convergence(
  data = pima_data,
  exposures = risk_factors,
  outcome = outcome,
  approach = "logbinomial"
)

check_convergence(
  data = pima_data,
  exposures = risk_factors,
  outcome = outcome,
  approach = "logbinomial",
  multivariate = TRUE,
  format = "gt"
)

## Try the full log-binomial model without stopping the script if it fails.
## If this prints an error, that is the teaching moment: move to robust Poisson.

logbin_full_attempt <- try(
  multi_reg(
    data = pima_data,
    outcome = outcome,
    exposures = risk_factors,
    approach = "logbinomial"
  ),
  silent = TRUE
)

logbin_full_attempt


## 5. Robust Poisson as the practical rescue ---------------------------------

## Robust Poisson estimates risk ratios for a binary outcome using a Poisson
## model with robust standard errors. It is commonly used when log-binomial
## regression does not converge.

check_convergence(
  data = pima_data,
  exposures = risk_factors,
  outcome = outcome,
  approach = "robpoisson"
)

check_convergence(
  data = pima_data,
  exposures = risk_factors,
  outcome = outcome,
  approach = "robpoisson",
  multivariate = TRUE
)


## 6. Univariable robust Poisson regression ----------------------------------

## Question:
## Which individual clinical factors are associated with diabetes risk?
##
## RR interpretation:
## RR > 1 suggests higher diabetes risk.
## RR < 1 suggests lower diabetes risk.
## RR = 1 is the no-association line.

uni_rr <- uni_reg(
  data = pima_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "robpoisson"
)

uni_rr
uni_rr$table_body
uni_rr$models
uni_rr$model_summaries
uni_rr$table

## Useful option: gt output for HTML/pkgdown-style viewing.
uni_reg(
  data = pima_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "robpoisson",
  format = "gt"
)


## 7. Multivariable robust Poisson regression --------------------------------

## Full model:
## This is the model that may be difficult with log-binomial regression.

multi_rr_full <- multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "robpoisson"
)

multi_rr_full
multi_rr_full$table_body
multi_rr_full$models
multi_rr_full$model_summaries

## Selected model:
## This is a compact model for a clearer case-study table.

multi_rr_best <- multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = best_risk_factors,
  approach = "robpoisson"
)

multi_rr_best

## Adjusted mode:
## Estimate selected exposures separately while adjusting for a shared core.

multi_adj_rr <- multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("bmi", "insulin_cat", "dpf_cat"),
  adjust_for = c("age_cat", "npreg_cat"),
  approach = "robpoisson"
)

multi_adj_rr

## Friendly interactive syntax also works at the console.
multi_reg(
  data = pima_data,
  outcome = diabetes,
  exposures = c(bmi, insulin_cat, dpf_cat),
  adjust_for = c(age_cat, npreg_cat),
  approach = robpoisson
)


## 8. Modify tables for publication ------------------------------------------

## Use readable labels before sending tables to a report.

uni_rr_paper <- modify_table(
  uni_rr,
  variable_labels = c(
    bmi = "BMI category",
    age_cat = "Age group",
    npreg_cat = "Parity",
    glucose_cat = "Glucose category",
    bp_cat = "Blood pressure category",
    triceps_cat = "Triceps skinfold category",
    insulin_cat = "Serum insulin category",
    dpf_cat = "Diabetes pedigree risk"
  ),
  level_labels = list(
    bmi = c(Obese = "Obese: BMI 30 or higher"),
    glucose_cat = c(High = "High glucose"),
    npreg_cat = c("High parity" = "More than 2 pregnancies")
  ),
  header_labels = c(
    estimate = "Crude RR",
    p.value = "P value"
  ),
  caption = "Table 1. Univariable robust Poisson regression for diabetes risk",
  caveat = "Robust Poisson is used to estimate risk ratios when log-binomial convergence is problematic."
)

uni_rr_paper

multi_rr_paper <- modify_table(
  multi_rr_best,
  variable_labels = c(
    age_cat = "Age group",
    insulin_cat = "Serum insulin category",
    bmi = "BMI category",
    dpf_cat = "Diabetes pedigree risk"
  ),
  header_labels = c(
    estimate = "Adjusted RR",
    p.value = "P value"
  ),
  caption = "Table 2. Multivariable robust Poisson regression for diabetes risk",
  caveat = "Adjusted model selected for demonstration after convergence screening."
)

multi_rr_paper

multi_adj_rr_paper <- modify_table(
  multi_adj_rr,
  variable_labels = c(
    bmi = "BMI category",
    insulin_cat = "Serum insulin category",
    dpf_cat = "Diabetes pedigree risk"
  ),
  header_labels = c(
    estimate = "Adjusted RR",
    p.value = "P value"
  ),
  caption = "Table 3. Exposure-specific adjusted robust Poisson models",
  caveat = "Each exposure is adjusted for age group and parity."
)

multi_adj_rr_paper

## Useful option: compact table for an appendix or quick review.
modify_table(
  multi_rr_best,
  remove_N_obs = TRUE,
  remove_abbreviations = TRUE,
  caption = "Compact robust Poisson model"
)


## 9. Visualise robust Poisson risk ratios -----------------------------------

## What to look for:
## - RR = 1 is the no-association line.
## - Points to the right of 1 indicate higher diabetes risk.
## - Points to the left of 1 indicate lower diabetes risk.
## - Confidence intervals crossing 1 suggest weaker statistical evidence.
## - Significant associations can be shown in different colours using
##   sig_color and sig_errorbar_color.

plot_uni_rr <- plot_reg(uni_rr)
plot_uni_rr

plot_multi_rr <- plot_reg(multi_rr_best)
plot_multi_rr

plot_adj_rr <- plot_reg(multi_adj_rr)
plot_adj_rr

plot_reg(
  uni_rr,
  title = "Crude robust-Poisson risk ratios",
  sig_color = "darkred",
  sig_errorbar_color = "black",
  base_size = 13
)

## Log-scale x-axis can make risk-ratio plots easier to read.
plot_reg(
  uni_rr,
  log_x = TRUE,
  sig_color = "darkred",
  sig_errorbar_color = "black"
)

## Compare crude and adjusted risk ratios visually.

plot_comb_rr <- plot_reg_combine(uni_rr, multi_rr_best)
plot_comb_rr

plot_reg_combine(
  uni_rr,
  multi_adj_rr,
  sig_color = "cyan4",
  sig_errorbar_color = "red",
  base_size = 13
)


## 10. Merge descriptive and robust Poisson tables ----------------------------

final_rr_table <- merge_tables(
  pima_summary,
  uni_rr_paper,
  multi_rr_paper,
  spanners = c("Clinical profile", "Crude RR", "Adjusted RR")
)

final_rr_table

final_rr_table_paper <- modify_table(
  final_rr_table,
  variable_labels = c(
    bmi = "BMI category",
    age_cat = "Age group",
    npreg_cat = "Parity",
    glucose_cat = "Glucose category",
    bp_cat = "Blood pressure category",
    triceps_cat = "Triceps skinfold category",
    insulin_cat = "Serum insulin category",
    dpf_cat = "Diabetes pedigree risk"
  ),
  caveat = "Robust Poisson preserves risk-ratio interpretation after log-binomial convergence concerns."
)

final_rr_table_paper


## 11. Forest plot dataset and forest plot -----------------------------------

## forest_df() prepares the data. forest_reg() draws the plot.
## For robust Poisson, the forest plot displays RR estimates.

df_uni_rr <- forest_df(uni_rr)
df_uni_rr

df_multi_rr <- forest_df(multi_rr_best)
df_multi_rr

df_both_rr <- forest_df(uni_rr, multi_rr_best)
df_both_rr

df_desc_rr <- forest_df(pima_summary)
df_desc_rr

df_uni_desc_rr <- forest_df(uni_rr, desc = pima_summary)
df_uni_desc_rr

df_multi_desc_rr <- forest_df(multi_rr_best, desc = pima_summary)
df_multi_desc_rr

df_both_desc_rr <- forest_df(uni_rr, multi_rr_best, desc = pima_summary)
df_both_desc_rr

forest_reg(df_uni_rr)
forest_reg(df_multi_rr)
forest_reg(df_both_rr)
forest_reg(df_uni_desc_rr)
forest_reg(df_multi_desc_rr)
forest_reg(df_both_desc_rr)

## Useful option: put the plot on the left.
forest_reg(df_both_desc_rr, side = "left")

## Build and draw in one call.
forest_reg(
  uni = uni_rr,
  multi = multi_rr_best,
  desc = pima_summary
)


## 12. Diagnostics and model selection ---------------------------------------

## Collinearity:
## Useful after fitting a multivariable model.

check_collinearity(multi_rr_best)
check_collinearity(multi_rr_best, format = "gt")

## Stepwise model selection:
## The publication-ready output states whether forward, backward, or both
## selection was used.

stepwise_forward <- select_models(
  data = pima_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "robpoisson",
  direction = "forward"
)

stepwise_forward
stepwise_forward$results_table
stepwise_forward$best_model

select_models(
  data = pima_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "robpoisson",
  direction = "backward",
  format = "gt"
)

select_models(
  data = pima_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "robpoisson",
  direction = "both",
  format = "tibble"
)


## 13. Confounding and interaction screening ---------------------------------

## identify_confounder() is a screening aid, not a substitute for a DAG.
## For binary outcomes, method = "mh" or method = "both" can add a
## Mantel-Haenszel comparison when variables are eligible.

conf_bmi_age <- identify_confounder(
  data = pima_data,
  outcome = outcome,
  exposure = "bmi",
  potential_confounder = "age_cat",
  approach = "robpoisson",
  method = "both"
)

conf_bmi_age
conf_bmi_age$table

identify_confounder(
  data = pima_data,
  outcome = outcome,
  exposure = c("bmi", "npreg_cat"),
  potential_confounder = c("age_cat", "insulin_cat", "dpf_cat"),
  approach = "robpoisson",
  method = "both",
  format = "gt"
)

## interaction_models() is for a focused planned interaction term.
## Here we ask whether the BMI-diabetes association differs by glucose category,
## adjusting for insulin, age group, and diabetes pedigree risk.

interaction_models(
  data = pima_data,
  outcome = outcome,
  exposure = "bmi",
  effect_modifier = "glucose_cat",
  covariates = c("insulin_cat", "age_cat", "dpf_cat"),
  approach = "robpoisson"
)

interaction_models(
  data = pima_data,
  outcome = outcome,
  exposure = "bmi",
  effect_modifier = "age_cat",
  covariates = c("insulin_cat", "glucose_cat", "dpf_cat"),
  approach = "robpoisson",
  test = "Wald",
  format = "gt"
)


## 14. Stratified robust Poisson regression ----------------------------------

## Question:
## Do risk ratios look different across age groups or glucose categories?
## This is exploratory. Use planned interaction models when a formal
## effect-modification test is needed.

str_uni_rr <- stratified_uni_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("bmi", "npreg_cat", "bp_cat", "triceps_cat", "insulin_cat", "dpf_cat"),
  stratifier = "age_cat",
  approach = "robpoisson"
)

str_uni_rr
str_uni_rr$models
str_uni_rr$model_summaries

str_multi_rr <- stratified_multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("bmi", "insulin_cat", "age_cat", "dpf_cat"),
  stratifier = "glucose_cat",
  approach = "robpoisson"
)

str_multi_rr
str_multi_rr$models
str_multi_rr$model_summaries

stratified_multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("bmi", "dpf_cat"),
  stratifier = "age_cat",
  adjust_for = c("insulin_cat", "npreg_cat"),
  approach = "robpoisson"
)


## 15. Export outputs ---------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This is CRAN-friendly and prevents accidental clutter.

save_table(final_rr_table_paper, filename = "pima-robpoisson-table", format = "docx")
save_plot(plot_comb_rr, filename = "pima-robpoisson-plot", format = "png")

save_docx(
  tables = list(uni_rr_paper, multi_rr_paper, final_rr_table_paper),
  plots = list(plot_uni_rr, plot_multi_rr, plot_comb_rr),
  titles = list(
    "Univariable robust Poisson regression",
    "Multivariable robust Poisson regression",
    "Combined descriptive and robust-Poisson table",
    "Forest plot - univariable",
    "Forest plot - multivariable",
    "Crude versus adjusted risk-ratio plot"
  ),
  filename = "pima-robpoisson-report"
)


## 16. Final checklist --------------------------------------------------------

## Things to confirm manually:
## - Tables display by default as flextable.
## - Robust Poisson tables use RR (95% CI), not OR.
## - check_convergence() shows why log-binomial may be difficult.
## - Robust Poisson model outputs remain available through $models and
##   $model_summaries.
## - modify_table() labels appear in tables and downstream plots.
## - plot_reg() highlights significant associations when colours are supplied.
## - plot_reg_combine() makes crude versus adjusted changes easy to see.
## - forest_reg() displays risk ratios with a no-effect reference line at 1.
## - select_models() output mentions the selection direction.
## - identify_confounder() prints a console summary and has a formatted $table.
## - interaction_models() returns a readable screening table and model objects.
## - stratified outputs are useful for exploratory effect-pattern checks.
## - save_table(), save_plot(), and save_docx() write files to tempdir when the
##   filename has no directory.

## End of manual robust Poisson case study -----------------------------------
