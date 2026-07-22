## Manual real-time test: linear regression case study
## Package: gtregression 1.1
##
## Story:
## A diabetes clinic wants to understand which clinical measurements are
## associated with plasma glucose. The outcome is continuous, so this script
## uses linear regression and follows the analysis from raw data inspection to
## publication-ready tables, plots, diagnostics, and export.
##
## How to use:
## Run this script section by section. The comments tell the user what to look
## for and when a result is mainly a screening aid.

## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)

library(gtregression)
library(dplyr)

data("data_PimaIndiansDiabetes", package = "gtregression")


## 1. Prepare the diabetes clinic dataset ------------------------------------

## The raw data include continuous clinical measures and a diabetes class.
## We create readable categories for descriptive summaries, stratified analyses,
## and interaction/confounding screening.

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

outcome <- "glucose"
clinical_predictors <- c(
  "age", "pregnant", "pressure", "triceps", "mass", "insulin", "pedigree"
)
categorical_predictors <- c(
  "bmi", "age_cat", "npreg_cat", "bp_cat", "insulin_cat", "dpf_cat"
)


## 2. Inspect data before modelling ------------------------------------------

## Default output is a publication-style flextable.
dissect(pima_data)

## Use tibble output when you want to inspect compatibility programmatically.
pima_dissect <- dissect(pima_data, format = "tibble")
pima_dissect

## Optional external data overview when skimr is available.
if (requireNamespace("skimr", quietly = TRUE)) {
  skimr::skim(pima_data)
}


## 3. Descriptive table: glucose risk profile --------------------------------

## The grouping variable is diabetes status. The regression outcome below is
## glucose, but this descriptive table helps readers understand the clinical
## profile of the cohort.

pima_summary <- descriptive_table(
  data = pima_data,
  exposures = c(
    "age", "pregnant", "glucose", "pressure", "triceps",
    "insulin", "mass", "pedigree", "bmi", "age_cat"
  ),
  by = "diabetes_cat",
  percent = "column",
  show_overall = "last"
)

pima_summary

## Useful option: row percentages for clinical profile questions.
descriptive_table(
  data = pima_data,
  exposures = c("bmi", "age_cat", "npreg_cat", "insulin_cat"),
  by = "diabetes_cat",
  percent = "row",
  show_missing = "no",
  show_overall = "first"
)


## 4. Minimal linear regression ----------------------------------------------

## Question:
## Is BMI category associated with mean glucose?

uni_bmi_lm <- uni_reg(
  data = pima_data,
  outcome = "glucose",
  exposures = "bmi",
  approach = "linear"
)

uni_bmi_lm
uni_bmi_lm$table_body
uni_bmi_lm$models
uni_bmi_lm$model_summaries

## Linear regression has diagnostic output.
## Look at heteroscedasticity, normality, and influential-observation checks.
uni_bmi_lm$reg_check


## 5. Univariable linear regression ------------------------------------------

## Question:
## Which individual clinical measurements are associated with glucose?

uni_lm <- uni_reg(
  data = pima_data,
  outcome = outcome,
  exposures = clinical_predictors,
  approach = "linear"
)

uni_lm
uni_lm$table_body
uni_lm$model_summaries
uni_lm$reg_check

## Useful option: add categorical predictors in a separate table.
uni_cat_lm <- uni_reg(
  data = pima_data,
  outcome = outcome,
  exposures = categorical_predictors,
  approach = "linear",
  format = "gt"
)

uni_cat_lm


## 6. Multivariable linear regression ----------------------------------------

## Question:
## Which associations remain after all clinical predictors enter one model?

multi_lm <- multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = clinical_predictors,
  approach = "linear"
)

multi_lm
multi_lm$table_body
multi_lm$model_summaries
multi_lm$reg_check

## Adjusted mode:
## Estimate selected predictors separately while adjusting for a common clinical
## core. This is useful for focused manuscript tables.

multi_adj_lm <- multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("mass", "insulin", "pedigree"),
  adjust_for = c("age", "pregnant", "pressure"),
  approach = "linear"
)

multi_adj_lm

## Friendly interactive syntax also works at the console.
multi_reg(
  data = pima_data,
  outcome = glucose,
  exposures = c(mass, insulin, pedigree),
  adjust_for = c(age, pregnant, pressure),
  approach = linear
)


## 7. Modify tables for publication ------------------------------------------

## Use modify_table() to turn technically correct output into a reader-friendly
## manuscript table.

uni_lm_paper <- modify_table(
  uni_lm,
  variable_labels = c(
    age = "Age",
    pregnant = "Number of pregnancies",
    pressure = "Diastolic blood pressure",
    triceps = "Triceps skinfold thickness",
    mass = "Body mass index",
    insulin = "Serum insulin",
    pedigree = "Diabetes pedigree function"
  ),
  header_labels = c(
    estimate = "Crude beta",
    p.value = "P value"
  ),
  caption = "Table 1. Univariable linear regression for plasma glucose",
  caveat = "Positive beta values indicate higher mean glucose."
)

uni_lm_paper

multi_adj_lm_paper <- modify_table(
  multi_adj_lm,
  variable_labels = c(
    mass = "Body mass index",
    insulin = "Serum insulin",
    pedigree = "Diabetes pedigree function"
  ),
  header_labels = c(
    estimate = "Adjusted beta",
    p.value = "P value"
  ),
  caption = "Table 2. Adjusted linear regression for plasma glucose",
  caveat = "Adjusted for age, number of pregnancies, and blood pressure."
)

multi_adj_lm_paper

## Useful option: remove technical footnotes for a compact appendix table.
modify_table(
  multi_adj_lm,
  remove_N_obs = TRUE,
  remove_abbreviations = TRUE,
  caption = "Compact adjusted linear model"
)


## 8. Regression plots ---------------------------------------------------------

## What to look for:
## - Positive beta values sit to the right of zero.
## - Negative beta values sit to the left of zero.
## - Confidence intervals crossing zero suggest weaker statistical evidence.
## - Significant associations can be shown in different colours using
##   sig_color and sig_errorbar_color.

plot_uni_lm <- plot_reg(uni_lm)
plot_uni_lm

plot_multi_lm <- plot_reg(multi_lm)
plot_multi_lm

plot_adj_lm <- plot_reg(multi_adj_lm)
plot_adj_lm

plot_reg(
  uni_lm,
  sig_color = "cyan4",
  sig_errorbar_color = "red",
  base_size = 13
)

## Compare crude and adjusted beta coefficients.
## This helps users see whether adjustment changes the direction or size of
## each association.

plot_comb_lm <- plot_reg_combine(
  tbl_uni = uni_lm,
  tbl_multi = multi_lm,
  title_uni = "Crude beta coefficients",
  title_multi = "Adjusted beta coefficients",
  base_size = 13
)

plot_comb_lm

plot_reg_combine(
  tbl_uni = uni_lm,
  tbl_multi = multi_adj_lm,
  title_uni = "Crude beta coefficients",
  title_multi = "Adjusted beta coefficients",
  sig_color = "cyan4",
  sig_errorbar_color = "red",
  base_size = 13
)


## 9. Merge descriptive and regression tables --------------------------------

final_lm_table <- merge_tables(
  pima_summary,
  uni_lm_paper,
  multi_adj_lm_paper,
  spanners = c("Clinical profile", "Crude", "Adjusted")
)

final_lm_table


## 10. Forest plot dataset and forest plot -----------------------------------

## forest_df() prepares a dataset that forest_reg() can draw.
## For linear regression, the forest plot displays beta coefficients.

df_uni_lm <- forest_df(uni_lm)
df_uni_lm

df_multi_lm <- forest_df(multi_lm)
df_multi_lm

df_both_lm <- forest_df(uni_lm, multi_lm)
df_both_lm

df_lm_desc <- forest_df(uni_lm, multi_adj_lm, desc = pima_summary)
df_lm_desc

forest_reg(df_uni_lm)
forest_reg(df_multi_lm)
forest_reg(df_both_lm)
forest_reg(df_lm_desc)

## Useful option: put labels on the right and plot on the left.
forest_reg(df_lm_desc, side = "left")


## 11. Diagnostics and model selection ---------------------------------------

## Collinearity:
## VIF is useful in multivariable linear regression because correlated
## predictors can make beta coefficients unstable.

check_collinearity(multi_lm)
check_collinearity(multi_lm, format = "gt")

## Stepwise model selection:
## The publication-ready output states whether forward, backward, or both
## selection was used.

stepwise_forward <- select_models(
  data = pima_data,
  outcome = outcome,
  exposures = clinical_predictors,
  approach = "linear",
  direction = "forward"
)

stepwise_forward
stepwise_forward$results_table
stepwise_forward$best_model
summary(stepwise_forward$best_model)

select_models(
  data = pima_data,
  outcome = outcome,
  exposures = clinical_predictors,
  approach = "linear",
  direction = "backward",
  format = "gt"
)

select_models(
  data = pima_data,
  outcome = outcome,
  exposures = clinical_predictors,
  approach = "linear",
  direction = "both",
  format = "tibble"
)


## 12. Confounding and interaction screening ---------------------------------

## identify_confounder() is a screening aid, not a substitute for a DAG.
## A plain call prints a tidy console summary. The formatted table is in $table.

conf_age <- identify_confounder(
  data = pima_data,
  outcome = outcome,
  exposure = "insulin",
  potential_confounder = "age_cat",
  approach = "linear"
)

conf_age
conf_age$table

identify_confounder(
  data = pima_data,
  outcome = outcome,
  exposure = c("insulin", "pedigree"),
  potential_confounder = c("age_cat", "bmi"),
  approach = "linear",
  method = "change",
  format = "gt"
)

## interaction_models() tests a planned interaction term.
## In this example we ask whether the insulin-glucose association differs by
## age group after accounting for pregnancy history.

interaction_models(
  data = pima_data,
  outcome = outcome,
  exposure = "insulin",
  effect_modifier = "age_cat",
  covariates = "pregnant",
  approach = "linear"
)

interaction_models(
  data = pima_data,
  outcome = outcome,
  exposure = "insulin",
  effect_modifier = "age_cat",
  covariates = c("pregnant", "mass"),
  approach = "linear",
  test = "Wald",
  format = "gt"
)


## 13. Stratified linear regression ------------------------------------------

## Question:
## Do beta coefficients look different across age groups?
## This is descriptive and exploratory. Use planned interaction models when a
## formal effect-modification test is needed.

stratified_uni_lm <- stratified_uni_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("mass", "triceps", "pregnant", "pressure", "insulin"),
  stratifier = "age_cat",
  approach = "linear"
)

stratified_uni_lm
stratified_uni_lm$model_summaries

stratified_multi_lm <- stratified_multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("mass", "triceps", "pregnant", "pressure", "insulin"),
  stratifier = "age_cat",
  approach = "linear"
)

stratified_multi_lm

stratified_multi_reg(
  data = pima_data,
  outcome = outcome,
  exposures = c("mass", "insulin", "pedigree"),
  stratifier = "age_cat",
  adjust_for = c("pregnant", "pressure"),
  approach = "linear"
)


## 14. Export outputs ---------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This is CRAN-friendly and prevents accidental clutter.

save_table(final_lm_table, filename = "pima-linear-table", format = "docx")
save_plot(plot_comb_lm, filename = "pima-linear-plot", format = "png")

save_docx(
  tables = list(uni_lm_paper, multi_adj_lm_paper, final_lm_table),
  plots = list(plot_uni_lm, plot_adj_lm, plot_comb_lm),
  titles = list(
    "Univariable linear regression",
    "Adjusted linear regression",
    "Combined descriptive and regression table",
    "Forest plot - univariable",
    "Forest plot - adjusted",
    "Crude versus adjusted beta plot"
  ),
  filename = "pima-linear-report"
)


## 15. Final checklist --------------------------------------------------------

## Things to confirm manually:
## - Tables display by default as flextable.
## - Linear regression tables use Beta (95% CI), not OR/RR.
## - reg_check output appears for linear models.
## - plot_reg() highlights significant associations when colours are supplied.
## - plot_reg_combine() makes crude versus adjusted changes easy to see.
## - select_models() output mentions the selection direction.
## - identify_confounder() prints a console summary and has a formatted $table.
## - interaction_models() returns a readable screening table and model objects.
## - save_table(), save_plot(), and save_docx() write files to tempdir when the
##   filename has no directory.

## End of manual linear case study -------------------------------------------
