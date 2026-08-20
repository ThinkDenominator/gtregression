## Manual real-time test: log-binomial regression case study
## Package: gtregression 1.1
##
## Story:
## A diabetes prevention team wants risk ratios, not odds ratios, because risk
## ratios are easier for clinicians and patients to interpret. The outcome is
## diabetes status, and the question is whether BMI, parity, age group, blood
## pressure, insulin, and family-risk markers are associated with diabetes risk.
##
## Log-binomial models estimate risk ratios directly. They are useful, but they
## may fail to converge in complex models. This script teaches users to inspect
## convergence, start with focused models, and build clean outputs.
##
## How to use:
## Run this script section by section. The comments tell the user what to look
## for, especially when a result is mainly exploratory or a screening aid.

## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)

library(gtregression)
library(dplyr)

data("data_SynthDiabetes", package = "gtregression")


## 1. Prepare the diabetes risk dataset --------------------------------------

## The outcome is converted to numeric 0/1 because log-binomial models use a
## binary outcome. We also create clinical categories for readable tables and
## risk-ratio comparisons.

synth_data <- data_SynthDiabetes |>
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
  "bmi", "age_cat", "npreg_cat", "bp_cat",
  "triceps_cat", "insulin_cat", "dpf_cat"
)

core_risk_factors <- c("age_cat", "npreg_cat", "dpf_cat")

## Clearer labels for the reader:
## Label once, then all gtregression tables and plots use these display names.
attr(synth_data$glucose, "label") <- "Plasma glucose"
attr(synth_data$mass, "label") <- "Body mass index"
attr(synth_data$bmi, "label") <- "BMI category"
attr(synth_data$age_cat, "label") <- "Age group"
attr(synth_data$npreg_cat, "label") <- "Parity"
attr(synth_data$glucose_cat, "label") <- "Glucose category"
attr(synth_data$bp_cat, "label") <- "Blood pressure category"
attr(synth_data$triceps_cat, "label") <- "Triceps skinfold category"
attr(synth_data$insulin_cat, "label") <- "Serum insulin category"
attr(synth_data$dpf_cat, "label") <- "Diabetes pedigree risk"


## 2. Inspect data before modelling ------------------------------------------

## Default output is a publication-style flextable.
dissect(synth_data)

## Use tibble output for a quick scan of missingness, variable type, and levels.
synth_dissect <- dissect(synth_data, format = "tibble")
synth_dissect

## Optional external overview if skimr is installed.
if (requireNamespace("skimr", quietly = TRUE)) {
  skimr::skim(synth_data)
}


## 3. Descriptive table: diabetes risk profile -------------------------------

## Before modelling, show the clinical profile by diabetes status.
## Column percentages answer:
## "Within each diabetes group, what proportion had each risk factor?"

synth_summary <- descriptive_table(
  data = synth_data,
  exposures = risk_factors,
  by = "diabetes_cat",
  percent = "column",
  show_missing = "ifany",
  show_overall = "first"
)

synth_summary

## Row percentages answer:
## "Within each risk-factor level, what proportion were diabetes positive?"

synth_summary_row <- descriptive_table(
  data = synth_data,
  exposures = risk_factors,
  by = "diabetes_cat",
  percent = "row",
  show_missing = "no",
  show_overall = "last"
)

synth_summary_row

## Explicit HTML-style output.
descriptive_table(
  data = synth_data,
  exposures = risk_factors,
  by = "diabetes_cat",
  percent = "column",
  show_overall = "last",
  format = "gt"
)

## Mixed summaries:
## A numeric variable can be treated as categorical when clinical reporting
## needs counts and percentages.
descriptive_table(
  data = synth_data,
  exposures = c("age_cat", "npreg_cat", "glucose", "mass"),
  by = diabetes_cat,
  statistic = c(glucose = mean, mass = median),
  percent = column,
  show_missing = no
)


## 4. Minimal log-binomial regression ----------------------------------------

## Question:
## Is BMI category associated with diabetes risk?
##
## Risk ratio interpretation:
## RR > 1 suggests higher risk compared with the reference group.
## RR < 1 suggests lower risk compared with the reference group.

uni_bmi_rr <- uni_reg(
  data = synth_data,
  outcome = "diabetes",
  exposures = "bmi",
  approach = "logbinomial"
)

uni_bmi_rr
uni_bmi_rr$table_body
uni_bmi_rr$models
uni_bmi_rr$model_summaries

## The old spelling remains accepted for backward compatibility.
uni_reg(
  data = synth_data,
  outcome = "diabetes",
  exposures = "bmi",
  approach = "log-binomial"
)


## 5. Check convergence before building larger models -------------------------

## Log-binomial regression can be sensitive to model complexity.
## Start by checking each univariable model, then check the intended
## multivariable model.

check_convergence(
  data = synth_data,
  exposures = risk_factors,
  outcome = "diabetes",
  approach = "logbinomial"
)

check_convergence(
  data = synth_data,
  exposures = risk_factors,
  outcome = "diabetes",
  approach = "logbinomial",
  multivariate = TRUE
)

## If the full model struggles, simplify the model or consider robust Poisson
## in the separate robust-Poisson case study.
## For this log-binomial walkthrough, we use a smaller model that is easier to
## interpret and more likely to converge.

check_convergence(
  data = synth_data,
  exposures = core_risk_factors,
  outcome = "diabetes",
  approach = "logbinomial",
  multivariate = TRUE
)


## 6. Univariable risk-ratio table -------------------------------------------

## Question:
## Which single risk factors are associated with diabetes?

uni_rr <- uni_reg(
  data = synth_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "logbinomial"
)

uni_rr
uni_rr$table_body
uni_rr$model_summaries
uni_rr$table

## Optional model statistics are stored outside the publication table.
uni_rr_stats <- uni_reg(
  data = synth_data,
  outcome = diabetes,
  exposures = risk_factors,
  approach = logbinomial,
  model_stats = TRUE
)

uni_rr_stats$model_stats

## Useful option: gt output for HTML/pkgdown viewing.
uni_reg(
  data = synth_data,
  outcome = outcome,
  exposures = risk_factors,
  approach = "logbinomial",
  format = "gt"
)


## 7. Multivariable risk-ratio table -----------------------------------------

## Question:
## Which selected risk factors remain associated when modelled together?

multi_rr <- multi_reg(
  data = synth_data,
  outcome = outcome,
  exposures = core_risk_factors,
  approach = logbinomial
)

multi_rr
multi_rr$table_body
multi_rr$models
multi_rr$model_summaries

## Adjusted mode:
## Estimate each exposure separately while adjusting for a common core.

multi_adj_rr <- multi_reg(
  data = synth_data,
  outcome = outcome,
  exposures = c("npreg_cat", "dpf_cat"),
  adjust_for = c("age_cat", "bmi"),
  approach = "logbinomial"
)

multi_adj_rr

## Friendly interactive syntax also works at the console.
multi_reg(
  data = synth_data,
  outcome = diabetes,
  exposures = c(npreg_cat, dpf_cat),
  adjust_for = c(age_cat, bmi),
  approach = logbinomial
)

multi_adj_rr_stats <- multi_reg(
  data = synth_data,
  outcome = diabetes,
  exposures = c(npreg_cat, dpf_cat),
  adjust_for = c(age_cat, bmi),
  approach = logbinomial,
  model_stats = TRUE
)

multi_adj_rr_stats$model_stats


## 8. Modify tables for publication ------------------------------------------

## This is where variable names become easier for readers to follow.
## The labels below also flow into downstream plots when you plot the modified
## table object.

uni_rr_paper <- modify_table(
  uni_rr,
  variable_labels = c(
    bmi = "BMI category",
    age_cat = "Age group",
    npreg_cat = "Parity",
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
  caption = "Table 1. Univariable log-binomial regression for diabetes risk",
  caveat = "Risk ratios compare each level with the reference group."
)

uni_rr_paper

multi_adj_rr_paper <- modify_table(
  multi_adj_rr,
  variable_labels = c(
    npreg_cat = "Parity",
    dpf_cat = "Diabetes pedigree risk"
  ),
  header_labels = c(
    estimate = "Adjusted RR",
    p.value = "P value"
  ),
  caption = "Table 2. Adjusted log-binomial regression for diabetes risk",
  remove_adjustment_note = TRUE,
  caveat = "Adjusted for selected baseline characteristics"
)

multi_adj_rr_paper

## Useful option: compact table without sample-size or abbreviation notes.
modify_table(
  multi_adj_rr,
  remove_N_obs = TRUE,
  remove_abbreviations = TRUE,
  remove_adjustment_note = TRUE,
  caption = "Compact adjusted risk-ratio table"
)


## 9. Visualise risk ratios ---------------------------------------------------

## What to look for:
## - RR = 1 is the no-association line.
## - Points to the right of 1 indicate higher risk.
## - Points to the left of 1 indicate lower risk.
## - Confidence intervals crossing 1 suggest weaker statistical evidence.
## - Significant associations can be shown in different colours using
##   sig_color and sig_errorbar_color.

plot_uni_rr <- plot_reg(uni_rr)
plot_uni_rr

plot_multi_rr <- plot_reg(multi_rr)
plot_multi_rr

plot_adj_rr <- plot_reg(multi_adj_rr)
plot_adj_rr

plot_reg(
  uni_rr,
  title = "Crude risk ratios for diabetes",
  sig_color = "darkred",
  sig_errorbar_color = "black",
  base_size = 13
)

## Log-scale x-axis can make risk-ratio plots easier to read when estimates
## vary widely.
plot_reg(
  uni_rr,
  log_x = TRUE,
  sig_color = "darkred",
  sig_errorbar_color = "black"
)

## Compare crude and adjusted risk ratios.
## This helps users see whether adjustment changes the size of the association.

plot_comb_rr <- plot_reg_combine(
  tbl_uni = uni_rr,
  tbl_multi = multi_rr
)

plot_comb_rr

plot_reg_combine(
  tbl_uni = uni_rr,
  tbl_multi = multi_adj_rr,
  sig_color = "cyan4",
  sig_errorbar_color = "red",
  base_size = 13
)


## 10. Merge descriptive and risk-ratio tables --------------------------------

final_rr_table <- merge_tables(
  synth_summary,
  uni_rr_paper,
  multi_adj_rr_paper,
  spanners = c("Clinical profile", "Crude RR", "Adjusted RR")
)

final_rr_table

## Further table polishing can be done after merging.
final_rr_table_paper <- modify_table(
  final_rr_table,
  variable_labels = c(
    bmi = "BMI category",
    age_cat = "Age group",
    npreg_cat = "Parity",
    bp_cat = "Blood pressure category",
    triceps_cat = "Triceps skinfold category",
    insulin_cat = "Serum insulin category",
    dpf_cat = "Diabetes pedigree risk"
  ),
  remove_N = FALSE,
  remove_N_obs = FALSE,
  remove_abbreviations = FALSE,
  caveat = "Log-binomial models estimate risk ratios directly."
)

final_rr_table_paper


## 11. Forest plot dataset and forest plot -----------------------------------

## forest_df() prepares the data. forest_reg() draws the plot.
## For log-binomial regression, the forest plot displays RR estimates.

df_uni_rr <- forest_df(uni_rr)


df_multi_rr <- forest_df(multi_rr)


df_both_rr <- forest_df(uni_rr, multi_rr)


df_desc_rr <- forest_df(synth_summary)


df_uni_desc_rr <- forest_df(uni_rr, desc = synth_summary)


df_multi_desc_rr <- forest_df(multi_adj_rr, desc = synth_summary)


df_both_desc_rr <- forest_df(uni_rr, multi_adj_rr, desc = synth_summary)


forest_reg(df_uni_rr)
forest_reg(df_multi_rr)
forest_reg(df_both_rr)
forest_reg(df_uni_desc_rr)
forest_reg(df_multi_desc_rr)
forest_reg(df_both_desc_rr)

## Layout check:
## If x-axis tick labels overlap, control the axis with xlim and ticks_at.
## Use a list when the forest table has crude and adjusted plot columns.
forest_reg(
  df_both_desc_rr,
  xlim = list(c(0.5, 8), c(0.8, 12)),
  ticks_at = list(
    c(0.5, 1, 2, 4, 8),
    c(1, 2, 4, 8, 12)
  )
)

## If the CI plot panel is too narrow or too wide, tune ci_col_width.
forest_reg(df_both_desc_rr, ci_col_width = c(18, 22))

## Useful option: put the plot on the left.
forest_reg(df_both_desc_rr, side = "left")

## Build and draw in one call.
forest_reg(
  uni = uni_rr,
  multi = multi_adj_rr,
  desc = synth_summary
)


## 12. Diagnostics and model selection ---------------------------------------

## Collinearity:
## VIF is useful when several categorical risk factors are modelled together.

check_collinearity(multi_rr)
check_collinearity(multi_rr, format = "gt")

## Stepwise model selection:
## The formatted output states whether forward, backward, or both
## selection was used.

select_models(
  data = synth_data,
  outcome = outcome,
  exposures = core_risk_factors,
  approach = "logbinomial",
  direction = "forward"
)

select_models(
  data = synth_data,
  outcome = outcome,
  exposures = core_risk_factors,
  approach = "logbinomial",
  direction = "backward",
  format = "gt"
)

select_models(
  data = synth_data,
  outcome = outcome,
  exposures = core_risk_factors,
  approach = "logbinomial",
  direction = "both",
  format = "tibble"
)


## 13. Confounding and interaction screening ---------------------------------

## identify_confounder() is a screening aid, not a substitute for a DAG.
## For binary outcomes, method = "mh" or method = "both" can add a
## Mantel-Haenszel comparison when the variables are eligible.

conf_age_rr <- identify_confounder(
  data = synth_data,
  outcome = outcome,
  exposure = "npreg_cat",
  potential_confounder = "age_cat",
  approach = "logbinomial",
  method = "both"
)

conf_age_rr
conf_age_rr$table

identify_confounder(
  data = synth_data,
  outcome = outcome,
  exposure = c("npreg_cat", "dpf_cat"),
  potential_confounder = c("age_cat", "bmi"),
  approach = "logbinomial",
  method = "both",
  format = "gt"
)

## interaction_models() is for a focused planned interaction term.
## Here we ask whether the parity-diabetes risk ratio differs by age group,
## adjusting for diabetes pedigree risk.

interaction_models(
  data = synth_data,
  outcome = outcome,
  exposure = "npreg_cat",
  effect_modifier = "age_cat",
  covariates = "dpf_cat",
  approach = "logbinomial"
)

interaction_models(
  data = synth_data,
  outcome = outcome,
  exposure = "npreg_cat",
  effect_modifier = "age_cat",
  covariates = c("dpf_cat", "bmi"),
  approach = "logbinomial",
  test = "Wald",
  format = "gt"
)


## 14. Stratified log-binomial regression ------------------------------------

## Question:
## Do risk ratios look different across age groups?
## This is exploratory. Use planned interaction models when a formal
## effect-modification test is needed.

## First profile the age strata. Risk ratios are easier to interpret when the
## reader can see how glucose, BMI, parity, and pedigree risk are distributed.
age_profile_rr <- descriptive_table(
  data = synth_data,
  exposures = c("glucose_cat", "bmi", "npreg_cat", "dpf_cat"),
  by = age_cat,
  percent = column,
  show_overall = last
)

age_profile_rr

str_uni_rr <- stratified_uni_reg(
  data = synth_data,
  outcome = outcome,
  exposures = c("npreg_cat", "dpf_cat"),
  stratifier = "age_cat",
  approach = "logbinomial"
)

str_uni_rr
str_uni_rr$models
str_uni_rr$model_summaries

str_multi_rr <- stratified_multi_reg(
  data = synth_data,
  outcome = outcome,
  exposures = c("age_cat", "dpf_cat"),
  stratifier = "npreg_cat",
  approach = "logbinomial"
)

str_multi_rr
str_multi_rr$models
str_multi_rr$model_summaries

stratified_multi_reg(
  data = synth_data,
  outcome = outcome,
  exposures = c("npreg_cat", "dpf_cat"),
  stratifier = "age_cat",
  adjust_for = "bmi",
  approach = "logbinomial"
)


## 15. Export outputs ---------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This keeps examples CRAN-safe and avoids accidental clutter.

save_table(final_rr_table_paper, filename = "synthetic-diabetes-logbinomial-table", format = "docx")
save_plot(plot_comb_rr, filename = "synthetic-diabetes-logbinomial-plot", format = "png")

save_docx(
  tables = list(uni_rr_paper, multi_adj_rr_paper, final_rr_table_paper),
  plots = list(plot_uni_rr, plot_adj_rr, plot_comb_rr),
  titles = list(
    "Univariable log-binomial regression",
    "Adjusted log-binomial regression",
    "Combined descriptive and risk-ratio table",
    "Forest plot - univariable",
    "Forest plot - adjusted",
    "Crude versus adjusted risk-ratio plot"
  ),
  filename = "synthetic-diabetes-logbinomial-report",
  table_width = 6.5
)


## 16. Final checklist --------------------------------------------------------

## Things to confirm manually:
## - Tables display by default as flextable.
## - Log-binomial tables use RR (95% CI), not OR.
## - The old spelling "log-binomial" still works, but "logbinomial" is preferred.
## - check_convergence() helps users understand why full log-binomial models may
##   need simplification.
## - plot_reg() highlights significant associations when colours are supplied.
## - plot_reg_combine() makes crude versus adjusted changes easy to see.
## - forest_reg() displays risk ratios with a sensible no-effect reference line.
## - model_stats = TRUE stores fit statistics in $model_stats.
## - save_docx(table_width = 6.5) keeps wide flextables fitted to a Word page.
## - select_models() output mentions the selection direction.
## - identify_confounder() prints a console summary and has a formatted $table.
## - interaction_models() returns a readable screening table and model objects.
## - save_table(), save_plot(), and save_docx() write files to tempdir when the
##   filename has no directory.

## End of manual log-binomial case study -------------------------------------
