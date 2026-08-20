## Manual real-time test: logistic regression case study
## Package: gtregression 1.1
##
## Story:
## A clinical team wants to explore risk factors for low birth weight.
## The outcome is low birth weight status. We will start with data inspection,
## build descriptive and regression tables, customise them for reporting,
## visualise the estimates, create forest plots, screen models, and export
## the final outputs.
##
## How to use:
## Run this script section by section. Do not source the whole file blindly
## unless you want every table, plot, and export example to run.

## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)
## pak::pak("ThinkDenominator/gtregression")
library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")


## 1. Prepare the clinical dataset -------------------------------------------

## The raw dataset uses numeric codes. Convert the clinical variables to factors
## so that tables and regression outputs use readable labels.

birthwt_data <- data_birthwt |>
  mutate(
    race = factor(
      race,
      levels = c(1, 2, 3),
      labels = c("White", "Black", "Other")
    ),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    ptl_cat = ifelse(ptl > 0, "Yes", "No"),
    ftv_cat = case_when(
      ftv == 0 ~ "None",
      ftv == 1 ~ "One",
      ftv >= 2 ~ "Two or more"
    )
  ) |>
  mutate(
    ptl_cat = factor(ptl_cat, levels = c("No", "Yes")),
    ftv_cat = factor(ftv_cat, levels = c("None", "One", "Two or more"))
  )

exposures <- c(
  "age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"
)

## Clearer labels for the reader:
## Set variable labels once, then descriptive tables, regression tables, plots,
## merged tables, and forest outputs use them automatically.
attr(birthwt_data$age, "label") <- "Maternal age"
attr(birthwt_data$lwt, "label") <- "Maternal weight"
attr(birthwt_data$race, "label") <- "Maternal race"
attr(birthwt_data$smoke, "label") <- "Smoking during pregnancy"
attr(birthwt_data$ht, "label") <- "Hypertension"
attr(birthwt_data$ui, "label") <- "Uterine irritability"
attr(birthwt_data$ptl_cat, "label") <- "Previous premature labour"
attr(birthwt_data$ftv_cat, "label") <- "First trimester visits"


## 2. Inspect data before modelling ------------------------------------------

## Default output is a publication-style flextable.
dissect(birthwt_data)

## Use tibble output when you want to inspect or pipe the result.
birthwt_dissect <- dissect(birthwt_data, format = "tibble")
birthwt_dissect

## Use gt output for HTML/pkgdown-style viewing.
dissect(birthwt_data, format = "gt")


## 3. Descriptive table: who has low birth weight? ----------------------------

## Minimal grouped descriptive table.
birthwt_summary <- descriptive_table(
  data = birthwt_data,
  exposures = exposures,
  by = "low"
)

birthwt_summary

## Column percentages answer:
## "Within each birth-weight group, what proportion had each risk factor?"
birthwt_summary_column <- descriptive_table(
  data = birthwt_data,
  exposures = exposures,
  by = "low",
  percent = "column",
  show_overall = "last"
)

birthwt_summary_column

## Row percentages answer:
## "Within each exposure level, how are women distributed by birth-weight group?"
birthwt_summary_row <- descriptive_table(
  data = birthwt_data,
  exposures = exposures,
  by = "low",
  percent = "row",
  show_missing = "no",
  show_overall = "first"
)

birthwt_summary_row

## Explicit output engines.
descriptive_table(
  data = birthwt_data,
  exposures = exposures,
  by = "low",
  percent = "column",
  show_overall = "last",
  format = "flextable"
)

descriptive_table(
  data = birthwt_data,
  exposures = exposures,
  by = "low",
  percent = "column",
  show_overall = "last",
  format = "gt"
)

## Friendly interactive syntax also works.
## Quoted names are preferred in scripts, but bare names are useful at console.
descriptive_table(
  data = birthwt_data,
  exposures = c(age, lwt, race, smoke),
  by = low,
  percent = row,
  show_overall = last
)

## Mixed summaries:
## - age as mean
## - lwt as median
## - ftv kept categorical even though it is numeric in the raw data
descriptive_table(
  data = birthwt_data,
  exposures = c("age", "lwt", "ftv", "smoke"),
  by = low,
  statistic = c(age = mean, lwt = median, ftv = categorical),
  percent = column,
  show_missing = no
)


## 4. Univariable logistic regression ----------------------------------------

## Question:
## Which individual variables are associated with low birth weight?

uni_or <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit"
)

uni_or
uni_or$table_body
uni_or$models
uni_or$model_summaries

## Optional model statistics are stored outside the publication table.
uni_or_stats <- uni_reg(
  data = birthwt_data,
  outcome = low,
  exposures = exposures,
  approach = logit,
  model_stats = TRUE
)

uni_or_stats$model_stats

## Useful option: gt output for HTML viewing.
uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  format = "gt"
)


## 5. Multivariable logistic regression --------------------------------------

## Question:
## Which risk factors remain associated after putting all exposures in one model?

multi_or <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit"
)

multi_or
multi_or$table_body
multi_or$models
multi_or$model_summaries

## Adjusted mode:
## Estimate each exposure separately, adjusting for the same core confounders.

multi_adj <- multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = "logit"
)

multi_adj

## Friendly interactive syntax for model arguments.
multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c(smoke, ht, ui),
  adjust_for = c(age, lwt, race),
  approach = logit
)

multi_adj_stats <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c(smoke, ht, ui),
  adjust_for = c(age, lwt, race),
  approach = logit,
  model_stats = TRUE
)

multi_adj_stats$model_stats


## 6. Modify tables for publication ------------------------------------------

## This is where the table is tuned for manuscript use.
## Use variable labels, level labels, custom headers, captions, and caveats.

uni_or_paper <- modify_table(
  uni_or,
  variable_labels = c(
    age = "Maternal age",
    lwt = "Maternal weight",
    race = "Race",
    smoke = "Smoking during pregnancy",
    ht = "Hypertension",
    ui = "Uterine irritability",
    ptl_cat = "Previous premature labour",
    ftv_cat = "First trimester visits"
  ),
  level_labels = list(
    smoke = c(Yes = "Smoker"),
    ht = c(Yes = "Hypertension present"),
    ui = c(Yes = "Uterine irritability present")
  ),
  header_labels = c(
    estimate = "Crude OR",
    p.value = "P value"
  ),
  caption = "Table 1. Univariable logistic regression for low birth weight",
  caveat = "Screening analysis; interpret with clinical context."
)

uni_or_paper

multi_adj_paper <- modify_table(
  multi_adj,
  variable_labels = c(
    smoke = "Smoking during pregnancy",
    ht = "Hypertension",
    ui = "Uterine irritability"
  ),
  header_labels = c(
    estimate = "Adjusted OR",
    p.value = "P value"
  ),
  caption = "Table 2. Adjusted logistic regression for selected risk factors",
  remove_N_obs = FALSE,
  remove_adjustment_note= TRUE,
  caveat = "Adjusted for baseline characteristics"
)

multi_adj_paper

## Useful option: remove sample-size and abbreviation notes for compact reports.
modify_table(
  multi_adj,
  remove_N_obs = TRUE,
  remove_abbreviations = TRUE,
  caption = "Compact adjusted regression table"
)

## 6b. Model fit checks --------------------------------------------------------

## Logistic regression diagnostics are for model checking, not publication
## tables. For a single binary exposure such as smoking, residual and influence
## plots are usually more helpful than calibration because the model has only
## two fitted probabilities.

plot_model_fit(
  uni_or,
  model_name = smoke,
  type = residual
)

plot_model_fit(
  uni_or,
  model_name = smoke,
  type = all,
  bins = 6
)

## Calibration is more informative for a multivariable model because predicted
## probabilities vary across many covariate profiles. Points closer to the
## dashed line indicate better agreement between predicted and observed risk.

plot_model_fit(
  multi_or,
  type = calibration,
  bins = 6
)

plot_model_fit(
  multi_or,
  type = all
)
## 7. Regression plots ---------------------------------------------------------

## Plot univariable and multivariable estimates.
## What to look for:
## - Estimates to the right of 1 suggest higher odds of low birth weight.
## - Estimates to the left of 1 suggest lower odds.
## - Variables with statistically significant associations can be highlighted
##   in different colours using sig_color and sig_errorbar_color.

plot_uni <- plot_reg(uni_or)
plot_uni

plot_multi <- plot_reg(multi_or)
plot_multi

plot_adj <- plot_reg(multi_adj)
plot_adj

## Useful styling options.
plot_reg(
  uni_or,
  sig_color = "red",
  sig_errorbar_color = "cyan",
  base_size = 12
)

## Compare crude and adjusted estimates visually.
## This is useful for spotting variables whose effect changes after adjustment.
plot_comb <- plot_reg_combine(uni_or, multi_or)
plot_comb

plot_reg_combine(
  uni_or,
  multi_adj,
  sig_color = "red",
  sig_errorbar_color = "cyan",
  base_size = 12
)


## 8. Merge descriptive and regression tables --------------------------------

## Create a single table for a manuscript or report.

final_table <- merge_tables(
  birthwt_summary_column,
  uni_or_paper,
  multi_adj_paper,
  spanners = c("Descriptive", "Crude", "Adjusted")
)

final_table

merge_tables(uni_or, multi_adj)
## 9. Forest plot dataset and forest plot ------------------------------------

## forest_df() prepares the data. forest_reg() draws the plot.
## The forest plot is the most visual publication output: users can inspect
## direction, uncertainty, and clinically important signals in one place.

df_uni <- forest_df(uni_or)
df_uni

df_multi <- forest_df(multi_or)
df_multi

df_both <- forest_df(uni_or, multi_or)
df_both

df_desc <- forest_df(birthwt_summary_column)
df_desc

df_uni_desc <- forest_df(uni_or, desc = birthwt_summary_column)
df_uni_desc

df_multi_desc <- forest_df(multi_adj, desc = birthwt_summary_column)
df_multi_desc

df_both_desc <- forest_df(uni_or, multi_adj, desc = birthwt_summary_column)
df_both_desc

forest_reg(df_uni)
forest_reg(df_multi)
forest_reg(df_both)
forest_reg(df_uni_desc)
forest_reg(df_multi_desc)
forest_reg(df_both_desc)

## Layout check:
## If x-axis tick labels overlap, control the axis with xlim and ticks_at.
## Use a list when the forest table has crude and adjusted plot columns.
forest_reg(
  df_both_desc,
  xlim = list(c(0.25, 8), c(0.8, 25)),
  ticks_at = list(
    c(0.5, 1, 2, 4, 8),
    c(1, 2, 4, 8, 16)
  )
)

## If the CI plot panel is too narrow or too wide, tune ci_col_width.
## Larger values give more room to the CI panel; smaller values compact it.
forest_reg(df_both_desc, ci_col_width = c(18, 22))

## Useful option: put the plot on the left.
forest_reg(df_both_desc, side = "left")

## Build and draw in one call.
forest_reg(
  uni = uni_or,
  multi = multi_adj,
  desc = birthwt_summary_column
)


## 10. Stratified analysis ----------------------------------------------------

## Question:
## Do the observed associations look different across race groups?
## Read this section as a visual check for consistency across strata, not as a
## replacement for a planned interaction analysis.

## First profile the strata. This tells the reader whether the race groups have
## enough observations and whether their baseline clinical profiles differ.
race_profile <- descriptive_table(
  data = birthwt_data,
  exposures = c("age", "lwt", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  by = race,
  percent = column,
  show_overall = last
)

race_profile

strat_uni <- stratified_uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  stratifier = "race",
  approach = "logit"
)

strat_uni
strat_uni$model_summaries

strat_multi <- stratified_multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  stratifier = "race",
  approach = "logit"
)

strat_multi

stratified_multi_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  stratifier = "race",
  adjust_for = c("age", "lwt"),
  approach = "logit"
)

## Quick stratified plots:
## plot_reg() accepts one stratified gtregression object and draws one panel per
## stratum. This is useful for slides, meetings, and checking whether the
## direction of association looks broadly similar across race groups.
plot_reg(
  strat_uni,
  title = "Crude odds ratios by race"
)

plot_reg(
  strat_multi,
  title = "Adjusted odds ratios by race"
)

## plot_reg_combine() is intentionally not used for stratified objects. Crude
## plus adjusted panels across several strata become too busy for a slide.
## Use forest_reg() below for the publication-style stratified display.

## Forest plot by stratum:
## forest_df() accepts one stratified regression object at a time. The stratum
## headers are shaded in forest_reg(), which makes it easier to scan the race
## groups without confusing them with ordinary predictor rows.
strat_forest_data <- forest_df(strat_multi)


forest_reg(strat_forest_data)

## If a reviewer needs more room for the confidence intervals, widen the CI
## plotting column without changing the underlying estimates.
forest_reg(strat_forest_data, ci_col_width = 24)


## 11. Model checks and selection --------------------------------------------

## Convergence check:
## Look for failed models or impossible fitted values.

check_convergence(
  data = birthwt_data,
  exposures = exposures,
  outcome = "low",
  approach = "logit"
)

check_convergence(
  data = birthwt_data,
  exposures = exposures,
  outcome = "low",
  approach = "logit",
  multivariate = TRUE,
  format = "gt"
)

## Collinearity check:
## Useful after fitting a multivariable model.

check_collinearity(multi_or)
check_collinearity(multi_or, format = "gt")

## Prespecified model comparison:
## compare_models() is different from select_models(). Here we fit the models
## with multi_reg(), then ask gtregression to compare those package outputs.
## The comparison should not refit models or do hidden complete-case filtering.

logit_m0 <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = smoke,
  approach = logit
)

logit_m1 <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c(smoke, age, lwt),
  approach = logit
)

logit_m2 <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c(smoke, age, lwt, race, ht, ui),
  approach = logit
)

logit_compare <- compare_models(
  logit_m0,
  logit_m1,
  logit_m2,
  model_names = c(
    "Smoking only",
    "Add maternal age and weight",
    "Add clinical risk factors"
  ),
  primary_exposure = smoke
)

logit_compare
logit_compare$table_body
logit_compare$table_display

## Named-list input is handy when models have already been stored.
compare_models(
  list(
    "Smoking only" = logit_m0,
    "Adjusted core" = logit_m1,
    "Clinical risk model" = logit_m2
  ),
  primary_exposure = "smoke",
  format = gt
)

## Stepwise model selection:
## Publication-ready output now reports the direction used.

select_models(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  direction = "forward"
)

select_models(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  direction = "backward",
  format = "gt"
)

select_models(
  data = birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  direction = "both",
  format = "tibble"
)


## 12. Confounding and interaction screening ---------------------------------

## identify_confounder() is a screening aid, not a substitute for a DAG.
## A plain call prints a tidy console summary. The formatted table is in $table.

conf_race <- identify_confounder(
  data = birthwt_data,
  outcome = "low",
  exposure = "smoke",
  potential_confounder = "race",
  approach = "logit"
)

conf_race
conf_race$table

identify_confounder(
  data = birthwt_data,
  outcome = "low",
  exposure = "smoke",
  potential_confounder = c("race", "ht", "ui"),
  approach = "logit",
  method = "both",
  format = "gt"
)

## interaction_models() is for a focused planned interaction term.
## Use this when you want to formally compare models with and without the
## exposure-by-modifier term. The table is for screening and interpretation,
## while the model objects are available for deeper review.

interaction_models(
  data = birthwt_data,
  outcome = "low",
  exposure = "smoke",
  effect_modifier = "race",
  covariates = c("age", "lwt"),
  approach = "logit",
  test = "LRT"
)

interaction_models(
  data = birthwt_data,
  outcome = "low",
  exposure = "smoke",
  effect_modifier = "race",
  covariates = c("age", "lwt"),
  approach = "logit",
  test = "Wald",
  format = "gt"
)


## 13. Export outputs ---------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This keeps examples CRAN-safe and avoids accidental clutter.

save_table(final_table, filename = "birthwt-logit-table", format = "docx")
save_plot(plot_comb, filename = "birthwt-logit-plot", format = "png")

save_docx(
  tables = list(uni_or_paper, multi_adj_paper, final_table),
  plots = list(plot_uni, plot_adj, plot_comb),
  titles = list(
    "Univariable logistic regression",
    "Adjusted logistic regression",
    "Combined descriptive and regression table",
    "Forest plot - univariable",
    "Forest plot - adjusted",
    "Crude versus adjusted forest plot"
  ),
  filename = "birthwt-logit-report",
  table_width = 6.5
)


## 14. Final checklist --------------------------------------------------------

## Things to confirm manually:
## - Tables display by default as flextable.
## - format = "gt" renders attractive HTML/pkgdown tables.
## - modify_table() labels and captions appear correctly.
## - plot_reg() and plot_reg_combine() include adjusted-variable footnotes.
## - select_models() output mentions the selection direction.
## - identify_confounder() prints a console summary and has a formatted $table.
## - model_stats = TRUE stores fit statistics in $model_stats without cluttering
##   the publication table.
## - plot_reg() draws faceted plots for one stratified object.
## - plot_reg_combine() is intentionally reserved for non-stratified crude versus
##   adjusted comparisons.
## - save_docx(table_width = 6.5) keeps wide flextables fitted to a Word page.
## - save_table(), save_plot(), and save_docx() write files to tempdir when the
##   filename has no directory.
## End of manual logit case study --------------------------------------------
