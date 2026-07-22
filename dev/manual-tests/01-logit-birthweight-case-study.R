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
pak::pak("ThinkDenominator/gtregression")
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


## 6. Modify tables for publication ------------------------------------------

## This is where the table becomes manuscript-friendly.
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
  caveat = "Adjusted for maternal age, maternal weight, and race."
)

multi_adj_paper

## Useful option: remove sample-size and abbreviation notes for compact reports.
modify_table(
  multi_adj,
  remove_N_obs = TRUE,
  remove_abbreviations = TRUE,
  caption = "Compact adjusted regression table"
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
## path is supplied. This is CRAN-friendly and prevents accidental clutter.

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
  filename = "birthwt-logit-report"
)


## 14. Final checklist --------------------------------------------------------

## Things to confirm manually:
## - Tables display by default as flextable.
## - format = "gt" renders attractive HTML/pkgdown tables.
## - modify_table() labels and captions appear correctly.
## - plot_reg() and plot_reg_combine() include adjusted-variable footnotes.
## - select_models() output mentions the selection direction.
## - identify_confounder() prints a console summary and has a formatted $table.
## - save_table(), save_plot(), and save_docx() write files to tempdir when the
##   filename has no directory.

## End of manual logit case study --------------------------------------------
