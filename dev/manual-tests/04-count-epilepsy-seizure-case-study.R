## Manual real-time test: count regression case study
## Package: gtregression 1.1
##
## Story:
## A neurology team is reviewing seizure counts from an epilepsy treatment
## dataset. The outcome is the number of seizures during each follow-up period.
## Count outcomes are commonly modelled with Poisson regression, but seizure
## counts are often overdispersed. This case study starts with Poisson, checks
## dispersion, then compares negative binomial regression.
##
## Important note:
## This script is designed to test gtregression functions and teach the count
## modelling workflow. The epilepsy data contain repeated periods per subject.
## A final clinical analysis may need clustered, repeated-measures, or mixed
## modelling methods. Here, rows are treated as independent for demonstration.
##
## How to use:
## Run this script section by section. The comments tell the user what to look
## for and when a result is mainly exploratory.

## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)

library(gtregression)
library(dplyr)

data("data_epilepsy", package = "gtregression")


## 1. Prepare the seizure-count dataset --------------------------------------

## The raw dataset uses compact variable names. We create readable variables for
## tables, plots, stratified analysis, confounding checks, and interactions.

epilepsy_data <- data_epilepsy |>
  mutate(
    seizures = y,
    treatment = trt,
    period_cat = factor(
      period,
      levels = sort(unique(period)),
      labels = paste("Period", sort(unique(period)))
    ),
    final_period = factor(V4, levels = c(0, 1), labels = c("No", "Yes")),
    age_cat = case_when(
      age < 25 ~ "Young",
      age >= 25 & age < 35 ~ "Middle-aged",
      age >= 35 ~ "Older",
      TRUE ~ NA_character_
    ),
    age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
    baseline_cat = case_when(
      base < 15 ~ "Low",
      base >= 15 & base < 40 ~ "Medium",
      base >= 40 ~ "High",
      TRUE ~ NA_character_
    ),
    baseline_cat = factor(baseline_cat, levels = c("Low", "Medium", "High")),
    sex = ifelse(subject %% 2 == 0, "Female", "Male"),
    sex = factor(sex)
  )

outcome <- "seizures"
count_predictors <- c("treatment", "age_cat", "sex", "baseline_cat", "period_cat")
core_predictors <- c("treatment", "age_cat", "baseline_cat")


## 2. Inspect data before count modelling ------------------------------------

## Default output is a publication-style flextable.
dissect(epilepsy_data)

## Use tibble output when you want a console-friendly audit.
epilepsy_dissect <- dissect(epilepsy_data, format = "tibble")
epilepsy_dissect

## Check the observed seizure distribution directly.
summary(epilepsy_data$seizures)
table(epilepsy_data$treatment)
table(epilepsy_data$baseline_cat, useNA = "ifany")


## 3. Descriptive table: seizure burden by treatment --------------------------

## This descriptive table tells the reader who received each treatment and how
## baseline seizure burden is distributed.

epilepsy_summary <- descriptive_table(
  data = epilepsy_data,
  exposures = c("seizures", "base", "age", count_predictors),
  by = "treatment",
  percent = "column",
  show_overall = "last"
)

epilepsy_summary

## Row percentages answer:
## "Within each baseline seizure category, how are patients distributed by
## treatment group?"

descriptive_table(
  data = epilepsy_data,
  exposures = c("baseline_cat", "age_cat", "sex", "period_cat"),
  by = "treatment",
  percent = "row",
  show_missing = "no",
  show_overall = "first"
)

## Explicit gt output for HTML/pkgdown-style viewing.
descriptive_table(
  data = epilepsy_data,
  exposures = c("seizures", "base", "age", count_predictors),
  by = "treatment",
  percent = "column",
  show_overall = "last",
  format = "gt"
)


## 4. Minimal Poisson regression ---------------------------------------------

## Question:
## Is treatment associated with seizure count?
##
## Incidence rate ratio interpretation:
## IRR > 1 suggests a higher seizure rate/count.
## IRR < 1 suggests a lower seizure rate/count.
## IRR = 1 is the no-association line.

uni_treatment_pois <- uni_reg(
  data = epilepsy_data,
  outcome = "seizures",
  exposures = "treatment",
  approach = "poisson"
)

uni_treatment_pois
uni_treatment_pois$table_body
uni_treatment_pois$models
uni_treatment_pois$model_summaries


## 5. Univariable Poisson regression -----------------------------------------

## Question:
## Which individual variables are associated with seizure count?

uni_pois <- uni_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = count_predictors,
  approach = "poisson"
)

uni_pois
uni_pois$table_body
uni_pois$model_summaries

## Useful option: gt output for HTML viewing.
uni_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = count_predictors,
  approach = "poisson",
  format = "gt"
)


## 6. Multivariable Poisson regression ---------------------------------------

## Question:
## Which variables remain associated after adjustment?

multi_pois <- multi_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = count_predictors,
  approach = "poisson"
)

multi_pois
multi_pois$table_body
multi_pois$models
multi_pois$model_summaries

## Adjusted mode:
## Estimate treatment and baseline burden separately, adjusting for age and sex.

multi_adj_pois <- multi_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = c("treatment", "baseline_cat"),
  adjust_for = c("age_cat", "sex", "period_cat"),
  approach = "poisson"
)

multi_adj_pois

## Friendly interactive syntax also works at the console.
multi_reg(
  data = epilepsy_data,
  outcome = seizures,
  exposures = c(treatment, baseline_cat),
  adjust_for = c(age_cat, sex, period_cat),
  approach = poisson
)


## 7. Check convergence and overdispersion -----------------------------------

## Convergence check:
## Look for failed models or impossible fitted values.

check_convergence(
  data = epilepsy_data,
  exposures = count_predictors,
  outcome = outcome,
  approach = "poisson"
)

check_convergence(
  data = epilepsy_data,
  exposures = count_predictors,
  outcome = outcome,
  approach = "poisson",
  multivariate = TRUE,
  format = "gt"
)

## Manual overdispersion check:
## A Pearson dispersion ratio near 1 supports the Poisson mean-variance
## assumption. Values clearly above 1.5 or 2 suggest overdispersion, where
## negative binomial regression may be more appropriate.

poisson_fit <- multi_pois$models$multivariable_model
poisson_dispersion <- sum(stats::residuals(poisson_fit, type = "pearson")^2) /
  poisson_fit$df.residual
poisson_dispersion


## 8. Negative binomial regression -------------------------------------------

## Negative binomial regression is useful when count data are overdispersed.
## The output still uses IRR (95% CI), so interpretation is familiar.

uni_nb <- uni_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = count_predictors,
  approach = "negbin"
)

uni_nb
uni_nb$table_body
uni_nb$model_summaries

multi_nb <- multi_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = count_predictors,
  approach = "negbin"
)

multi_nb
multi_nb$table_body
multi_nb$models
multi_nb$model_summaries

multi_adj_nb <- multi_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = c("treatment", "baseline_cat"),
  adjust_for = c("age_cat", "sex", "period_cat"),
  approach = "negbin"
)

multi_adj_nb


## 9. Modify tables for publication ------------------------------------------

## Use modify_table() to make the count-model output publication friendly.

multi_pois_paper <- modify_table(
  multi_adj_pois,
  variable_labels = c(
    treatment = "Treatment group",
    baseline_cat = "Baseline seizure burden"
  ),
  level_labels = list(
    treatment = c(progabide = "Progabide"),
    baseline_cat = c(High = "High baseline burden")
  ),
  header_labels = c(
    estimate = "Adjusted IRR",
    p.value = "P value"
  ),
  caption = "Table 1. Adjusted Poisson regression for seizure counts",
  caveat = "Poisson regression assumes the count variance is close to the mean."
)

multi_pois_paper

multi_nb_paper <- modify_table(
  multi_adj_nb,
  variable_labels = c(
    treatment = "Treatment group",
    baseline_cat = "Baseline seizure burden"
  ),
  level_labels = list(
    treatment = c(progabide = "Progabide"),
    baseline_cat = c(High = "High baseline burden")
  ),
  header_labels = c(
    estimate = "Adjusted IRR",
    p.value = "P value"
  ),
  caption = "Table 2. Adjusted negative binomial regression for seizure counts",
  caveat = "Negative binomial regression is often preferred when counts are overdispersed."
)

multi_nb_paper

## Useful option: remove technical notes for a compact appendix table.
modify_table(
  multi_nb,
  remove_N_obs = TRUE,
  remove_abbreviations = TRUE,
  caption = "Compact negative binomial model"
)


## 10. Visualise count-model IRRs --------------------------------------------

## What to look for:
## - IRR = 1 is the no-association line.
## - Points to the right of 1 indicate higher seizure counts/rates.
## - Points to the left of 1 indicate lower seizure counts/rates.
## - Confidence intervals crossing 1 suggest weaker statistical evidence.
## - Significant associations can be shown in different colours using
##   sig_color and sig_errorbar_color.

plot_uni_pois <- plot_reg(uni_pois)
plot_uni_pois

plot_multi_pois <- plot_reg(multi_pois)
plot_multi_pois

plot_multi_nb <- plot_reg(multi_nb)
plot_multi_nb

plot_reg(
  multi_nb,
  title = "Adjusted IRRs from negative binomial regression",
  sig_color = "darkred",
  sig_errorbar_color = "black",
  base_size = 13
)

## Compare Poisson and negative binomial estimates separately through tables,
## and compare crude versus adjusted estimates visually within each approach.

plot_pois_comb <- plot_reg_combine(uni_pois, multi_pois)
plot_pois_comb

plot_nb_comb <- plot_reg_combine(uni_nb, multi_nb)
plot_nb_comb

plot_reg_combine(
  uni_nb,
  multi_adj_nb,
  sig_color = "cyan4",
  sig_errorbar_color = "red",
  base_size = 13
)


## 11. Merge and compare Poisson versus negative binomial tables --------------

## This side-by-side table helps users see whether the modelling choice changes
## the clinical interpretation.

count_model_comparison <- merge_tables(
  multi_pois_paper,
  multi_nb_paper,
  spanners = c("Poisson", "Negative binomial")
)

count_model_comparison

final_count_table <- merge_tables(
  epilepsy_summary,
  multi_pois_paper,
  multi_nb_paper,
  spanners = c("Clinical profile", "Poisson", "Negative binomial")
)

final_count_table


## 12. Forest plot dataset and forest plot -----------------------------------

## forest_df() prepares the data. forest_reg() draws the plot.
## For count regression, the forest plot displays IRR estimates.

df_uni_pois <- forest_df(uni_pois)
df_uni_pois

df_multi_pois <- forest_df(multi_pois)
df_multi_pois

df_uni_nb <- forest_df(uni_nb)
df_uni_nb

df_multi_nb <- forest_df(multi_nb)
df_multi_nb

df_pois_both <- forest_df(uni_pois, multi_pois)
df_pois_both

df_nb_both <- forest_df(uni_nb, multi_nb)
df_nb_both

df_count_desc_nb <- forest_df(uni_nb, multi_adj_nb, desc = epilepsy_summary)
df_count_desc_nb

forest_reg(df_uni_pois)
forest_reg(df_multi_pois)
forest_reg(df_pois_both)
forest_reg(df_nb_both)
forest_reg(df_count_desc_nb)

## Useful option: put the plot on the left.
forest_reg(df_count_desc_nb, side = "left")

## Build and draw in one call.
forest_reg(
  uni = uni_nb,
  multi = multi_adj_nb,
  desc = epilepsy_summary
)


## 13. Diagnostics and model selection ---------------------------------------

## Collinearity:
## VIF is useful when several predictors are included together.

check_collinearity(multi_pois)
check_collinearity(multi_nb)
check_collinearity(multi_nb, format = "gt")

## Stepwise model selection:
## The publication-ready output states whether forward, backward, or both
## selection was used.

select_models(
  data = epilepsy_data,
  outcome = outcome,
  exposures = core_predictors,
  approach = "poisson",
  direction = "forward"
)

select_models(
  data = epilepsy_data,
  outcome = outcome,
  exposures = core_predictors,
  approach = "negbin",
  direction = "backward",
  format = "gt"
)

select_models(
  data = epilepsy_data,
  outcome = outcome,
  exposures = core_predictors,
  approach = "negbin",
  direction = "both",
  format = "tibble"
)


## 14. Confounding and interaction screening ---------------------------------

## identify_confounder() is a screening aid, not a substitute for a DAG.
## For count outcomes, method = "change" compares crude and adjusted model
## estimates. Mantel-Haenszel is for eligible binary outcome settings, not this
## count example.

conf_baseline_pois <- identify_confounder(
  data = epilepsy_data,
  outcome = outcome,
  exposure = "treatment",
  potential_confounder = "baseline_cat",
  approach = "poisson",
  method = "change"
)

conf_baseline_pois
conf_baseline_pois$table

identify_confounder(
  data = epilepsy_data,
  outcome = outcome,
  exposure = "treatment",
  potential_confounder = c("age_cat", "baseline_cat", "sex"),
  approach = "negbin",
  method = "change",
  format = "gt"
)

## interaction_models() tests a planned interaction term.
## Here we ask whether the treatment effect differs by baseline seizure burden,
## adjusting for age group and sex.

interaction_models(
  data = epilepsy_data,
  outcome = outcome,
  exposure = "treatment",
  effect_modifier = "baseline_cat",
  covariates = c("age_cat", "sex"),
  approach = "poisson"
)

interaction_models(
  data = epilepsy_data,
  outcome = outcome,
  exposure = "treatment",
  effect_modifier = "baseline_cat",
  covariates = c("age_cat", "sex"),
  approach = "negbin",
  test = "Wald",
  format = "gt"
)


## 15. Stratified count regression -------------------------------------------

## Question:
## Do treatment IRRs look different across baseline seizure burden groups?
## This is exploratory. Use planned interaction models when a formal
## effect-modification test is needed.

strat_uni_pois <- stratified_uni_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = c("treatment", "age_cat", "sex"),
  stratifier = "baseline_cat",
  approach = "poisson"
)

strat_uni_pois
strat_uni_pois$models
strat_uni_pois$model_summaries

strat_multi_nb <- stratified_multi_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = c("treatment", "age_cat", "sex"),
  stratifier = "baseline_cat",
  approach = "negbin"
)

strat_multi_nb
strat_multi_nb$models
strat_multi_nb$model_summaries

stratified_multi_reg(
  data = epilepsy_data,
  outcome = outcome,
  exposures = "treatment",
  stratifier = "baseline_cat",
  adjust_for = c("age_cat", "sex", "period_cat"),
  approach = "negbin"
)


## 16. Export outputs ---------------------------------------------------------

## Files are written to a temporary folder by default when no full destination
## path is supplied. This is CRAN-friendly and prevents accidental clutter.

save_table(final_count_table, filename = "epilepsy-count-table", format = "docx")
save_plot(plot_nb_comb, filename = "epilepsy-count-plot", format = "png")

save_docx(
  tables = list(multi_pois_paper, multi_nb_paper, final_count_table),
  plots = list(plot_multi_pois, plot_multi_nb, plot_nb_comb),
  titles = list(
    "Adjusted Poisson regression",
    "Adjusted negative binomial regression",
    "Combined descriptive and count-model table",
    "Forest plot - Poisson",
    "Forest plot - negative binomial",
    "Crude versus adjusted negative binomial plot"
  ),
  filename = "epilepsy-count-report"
)


## 17. Final checklist --------------------------------------------------------

## Things to confirm manually:
## - Tables display by default as flextable.
## - Poisson and negative binomial tables use IRR (95% CI).
## - check_convergence() works for Poisson and negative binomial models.
## - The Pearson dispersion ratio helps explain when negative binomial may be
##   preferable to Poisson.
## - plot_reg() highlights significant associations when colours are supplied.
## - plot_reg_combine() makes crude versus adjusted changes easy to see.
## - forest_reg() displays IRRs with a no-effect reference line at 1.
## - select_models() output mentions the selection direction.
## - identify_confounder() prints a console summary and has a formatted $table.
## - interaction_models() returns a readable screening table and model objects.
## - stratified outputs are useful for exploratory effect-pattern checks.
## - save_table(), save_plot(), and save_docx() write files to tempdir when the
##   filename has no directory.

## End of manual count regression case study ---------------------------------
