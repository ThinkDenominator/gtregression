## Manual real-time test: matched case-control analysis
## Package: gtregression 1.2
##
## Story:
## Investigators matched one case to two controls within each matched set. They
## want matched odds ratios for an exposure, first alone and then adjusted for
## age and sex. Ordinary logistic regression would ignore the matching.
##
## How to use:
## Run this script section by section. Inspect each printed table, note, and
## plot before moving to the next section.


## 0. Setup -------------------------------------------------------------------

## During package development: devtools::load_all(".")
## After installation: library(gtregression)

library(gtregression)


## 1. Create a reproducible matched dataset ----------------------------------

set.seed(1201)

matched_data <- data.frame(
  matched_set = rep(seq_len(80), each = 3),
  case = rep(c(1, 0, 0), 80),
  age = rep(seq(35, 74, length.out = 80), each = 3) +
    rep(c(0, -2, 2), 80),
  sex = factor(sample(c("Female", "Male"), 240, replace = TRUE))
)

matched_data$exposed <- factor(
  stats::rbinom(
    nrow(matched_data),
    1,
    ifelse(matched_data$case == 1, 0.62, 0.34)
  ),
  levels = c(0, 1),
  labels = c("No", "Yes")
)

attr(matched_data$case, "label") <- "Case status"
attr(matched_data$exposed, "label") <- "Primary exposure"
attr(matched_data$age, "label") <- "Participant age"
attr(matched_data$sex, "label") <- "Participant sex"
attr(matched_data$matched_set, "label") <- "Matched set"

head(matched_data)
table(matched_data$case)
table(matched_data$matched_set)
table(matched_data$case, matched_data$exposed)
dissect(matched_data)


## 2. Minimal matched analysis ------------------------------------------------

matched_crude <- conditional_logit_reg(
  data = matched_data,
  outcome = case,
  exposures = exposed,
  matched_set = matched_set
)

matched_crude
matched_crude$table_body
matched_crude$models
matched_crude$model_summaries


## 3. Several exposure-specific matched models -------------------------------

matched_univariable <- conditional_logit_reg(
  data = matched_data,
  outcome = case,
  exposures = c(exposed, age, sex),
  matched_set = matched_set,
  model_stats = TRUE
)

matched_univariable
matched_univariable$model_stats


## 4. Exposure-specific adjusted model ---------------------------------------

## Adjustment variables should be named in the note but hidden from rows.

matched_adjusted <- conditional_logit_reg(
  data = matched_data,
  outcome = case,
  exposures = exposed,
  matched_set = matched_set,
  adjust_for = c(age, sex),
  model_stats = TRUE
)

matched_adjusted
matched_adjusted$model_stats


## 5. One joint multivariable matched model ----------------------------------

matched_joint <- conditional_logit_reg(
  data = matched_data,
  outcome = case,
  exposures = c(exposed, age, sex),
  matched_set = matched_set,
  multivariable = TRUE,
  model_stats = TRUE
)

matched_joint
matched_joint$table_body
matched_joint$model_stats


## 6. Reference rows and output formats --------------------------------------

conditional_logit_reg(
  data = matched_data,
  outcome = case,
  exposures = c(exposed, sex),
  matched_set = matched_set,
  show_ref = FALSE
)

conditional_logit_reg(
  data = matched_data,
  outcome = case,
  exposures = exposed,
  matched_set = matched_set,
  format = gt
)


## 7. Modify and merge --------------------------------------------------------

matched_for_paper <- modify_table(
  matched_adjusted,
  caption = "Matched association with case status",
  header_labels = c(estimate = "Matched adjusted OR"),
  caveat = "Conditional logistic regression accounted for matched sets."
)

matched_for_paper

matched_merged <- merge_tables(
  matched_crude,
  matched_adjusted,
  spanners = c("Matched crude", "Matched adjusted")
)

matched_merged


## 8. Plot and forest output --------------------------------------------------

matched_plot <- plot_reg(matched_adjusted)
matched_plot

matched_forest_data <- forest_df(matched_crude, matched_adjusted)
matched_forest_data

matched_forest <- forest_reg(
  matched_forest_data
)

matched_forest


## 9. What to inspect ---------------------------------------------------------

## - The output says matched odds ratio, not ordinary odds ratio.
## - matched_set is used for conditioning but has no reported coefficient.
## - Model statistics show 80 cases, 160 controls, and 80 matched sets.
## - Adjustment notes use readable display labels.
## - Reference rows respond to show_ref.
## - Modified, merged, plot, and forest outputs render cleanly.
