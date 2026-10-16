## Manual real-time test: ordinal logistic regression
## Package: gtregression 1.2
##
## Story:
## Clinical recovery has four ordered categories: Poor, Fair, Good, and
## Excellent. We want common odds ratios for being in a higher category.
##
## How to use:
## Run section by section. Pay special attention to outcome order and the
## proportional-odds assumption results.


## 0. Setup -------------------------------------------------------------------

## During development: devtools::load_all(".")
## After installation: library(gtregression)

library(gtregression)


## 1. Create an ordered recovery outcome -------------------------------------

set.seed(1401)
n <- 600

recovery_data <- data.frame(
  treatment = factor(
    sample(c("Usual care", "Intervention"), n, replace = TRUE),
    levels = c("Usual care", "Intervention")
  ),
  age = stats::rnorm(n, 62, 11),
  sex = factor(sample(c("Female", "Male"), n, replace = TRUE))
)

latent_recovery <-
  0.8 * (recovery_data$treatment == "Intervention") -
  0.02 * recovery_data$age +
  0.2 * (recovery_data$sex == "Male") +
  stats::rlogis(n)

recovery_data$recovery <- cut(
  latent_recovery,
  breaks = c(-Inf, -1.8, -0.8, 0.2, Inf),
  labels = c("Poor", "Fair", "Good", "Excellent"),
  ordered_result = TRUE
)

attr(recovery_data$treatment, "label") <- "Treatment group"
attr(recovery_data$age, "label") <- "Participant age"
attr(recovery_data$sex, "label") <- "Participant sex"
attr(recovery_data$recovery, "label") <- "Clinical recovery"

head(recovery_data)
table(recovery_data$recovery)
levels(recovery_data$recovery)
is.ordered(recovery_data$recovery)
dissect(recovery_data)


## 2. Crude ordinal models ----------------------------------------------------

ordinal_crude <- ordinal_reg(
  data = recovery_data,
  outcome = recovery,
  exposures = c(treatment, age, sex),
  model_stats = TRUE
)

ordinal_crude
ordinal_crude$table_body
ordinal_crude$models
ordinal_crude$model_stats
ordinal_crude$assumption_tests


## 3. Exposure-specific adjusted models --------------------------------------

ordinal_adjusted <- ordinal_reg(
  data = recovery_data,
  outcome = recovery,
  exposures = c(treatment, age),
  adjust_for = sex,
  model_stats = TRUE
)

ordinal_adjusted
ordinal_adjusted$model_stats
ordinal_adjusted$assumption_tests


## 4. One joint multivariable model ------------------------------------------

ordinal_joint <- ordinal_reg(
  data = recovery_data,
  outcome = recovery,
  exposures = c(treatment, age, sex),
  multivariable = TRUE,
  model_stats = TRUE
)

ordinal_joint
ordinal_joint$table_body
ordinal_joint$model_stats
ordinal_joint$assumption_tests


## 5. Reverse the outcome order ----------------------------------------------

## This demonstrates why level order matters. Effect directions should reverse.

recovery_reversed <- recovery_data
recovery_reversed$recovery <- ordered(
  recovery_reversed$recovery,
  levels = rev(levels(recovery_data$recovery))
)

ordinal_reversed <- ordinal_reg(
  data = recovery_reversed,
  outcome = recovery,
  exposures = treatment,
  assumption_test = FALSE
)

ordinal_reversed


## 6. Reference and output options -------------------------------------------

ordinal_reg(
  data = recovery_data,
  outcome = recovery,
  exposures = c(treatment, sex),
  show_ref = FALSE,
  assumption_test = FALSE
)

ordinal_reg(
  data = recovery_data,
  outcome = recovery,
  exposures = treatment,
  format = gt,
  assumption_test = FALSE
)


## 7. Modify, merge, plot, and forest -----------------------------------------

ordinal_for_paper <- modify_table(
  ordinal_joint,
  caption = "Factors associated with better clinical recovery",
  header_labels = c(estimate = "Common adjusted OR")
)

ordinal_for_paper

ordinal_merged <- merge_tables(
  ordinal_crude,
  ordinal_joint,
  spanners = c("Crude", "Adjusted")
)

ordinal_merged

ordinal_plot <- plot_reg(ordinal_joint)
ordinal_plot

ordinal_forest_data <- forest_df(ordinal_crude, ordinal_joint)
ordinal_forest_data
forest_reg(ordinal_forest_data)


## 8. What to inspect ---------------------------------------------------------

## - The outcome is an ordered factor; order is never guessed silently.
## - The displayed direction is Poor -> Fair -> Good -> Excellent.
## - Reversing the order changes effect direction.
## - Adjustment variables appear in notes but not adjusted rows.
## - Assumption tests identify each tested model term.
## - Downstream outputs preserve the common-OR interpretation.
