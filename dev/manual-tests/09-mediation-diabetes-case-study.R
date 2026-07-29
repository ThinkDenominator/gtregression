# gtregression 1.1 manual test script
# Causal mediation case study: obesity, glucose, and diabetes
#
# Story:
# In a diabetes screening dataset, obesity is associated with diabetes risk.
# A clinically plausible question is whether part of that association may pass
# through plasma glucose. This script tests the mediation workflow in a way that
# is useful for teaching, checking, and package release review.
#
# Important:
# Mediation is not automatic causal proof. Use DAGs, temporal ordering, study
# design, and subject-matter knowledge before interpreting these estimates
# causally.

library(gtregression)

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)


# 1. Load and inspect the teaching dataset ------------------------------------

data("data_diabetes_mediation", package = "gtregression")

head(data_diabetes_mediation)
str(data_diabetes_mediation)

table(data_diabetes_mediation$obesity, data_diabetes_mediation$diabetes)


# 2. Minimal logistic mediation analysis --------------------------------------

# Exposure: obesity
# Mediator: plasma glucose
# Outcome: diabetes
# Covariates: age, blood pressure, pregnancies, diabetes pedigree function
#
# For logistic outcomes, mediation effects are shown as predicted probability
# differences, not odds ratios.

diabetes_med <- mediation_analysis(
  data = data_diabetes_mediation,
  exposure = obesity,
  mediator = glucose,
  outcome = diabetes,
  covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
  outcome_approach = logit,
  sims = 500,
  seed = 123
)

diabetes_med
diabetes_med$table


# 3. Inspect the object --------------------------------------------------------

# The displayed table is formatted for reporting, but the object remains transparent.

diabetes_med$table_body
diabetes_med$values
diabetes_med$models$mediator
diabetes_med$models$outcome
head(diabetes_med$boot)


# 4. Plot the mediation pathway -----------------------------------------------

# The path diagram is useful for teaching and presentation slides.

plot_mediation(diabetes_med)

plot_mediation(diabetes_med, show_estimates = FALSE)


# 5. gt output ----------------------------------------------------------------

# flextable is the default for Word-friendly workflows. gt remains available
# for HTML/pkgdown-style output.

med_gt <- mediation_analysis(
  data = data_diabetes_mediation,
  exposure = obesity,
  mediator = glucose,
  outcome = diabetes,
  covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
  outcome_approach = logit,
  format = gt,
  sims = 300,
  seed = 123
)

med_gt$table


# 6. Quoted names --------------------------------------------------------------

# Quoted names are useful inside scripts, functions, Shiny apps, and stored
# workflows.

exposure_var <- "obesity"
mediator_var <- "glucose"
outcome_var <- "diabetes"
covariate_vars <- c("age", "blood_pressure", "pregnancies", "diabetes_pedigree")

med_quoted <- mediation_analysis(
  data = data_diabetes_mediation,
  exposure = exposure_var,
  mediator = mediator_var,
  outcome = outcome_var,
  covariates = covariate_vars,
  outcome_approach = "logit",
  sims = 300,
  seed = 456
)

med_quoted$table


# 7. Linear-outcome example ----------------------------------------------------

# This is mainly to test that continuous outcomes work too. Here BMI is used as
# a continuous outcome for demonstration, with obesity as the exposure and
# glucose as the mediator.

med_linear <- mediation_analysis(
  data = data_diabetes_mediation,
  exposure = obesity,
  mediator = glucose,
  outcome = bmi,
  covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
  outcome_approach = linear,
  sims = 300,
  seed = 789
)

med_linear$table
plot_mediation(med_linear)


# 8. What to tell users --------------------------------------------------------

# Total effect:
#   Overall exposure-outcome association on the chosen scale.
#
# Direct effect:
#   Estimated association not operating through the mediator.
#
# Indirect effect:
#   Estimated association operating through the mediator.
#
# Proportion mediated:
#   Indirect effect divided by total effect. This is unstable when the total
#   effect is very small.
#
# Causal interpretation requires:
#   - correct exposure -> mediator -> outcome timing
#   - no unmeasured exposure-outcome confounding
#   - no unmeasured exposure-mediator confounding
#   - no unmeasured mediator-outcome confounding
#   - appropriate model specification
