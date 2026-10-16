## Manual real-time test: random-intercept multilevel regression
## Package: gtregression 1.2
##
## Story:
## Patients are grouped within clinics, so observations from the same clinic
## may be correlated. We will fit continuous and binary outcome models with
## one clinic-level random intercept.
##
## How to use:
## Run section by section. Inspect fixed effects, cluster statistics, variance
## components, ICC values, convergence messages, and publication notes.


## 0. Setup -------------------------------------------------------------------

## During development: devtools::load_all(".")
## After installation: library(gtregression)

library(gtregression)


## 1. Create clustered clinic data -------------------------------------------

set.seed(1301)
n_clinics <- 30
patients_per_clinic <- 18
n <- n_clinics * patients_per_clinic

clinic_data <- data.frame(
  clinic = factor(rep(seq_len(n_clinics), each = patients_per_clinic)),
  treatment = factor(
    rep(c("Usual care", "Intervention"), length.out = n),
    levels = c("Usual care", "Intervention")
  ),
  age = stats::rnorm(n, 61, 10),
  female = factor(
    stats::rbinom(n, 1, 0.55),
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
)

clinic_effect <- stats::rnorm(n_clinics, 0, 2.5)

clinic_data$quality_score <- 55 +
  2.4 * (clinic_data$treatment == "Intervention") +
  0.12 * clinic_data$age +
  clinic_effect[clinic_data$clinic] +
  stats::rnorm(n, 0, 2)

improvement_lp <- -1.2 +
  0.6 * (clinic_data$treatment == "Intervention") +
  0.012 * clinic_data$age +
  clinic_effect[clinic_data$clinic] / 2

clinic_data$improved <- factor(
  stats::rbinom(n, 1, stats::plogis(improvement_lp)),
  levels = c(0, 1),
  labels = c("No", "Yes")
)

attr(clinic_data$clinic, "label") <- "Treatment clinic"
attr(clinic_data$treatment, "label") <- "Treatment group"
attr(clinic_data$age, "label") <- "Participant age"
attr(clinic_data$female, "label") <- "Female"
attr(clinic_data$quality_score, "label") <- "Quality-of-life score"
attr(clinic_data$improved, "label") <- "Clinical improvement"

head(clinic_data)
table(clinic_data$clinic)
table(clinic_data$improved)
dissect(clinic_data)


## 2. Continuous outcome ------------------------------------------------------

linear_clustered <- multilevel_reg(
  data = clinic_data,
  outcome = quality_score,
  exposures = c(treatment, age),
  group = clinic,
  adjust_for = female,
  approach = linear,
  model_stats = TRUE
)

linear_clustered
linear_clustered$table_body
linear_clustered$models
linear_clustered$model_stats


## 3. Binary outcome ----------------------------------------------------------

logistic_clustered <- multilevel_reg(
  data = clinic_data,
  outcome = improved,
  exposures = c(treatment, age),
  group = clinic,
  adjust_for = female,
  approach = logit,
  model_stats = TRUE
)

logistic_clustered
logistic_clustered$table_body
logistic_clustered$models
logistic_clustered$model_stats


## 4. Minimal and formatted alternatives -------------------------------------

multilevel_reg(
  data = clinic_data,
  outcome = improved,
  exposures = treatment,
  group = clinic,
  approach = logit
)

multilevel_reg(
  data = clinic_data,
  outcome = quality_score,
  exposures = treatment,
  group = clinic,
  approach = linear,
  format = gt
)


## 5. Reference rows ----------------------------------------------------------

multilevel_reg(
  data = clinic_data,
  outcome = improved,
  exposures = c(treatment, female),
  group = clinic,
  approach = logit,
  show_ref = FALSE
)


## 6. Modify, merge, and visualise --------------------------------------------

clustered_for_paper <- modify_table(
  logistic_clustered,
  caption = "Clinical improvement accounting for clinic clustering",
  header_labels = c(estimate = "Adjusted OR")
)

clustered_for_paper

clustered_sensitivity <- multilevel_reg(
  data = clinic_data,
  outcome = improved,
  exposures = treatment,
  group = clinic,
  approach = logit
)

merge_tables(
  clustered_sensitivity,
  logistic_clustered,
  spanners = c("Treatment only", "Adjusted model")
)

clustered_plot <- plot_reg(logistic_clustered)
clustered_plot

clustered_forest_data <- forest_df(logistic_clustered)
clustered_forest_data
forest_reg(clustered_forest_data)


## 7. What to inspect ---------------------------------------------------------

## - The effect label matches linear versus logistic models.
## - The group appears in a note but not as an estimated fixed effect.
## - N and the number of clinics are correct.
## - Random-intercept variance and ICC are populated.
## - Convergence and singular-fit status are visible.
## - This API fits one random intercept, not random slopes or nesting.
