## Manual real-time test: multinomial logistic regression
## Package: gtregression 1.2
##
## Story:
## Discharge destination has three unordered categories: Home,
## Rehabilitation, and Hospital. We want separate odds ratios for each
## non-reference destination compared with Home.
##
## How to use:
## Run section by section. Confirm the reference outcome before interpreting
## any odds ratio.


## 0. Setup -------------------------------------------------------------------

## During development: devtools::load_all(".")
## After installation: library(gtregression)

library(gtregression)


## 1. Create a nominal outcome ------------------------------------------------

set.seed(1501)
n <- 700

destination_data <- data.frame(
  treatment = factor(
    sample(c("Usual care", "Intervention"), n, replace = TRUE),
    levels = c("Usual care", "Intervention")
  ),
  age = stats::rnorm(n, 66, 12),
  frailty = factor(
    sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.65, 0.35)),
    levels = c("No", "Yes")
  )
)

rehabilitation_lp <-
  -0.3 +
  0.5 * (destination_data$treatment == "Intervention") +
  0.018 * destination_data$age +
  0.7 * (destination_data$frailty == "Yes")

hospital_lp <-
  -0.8 -
  0.2 * (destination_data$treatment == "Intervention") +
  0.025 * destination_data$age +
  1.0 * (destination_data$frailty == "Yes")

denominator <- 1 + exp(rehabilitation_lp) + exp(hospital_lp)
random_draw <- stats::runif(n)

destination_data$destination <- factor(
  ifelse(
    random_draw < 1 / denominator,
    "Home",
    ifelse(
      random_draw < (1 + exp(rehabilitation_lp)) / denominator,
      "Rehabilitation",
      "Hospital"
    )
  ),
  levels = c("Home", "Rehabilitation", "Hospital")
)

attr(destination_data$treatment, "label") <- "Treatment group"
attr(destination_data$age, "label") <- "Participant age"
attr(destination_data$frailty, "label") <- "Clinical frailty"
attr(destination_data$destination, "label") <- "Discharge destination"

head(destination_data)
table(destination_data$destination)
levels(destination_data$destination)
dissect(destination_data)


## 2. Crude multinomial models ------------------------------------------------

destination_crude <- multinomial_reg(
  data = destination_data,
  outcome = destination,
  exposures = c(treatment, age, frailty),
  model_stats = TRUE
)

destination_crude
destination_crude$table_body
destination_crude$models
destination_crude$model_stats

## Each column compares Rehabilitation or Hospital with Home.


## 3. Exposure-specific adjusted models --------------------------------------

destination_adjusted <- multinomial_reg(
  data = destination_data,
  outcome = destination,
  exposures = c(treatment, age),
  adjust_for = frailty,
  model_stats = TRUE
)

destination_adjusted
destination_adjusted$model_stats


## 4. One joint multivariable model ------------------------------------------

destination_joint <- multinomial_reg(
  data = destination_data,
  outcome = destination,
  exposures = c(treatment, age, frailty),
  multivariable = TRUE,
  model_stats = TRUE
)

destination_joint
destination_joint$table_body
destination_joint$model_stats
names(destination_joint$per_outcome)


## 5. Inspect one outcome comparison -----------------------------------------

rehabilitation_result <-
  destination_joint$per_outcome[["Rehabilitation"]]
hospital_result <- destination_joint$per_outcome[["Hospital"]]

rehabilitation_result
hospital_result

plot_reg(rehabilitation_result)

rehabilitation_forest_data <- forest_df(rehabilitation_result)
rehabilitation_forest_data
forest_reg(rehabilitation_forest_data)


## 6. Change the reference outcome -------------------------------------------

hospital_reference_data <- destination_data
hospital_reference_data$destination <- stats::relevel(
  hospital_reference_data$destination,
  ref = "Hospital"
)

levels(hospital_reference_data$destination)

hospital_reference <- multinomial_reg(
  data = hospital_reference_data,
  outcome = destination,
  exposures = treatment,
  model_stats = TRUE
)

hospital_reference
names(hospital_reference$per_outcome)


## 7. Reference rows and gt output -------------------------------------------

multinomial_reg(
  data = destination_data,
  outcome = destination,
  exposures = c(treatment, frailty),
  show_ref = FALSE
)

multinomial_reg(
  data = destination_data,
  outcome = destination,
  exposures = treatment,
  format = gt
)


## 8. Modify the publication table -------------------------------------------

destination_for_paper <- modify_table(
  destination_joint,
  caption = "Factors associated with discharge destination",
  caveat = "Home was the reference outcome category."
)

destination_for_paper


## 9. What to inspect ---------------------------------------------------------

## - Home is clearly identified as the original reference outcome.
## - Separate columns compare Rehabilitation vs Home and Hospital vs Home.
## - Categories are nominal; their order has no severity interpretation.
## - Releveling to Hospital changes the comparisons reproducibly.
## - $per_outcome components support focused plots and forest tables.
## - Predictor reference rows respond to show_ref independently of the
##   reference outcome category.
