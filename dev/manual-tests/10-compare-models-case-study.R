## Manual real-time test: compare_models() case study
## Package: gtregression 1.1
##
## Story:
## A clinical researcher has already fitted a few candidate gtregression models
## based on clinical reasoning and wants one clean table to compare them. This
## is not stepwise selection. The candidate models are fitted first with multi_reg(),
## cox_reg(), or surv_reg(), then compare_models() summarises AIC, BIC,
## log-likelihood, likelihood-ratio tests, sample size, and the change in a
## primary exposure estimate.
##
## How to use:
## Run this script section by section. Inspect the table output and then inspect
## $table_body to confirm the exact values used behind the publication table.


## 0. Setup -------------------------------------------------------------------

## During package development, run from the package root:
## devtools::load_all(".")
##
## After installing from GitHub or CRAN, use:
## library(gtregression)

library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")
data("data_lungcancer", package = "gtregression")


## 1. Prepare datasets --------------------------------------------------------

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
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

lung_data <- data_lungcancer |>
  mutate(
    trt = factor(
      trt,
      levels = c(1, 2),
      labels = c("Standard treatment", "Test treatment")
    ),
    prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes")),
    celltype = factor(
      celltype,
      levels = c("squamous", "smallcell", "adeno", "large"),
      labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
    )
  )


## 2. Logistic regression model comparison -----------------------------------

## Question:
## Does the model fit improve when we move from smoking alone to a clinically
## adjusted low-birth-weight model?

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
    "Add age and weight",
    "Full clinical model"
  ),
  primary_exposure = smoke
)

logit_compare
logit_compare$table_body

## A formatted gt table should also render.
compare_models(
  list(
    "Smoking only" = logit_m0,
    "Add age and weight" = logit_m1,
    "Full clinical model" = logit_m2
  ),
  primary_exposure = "smoke",
  format = gt
)


## 3. Linear regression model comparison -------------------------------------

## Question:
## For birth weight as a continuous outcome, does adding clinical risk factors
## improve fit beyond maternal age and weight?

lm_m0 <- multi_reg(
  data = birthwt_data,
  outcome = bwt,
  exposures = c(age, lwt),
  approach = linear
)

lm_m1 <- multi_reg(
  data = birthwt_data,
  outcome = bwt,
  exposures = c(age, lwt, smoke),
  approach = linear
)

lm_m2 <- multi_reg(
  data = birthwt_data,
  outcome = bwt,
  exposures = c(age, lwt, smoke, race, ht, ui),
  approach = linear
)

lm_compare <- compare_models(
  lm_m0,
  lm_m1,
  lm_m2,
  model_names = c(
    "Age and weight",
    "Add smoking",
    "Full clinical model"
  ),
  primary_exposure = smoke
)

lm_compare
lm_compare$table_body

## If models are not nested, set nested = FALSE so LR test columns are not used.
compare_models(
  list(
    "Age and weight" = lm_m0,
    "Smoking model" = lm_m1,
    "Clinical model" = lm_m2
  ),
  nested = FALSE,
  primary_exposure = "smoke",
  format = flextable
)


## 4. Cox regression model comparison ----------------------------------------

## Question:
## In lung cancer survival, does adding age, performance score, cell type, and
## prior therapy improve the treatment model?

cox_m0 <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt
)

cox_m1 <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno)
)

cox_m2 <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno, celltype, prior),
  multivariable = TRUE
)

cox_compare <- compare_models(
  cox_m0,
  cox_m1,
  cox_m2,
  model_names = c(
    "Treatment only",
    "Add age and performance",
    "Full clinical model"
  ),
  primary_exposure = trt
)

cox_compare
cox_compare$table_body

## Cox-specific checks:
## These should agree with the fitted model stored inside the cox_reg() object.
cox_compare$table_body$n[1]
cox_m0$models[[1]]$n

cox_compare$table_body$events[1]
cox_m0$models[[1]]$nevent

cox_compare$table_body$AIC[1]
stats::AIC(cox_m0$models[[1]])


## 5. Parametric survival model comparison -----------------------------------

## Question:
## For a Weibull accelerated failure time model, what happens when we add the
## same clinical predictors?

aft_m0 <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  distribution = weibull
)

aft_m1 <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno),
  distribution = weibull
)

aft_m2 <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, age, karno, celltype, prior),
  multivariable = TRUE,
  distribution = weibull
)

aft_compare <- compare_models(
  aft_m0,
  aft_m1,
  aft_m2,
  model_names = c(
    "Treatment only",
    "Add age and performance",
    "Full clinical model"
  ),
  primary_exposure = trt
)

aft_compare
aft_compare$table_body


## 6. Final checklist ---------------------------------------------------------

## Things to confirm manually:
## - compare_models() accepts separate gtregression model objects.
## - compare_models() accepts a named list of gtregression model objects.
## - format = flextable is publication ready by default.
## - format = gt renders for HTML/pkgdown-style viewing.
## - $table_body exposes exact values for checking.
## - multi_reg(), cox_reg(), and surv_reg() outputs work directly.
## - Logistic and linear models report N, parameters, AIC, BIC, and logLik.
## - Cox models additionally report events and concordance.
## - Best AIC and Best BIC identify better-fitting candidate models.
## - primary_exposure reports the selected estimate and percentage change.
## - nested = FALSE suppresses likelihood-ratio comparison columns.
## - The function compares models stored inside gtregression objects; it should not refit them.
