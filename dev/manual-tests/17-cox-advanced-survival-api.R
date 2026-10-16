## Manual API test: advanced Cox workflows and PH plots
## Package: gtregression 1.2.0
##
## Run this file one numbered section at a time. It checks the user-facing
## output added for proportional-hazards plots, delayed entry/start-stop data,
## clustered robust variance, weights, ties, adjusted curves, and merged-table
## borders. No GUI code is exercised here.


## 0. Setup -------------------------------------------------------------------

## During package development, run this first from the package root:
## devtools::load_all(".")
##
## For an installed package, use:
## library(gtregression)

library(gtregression)
library(dplyr)

data("data_lungcancer", package = "gtregression")

lung_api <- data_lungcancer |>
  mutate(
    patient_id = seq_len(n()),
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
    ),
    centre = factor(rep(seq_len(12), length.out = n())),
    analysis_weight = rep(c(1, 1.25, 0.75), length.out = n())
  )

attr(lung_api$trt, "label") <- "Treatment group"
attr(lung_api$celltype, "label") <- "Cancer cell type"
attr(lung_api$karno, "label") <- "Karnofsky performance score"
attr(lung_api$age, "label") <- "Age"
attr(lung_api$prior, "label") <- "Prior therapy"
attr(lung_api$centre, "label") <- "Treatment centre"
attr(lung_api$analysis_weight, "label") <- "Analysis weight"


## 1. Multivariable Cox model -------------------------------------------------

cox_main <- cox_reg(
  data = lung_api,
  time = time,
  event = status,
  exposures = c(trt, celltype, karno, age, prior),
  multivariable = TRUE,
  model_stats = TRUE
)

cox_main
cox_main$model_stats

## Check:
## - The table reports Adjusted HR (95% CI).
## - Reference and comparison levels align beneath each exposure label.
## - Model statistics include N, events, AIC, BIC, and concordance.


## 2. Proportional-hazards table and plots -----------------------------------

ph_main <- check_ph(cox_main)
ph_main

## plot() should use the stored cox.zph diagnostics. This model contains one
## fitted multivariable Cox model, so model selection is not required.
plot(ph_main)

## Draw one term only. The term name must match the cox.zph table.
plot(ph_main, term = "age")

## Inspect the exact diagnostic values when needed.
ph_main_tibble <- check_ph(cox_main, transform = rank, format = tibble)
ph_main_tibble

## Check:
## - The table still prints as the requested publication-ready format.
## - plot(ph_main) opens Schoenfeld residual panels.
## - plot(ph_main, term = "age") shows only age.
## - A small p-value is treated as a screening signal, not automatic proof.


## 3. Multiple Cox models and plot selection ---------------------------------

cox_by_exposure <- cox_reg(
  data = lung_api,
  time = time,
  event = status,
  exposures = c(trt, prior),
  adjust_for = c(age, karno)
)

ph_by_exposure <- check_ph(cox_by_exposure)
ph_by_exposure

## A cox_reg() exposure-by-exposure result contains one fitted model per
## exposure. Select the model by its exposure name.
plot(ph_by_exposure, model = "trt")
plot(ph_by_exposure, model = "prior", term = "karno")


## 4. Delayed entry -----------------------------------------------------------

## Create a valid demonstration entry time in the same unit as follow-up.
## Every participant enters before their observed exit time.
lung_delayed <- lung_api |>
  mutate(entry = pmin(30, time / 2))

attr(lung_delayed$entry, "label") <- "Study entry time"

cox_delayed <- cox_reg(
  data = lung_delayed,
  time_start = entry,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  multivariable = TRUE
)

cox_delayed
cox_delayed$time_start

## Check:
## - The model fits Surv(entry, time, status).
## - The footnote identifies the entry-time variable.
## - The adjusted HR table remains publication ready.


## 5. Start-stop records and clustered variance ------------------------------

## Split each participant into two intervals. Only the final interval carries
## the event. patient_id identifies the two records from the same participant.
first_interval <- lung_api |>
  transmute(
    across(everything()),
    start = 0,
    stop = time / 2,
    interval_event = 0L
  )

second_interval <- lung_api |>
  transmute(
    across(everything()),
    start = time / 2,
    stop = time,
    interval_event = status
  )

lung_start_stop <- bind_rows(first_interval, second_interval) |>
  arrange(patient_id, start)

cox_start_stop <- cox_reg(
  data = lung_start_stop,
  time_start = start,
  time = stop,
  event = interval_event,
  exposures = c(trt, age, karno),
  multivariable = TRUE,
  cluster = patient_id
)

cox_start_stop
summary(cox_start_stop$models[[1]])$coefficients

## Check:
## - The model summary contains a "robust se" column.
## - The table confidence intervals use that robust standard error.
## - The footnote identifies patient_id as the clustering variable.


## 6. Case weights, robust variance, ties, and control ------------------------

cox_weighted <- cox_reg(
  data = lung_api,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  multivariable = TRUE,
  weights = analysis_weight,
  robust = TRUE,
  ties = breslow,
  control = list(iter.max = 30)
)

cox_weighted
cox_weighted$ties
cox_weighted$control
summary(cox_weighted$models[[1]])$coefficients

## Check:
## - The table notes the case-weight variable and Breslow ties method.
## - Robust standard errors appear in the model summary.
## - The control list records iter.max = 30.


## 7. Baseline-hazard strata versus clustering -------------------------------

cox_baseline_strata <- cox_reg(
  data = lung_api,
  time = time,
  event = status,
  exposures = c(trt, age, karno),
  multivariable = TRUE,
  baseline_strata = celltype
)

cox_baseline_strata

## Check:
## - Cancer cell type has no reported HR.
## - The footnote states that baseline hazards are stratified by cell type.
## - This is conceptually different from cluster = centre, which changes the
##   variance estimate rather than the baseline hazard.


## 8. Adjusted Cox survival curves -------------------------------------------

cox_adjusted_curves <- plot_cox_survival(
  cox_main,
  by = trt,
  break_time_by = 200,
  xlab = "Days after randomization",
  legend_title = "Treatment group"
)

cox_adjusted_curves
attr(cox_adjusted_curves, "prediction_data")

## Check:
## - One curve appears for each treatment group.
## - Other covariates are held at representative values shown above.
## - Confidence bands remain within the 0 to 1 probability scale.


## 9. Curves at explicitly chosen clinical profiles --------------------------

profile_data <- data.frame(
  trt = factor(
    c("Standard treatment", "Test treatment"),
    levels = levels(lung_api$trt)
  ),
  celltype = factor(
    c("Squamous", "Squamous"),
    levels = levels(lung_api$celltype)
  ),
  karno = c(70, 70),
  age = c(60, 60),
  prior = factor(c("No", "No"), levels = levels(lung_api$prior))
)
rownames(profile_data) <- c("Standard profile", "Test profile")

plot_cox_survival(
  cox_main,
  newdata = profile_data,
  break_time_by = 200,
  title = "Adjusted survival at prespecified clinical profiles"
)

## Check that the legend uses the two row names above.


## 10. Merged Cox tables and publication rules -------------------------------

cox_crude <- cox_reg(
  data = lung_api,
  time = time,
  event = status,
  exposures = c(trt, celltype, karno, age, prior)
)

cox_merged <- merge_tables(
  cox_crude,
  cox_main,
  spanners = c("Crude", "Adjusted")
)

cox_merged

## Check:
## - A clear rule appears above and below the two-row header.
## - A bottom rule appears after the final body row.
## - No horizontal rule appears after only the first exposure.
## - Reference and comparison levels remain aligned.


## 11. Expected validation messages ------------------------------------------

## These calls should return readable errors without opening Browse mode.
try(
  cox_reg(
    lung_delayed,
    time_start = time,
    time = time,
    event = status,
    exposures = age
  )
)

try(
  cox_reg(
    lung_api,
    time = time,
    event = status,
    exposures = age,
    cluster = centre,
    robust = FALSE
  )
)

try(plot(ph_by_exposure))

## Expected messages mention:
## - time_start must be less than time;
## - clustered models require robust variance;
## - a model must be selected when check_ph() contains several models.


message("Advanced Cox and survival API manual checks completed.")
