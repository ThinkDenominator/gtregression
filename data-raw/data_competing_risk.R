set.seed(1205)

n <- 500L
treatment <- factor(
  sample(c("Standard", "Intervention"), n, replace = TRUE),
  levels = c("Standard", "Intervention")
)
age <- pmin(85L, pmax(30L, round(stats::rnorm(n, mean = 63, sd = 10))))
sex <- factor(
  sample(c("Female", "Male"), n, replace = TRUE),
  levels = c("Female", "Male")
)
stage <- factor(
  sample(c("I", "II", "III"), n, replace = TRUE,
         prob = c(0.34, 0.41, 0.25)),
  levels = c("I", "II", "III")
)
performance_status <- sample(
  0:2, n, replace = TRUE, prob = c(0.55, 0.34, 0.11)
)

relapse_lp <-
  -0.50 * (treatment == "Intervention") +
  0.018 * (age - 63) +
  c(I = 0, II = 0.35, III = 0.78)[as.character(stage)] +
  0.25 * performance_status

death_lp <-
  0.030 * (age - 63) +
  0.12 * (sex == "Male") +
  c(I = 0, II = 0.20, III = 0.55)[as.character(stage)] +
  0.34 * performance_status

relapse_time <- stats::rexp(n, rate = 0.025 * exp(relapse_lp))
death_time <- stats::rexp(n, rate = 0.017 * exp(death_lp))
censor_time <- stats::runif(n, min = 12, max = 48)

first_time <- pmin(relapse_time, death_time, censor_time)
outcome <- ifelse(
  relapse_time == first_time,
  "Relapse",
  ifelse(death_time == first_time, "Death without relapse", "Censored")
)

data_competing_risk <- data.frame(
  patient_id = sprintf("CR%04d", seq_len(n)),
  follow_up_months = pmax(0.1, round(first_time, 1)),
  outcome = factor(
    outcome,
    levels = c("Censored", "Relapse", "Death without relapse")
  ),
  treatment = treatment,
  age = age,
  sex = sex,
  stage = stage,
  performance_status = performance_status,
  stringsAsFactors = FALSE
)

attr(data_competing_risk$follow_up_months, "label") <-
  "Follow-up time (months)"
attr(data_competing_risk$outcome, "label") <- "First observed outcome"
attr(data_competing_risk$treatment, "label") <- "Treatment group"
attr(data_competing_risk$age, "label") <- "Age (years)"
attr(data_competing_risk$sex, "label") <- "Sex"
attr(data_competing_risk$stage, "label") <- "Disease stage"
attr(data_competing_risk$performance_status, "label") <-
  "Performance status"

usethis::use_data(data_competing_risk, overwrite = TRUE)
