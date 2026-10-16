# A variable cannot take two roles in one analysis; each function says so.

test_that("survival functions refuse time or event as a predictor or group", {
  skip_if_not_installed("survival")
  lung <- data_lungcancer
  expect_error(cox_reg(lung, time = "time", event = "status", exposures = c("time", "age")),
               "time and event variables cannot also be exposures")
  expect_error(cox_reg(lung, time = "time", event = "status", exposures = "age", adjust_for = "status"),
               "time and event variables cannot also be exposures")
  expect_error(surv_reg(lung, time = "time", event = "status", exposures = "time"),
               "time and event variables cannot also be exposures")
  expect_error(km_plot(lung, time = "time", event = "status", by = "status"),
               "separate grouping variable")
  expect_error(survival_summary(lung, time = "time", event = "status", by = "time"),
               "separate grouping variable")
  expect_error(logrank_test(lung, time = "time", event = "status", by = "status"),
               "separate grouping variable")
})

test_that("assessment tools refuse the outcome as a predictor", {
  b <- data_birthwt
  b$race <- factor(b$race)
  b$smoke <- factor(b$smoke)
  expect_error(identify_confounder(b, outcome = "low", exposure = "smoke", potential_confounder = "low",
                                   approach = "logit"),
               "outcome variable cannot also be")
  expect_error(interaction_models(b, outcome = "low", exposure = "smoke", effect_modifier = "smoke",
                                  approach = "logit"),
               "different variable from `exposure`")
  expect_error(interaction_models(b, outcome = "low", exposure = "smoke", effect_modifier = "low",
                                  approach = "logit"),
               "outcome variable cannot also be")
  expect_error(interaction_models(b, outcome = "low", exposure = "smoke", effect_modifier = "race",
                                  covariates = c("age", "race"), approach = "logit"),
               "must not include the exposure or the effect modifier")
  expect_error(select_models(b, outcome = "low", exposures = c("low", "age"), approach = "logit"),
               "outcome variable cannot also be a candidate")
})

test_that("mediation refuses covariates that repeat another role", {
  expect_error(
    mediation_analysis(data_diabetes_mediation, exposure = "obesity", mediator = "glucose",
                       outcome = "diabetes", covariates = c("age", "obesity"),
                       outcome_approach = "logit", sims = 20),
    "must not include the exposure, mediator or outcome: obesity"
  )
})

test_that("descriptive_table() leaves the by variable out of the summary", {
  b <- data_birthwt
  b$low <- factor(b$low)
  b$smoke <- factor(b$smoke)
  expect_message(res <- descriptive_table(b, exposures = c("smoke", "low"), by = "low"),
                 "left out of `exposures`")
  same <- suppressMessages(descriptive_table(b, exposures = "smoke", by = "low"))
  expect_identical(res$table_display, same$table_display)
  expect_error(suppressMessages(descriptive_table(b, exposures = "low", by = "low")),
               "other than `by`")
})
