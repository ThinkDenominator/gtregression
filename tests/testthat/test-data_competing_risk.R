test_that("data_competing_risk is suitable for competing-risk examples", {
  expect_s3_class(data_competing_risk, "data.frame")
  expect_equal(nrow(data_competing_risk), 500L)
  expect_named(data_competing_risk, c(
    "patient_id", "follow_up_months", "outcome", "treatment", "age",
    "sex", "stage", "performance_status"
  ))
  expect_false(anyNA(data_competing_risk))
  expect_true(all(data_competing_risk$follow_up_months > 0))
  expect_equal(
    levels(data_competing_risk$outcome),
    c("Censored", "Relapse", "Death without relapse")
  )
  expect_equal(levels(data_competing_risk$treatment),
               c("Standard", "Intervention"))
  expect_gt(sum(data_competing_risk$outcome == "Relapse"), 50L)
  expect_gt(sum(data_competing_risk$outcome == "Death without relapse"), 50L)
  expect_gt(sum(data_competing_risk$outcome == "Censored"), 50L)
  expect_equal(
    attr(data_competing_risk$follow_up_months, "label"),
    "Follow-up time (months)"
  )
})

test_that("data_competing_risk works in the documented core workflows", {
  skip_if_not_installed("cmprsk")

  cif <- cuminc_plot(
    data_competing_risk,
    time = follow_up_months,
    event = outcome,
    event_of_interest = Relapse,
    censor_code = Censored,
    by = treatment
  )
  model <- competing_risk_reg(
    data_competing_risk,
    time = follow_up_months,
    event = outcome,
    event_of_interest = Relapse,
    censor_code = Censored,
    exposures = treatment,
    adjust_for = c(age, stage)
  )

  expect_s3_class(cif, "ggplot")
  expect_s3_class(model, "competing_risk_reg")
  expect_gt(nrow(model$table_body), 0L)
})
