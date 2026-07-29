test_that("mediation_analysis returns formatted logit mediation output", {
  med <- mediation_analysis(
    data = data_diabetes_mediation,
    exposure = obesity,
    mediator = glucose,
    outcome = diabetes,
    covariates = c(age, blood_pressure, pregnancies, diabetes_pedigree),
    outcome_approach = logit,
    sims = 30,
    seed = 123
  )

  expect_s3_class(med, "gtregression")
  expect_s3_class(med, "mediation_analysis")
  expect_s3_class(med$table, "flextable")
  expect_named(med$table_body, c(
    "effect", "Effect", "estimate", "conf.low", "conf.high", "p.value",
    "Interpretation"
  ))
  expect_equal(med$table_body$effect, c("total", "direct", "indirect", "proportion"))
  expect_true(all(is.finite(med$table_body$estimate)))
  expect_s3_class(med$models$mediator, "lm")
  expect_s3_class(med$models$outcome, "glm")
  expect_equal(med$values$reference_value, "No")
  expect_equal(med$values$exposure_value, "Yes")
  expect_equal(med$variable_labels[["obesity"]], "Obesity")
  expect_equal(med$variable_labels[["glucose"]], "Plasma glucose")
})

test_that("mediation_analysis accepts quoted names and gt output", {
  med <- mediation_analysis(
    data = data_diabetes_mediation,
    exposure = "obesity",
    mediator = "glucose",
    outcome = "diabetes",
    covariates = c("age", "blood_pressure"),
    outcome_approach = "logit",
    format = "gt",
    sims = 25,
    seed = 321
  )

  expect_s3_class(med$table, "gt_tbl")
  expect_equal(med$covariates, c("age", "blood_pressure"))
})

test_that("mediation_analysis supports a linear outcome", {
  med <- mediation_analysis(
    data = data_diabetes_mediation,
    exposure = obesity,
    mediator = glucose,
    outcome = bmi,
    covariates = c(age, blood_pressure),
    outcome_approach = linear,
    sims = 25,
    seed = 456
  )

  expect_s3_class(med$models$outcome, "lm")
  expect_equal(med$outcome_approach, "linear")
  expect_true(all(is.finite(med$table_body$estimate[1:3])))
})

test_that("mediation_analysis validates inputs", {
  expect_error(
    mediation_analysis(
      data = data_diabetes_mediation,
      exposure = obesity,
      mediator = diabetes,
      outcome = glucose,
      sims = 20
    ),
    "`mediator` must be numeric",
    fixed = TRUE
  )

  expect_error(
    mediation_analysis(
      data = data_diabetes_mediation,
      exposure = obesity,
      mediator = glucose,
      outcome = bmi,
      outcome_approach = logit,
      sims = 20
    ),
    "`outcome` must have exactly two non-missing values",
    fixed = TRUE
  )

  expect_error(
    mediation_analysis(
      data = data_diabetes_mediation,
      exposure = obesity,
      mediator = glucose,
      outcome = diabetes,
      outcome_approach = logit,
      sims = 10
    ),
    "`sims` must be a single number greater than or equal to 20",
    fixed = TRUE
  )
})
