test_that("data_diabetes_mediation is available and suitable for mediation examples", {
  expect_s3_class(data_diabetes_mediation, "data.frame")
  expect_named(data_diabetes_mediation, c(
    "diabetes", "obesity", "glucose", "bmi", "age", "blood_pressure",
    "pregnancies", "diabetes_pedigree"
  ))
  expect_gt(nrow(data_diabetes_mediation), 500)
  expect_equal(levels(data_diabetes_mediation$diabetes), c("No", "Yes"))
  expect_equal(levels(data_diabetes_mediation$obesity), c("No", "Yes"))
  expect_true(is.numeric(data_diabetes_mediation$glucose))
  expect_true(is.numeric(data_diabetes_mediation$bmi))
  expect_false(anyNA(data_diabetes_mediation))
  expect_equal(attr(data_diabetes_mediation$glucose, "label"), "Plasma glucose")
})
