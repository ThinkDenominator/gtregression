test_that("stratified_uni_reg returns a gtsummary tbl_merge object with logit", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |>
    mutate(
      bmi = factor(case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese"
      ), levels = c("Normal", "Overweight", "Obese")),
      age_cat = factor(case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ), levels = c("Young", "Middle-aged", "Older")),
      glucose_cat = factor(case_when(
        glucose < 140 ~ "Normal",
        glucose >= 140 ~ "High"
      ), levels = c("Normal", "High"))
    )

  result <- suppressWarnings(
    stratified_uni_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("age_cat", "bmi"),
      stratifier = "glucose_cat",
      approach = "logit"
    )
  )

  expect_s3_class(result, "tbl_merge")
  expect_true("gtsummary" %in% class(result))
})


test_that("stratified_uni_reg excludes NA values in stratifier", {
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima_data <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      age_cat = factor(ifelse(age < 50, "Under 50", "50+")),
      glucose_cat = factor(ifelse(glucose >= 140, "High", "Normal")),
      bmi = factor(ifelse(mass >= 30, "Obese", "Not obese"))
    )

  pima_data$glucose_cat[1:5] <- NA # introduce missing

  result <- suppressWarnings(
    stratified_uni_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("age_cat", "bmi"),
      stratifier = "glucose_cat",
      approach = "logit"
    )
  )

  expect_s3_class(result, "tbl_merge")
})


test_that("stratified_uni_reg errors for invalid inputs", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      glucose_cat = factor(ifelse(glucose >= 140, "High", "Normal"))
    )

  expect_error(
    stratified_uni_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("invalid_var"),
      stratifier = "glucose_cat",
      approach = "logit"
    ),
    "One or more exposure variables not found in dataset."
  )

  expect_error(
    stratified_uni_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("age"),
      stratifier = "not_in_data",
      approach = "logit"
    ),
    "Stratifier not found in dataset."
  )
})


test_that("stratified_uni_reg works with robpoisson", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      age_cat = factor(ifelse(age < 50, "Under 50", "50+")),
      glucose_cat = factor(ifelse(glucose >= 140, "High", "Normal")),
      bmi = factor(ifelse(mass >= 30, "Obese", "Not obese"))
    )

  result <- suppressWarnings(
    stratified_uni_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("age_cat", "bmi"),
      stratifier = "glucose_cat",
      approach = "robpoisson"
    )
  )

  expect_s3_class(result, "tbl_merge")
})


test_that("stratified_uni_reg errors when no valid strata exist", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      glucose_cat = NA # All NA in stratifier
    )

  expect_error(
    stratified_uni_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("age", "mass"),
      stratifier = "glucose_cat",
      approach = "logit"
    ),
    regexp = "No valid models across strata."
  )
})
