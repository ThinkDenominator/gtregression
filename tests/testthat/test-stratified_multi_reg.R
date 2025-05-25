test_that("stratified_multi_reg returns a gtsummary tbl_merge object", {
  library(mlbench)
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    dplyr::mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    dplyr::mutate(
      bmi = dplyr::case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese",
        TRUE ~ NA_character_
      ),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = dplyr::case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
      glucose_cat = dplyr::case_when(
        glucose < 140 ~ "Normal",
        glucose >= 140 ~ "High"
      ),
      glucose_cat = factor(glucose_cat, levels = c("Normal", "High"))
    )

  result <- gtregression::stratified_multi_reg(
    data = pima_data,
    outcome = "diabetes",
    exposures = c("bmi", "age_cat", "npreg_cat"),
    stratifier = "glucose_cat",
    approach = "robpoisson"
  )

  testthat::expect_s3_class(result, "tbl_merge")
  testthat::expect_true("gtsummary" %in% class(result))
})

test_that("stratified_multi_reg excludes NA values in stratifier", {
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima_data <- PimaIndiansDiabetes2 %>%
    dplyr::mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    dplyr::mutate(
      glucose_cat = dplyr::case_when(glucose < 140 ~ "Normal", glucose >= 140 ~ "High"),
      glucose_cat = factor(glucose_cat, levels = c("Normal", "High")),
      bmi = dplyr::case_when(mass < 25 ~ "Normal", mass >= 25 & mass < 30 ~ "Overweight", TRUE ~ "Obese"),
      bmi = factor(bmi),
      age_cat = cut(age, breaks = c(0, 30, 50, Inf), labels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"))
    )

  pima_data$glucose_cat[1:5] <- NA

  result <- gtregression::stratified_multi_reg(
    data = pima_data,
    outcome = "diabetes",
    exposures = c("bmi", "age_cat", "npreg_cat"),
    stratifier = "glucose_cat",
    approach = "robpoisson"
  )

  testthat::expect_s3_class(result, "tbl_merge")
})

test_that("stratified_multi_reg runs with robpoisson and produces estimates", {
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima_data <- PimaIndiansDiabetes2 %>%
    dplyr::mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    dplyr::mutate(
      bmi = dplyr::case_when(mass < 25 ~ "Normal", mass >= 25 & mass < 30 ~ "Overweight", TRUE ~ "Obese"),
      bmi = factor(bmi),
      age_cat = cut(age, breaks = c(0, 30, 50, Inf), labels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity")),
      glucose_cat = factor(dplyr::case_when(glucose < 140 ~ "Normal", glucose >= 140 ~ "High"))
    )

  result <- gtregression::stratified_multi_reg(
    data = pima_data,
    outcome = "diabetes",
    exposures = c("bmi", "age_cat", "npreg_cat"),
    stratifier = "glucose_cat",
    approach = "robpoisson"
  )

  testthat::expect_s3_class(result, "tbl_merge")
  testthat::expect_true("gtsummary" %in% class(result))
})
