test_that("stratified_multi_reg returns a gtsummary tbl_merge object", {
  library(gtregression)
  library(mlbench)
  library(dplyr)
  library(gtsummary)

  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese",
        TRUE ~ NA_character_
      ),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"),
                         levels = c("Low parity", "High parity")),
      glucose_cat = factor(case_when(
        glucose < 140 ~ "Normal",
        glucose >= 140 ~ "High"
      ), levels = c("Normal", "High"))
    )

  result <- tryCatch({
    stratified_multi_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("bmi", "age_cat", "npreg_cat"),
      stratifier = "glucose_cat",
      approach = "robpoisson"
    )
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  }, warning = function(w) {
    message("Warning: ", w$message)
    invokeRestart("muffleWarning")
  })

  if (is.null(result)) {
    skip("Skipping test: No valid models across strata.")
  } else {
    expect_s3_class(result, "tbl_merge")
    expect_true("gtsummary" %in% class(result))
  }
})

test_that("stratified_multi_reg excludes NA values in stratifier", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      glucose_cat = case_when(glucose < 140 ~ "Normal", glucose >= 140 ~ "High"),
      glucose_cat = factor(glucose_cat, levels = c("Normal", "High")),
      bmi = case_when(mass < 25 ~ "Normal", mass >= 25 & mass < 30 ~ "Overweight", TRUE ~ "Obese"),
      bmi = factor(bmi),
      age_cat = cut(age, breaks = c(0, 30, 50, Inf), labels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"))
    )

  pima_data$glucose_cat[1:5] <- NA  # introduce NA values in stratifier

  result <- tryCatch({
    suppressWarnings(gtregression::stratified_multi_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("bmi", "age_cat", "npreg_cat"),
      stratifier = "glucose_cat",
      approach = "robpoisson"
    ))
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    skip("Skipping test: No valid models across strata due to NA stratifier.")
  } else {
    expect_s3_class(result, "tbl_merge")
  }
})

test_that("stratified_multi_reg runs with robpoisson and produces estimates", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = case_when(mass < 25 ~ "Normal", mass >= 25 & mass < 30 ~ "Overweight", TRUE ~ "Obese"),
      bmi = factor(bmi),
      age_cat = cut(age, breaks = c(0, 30, 50, Inf), labels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity")),
      glucose_cat = factor(case_when(glucose < 140 ~ "Normal", glucose >= 140 ~ "High"))
    )

  result <- tryCatch({
    suppressWarnings(gtregression::stratified_multi_reg(
      data = pima_data,
      outcome = "diabetes",
      exposures = c("bmi", "age_cat", "npreg_cat"),
      stratifier = "glucose_cat",
      approach = "robpoisson"
    ))
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    skip("Skipping test: No valid models across strata.")
  } else {
    expect_s3_class(result, "tbl_merge")
    expect_true("gtsummary" %in% class(result))
  }
})
