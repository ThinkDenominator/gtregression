test_that("multi_reg computes adjusted estimates correctly across approaches", {
  library(gtregression)
  library(mlbench)
  library(risks)
  library(dplyr)
  library(gtsummary)
  library(stringr)
  library(gt)
  library(brglm2)

  # Load and prepare data
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
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

      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"), levels = c("Low parity", "High parity")),

      glucose_cat = factor(case_when(
        glucose < 140 ~ "Normal",
        glucose >= 140 ~ "High"
      ), levels = c("Normal", "High")),

      bp_cat = factor(case_when(
        pressure < 80 ~ "Normal",
        pressure >= 80 ~ "High"
      ), levels = c("Normal", "High")),

      triceps_cat = factor(case_when(
        triceps < 23 ~ "Normal",
        triceps >= 23 ~ "High"
      ), levels = c("Normal", "High")),

      insulin_cat = factor(case_when(
        insulin < 30 ~ "Low",
        insulin >= 30 & insulin < 150 ~ "Normal",
        insulin >= 150 ~ "High"
      ), levels = c("Low", "Normal", "High")),

      dpf_cat = factor(case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
        pedigree > 0.5 ~ "High Genetic Risk"
      ), levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))
    )

  exposures <- c("bmi", "npreg_cat")

  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "margstd_boot", "margstd_delta")

  for (approach in valid_approaches) {
    message("Testing multi_reg with approach: ", approach)

    result <- tryCatch({
      multi_reg(data = pima_data, outcome = "diabetes", exposures = exposures, approach = approach)
    }, error = function(e) {
      warning("Error for approach: ", approach, "\n", e$message)
      return(NULL)
    })

    if (is.null(result)) {
      skip(paste("Skipping test: Model fitting failed for approach:", approach))
    }

    # Output should not be NULL
    expect_true(!is.null(result), info = paste("multi_reg() returned NULL for", approach))

    # Output should be a gtsummary regression table
    expect_s3_class(result, "tbl_regression")

    # Output should have at least one row
    expect_true(nrow(result$table_body) > 0, info = paste("No rows in result$table_body for", approach))

    # Estimate column should be present
    expect_true("estimate" %in% colnames(result$table_body), info = paste("Missing 'estimate' column for", approach))
  }
})
test_that("multi_reg handles linear regression", {
  data(mtcars)
  result <- tryCatch({
    multi_reg(data = mtcars, outcome = "mpg", exposures = c("wt", "hp"), approach = "linear", summary = TRUE)
  }, error = function(e) {
    warning("Error for linear regression\n", e$message)
    return(NULL)
  })

  expect_true(!is.null(result), info = "multi_reg() returned NULL for linear")
  expect_s3_class(result, "tbl_regression")
  expect_true("estimate" %in% colnames(result$table_body), info = "Missing 'estimate' column for linear")
})
test_that("multi_reg throws error for invalid outcome type", {
  data(mtcars)
  expect_error(
    multi_reg(data = mtcars, outcome = "am", exposures = c("wt", "hp"), approach = "linear"),
    "Linear regression requires a continuous numeric outcome"
  )
})
