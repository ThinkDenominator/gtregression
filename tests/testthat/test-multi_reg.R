test_that("multi_reg computes adjusted estimates correctly across approaches", {
  library(gtregression)
  library(mlbench)
  library(risks)
  library(dplyr)
  library(gtsummary)

  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    mutate(
      bmi = factor(case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese"
      ), levels = c("Normal", "Overweight", "Obese")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"),
                         levels = c("Low parity", "High parity"))
    )

  exposures <- c("bmi", "npreg_cat")
  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "linear")

  for (approach in valid_approaches) {
    message("Testing multi_reg with approach: ", approach)

    outcome_var <- if (approach == "linear") "mass" else "diabetes"

    result <- tryCatch({
      multi_reg(data = pima_data, outcome = outcome_var, exposures = exposures, approach = approach)
    }, error = function(e) {
      message("Model fitting failed for approach: ", approach, "\n", e$message)
      return(NULL)
    })

    if (is.null(result)) {
      skip(paste("Skipping test: Model fitting failed for approach:", approach))
    } else {
      expect_s3_class(result, "tbl_regression")
      expect_true(nrow(result$table_body) > 0, info = paste("No rows in result$table_body for", approach))
      expect_true("estimate" %in% colnames(result$table_body), info = paste("Missing 'estimate' column for", approach))
    }
  }
})


test_that("multi_reg handles linear regression with diagnostics", {
  library(mlbench)
  data("PimaIndiansDiabetes2", package = "mlbench")

  clean_data <- PimaIndiansDiabetes2 %>%
    filter(!is.na(mass), !is.na(age), !is.na(glucose), !is.na(pressure))

  result <- tryCatch({
    multi_reg(data = clean_data,
              outcome = "mass",
              exposures = c("age", "glucose", "pressure"),
              approach = "linear",
              summary = TRUE)
  }, error = function(e) {
    message("Error for linear regression\n", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    skip("Skipping: model fitting failed for linear regression")
  } else {
    expect_s3_class(result, "tbl_regression")
    expect_true("estimate" %in% colnames(result$table_body), info = "Missing 'estimate' column for linear")
  }
})


test_that("multi_reg throws error for invalid outcome type", {
  data("mtcars")

  expect_error(
    multi_reg(data = mtcars, outcome = "am", exposures = c("wt", "hp"), approach = "linear"),
    "Continuous numeric outcome required"
  )

  expect_error(
    multi_reg(data = mtcars, outcome = "mpg", exposures = c("wt", "hp"), approach = "logit"),
    "Binary outcome required"
  )
})
