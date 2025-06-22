test_that("multi_reg computes estimates correctly across approaches", {
  library(gtregression)
  library(mlbench)
  library(risks)
  library(dplyr)
  library(gtsummary)

  # Load and prepare the dataset
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
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
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
      glucose_cat = factor(ifelse(glucose < 140, "Normal", "High"), levels = c("Normal", "High")),
      bp_cat = factor(ifelse(pressure < 80, "Normal", "High"), levels = c("Normal", "High")),
      triceps_cat = factor(ifelse(triceps < 23, "Normal", "High"), levels = c("Normal", "High")),
      insulin_cat = case_when(
        insulin < 30 ~ "Low",
        insulin >= 30 & insulin < 150 ~ "Normal",
        insulin >= 150 ~ "High"
      ),
      insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High")),
      dpf_cat = case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
        pedigree > 0.5 ~ "High Genetic Risk"
      ),
      dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))
    )

  exposures <- c("bmi", "age_cat", "npreg_cat", "glucose_cat", "bp_cat", "triceps_cat", "insulin_cat", "dpf_cat")
  valid_approaches <- c("logit", "log-binomial", "poisson", "robpoisson", "linear")

  for (approach in valid_approaches) {
    outcome_var <- if (approach == "linear") "mass" else if (approach == "poisson") "pregnant" else "diabetes"

    if (approach == "log-binomial") {
      skip_on_cran()  # May fail due to convergence on CRAN

      expect_error(
        suppressWarnings(
          multi_reg(
            data = pima_data,
            outcome = outcome_var,
            exposures = exposures,
            approach = "log-binomial"
          )
        ),
        regexp = "Could not fit the model"
      )
    } else {
      result <- suppressWarnings(
        multi_reg(
          data = pima_data,
          outcome = outcome_var,
          exposures = exposures,
          approach = approach
        )
      )

      expect_s3_class(result, "tbl_regression")
      expect_s3_class(result, "gtsummary")
      expect_true("estimate" %in% names(result$table_body))
      expect_equal(attr(result, "approach"), approach)
      expect_equal(attr(result, "source"), "multi_reg")
    }
  }

  # Validate known input errors
  expect_error(
    multi_reg(data = pima_data, outcome = "mass", exposures = c("age_cat"), approach = "logit"),
    "Binary outcome required"
  )

  expect_error(
    multi_reg(data = pima_data, outcome = "diabetes", exposures = c("bmi"), approach = "linear"),
    "Continuous numeric outcome required"
  )

  # Valid Poisson test with count outcome
  expect_s3_class(
    suppressWarnings(multi_reg(data = pima_data, outcome = "pregnant", exposures = c("bmi"), approach = "poisson")),
    "tbl_regression"
  )
})
