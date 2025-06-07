test_that("uni_reg computes estimates correctly across approaches", {
  library(gtregression)
  library(mlbench)
  library(risks)
  library(dplyr)
  library(gtsummary)

  # Load dataset
  data("PimaIndiansDiabetes2", package = "mlbench")

  # Prepare the dataset
  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    mutate(
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
      glucose_cat = case_when(
        glucose < 140 ~ "Normal",
        glucose >= 140 ~ "High"
      ),
      glucose_cat = factor(glucose_cat, levels = c("Normal", "High")),
      bp_cat = case_when(
        pressure < 80 ~ "Normal",
        pressure >= 80 ~ "High"
      ),
      bp_cat = factor(bp_cat, levels = c("Normal", "High")),
      triceps_cat = case_when(
        triceps < 23 ~ "Normal",
        triceps >= 23 ~ "High"
      ),
      triceps_cat = factor(triceps_cat, levels = c("Normal", "High")),
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

  # Linear regression example with diagnostics
  result_lm <- suppressWarnings(uni_reg(
    data = PimaIndiansDiabetes2,
    outcome = "mass",
    exposures = c("age", "glucose", "pressure"),
    approach = "linear",
    summary = TRUE
  ))
  expect_s3_class(result_lm, "tbl_stack")


  # Loop through all valid approaches
  for (approach in valid_approaches) {
    outcome_var <- if (approach == "linear") "mass" else "diabetes"

    if (approach == "poisson") {
      expect_error(
        uni_reg(
          data = pima_data,
          outcome = outcome_var,
          exposures = exposures,
          approach = approach,
          summary = FALSE
        ),
        regexp = "Poisson regression is not appropriate for binary outcomes"
      )
    } else {
      expect_silent({
        result <- suppressWarnings(
          uni_reg(
            data = pima_data,
            outcome = outcome_var,
            exposures = exposures,
            approach = approach,
            summary = FALSE
          )
        )
      })

      expect_s3_class(result, "tbl_stack")
      expect_true("estimate" %in% names(result$table_body))

      # Check attributes
      expect_true(!is.null(attr(result, "approach")))
      expect_true(!is.null(attr(result, "source")))
      expect_equal(attr(result, "approach"), approach)
      expect_equal(attr(result, "source"), "uni_reg")
      expect_length(attr(result, "approach"), 1)
      expect_length(attr(result, "source"), 1)
    }
  }


  # Expected error: outcome must be binary for logit
  expect_error(
    uni_reg(data = PimaIndiansDiabetes2, outcome = "mass", exposures = c("age"), approach = "logit"),
    "Binary outcome required"
  )

  # Expected error: outcome must be continuous for linear
  expect_error(
    uni_reg(data = pima_data, outcome = "diabetes", exposures = c("bmi"), approach = "linear"),
    "Continuous numeric outcome required"
  )

  # Poisson regression example
  expect_s3_class(
    suppressWarnings(uni_reg(data = pima_data, outcome = "glucose", exposures = c("bmi"), approach = "poisson")),
    "tbl_stack"
  )

  # Linear regression prints summary message
  expect_output(
    suppressWarnings(uni_reg(
      data = PimaIndiansDiabetes2,
      outcome = "mass",
      exposures = c("age", "glucose"),
      approach = "linear",
      summary = TRUE
    )),
    "Summary for"
  )
})
