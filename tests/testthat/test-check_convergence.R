test_that("check_convergence runs correctly for valid approaches", {
  library(testthat)
  library(mlbench)
  library(MASS)
  library(dplyr)
  library(gtregression)

  # Load and prepare binary outcome data
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    mutate(
      bmi = case_when(mass < 25 ~ "Normal",
                      mass >= 25 & mass < 30 ~ "Overweight",
                      mass >= 30 ~ "Obese"),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(age < 30 ~ "Young",
                          age >= 30 & age < 50 ~ "Middle-aged",
                          age >= 50 ~ "Older"),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older"))
    )

  exposures <- c("bmi", "age_cat")
  outcome <- "diabetes"
  valid_approaches <- c("logit", "log-binomial", "robpoisson", "margstd_boot", "margstd_delta")

  for (approach in valid_approaches) {
    message("\U0001F50D Testing check_convergence for approach: ", approach)

    result_uni <- check_convergence(pima_data, exposures, outcome, approach = approach)
    expect_s3_class(result_uni, "data.frame")
    expect_true(all(c("Exposure", "Model", "Converged", "Max.prob.") %in% names(result_uni)))
    expect_true(nrow(result_uni) > 0)
    expect_true(any(result_uni$Converged))

    result_multi <- check_convergence(pima_data, exposures, outcome, approach = approach, multivariate = TRUE)
    expect_s3_class(result_multi, "data.frame")
    expect_true(nrow(result_multi) == 1)
    expect_true(result_multi$Converged)
  }

  # Poisson and negbin for count outcome
  data("quine", package = "MASS")
  quine_data <- quine %>% mutate(Days = as.numeric(Days))
  count_exposures <- c("Eth", "Sex", "Age", "Lrn")
  count_outcome <- "Days"

  for (approach in c("poisson", "negbin")) {
    result_uni <- check_convergence(quine_data, count_exposures, count_outcome, approach = approach)
    expect_s3_class(result_uni, "data.frame")
    expect_true(nrow(result_uni) > 0)
    expect_true(any(result_uni$Converged))

    result_multi <- check_convergence(quine_data, count_exposures, count_outcome, approach = approach, multivariate = TRUE)
    expect_s3_class(result_multi, "data.frame")
    expect_true(nrow(result_multi) == 1)
    expect_true(result_multi$Converged)
  }

  # Expect errors for incorrect outcome types
  expect_error(
    check_convergence(pima_data, exposures, outcome = "diabetes", approach = "poisson"),
    regexp = "must be a count"
  )

  expect_error(
    check_convergence(quine_data, count_exposures, count_outcome, approach = "logit"),
    regexp = "must be binary"
  )

  # Empty data edge case
  empty_data <- pima_data[0, ]
  result_empty <- check_convergence(empty_data, exposures, outcome)
  expect_true(nrow(result_empty) == 0, "\u274c Empty dataset should return no rows")

  message("\u2705 All tests passed for check_convergence()")
})
