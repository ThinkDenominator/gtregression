test_that("check_convergence runs correctly for valid approaches", {

  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = dplyr::case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese"
      ),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = dplyr::case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older"))
    )

  exposures <- c("bmi", "age_cat")
  outcome <- "diabetes"
  valid_approaches <- c("logit", "log-binomial", "robpoisson")

  for (approach in valid_approaches) {
    result_uni <- gtregression::check_convergence(pima_data, exposures, outcome, approach = approach)
    expect_s3_class(result_uni, "data.frame")
    expect_true(all(c("Exposure", "Model", "Converged", "Max.prob.") %in% names(result_uni)))
    expect_gt(nrow(result_uni), 0)
    expect_true(any(result_uni$Converged))

    result_multi <- gtregression::check_convergence(pima_data, exposures, outcome, approach = approach, multivariate = TRUE)
    expect_s3_class(result_multi, "data.frame")
    expect_equal(nrow(result_multi), 1)
    expect_true(result_multi$Converged)
  }

  data("quine", package = "MASS")

  quine_data <- quine |>
    dplyr::mutate(Days = as.numeric(Days))

  count_exposures <- c("Eth", "Sex", "Age", "Lrn")
  count_outcome <- "Days"

  for (approach in c("poisson", "negbin")) {
    result_uni <- gtregression::check_convergence(quine_data, count_exposures, count_outcome, approach = approach)
    expect_s3_class(result_uni, "data.frame")
    expect_gt(nrow(result_uni), 0)
    expect_true(any(result_uni$Converged))

    result_multi <- gtregression::check_convergence(quine_data, count_exposures, count_outcome, approach = approach, multivariate = TRUE)
    expect_s3_class(result_multi, "data.frame")
    expect_equal(nrow(result_multi), 1)
    expect_true(result_multi$Converged)
  }

  # Expect errors for wrong outcome types
  expect_error(
    gtregression::check_convergence(pima_data, exposures, outcome = "diabetes", approach = "poisson"),
    regexp = "must be a count"
  )

  expect_error(
    gtregression::check_convergence(quine_data, count_exposures, count_outcome, approach = "logit"),
    regexp = "must be binary"
  )

  # Empty data
  empty_data <- pima_data[0, ]
  result_empty <- gtregression::check_convergence(empty_data, exposures, outcome)
  expect_equal(nrow(result_empty), 0)
})
