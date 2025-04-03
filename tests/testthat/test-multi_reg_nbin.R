test_that("multi_reg_nbin computes adjusted IRRs using negative binomial regression", {
  library(testthat)
  library(MASS)
  library(dplyr)
  library(gtsummary)
  library(gt)
  library(purrr)
  library(gtregression)

  # Load dataset
  data("quine", package = "MASS")

  # Prepare data
  quine_data <- quine %>%
    mutate(
      Eth = factor(Eth),
      Sex = factor(Sex),
      Age = factor(Age),
      Lrn = factor(Lrn)
    )

  outcome <- "Days"
  exposures <- c("Eth", "Sex", "Age", "Lrn")

  # Main function test
  message("Testing multi_reg_nbin using quine dataset")

  result <- tryCatch({
    multi_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures)
  }, error = function(e) {
    warning(paste("multi_reg_nbin() failed:\n", e$message))
    return(NULL)
  })

  # Test expectations
  expect_true(!is.null(result), "multi_reg_nbin() returned NULL unexpectedly.")
  expect_s3_class(result, "tbl_regression")
  expect_true(nrow(result$table_body) > 0, "No rows in regression output.")
  expect_true("estimate" %in% names(result$table_body), "Missing 'estimate' column.")

  # Test summary = TRUE
  expect_error(
    multi_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures, summary = TRUE),
    NA,
    info = "summary = TRUE caused an unexpected error"
  )

  # Test validation for wrong outcome (should not be count)
  quine_data$not_count <- rnorm(nrow(quine_data))  # Continuous, not count

  expect_error(
    multi_reg_nbin(data = quine_data, outcome = "not_count", exposures = exposures),
    regexp = "requires a count outcome",
    info = "No error for invalid outcome type"
  )
})
