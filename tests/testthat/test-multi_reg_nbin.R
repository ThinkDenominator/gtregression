test_that("multi_reg_nbin computes adjusted IRRs using negative binomial regression", {
  library(gtregression)
  library(MASS)
  library(dplyr)
  library(gtsummary)

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

  message("Testing multi_reg_nbin using quine dataset...")

  result <- tryCatch({
    multi_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures)
  }, error = function(e) {
    warning("multi_reg_nbin() failed:\n", e$message)
    return(NULL)
  })

  # ✅ Basic success checks
  expect_true(!is.null(result), info = "multi_reg_nbin() returned NULL unexpectedly.")
  expect_s3_class(result, "tbl_regression")
  expect_true(nrow(result$table_body) > 0, info = "No rows in regression output.")
  expect_true("estimate" %in% names(result$table_body), info = "Missing 'estimate' column.")

  # ✅ summary = TRUE should not raise an error
  expect_error(
    multi_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures, summary = TRUE),
    NA,
    info = "summary = TRUE caused an unexpected error"
  )

  # ❌ Invalid outcome type (non-count)
  quine_data$not_count <- rnorm(nrow(quine_data))  # Continuous outcome

  expect_error(
    multi_reg_nbin(data = quine_data, outcome = "not_count", exposures = exposures),
    regexp = "count outcome",
    info = "Expected error when outcome is not count"
  )
})
