test_that("uni_reg_nbin computes unadjusted IRRs using negative binomial regression", {
  library(gtregression)
  library(MASS)
  library(dplyr)
  library(gtsummary)
  library(purrr)

  data("quine", package = "MASS")

  quine_data <- quine %>%
    mutate(
      Eth = factor(Eth, levels = c("A", "N")),
      Sex = factor(Sex, levels = c("M", "F")),
      Age = factor(Age, levels = c("F0", "F1", "F2", "F3")),
      Lrn = factor(Lrn, levels = c("AL", "SL"))
    )

  exposures <- c("Eth", "Sex", "Age", "Lrn")
  outcome <- "Days"

  # ✅ Test without summary
  result_default <- tryCatch({
    uni_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures)
  }, error = function(e) {
    warning("uni_reg_nbin() failed with summary = FALSE\n", e$message)
    return(NULL)
  })

  expect_true(!is.null(result_default), info = "uni_reg_nbin() returned NULL with summary = FALSE")
  expect_s3_class(result_default, "tbl_stack")
  expect_true(nrow(result_default$table_body) > 0)
  expect_true("estimate" %in% names(result_default$table_body))

  # ✅ Test with summary = TRUE
  result_summary <- tryCatch({
    uni_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures, summary = TRUE)
  }, error = function(e) {
    warning("uni_reg_nbin() failed with summary = TRUE\n", e$message)
    return(NULL)
  })

  expect_true(!is.null(result_summary), info = "uni_reg_nbin() returned NULL with summary = TRUE")
  expect_s3_class(result_summary, "tbl_stack")

  # ❌ Should fail: outcome is non-count
  expect_error(
    uni_reg_nbin(data = quine_data, outcome = "Sex", exposures = exposures),
    "Outcome must be a non-negative count variable",
    info = "Expected error when using a non-count outcome"
  )
})
