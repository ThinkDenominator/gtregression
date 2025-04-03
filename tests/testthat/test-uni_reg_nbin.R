test_that("uni_reg_nbin computes unadjusted IRRs using negative binomial regression", {
  library(testthat)
  library(MASS)         # For quine dataset and glm.nb
  library(dplyr)        # Data wrangling
  library(gtsummary)    # Publication-ready tables
  library(purrr)        # Mapping over exposures
  library(forcats)      # Handling missing values in factors
  library(gtregression)

  # Load quine dataset
  data("quine", package = "MASS")

  # Prepare dataset
  quine_data <- quine %>%
    mutate(
      Eth = factor(Eth, levels = c("A", "N")),       # Ethnicity
      Sex = factor(Sex, levels = c("M", "F")),       # Sex
      Age = factor(Age, levels = c("F0", "F1", "F2", "F3")), # Age
      Lrn = factor(Lrn, levels = c("AL", "SL"))      # Learning type
    )

  exposures <- c("Eth", "Sex", "Age", "Lrn")
  outcome <- "Days"

  # Check with summary = FALSE (default)
  result_default <- tryCatch({
    uni_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures)
  }, error = function(e) {
    warning(paste("uni_reg_nbin() failed with summary=FALSE\n", e$message))
    return(NULL)
  })

  expect_true(!is.null(result_default), "uni_reg_nbin() returned NULL with summary = FALSE")
  expect_s3_class(result_default, "tbl_stack")
  expect_true(nrow(result_default$table_body) > 0)

  # Check with summary = TRUE
  result_summary <- tryCatch({
    uni_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures, summary = TRUE)
  }, error = function(e) {
    warning(paste("uni_reg_nbin() failed with summary=TRUE\n", e$message))
    return(NULL)
  })

  expect_true(!is.null(result_summary), "uni_reg_nbin() returned NULL with summary = TRUE")
  expect_s3_class(result_summary, "tbl_stack")

  # Should fail when outcome is not count
  expect_error(
    uni_reg_nbin(data = quine_data, outcome = "Sex", exposures = exposures),
    "Negative binomial regression requires a count outcome",
    info = "Expected error when using a non-count outcome"
  )
})
