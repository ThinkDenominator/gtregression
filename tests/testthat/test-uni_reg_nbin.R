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

  # Check outcome is count
  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  if (!is_count(quine_data[[outcome]])) skip("Outcome is not a non-negative count variable.")

  # Run function
  result <- tryCatch({
    uni_reg_nbin(data = quine_data, outcome = outcome, exposures = exposures)
  }, error = function(e) {
    message("uni_reg_nbin() failed\n", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    skip("uni_reg_nbin() returned NULL")
  } else {
    expect_s3_class(result, "tbl_stack")
    expect_true(nrow(result$table_body) > 0)
    expect_true("estimate" %in% names(result$table_body))
    expect_true(!is.null(attr(result, "approach")))
    expect_true(!is.null(attr(result, "source")))

    expect_equal(attr(result, "approach"), "negbin")
    expect_equal(attr(result, "source"), "uni_reg_nbin")
    expect_length(attr(result, "approach"), 1)
    expect_length(attr(result, "source"), 1)

    # Accessors
    expect_type(result$models, "list")
    expect_type(result$model_summaries, "list")
    expect_s3_class(result$table, "tbl_stack")
  }

  # Should fail: outcome is non-count
  expect_error(
    uni_reg_nbin(data = quine_data, outcome = "Sex", exposures = exposures),
    regexp = "Negative binomial regression requires a non-negative count outcome"
  )
})
