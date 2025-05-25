test_that("stratified_multi_nbin returns a gtsummary tbl_merge object", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")

  data("quine", package = "MASS")

  quine_data <- dplyr::mutate(quine,
                              Eth = factor(Eth, levels = c("A", "N")),
                              Sex = factor(Sex, levels = c("M", "F")),
                              Age = factor(Age, levels = c("F0", "F1", "F2", "F3")),
                              Lrn = factor(Lrn, levels = c("AL", "SL"))
  )

  result <- gtregression::stratified_multi_nbin(
    data = quine_data,
    outcome = "Days",
    exposures = c("Eth", "Age", "Lrn"),
    stratifier = "Sex"
  )

  expect_s3_class(result, "tbl_merge")
  expect_true("gtsummary" %in% class(result))
})

test_that("stratified_multi_nbin excludes NA values in stratifier", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("dplyr")

  data("quine", package = "MASS")

  quine_data <- dplyr::mutate(quine,
                              Eth = factor(Eth, levels = c("A", "N")),
                              Sex = factor(Sex, levels = c("M", "F")),
                              Age = factor(Age, levels = c("F0", "F1", "F2", "F3")),
                              Lrn = factor(Lrn, levels = c("AL", "SL"))
  )

  quine_data$Sex[1:5] <- NA

  result <- gtregression::stratified_multi_nbin(
    data = quine_data,
    outcome = "Days",
    exposures = c("Eth", "Age", "Lrn"),
    stratifier = "Sex"
  )

  expect_s3_class(result, "tbl_merge")
})

test_that("stratified_multi_nbin errors for invalid inputs", {
  skip_if_not_installed("MASS")
  data("quine", package = "MASS")

  expect_error(
    gtregression::stratified_multi_nbin(
      data = quine,
      outcome = "Days",
      exposures = c("invalid_var"),
      stratifier = "Sex"
    ),
    "One or more exposures not found in the dataset."
  )

  expect_error(
    gtregression::stratified_multi_nbin(
      data = quine,
      outcome = "Days",
      exposures = c("Eth"),
      stratifier = "nonexistent_var"
    ),
    "Stratifier not found in the dataset."
  )
})

test_that("stratified_multi_nbin returns NULL when no valid strata are found", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("dplyr")

  data("quine", package = "MASS")

  quine_data <- dplyr::mutate(quine,
                              Eth = factor(Eth, levels = c("A", "N")),
                              Sex = NA,
                              Age = factor(Age, levels = c("F0", "F1", "F2", "F3")),
                              Lrn = factor(Lrn, levels = c("AL", "SL"))
  )

  expect_warning(
    result <- gtregression::stratified_multi_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ),
    "No valid models across strata."
  )

  expect_null(result)
})
