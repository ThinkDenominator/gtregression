# Load testthat
library(testthat)
library(gtregression)
library(MASS)

# Sample dataset
# Load quine dataset
data("quine", package = "MASS")

# Prepare dataset
quine_data <- quine %>%
  mutate(
    Eth = factor(Eth, levels = c("A", "N")),       # Ethnicity (A = Aboriginal, N = Not)
    Sex = factor(Sex, levels = c("M", "F")),       # Sex (M = Male, F = Female)
    Age = factor(Age, levels = c("F0", "F1", "F2", "F3")), # Age categories
    Lrn = factor(Lrn, levels = c("AL", "SL"))      # Learning type (AL = Average, SL = Slow)
  )

test_that("stratified_uni_nbin returns a gtsummary tbl_merge object", {
  result <- stratified_uni_nbin(
    data = quine_data,
    outcome = "Days",
    exposures = c("Eth", "Age", "Lrn"),
    stratifier = "Sex"
  )

  expect_s3_class(result, "tbl_merge")
  expect_true("gtsummary" %in% class(result))
})

test_that("stratified_uni_nbin excludes NA values in stratifier", {
  quine_data$Sex[1:5] <- NA  # Introduce some NA values

  result <- stratified_uni_nbin(
    data = quine_data,
    outcome = "Days",
    exposures = c("Eth", "Age", "Lrn"),
    stratifier = "Sex"
  )

  expect_s3_class(result, "tbl_merge")  # Should still return a valid output
})

test_that("stratified_uni_nbin errors for invalid inputs", {
  expect_error(
    stratified_uni_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("invalid_var"),
      stratifier = "Sex"
    ),
    "One or more exposures not found in the dataset."
  )

  expect_error(
    stratified_uni_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("Eth"),
      stratifier = "nonexistent_var"
    ),
    "Stratifier not found in the dataset."
  )
})

test_that("stratified_uni_nbin returns NULL when no valid strata are found", {
  quine_data$Sex <- NA  # Make all values of stratifier NA

  expect_warning(
    result <- stratified_uni_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ),
    "No valid models across strata."
  )

  expect_null(result)
})
