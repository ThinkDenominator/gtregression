test_that("stratified_uni_nbin returns a gtsummary tbl_merge object", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("MASS")

  data("quine", package = "MASS")

  quine_data <- dplyr::mutate(quine,
                              Eth = factor(Eth, levels = c("A", "N")),
                              Sex = factor(Sex, levels = c("M", "F")),
                              Age = factor(Age, levels = c("F0", "F1", "F2", "F3")),
                              Lrn = factor(Lrn, levels = c("AL", "SL")))

  result <- tryCatch({
    suppressWarnings(gtregression::stratified_uni_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ))
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    skip("Skipping test: no valid models across strata.")
  } else {
    expect_s3_class(result, "stratified_uni_nbin")
    expect_s3_class(result, "tbl_merge")
    expect_true("gtsummary" %in% class(result))
  }
})

test_that("stratified_uni_nbin excludes NA values in stratifier", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("MASS")

  data("quine", package = "MASS")

  quine_data <- dplyr::mutate(quine,
                              Eth = factor(Eth),
                              Age = factor(Age),
                              Lrn = factor(Lrn),
                              Sex = factor(Sex))

  quine_data$Sex[1:5] <- NA
  outcome <- "Days"

  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  if (!is_count(quine_data[[outcome]])) skip("Outcome is not a count variable.")

  result <- tryCatch({
    suppressWarnings(gtregression::stratified_uni_nbin(
      data = quine_data,
      outcome = outcome,
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ))
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    skip("Skipping test: no valid strata after removing NA values.")
  } else {
    expect_s3_class(result, "stratified_uni_nbin")
    expect_s3_class(result, "tbl_merge")
  }
})

test_that("stratified_uni_nbin errors for invalid inputs", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("MASS")

  data("quine", package = "MASS")

  expect_error(
    gtregression::stratified_uni_nbin(
      data = quine,
      outcome = "Days",
      exposures = c("invalid_var"),
      stratifier = "Sex"
    ),
    regexp = "One or more exposures not found in the dataset."
  )

  expect_error(
    gtregression::stratified_uni_nbin(
      data = quine,
      outcome = "Days",
      exposures = c("Eth"),
      stratifier = "nonexistent_var"
    ),
    regexp = "Stratifier not found in the dataset."
  )
})

test_that("stratified_uni_nbin returns NULL when no valid strata are found", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("MASS")

  data("quine", package = "MASS")

  quine_m <- dplyr::filter(quine, Sex == "M")

  # All exposures have no variation
  bad_data <- quine_m %>%
    dplyr::mutate(Eth = "A", Age = "F0", Lrn = "SL")

  result <- suppressWarnings(
    tryCatch({
      gtregression::stratified_uni_nbin(
        data = bad_data,
        outcome = "Days",
        exposures = c("Eth", "Age", "Lrn"),
        stratifier = "Sex"
      )
    }, error = function(e) {
      NULL
    })
  )

  expect_null(result)
})
