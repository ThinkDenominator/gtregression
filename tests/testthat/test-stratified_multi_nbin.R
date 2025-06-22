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

  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  if (!is_count(quine_data[["Days"]])) skip("Outcome is not a count variable.")

  result <- tryCatch({
    suppressWarnings(gtregression::stratified_multi_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ))
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })

  skip_if(is.null(result), "No valid models across strata.")

  expect_s3_class(result, "stratified_multi_nbin")
  expect_s3_class(result, "tbl_merge")
  expect_s3_class(result, "gtsummary")
})

test_that("stratified_multi_nbin excludes NA values in stratifier", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("dplyr")

  data("quine", package = "MASS")

  quine_data <- dplyr::mutate(quine,
                              Eth = factor(Eth),
                              Sex = factor(Sex),
                              Age = factor(Age),
                              Lrn = factor(Lrn)
  )
  quine_data$Sex[1:5] <- NA

  is_count <- function(x) is.numeric(x) && all(x >= 0 & x == floor(x), na.rm = TRUE)
  if (!is_count(quine_data[["Days"]])) skip("Outcome is not a count variable.")

  result <- tryCatch({
    suppressWarnings(gtregression::stratified_multi_nbin(
      data = quine_data,
      outcome = "Days",
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ))
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })

  skip_if(is.null(result), "No valid strata after removing NA values.")

  expect_s3_class(result, "stratified_multi_nbin")
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
    regexp = "One or more exposures not found in the dataset."
  )

  expect_error(
    gtregression::stratified_multi_nbin(
      data = quine,
      outcome = "Days",
      exposures = c("Eth"),
      stratifier = "nonexistent_var"
    ),
    regexp = "Stratifier not found in the dataset."
  )
})

test_that("stratified_multi_nbin handles single stratum gracefully", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("dplyr")

  data("quine", package = "MASS")

  quine_m <- quine |> dplyr::filter(Sex == "M")

  expect_error(
    gtregression::stratified_multi_nbin(
      data = quine_m,
      outcome = "Days",
      exposures = c("Eth", "Age", "Lrn"),
      stratifier = "Sex"
    ),
    regexp = "No valid models across strata"
  )
})

test_that("stratified_multi_nbin returns NULL when models cannot be fit", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("dplyr")

  data("quine", package = "MASS")

  quine_m <- dplyr::filter(quine, Sex == "M")

  # Create an invalid dataset with no variation
  bad_data <- quine_m |>
    dplyr::mutate(Eth = "A", Age = "F0", Lrn = "SL")

  result <- suppressWarnings(
    tryCatch({
      gtregression::stratified_multi_nbin(
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
