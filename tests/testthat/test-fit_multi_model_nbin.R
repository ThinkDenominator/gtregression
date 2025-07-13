test_that(".fit_multi_model_nbin fits a valid model", {
  skip_if_not_installed("MASS")

  data("quine", package = "MASS")

  df <- quine |>
    dplyr::mutate(Days = as.numeric(Days)) |>
    dplyr::filter(!is.na(Days))

  exposures <- c("Eth", "Sex", "Age", "Lrn")
  outcome <- "Days"

  model <- .fit_multi_model_nbin(data = df, outcome = outcome, exposures = exposures)

  expect_s3_class(model, "negbin")
  expect_true(inherits(model, "glm"))
  expect_true(startsWith(model$family$family, "Negative Binomial"))
  expect_equal(
    deparse(formula(model)),
    deparse(as.formula(paste(outcome, "~", paste(exposures, collapse = " + "))))
  )
})
