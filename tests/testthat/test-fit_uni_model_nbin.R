test_that(".fit_uni_model_nbin fits a valid model", {
  skip_if_not_installed("MASS")

  data("quine", package = "MASS")

  df <- quine |>
    dplyr::mutate(Days = as.numeric(Days)) |>
    dplyr::filter(!is.na(Days))

  model <- .fit_uni_model_nbin(data = df, outcome = "Days", exposure = "Age")

  expect_s3_class(model, "negbin")
  expect_true(inherits(model, "glm"))
  expect_true(startsWith(model$family$family, "Negative Binomial"))
  expect_equal(
    deparse(formula(model)),
    deparse(as.formula("Days ~ Age"))
  )
})

test_that(".fit_uni_model_nbin returns NULL when exposure has no variation", {
  df <- data.frame(
    y = c(2, 4, 5),
    constant = factor(c("A", "A", "A"))
  )

  model <- .fit_uni_model_nbin(df, outcome = "y", exposure = "constant")
  expect_null(model)
})

test_that(".fit_uni_model_nbin returns NULL on fitting failure", {
  df <- data.frame(
    y = c(2, 3, NA),
    x = c(1, 2, 3)
  )

  expect_warning(
    model <- .fit_uni_model_nbin(df, outcome = "y", exposure = "x"),
    regexp = "Model failed for"
  )

  expect_null(model)
})
