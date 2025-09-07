test_that("check_collinearity: happy path (numeric vector return)", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df <- mtcars
  fit <- gtregression::multi_reg(
    data = df,
    outcome = "mpg",
    exposures = c("hp", "wt"),   # numeric → car::vif returns a named numeric vector
    approach = "linear"
  )

  out <- check_collinearity(fit)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Variable", "VIF", "Interpretation") %in% names(out)))
  expect_gt(nrow(out), 0)
})

test_that("check_collinearity: GVIF matrix branch (factor predictors)", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df <- mtcars
  df$cyl  <- factor(df$cyl)   # 3 levels → car::vif returns a matrix (GVIF table)
  df$gear <- factor(df$gear)  # 3 levels

  fit <- gtregression::multi_reg(
    data = df,
    outcome = "mpg",
    exposures = c("cyl", "gear"),
    approach  = "linear"
  )

  out <- check_collinearity(fit)  # should hit: if (is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]
  expect_s3_class(out, "tbl_df")
  expect_gt(nrow(out), 0)
})

test_that("check_collinearity: univariate object errors as designed", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df <- mtcars
  fit_uni <- gtregression::uni_reg(
    data = df,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach  = "linear"
  )

  expect_error(
    check_collinearity(fit_uni),
    "VIF is not applicable for univariate models",
    fixed = TRUE
  )
})

test_that("check_collinearity: wrong object (missing source attr) errors", {
  skip_if_not_installed("car")

  # Plain glm → should hit 'Input must be a fitted model from ...' branch
  wrong <- stats::glm(am ~ hp + wt, data = transform(mtcars, am = as.integer(am > 0)),
                      family = binomial())
  expect_error(
    check_collinearity(wrong),
    "Input must be a fitted model from uni_reg",
    fixed = FALSE
  )
})

test_that("check_collinearity: missing model list attr errors", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df <- mtcars
  fit <- gtregression::multi_reg(df, outcome = "mpg", exposures = c("hp", "wt"), approach = "linear")

  attr(fit, "models") <- NULL  # remove to hit 'Model list not found' branch
  expect_error(
    check_collinearity(fit),
    "Model list not found",
    fixed = TRUE
  )
})

test_that("check_collinearity: unsupported inner model type errors", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df <- mtcars
  fit <- gtregression::multi_reg(df, outcome = "mpg", exposures = c("hp", "wt"), approach = "linear")

  # Replace first model with an arbitrary object → hit 'Unsupported model type'
  bad <- fit
  attr(bad, "models") <- list(structure(list(), class = "foo"))
  expect_error(check_collinearity(bad), "Unsupported model type", fixed = TRUE)
})

test_that("check_collinearity: requires >= 2 predictors", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df <- mtcars
  one <- gtregression::multi_reg(df, outcome = "mpg", exposures = "hp", approach = "linear")
  expect_error(
    check_collinearity(one),
    "at least two predictors",
    ignore.case = TRUE
  )
})

test_that("check_collinearity: vif() error path returns empty tibble", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("car")

  df  <- mtcars
  fit <- gtregression::multi_reg(df, outcome = "mpg",
                                 exposures = c("hp","wt"), approach = "linear")

  testthat::local_mocked_bindings(
    vif = function(...) stop("boom"),
    .package = "car"
  )

  out <- check_collinearity(fit)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})
