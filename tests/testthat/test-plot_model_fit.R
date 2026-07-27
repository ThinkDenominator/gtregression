test_that("plot_model_fit works for raw lm models", {
  fit <- stats::lm(mpg ~ wt + hp, data = mtcars)

  p_all <- plot_model_fit(fit)
  expect_s3_class(p_all, "patchwork")
  expect_equal(attr(p_all, "source"), "plot_model_fit")

  p_resid <- plot_model_fit(fit, type = residual)
  expect_s3_class(p_resid, "ggplot")
})

test_that("plot_model_fit works for raw binomial glm models", {
  fit <- stats::glm(am ~ mpg + wt, data = mtcars, family = stats::binomial())

  p_all <- plot_model_fit(fit)
  expect_s3_class(p_all, "patchwork")

  p_cal <- plot_model_fit(fit, type = calibration, bins = 4)
  expect_s3_class(p_cal, "ggplot")
})

test_that("plot_model_fit calibration handles few distinct fitted values", {
  d <- data.frame(
    y = c(rep(0, 8), rep(1, 6), rep(0, 5), rep(1, 9)),
    x = factor(rep(c("No", "Yes"), each = 14))
  )
  fit <- stats::glm(y ~ x, data = d, family = stats::binomial())

  p <- plot_model_fit(fit, type = calibration, bins = 6)
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_fit works for raw non-binomial glm models", {
  fit <- stats::glm(carb ~ mpg + wt, data = mtcars, family = stats::poisson())

  p <- plot_model_fit(fit, type = observed_predicted)
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_fit selects models from uni_reg results", {
  fit <- uni_reg(
    data = mtcars,
    outcome = am,
    exposures = c(mpg, wt),
    approach = logit
  )

  expect_message(
    p_default <- plot_model_fit(fit),
    "Multiple fitted models found"
  )
  expect_s3_class(p_default, "patchwork")
  expect_equal(attr(p_default, "model_name"), "mpg")

  p_named <- plot_model_fit(fit, model_name = wt, type = calibration, bins = 4)
  expect_s3_class(p_named, "ggplot")
  expect_equal(attr(p_named, "model_name"), "wt")
})

test_that("plot_model_fit selects models from multi_reg results", {
  fit <- multi_reg(
    data = mtcars,
    outcome = am,
    exposures = c(mpg, wt),
    approach = logit
  )

  p <- plot_model_fit(fit, type = cooks)
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "model_name"), "multivariable_model")
})

test_that("plot_model_fit validates inputs clearly", {
  expect_error(plot_model_fit(list()), "fitted model")

  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(plot_model_fit(fit, bins = 1), "`bins`")
  expect_error(plot_model_fit(fit, base_size = 0), "`base_size`")
  expect_error(plot_model_fit(fit, type = calibration), "binomial")
})

test_that("plot_model_fit directs survival objects to survival helpers", {
  fit <- list(models = list(model = structure(list(), class = "coxph")))
  class(fit) <- c("gtregression", "cox_reg", "list")

  expect_error(
    plot_model_fit(fit),
    "check_ph\\(\\).*plot_surv_fit\\(\\)"
  )
})
