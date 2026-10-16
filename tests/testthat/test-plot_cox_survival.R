test_that("plot_cox_survival builds adjusted curves from cox_reg", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- data_lungcancer
  df$trt <- factor(df$trt, levels = c(1, 2), labels = c("Standard", "Test"))
  fit <- cox_reg(
    df, time, status,
    exposures = c(trt, age, karno),
    multivariable = TRUE
  )
  p <- plot_cox_survival(fit, by = trt)

  expect_s3_class(p, "ggplot")
  expect_s3_class(attr(p, "survfit"), "survfit")
  expect_equal(levels(attr(p, "plot_data")$curve), c("Standard", "Test"))
  expect_true(all(c("trt", "age", "karno") %in%
                    names(attr(p, "prediction_data"))))
})

test_that("plot_cox_survival accepts explicit profiles and model selection", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- data_lungcancer
  fit <- cox_reg(df, time, status, c(age, karno))
  profiles <- data.frame(age = c(50, 70))
  rownames(profiles) <- c("Age 50", "Age 70")

  expect_error(plot_cox_survival(fit, newdata = profiles), "Select a `model`")
  p <- plot_cox_survival(fit, model_name = "age", newdata = profiles)
  expect_equal(levels(attr(p, "plot_data")$curve), c("Age 50", "Age 70"))
})

test_that("plot_cox_survival validates inputs", {
  skip_if_not_installed("survival")

  fit <- survival::coxph(
    survival::Surv(time, status) ~ age,
    data = survival::lung,
    model = TRUE,
    x = TRUE,
    y = TRUE
  )
  expect_error(plot_cox_survival(stats::lm(mpg ~ hp, mtcars)), "must be")
  expect_error(plot_cox_survival(fit, by = absent), "fitted Cox model")
  expect_error(plot_cox_survival(fit, conf.int = NA), "`conf.int`")
})
