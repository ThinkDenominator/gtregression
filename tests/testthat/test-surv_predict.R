lung_surv_predict_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("surv_predict returns profile-specific predicted probabilities", {
  skip_if_not_installed("survival")

  df <- lung_surv_predict_data()
  fit <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = "trt",
    adjust_for = c("age", "karno"),
    distribution = weibull
  )

  res <- surv_predict(
    model = fit$models$trt,
    newdata = data.frame(
      trt = factor("Test treatment", levels = levels(df$trt)),
      age = 60,
      karno = 70
    ),
    times = c(90, 180, 365)
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "surv_predict")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$model, "survreg")
  expect_equal(res$source, "surv_predict")
  expect_equal(res$times, c(90, 180, 365))
  expect_equal(res$distribution, "weibull")
  expect_equal(res$table_body$Time, c(90, 180, 365))
  expect_equal(round(res$table_body$Survival.probability, 3), c(0.553, 0.311, 0.097))
  expect_true("Predicted survival" %in% names(res$table_display))
})

test_that("surv_predict supports single-model surv_reg objects and tibble output", {
  skip_if_not_installed("survival")

  df <- lung_surv_predict_data()
  fit <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = "trt",
    adjust_for = c("age", "karno"),
    distribution = lognormal
  )

  out <- surv_predict(
    model = fit,
    times = c(90, 180),
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Profile, c(1, 1))
  expect_equal(out$Time, c(90, 180))
  expect_equal(out$Distribution, c("lognormal", "lognormal"))
  expect_equal(round(out$Survival.probability, 3), c(0.439, 0.218))
  expect_true(".profile" %in% names(out))
})

test_that("surv_predict supports multiple profiles and gt output", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_surv_predict_data()
  fit <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = "trt",
    adjust_for = c("age", "karno"),
    distribution = loglogistic
  )

  res <- surv_predict(
    model = fit$models$trt,
    newdata = data.frame(
      trt = factor(c("Standard treatment", "Test treatment"), levels = levels(df$trt)),
      age = c(60, 60),
      karno = c(70, 70)
    ),
    times = c(90, 180),
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$table_body$Profile, c(1, 2, 1, 2))
  expect_equal(res$table_body$Time, c(90, 90, 180, 180))
  expect_equal(round(res$table_body$Survival.probability, 3), c(0.567, 0.545, 0.299, 0.281))
})

test_that("surv_predict supports direct survreg models with supplied newdata", {
  skip_if_not_installed("survival")

  df <- lung_surv_predict_data()
  fit <- survival::survreg(
    survival::Surv(time, status) ~ trt + age,
    data = df,
    dist = "weibull"
  )

  out <- surv_predict(
    model = fit,
    newdata = data.frame(
      trt = factor("Test treatment", levels = levels(df$trt)),
      age = 60
    ),
    times = c(90, 180),
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Time, c(90, 180))
  expect_equal(out$Distribution, c("weibull", "weibull"))
  expect_true(all(out$Survival.probability >= 0 & out$Survival.probability <= 1))
})

test_that("surv_predict validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_surv_predict_data()
  one_fit <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = "trt",
    adjust_for = c("age", "karno")
  )
  multi_fit <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "prior")
  )

  expect_error(
    surv_predict(stats::lm(mpg ~ hp, data = mtcars), times = 90),
    "`model` must be"
  )
  expect_error(
    surv_predict(multi_fit, times = 90),
    "multiple fitted models"
  )
  expect_error(
    surv_predict(one_fit, newdata = "bad", times = 90),
    "`newdata` must be"
  )
  expect_error(
    surv_predict(one_fit, times = -1),
    "`times` must be"
  )
  expect_error(
    surv_predict(one_fit, times = 90, digits = -1),
    "`digits` must be"
  )
  expect_error(
    surv_predict(
      one_fit,
      newdata = data.frame(trt = "Test treatment", age = 60),
      times = 90
    ),
    "`newdata` is not compatible"
  )
})
