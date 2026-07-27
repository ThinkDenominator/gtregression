lung_plot_surv_fit_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("plot_surv_fit overlays grouped Kaplan-Meier and fitted curves", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_plot_surv_fit_data()

  res <- plot_surv_fit(
    data = df,
    time = time,
    event = status,
    by = trt,
    distributions = c(weibull, lognormal),
    break_time_by = 200
  )

  expect_s3_class(res, "ggplot")
  expect_s3_class(attr(res, "km_fit"), "survfit")
  expect_true(all(vapply(attr(res, "model_fits"), inherits, logical(1), what = "survreg")))

  observed <- attr(res, "observed_data")
  fitted <- attr(res, "fitted_data")

  expect_true(all(c("time", "survival", "strata", "Curve", "Distribution") %in% names(fitted)))
  expect_true(all(c("Standard treatment", "Test treatment") %in%
                    unique(as.character(observed$strata))))
  expect_equal(unique(fitted$Distribution), c("weibull", "lognormal"))
  expect_true(all(fitted$survival >= 0 & fitted$survival <= 1))
  expect_warning(ggplot2::ggplot_build(res), NA)
})

test_that("plot_surv_fit supports quoted names, adjustment, and single distribution", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_plot_surv_fit_data()
  time_var <- "time"
  event_var <- "status"

  res <- plot_surv_fit(
    data = df,
    time = time_var,
    event = event_var,
    by = "trt",
    adjust_for = c(age, karno),
    distributions = loglogistic,
    xlim = c(0, 800)
  )

  expect_s3_class(res, "ggplot")
  expect_named(attr(res, "model_fits"), "loglogistic")
  expect_true(all(c("age", "karno") %in% names(attr(res, "prediction_data"))))
  expect_equal(attr(res, "prediction_data")$age, rep(stats::median(df$age, na.rm = TRUE), 2))
  expect_warning(ggplot2::ggplot_build(res), NA)
})

test_that("plot_surv_fit normalizes common distribution spellings", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_plot_surv_fit_data()

  res <- plot_surv_fit(
    data = df,
    time = time,
    event = status,
    by = trt,
    distributions = c("Weibull", "log-logistic")
  )

  expect_named(attr(res, "model_fits"), c("weibull", "loglogistic"))
  expect_equal(unique(attr(res, "fitted_data")$Distribution), c("weibull", "loglogistic"))
})

test_that("plot_surv_fit works without grouping", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_plot_surv_fit_data()

  res <- plot_surv_fit(
    data = df,
    time = time,
    event = status,
    distributions = exponential
  )

  expect_s3_class(res, "ggplot")
  expect_equal(unique(as.character(attr(res, "observed_data")$strata)), "Overall")
  expect_equal(unique(attr(res, "fitted_data")$strata), "Overall")
})

test_that("plot_surv_fit validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_plot_surv_fit_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    plot_surv_fit(data = "not data", time = time, event = status),
    "`data` must be"
  )
  expect_error(
    plot_surv_fit(data = df, time = missing_time, event = status),
    "`time` must be"
  )
  expect_error(
    plot_surv_fit(data = df, time = time, event = missing_event),
    "`event` must be"
  )
  expect_error(
    plot_surv_fit(data = df, time = time, event = status, by = one_group),
    "`by` must contain at least two"
  )
  expect_error(
    plot_surv_fit(data = df, time = time, event = bad_event),
    "`event` must include at least one event"
  )
  expect_error(
    plot_surv_fit(data = df, time = time, event = status, adjust_for = missing_var),
    "adjustment variables were not found"
  )
  expect_error(
    plot_surv_fit(data = df, time = time, event = status, distributions = gaussian),
    "`distributions` must contain"
  )
  expect_error(
    plot_surv_fit(data = df, time = time, event = status, n_points = 10),
    "`n_points` must be"
  )
})
