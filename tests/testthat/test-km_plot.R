lung_km_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("km_plot returns an ungrouped ggplot when risk table is hidden", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    risk_table = FALSE
  )

  expect_s3_class(res, "ggplot")
  expect_s3_class(attr(res, "fit"), "survfit")
  expect_s3_class(attr(res, "plot_data"), "data.frame")
  expect_s3_class(attr(res, "risk_table"), "data.frame")
  expect_true(all(c("time", "survival", "conf.low", "conf.high", "strata") %in%
                    names(attr(res, "plot_data"))))
  expect_equal(unique(as.character(attr(res, "plot_data")$strata)), "Overall")
  expect_true(is.na(attr(res, "logrank_p")))
})

test_that("km_plot returns grouped curves with risk table and log-rank p-value", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  df <- lung_km_data()
  attr(df$trt, "label") <- "Treatment group"

  res <- km_plot(
    data = df,
    time = "time",
    event = "status",
    by = "trt",
    break_time_by = 200,
    title = "Kaplan-Meier survival by treatment"
  )

  expect_s3_class(res, "patchwork")
  expect_s3_class(attr(res, "fit"), "survfit")
  expect_true(is.finite(attr(res, "logrank_p")))
  expect_true(all(c("Standard treatment", "Test treatment") %in%
                    unique(as.character(attr(res, "plot_data")$strata))))
  expect_true(all(c(0, 200, 400) %in% attr(res, "risk_table")$time))
})

test_that("km_plot builds without dropping confidence interval rows", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    by = trt,
    risk_table = FALSE,
    break_time_by = 200
  )

  expect_warning(ggplot2::ggplot_build(res), NA)
})

test_that("km_plot validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_km_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    km_plot(data = "not data", time = time, event = status),
    "`data` must be"
  )
  expect_error(
    km_plot(data = df, time = missing_time, event = status),
    "`time` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = missing_event),
    "`event` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, by = one_group),
    "`by` must contain at least two"
  )
  expect_error(
    km_plot(data = df, time = time, event = bad_event),
    "`event` must include at least one event"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, break_time_by = 0),
    "`break_time_by` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, xlim = c(10, 1)),
    "`xlim` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, risk_table = NA),
    "`risk_table` must be"
  )
})
