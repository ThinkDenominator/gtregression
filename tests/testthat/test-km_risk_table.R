lung_km_risk_table_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment"))
    )
}

test_that("km_risk_table returns grouped Kaplan-Meier risk table", {
  skip_if_not_installed("survival")

  df <- lung_km_risk_table_data()

  res <- km_risk_table(
    data = df,
    time = time,
    event = status,
    by = trt,
    times = c(0, 90, 180, 365)
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "km_risk_table")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$fit, "survfit")
  expect_equal(res$source, "km_risk_table")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$by, "trt")
  expect_equal(res$times, c(0, 90, 180, 365))
  expect_true(res$extend)
  expect_equal(
    res$table_body$Group,
    rep(c("Standard treatment", "Test treatment"), each = 4)
  )
  expect_equal(res$table_body$Time, rep(c(0, 90, 180, 365), times = 2))
  expect_equal(res$table_body$N.risk, c(69, 37, 13, 4, 68, 25, 14, 6))
  expect_equal(res$table_body$Events, c(0, 31, 21, 8, 0, 42, 9, 7))
  expect_equal(res$table_body$Censored, c(0, 1, 3, 1, 0, 2, 1, 1))
  expect_equal(
    names(res$table_display),
    c("Group", "Time", "At risk", "Events", "Censored")
  )
})

test_that("km_risk_table returns overall tibble with quoted names", {
  skip_if_not_installed("survival")

  df <- lung_km_risk_table_data()
  time_var <- "time"
  event_var <- "status"

  out <- km_risk_table(
    data = df,
    time = time_var,
    event = event_var,
    times = c(0, 90, 180),
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Group, c("Overall", "Overall", "Overall"))
  expect_equal(out$Time, c(0, 90, 180))
  expect_equal(out$N.risk, c(137, 62, 27))
  expect_equal(out$Events, c(0, 73, 30))
  expect_equal(out$Censored, c(0, 3, 4))
})

test_that("km_risk_table returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_km_risk_table_data()

  res <- km_risk_table(
    data = df,
    time = time,
    event = status,
    by = trt,
    times = c(0, 90, 180),
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("km_risk_table validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_km_risk_table_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    km_risk_table(data = "not data", time = time, event = status, times = 90),
    "`data` must be"
  )
  expect_error(
    km_risk_table(data = df, time = missing_time, event = status, times = 90),
    "`time` must be"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = missing_event, times = 90),
    "`event` must be"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = status),
    "`times` must be"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = status, times = -1),
    "`times` must be"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = bad_event, times = 90),
    "`event` must include at least one event"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = status, by = one_group, times = 90),
    "`by` must contain at least two"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = status, times = 90, digits = -1),
    "`digits` must be"
  )
  expect_error(
    km_risk_table(data = df, time = time, event = status, times = 90, extend = NA),
    "`extend` must be"
  )
})
