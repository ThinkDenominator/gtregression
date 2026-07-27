lung_logrank_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment"))
    )
}

test_that("logrank_test returns grouped survival comparison table", {
  skip_if_not_installed("survival")

  df <- lung_logrank_data()

  res <- logrank_test(
    data = df,
    time = time,
    event = status,
    by = trt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "logrank_test")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$fit, "survdiff")
  expect_equal(res$source, "logrank_test")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$by, "trt")
  expect_equal(res$table_body$Group, c("Standard treatment", "Test treatment"))
  expect_equal(res$table_body$N, c(69, 68))
  expect_equal(res$table_body$Observed, c(64, 64))
  expect_equal(res$table_body$Expected, c(64.50315, 63.49685), tolerance = 1e-4)
  expect_equal(res$test$Chi.square, 0.008227343, tolerance = 1e-6)
  expect_equal(res$test$df, 1)
  expect_equal(res$test$p.value, 0.9277272, tolerance = 1e-6)
})

test_that("logrank_test supports quoted names and tibble output", {
  skip_if_not_installed("survival")

  df <- lung_logrank_data()
  time_var <- "time"
  event_var <- "status"

  out <- logrank_test(
    data = df,
    time = time_var,
    event = event_var,
    by = "trt",
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Group, c("Standard treatment", "Test treatment"))
  expect_true(all(c("Chi.square", "df", "p.value") %in% names(out)))
})

test_that("logrank_test returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_logrank_data()

  res <- logrank_test(
    data = df,
    time = time,
    event = status,
    by = trt,
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("logrank_test validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_logrank_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    logrank_test(data = "not data", time = time, event = status, by = trt),
    "`data` must be"
  )
  expect_error(
    logrank_test(data = df, time = missing_time, event = status, by = trt),
    "`time` must be"
  )
  expect_error(
    logrank_test(data = df, time = time, event = missing_event, by = trt),
    "`event` must be"
  )
  expect_error(
    logrank_test(data = df, time = time, event = status, by = missing_group),
    "`by` must be"
  )
  expect_error(
    logrank_test(data = df, time = time, event = bad_event, by = trt),
    "`event` must include at least one event"
  )
  expect_error(
    logrank_test(data = df, time = time, event = status, by = one_group),
    "`by` must contain at least two"
  )
  expect_error(
    logrank_test(data = df, time = time, event = status, by = trt, digits = -1),
    "`digits` must be"
  )
})
