lung_rmst_table_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment"))
    )
}

test_that("rmst_table returns grouped RMST table with two-group difference", {
  skip_if_not_installed("survival")

  df <- lung_rmst_table_data()

  res <- rmst_table(
    data = df,
    time = time,
    event = status,
    by = trt,
    tau = 365
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "rmst_table")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$fit, "survfit")
  expect_equal(res$source, "rmst_table")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$by, "trt")
  expect_equal(res$tau, 365)
  expect_equal(res$conf.level, 0.95)

  expect_equal(res$table_body$Type, c("Group", "Group", "Difference"))
  expect_equal(
    res$table_body$Group,
    c(
      "Standard treatment",
      "Test treatment",
      "Difference (Test treatment - Standard treatment)"
    )
  )
  expect_equal(res$table_body$N, c(69, 68, NA))
  expect_equal(res$table_body$Events, c(64, 64, NA))
  expect_equal(round(res$table_body$RMST, 3), c(118.972, 112.404, NA))
  expect_equal(round(res$table_body$SE, 3), c(13.020, 14.875, 19.768))
  expect_equal(round(res$table_body$CI.lower, 3), c(93.452, 83.250, NA))
  expect_equal(round(res$table_body$CI.upper, 3), c(144.491, 141.558, NA))
  expect_equal(round(res$table_body$Difference, 3), c(NA, NA, -6.567))
  expect_equal(round(res$table_body$Difference.CI.lower, 3), c(NA, NA, -45.313))
  expect_equal(round(res$table_body$Difference.CI.upper, 3), c(NA, NA, 32.178))
  expect_equal(round(res$table_body$p.value, 3), c(NA, NA, 0.740))
  expect_true("RMST (95% CI)" %in% names(res$table_display))
  expect_true("RMST difference (95% CI)" %in% names(res$table_display))
})

test_that("rmst_table returns overall tibble with quoted names", {
  skip_if_not_installed("survival")

  df <- lung_rmst_table_data()
  time_var <- "time"
  event_var <- "status"

  out <- rmst_table(
    data = df,
    time = time_var,
    event = event_var,
    tau = 180,
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Type, "Group")
  expect_equal(out$Group, "Overall")
  expect_equal(out$Tau, 180)
  expect_equal(out$N, 137)
  expect_equal(out$Events, 128)
  expect_equal(round(out$RMST, 3), 88.521)
  expect_equal(round(out$SE, 3), 5.639)
})

test_that("rmst_table returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_rmst_table_data()

  res <- rmst_table(
    data = df,
    time = time,
    event = status,
    by = trt,
    tau = 365,
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("rmst_table validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_rmst_table_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    rmst_table(data = "not data", time = time, event = status, tau = 365),
    "`data` must be"
  )
  expect_error(
    rmst_table(data = df, time = missing_time, event = status, tau = 365),
    "`time` must be"
  )
  expect_error(
    rmst_table(data = df, time = time, event = missing_event, tau = 365),
    "`event` must be"
  )
  expect_error(
    rmst_table(data = df, time = time, event = status),
    "`tau` must be"
  )
  expect_error(
    rmst_table(data = df, time = time, event = status, tau = 0),
    "`tau` must be"
  )
  expect_error(
    rmst_table(data = df, time = time, event = bad_event, tau = 365),
    "`event` must include at least one event"
  )
  expect_error(
    rmst_table(data = df, time = time, event = status, by = one_group, tau = 365),
    "`by` must contain at least two"
  )
  expect_error(
    rmst_table(data = df, time = time, event = status, tau = 365, digits = -1),
    "`digits` must be"
  )
  expect_error(
    rmst_table(data = df, time = time, event = status, tau = 365, conf.level = 1),
    "`conf.level` must be"
  )
})
