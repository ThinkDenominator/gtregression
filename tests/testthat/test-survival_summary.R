lung_survival_summary_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("survival_summary returns grouped Kaplan-Meier summary table", {
  skip_if_not_installed("survival")

  df <- lung_survival_summary_data()

  res <- survival_summary(
    data = df,
    time = time,
    event = status,
    by = trt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "survival_summary")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$fit, "survfit")
  expect_equal(res$source, "survival_summary")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$by, "trt")
  expect_equal(res$table_body$Group, c("Standard treatment", "Test treatment"))
  expect_equal(res$table_body$N, c(69, 68))
  expect_equal(res$table_body$Events, c(64, 64))
  expect_equal(res$table_body$Censored, c(5, 4))
  expect_equal(res$table_body$Median, c(103, 52.5))
  expect_true("Median survival (95% CI)" %in% names(res$table_display))
})

test_that("survival_summary returns overall tibble with quoted names", {
  skip_if_not_installed("survival")

  df <- lung_survival_summary_data()
  time_var <- "time"
  event_var <- "status"

  out <- survival_summary(
    data = df,
    time = time_var,
    event = event_var,
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Group, "Overall")
  expect_equal(out$N, 137)
  expect_equal(out$Events, 128)
  expect_equal(out$Censored, 9)
  expect_equal(out$Median, 80)
})

test_that("survival_summary returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_survival_summary_data()

  res <- survival_summary(
    data = df,
    time = time,
    event = status,
    by = trt,
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("survival_summary validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_survival_summary_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    survival_summary(data = "not data", time = time, event = status),
    "`data` must be"
  )
  expect_error(
    survival_summary(data = df, time = missing_time, event = status),
    "`time` must be"
  )
  expect_error(
    survival_summary(data = df, time = time, event = missing_event),
    "`event` must be"
  )
  expect_error(
    survival_summary(data = df, time = time, event = bad_event),
    "`event` must include at least one event"
  )
  expect_error(
    survival_summary(data = df, time = time, event = status, by = one_group),
    "`by` must contain at least two"
  )
  expect_error(
    survival_summary(data = df, time = time, event = status, digits = -1),
    "`digits` must be"
  )
})
