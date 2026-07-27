lung_survival_quantiles_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment"))
    )
}

test_that("survival_quantiles returns grouped Kaplan-Meier quantiles", {
  skip_if_not_installed("survival")

  df <- lung_survival_quantiles_data()

  res <- survival_quantiles(
    data = df,
    time = time,
    event = status,
    by = trt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "survival_quantiles")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$fit, "survfit")
  expect_equal(res$source, "survival_quantiles")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$by, "trt")
  expect_equal(res$probs, c(0.25, 0.5, 0.75))
  expect_equal(
    res$table_body$Group,
    rep(c("Standard treatment", "Test treatment"), each = 3)
  )
  expect_equal(res$table_body$Probability[1:3], c(0.25, 0.5, 0.75))
  expect_equal(res$table_body$Survival.probability[1:3], c(0.75, 0.5, 0.25))
  expect_equal(res$table_body$Time, c(27, 103, 162, 24.5, 52.5, 140))
  expect_true("Time (95% CI)" %in% names(res$table_display))
})

test_that("survival_quantiles returns overall tibble with quoted names", {
  skip_if_not_installed("survival")

  df <- lung_survival_quantiles_data()
  time_var <- "time"
  event_var <- "status"

  out <- survival_quantiles(
    data = df,
    time = time_var,
    event = event_var,
    probs = c(0.25, 0.5),
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Group, c("Overall", "Overall"))
  expect_equal(out$Probability, c(0.25, 0.5))
  expect_equal(out$Time, c(25, 80))
})

test_that("survival_quantiles returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_survival_quantiles_data()

  res <- survival_quantiles(
    data = df,
    time = time,
    event = status,
    by = trt,
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("survival_quantiles validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_survival_quantiles_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    survival_quantiles(data = "not data", time = time, event = status),
    "`data` must be"
  )
  expect_error(
    survival_quantiles(data = df, time = missing_time, event = status),
    "`time` must be"
  )
  expect_error(
    survival_quantiles(data = df, time = time, event = missing_event),
    "`event` must be"
  )
  expect_error(
    survival_quantiles(data = df, time = time, event = bad_event),
    "`event` must include at least one event"
  )
  expect_error(
    survival_quantiles(data = df, time = time, event = status, by = one_group),
    "`by` must contain at least two"
  )
  expect_error(
    survival_quantiles(data = df, time = time, event = status, probs = c(0, 0.5)),
    "`probs` must be"
  )
  expect_error(
    survival_quantiles(data = df, time = time, event = status, digits = -1),
    "`digits` must be"
  )
})
