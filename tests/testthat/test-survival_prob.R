lung_survival_prob_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment"))
    )
}

test_that("survival_prob returns grouped Kaplan-Meier survival probabilities", {
  skip_if_not_installed("survival")

  df <- lung_survival_prob_data()

  res <- survival_prob(
    data = df,
    time = time,
    event = status,
    by = trt,
    times = c(90, 180, 365)
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "survival_prob")
  expect_s3_class(res$table, "flextable")
  expect_s3_class(res$fit, "survfit")
  expect_equal(res$source, "survival_prob")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$by, "trt")
  expect_equal(res$times, c(90, 180, 365))
  expect_true(res$extend)
  expect_equal(
    res$table_body$Group,
    rep(c("Standard treatment", "Test treatment"), each = 3)
  )
  expect_equal(res$table_body$Time, rep(c(90, 180, 365), times = 2))
  expect_equal(res$table_body$N.risk, c(37, 13, 4, 25, 14, 6))
  expect_equal(res$table_body$Events, c(31, 21, 8, 42, 9, 7))
  expect_equal(res$table_body$Censored, c(1, 3, 1, 2, 1, 1))
  expect_equal(
    round(res$table_body$Survival.probability, 3),
    c(0.547, 0.212, 0.071, 0.380, 0.233, 0.110)
  )
  expect_true("Survival probability (95% CI)" %in% names(res$table_display))
})

test_that("survival_prob returns overall tibble with quoted names", {
  skip_if_not_installed("survival")

  df <- lung_survival_prob_data()
  time_var <- "time"
  event_var <- "status"

  out <- survival_prob(
    data = df,
    time = time_var,
    event = event_var,
    times = c(90, 180),
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Group, c("Overall", "Overall"))
  expect_equal(out$Time, c(90, 180))
  expect_equal(out$N.risk, c(62, 27))
  expect_equal(out$Events, c(73, 30))
  expect_equal(out$Censored, c(3, 4))
  expect_equal(round(out$Survival.probability, 3), c(0.464, 0.222))
})

test_that("survival_prob returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_survival_prob_data()

  res <- survival_prob(
    data = df,
    time = time,
    event = status,
    by = trt,
    times = c(90, 180),
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("survival_prob validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_survival_prob_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    survival_prob(data = "not data", time = time, event = status, times = 90),
    "`data` must be"
  )
  expect_error(
    survival_prob(data = df, time = missing_time, event = status, times = 90),
    "`time` must be"
  )
  expect_error(
    survival_prob(data = df, time = time, event = missing_event, times = 90),
    "`event` must be"
  )
  expect_error(
    survival_prob(data = df, time = time, event = status),
    "`times` must be"
  )
  expect_error(
    survival_prob(data = df, time = time, event = status, times = -1),
    "`times` must be"
  )
  expect_error(
    survival_prob(data = df, time = time, event = bad_event, times = 90),
    "`event` must include at least one event"
  )
  expect_error(
    survival_prob(data = df, time = time, event = status, by = one_group, times = 90),
    "`by` must contain at least two"
  )
  expect_error(
    survival_prob(data = df, time = time, event = status, times = 90, digits = -1),
    "`digits` must be"
  )
  expect_error(
    survival_prob(data = df, time = time, event = status, times = 90, extend = NA),
    "`extend` must be"
  )
})
