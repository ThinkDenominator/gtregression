lung_surv_model_compare_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("surv_model_compare compares candidate distributions", {
  skip_if_not_installed("survival")

  df <- lung_surv_model_compare_data()

  res <- surv_model_compare(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "prior"),
    adjust_for = c("age", "karno")
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "surv_model_compare")
  expect_s3_class(res$table, "flextable")
  expect_equal(res$source, "surv_model_compare")
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_equal(res$exposures, c("trt", "celltype", "prior"))
  expect_equal(res$adjust_for, c("age", "karno"))
  expect_equal(res$distributions, c("weibull", "exponential", "lognormal", "loglogistic"))
  expect_true(all(vapply(res$models, inherits, logical(1), what = "survreg")))

  expect_equal(
    res$table_body$Distribution,
    c("loglogistic", "lognormal", "exponential", "weibull")
  )
  expect_equal(round(res$table_body$AIC, 3), c(1441.926, 1447.286, 1448.319, 1449.106))
  expect_equal(round(res$table_body$BIC, 3), c(1468.206, 1473.566, 1471.679, 1475.386))
  expect_equal(round(res$table_body$logLik, 3), c(-711.963, -714.643, -716.160, -715.553))
  expect_equal(round(res$table_body$Scale, 3), c(0.579, 1.060, 1.000, 0.928))
  expect_equal(res$table_body$N, rep(137, 4))
  expect_equal(res$table_body$Events, rep(128, 4))
  expect_equal(res$table_body$Best.AIC, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(res$table_body$Best.BIC, c(TRUE, FALSE, FALSE, FALSE))
  expect_true("Best AIC" %in% names(res$table_display))
})

test_that("surv_model_compare supports quoted names and unquoted distribution vector", {
  skip_if_not_installed("survival")

  df <- lung_surv_model_compare_data()
  time_var <- "time"
  event_var <- "status"

  out <- surv_model_compare(
    data = df,
    time = time_var,
    event = event_var,
    exposures = c(trt, prior),
    distributions = c(weibull, lognormal),
    format = tibble
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Distribution, c("weibull", "lognormal"))
  expect_equal(round(out$AIC, 1), c(1503.1, 1506.2))
  expect_equal(round(out$BIC, 1), c(1514.8, 1517.9))
  expect_true(all(is.finite(out$logLik)))
  expect_equal(out$Best.AIC, c(TRUE, FALSE))
})

test_that("surv_model_compare normalizes common distribution spellings", {
  skip_if_not_installed("survival")

  df <- lung_surv_model_compare_data()

  out <- surv_model_compare(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, prior),
    distributions = c("Weibull", "log-logistic", "log_normal", exp),
    format = tibble
  )

  expect_setequal(
    out$Distribution,
    c("weibull", "loglogistic", "lognormal", "exponential")
  )
})

test_that("surv_model_compare returns gt table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("gt")

  df <- lung_surv_model_compare_data()

  res <- surv_model_compare(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "prior"),
    distributions = c("weibull", "lognormal"),
    format = gt
  )

  expect_s3_class(res$table, "gt_tbl")
})

test_that("surv_model_compare validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_surv_model_compare_data()
  df$bad_event <- 0

  expect_error(
    surv_model_compare(data = "not data", time = time, event = status, exposures = "trt"),
    "`data` must be"
  )
  expect_error(
    surv_model_compare(data = df, time = missing_time, event = status, exposures = "trt"),
    "`time` must be"
  )
  expect_error(
    surv_model_compare(data = df, time = time, event = missing_event, exposures = "trt"),
    "`event` must be"
  )
  expect_error(
    surv_model_compare(data = df, time = time, event = status, exposures = "missing_exposure"),
    "exposure variables were not found"
  )
  expect_error(
    surv_model_compare(data = df, time = time, event = bad_event, exposures = "trt"),
    "`event` must include at least one event"
  )
  expect_error(
    surv_model_compare(
      data = df,
      time = time,
      event = status,
      exposures = "trt",
      distributions = c(weibull, gaussian)
    ),
    "`distributions` must contain"
  )
  expect_error(
    surv_model_compare(data = df, time = time, event = status, exposures = "trt", digits = -1),
    "`digits` must be"
  )
})
