competing_test_data <- function() {
  set.seed(412)
  data.frame(
    time = rexp(180, rate = 0.08),
    status = sample(
      c("Censored", "Relapse", "Death"),
      180,
      replace = TRUE,
      prob = c(0.35, 0.40, 0.25)
    ),
    treatment = factor(rep(c("Control", "Intervention"), each = 90)),
    stringsAsFactors = FALSE
  )
}

test_that("cuminc_plot returns cumulative incidence and Gray's test", {
  skip_if_not_installed("cmprsk")
  df <- competing_test_data()
  attr(df$treatment, "label") <- "Treatment group"

  out <- cuminc_plot(
    df,
    time = time,
    event = status,
    event_of_interest = Relapse,
    censor_code = Censored,
    by = treatment
  )

  expect_s3_class(out, "ggplot")
  expect_s3_class(attr(out, "fit"), "cuminc")
  expect_true(all(c("time", "estimate", "conf.low", "conf.high", "group") %in%
                    names(attr(out, "plot_data"))))
  expect_setequal(unique(attr(out, "plot_data")$group),
                  c("Control", "Intervention"))
  expect_equal(attr(out, "event_of_interest"), "Relapse")
  expect_equal(attr(out, "censor_code"), "Censored")
  expect_true(is.data.frame(attr(out, "gray_test")))
  expect_true(is.finite(attr(out, "gray_test")$p.value))
})

test_that("cuminc_plot supports ungrouped numeric event codes", {
  skip_if_not_installed("cmprsk")
  df <- competing_test_data()
  df$status <- match(df$status, c("Censored", "Relapse", "Death")) - 1L

  out <- cuminc_plot(
    df, time, status,
    event_of_interest = 1,
    censor_code = 0,
    conf.int = FALSE,
    y_percent = FALSE
  )

  expect_s3_class(out, "ggplot")
  expect_equal(unique(attr(out, "plot_data")$group), "Overall")
  expect_null(attr(out, "gray_test"))
})

test_that("cuminc_plot preserves valid probability bounds", {
  skip_if_not_installed("cmprsk")
  df <- competing_test_data()
  out <- cuminc_plot(
    df, time, status, Relapse,
    censor_code = Censored,
    by = treatment
  )
  plot_data <- attr(out, "plot_data")

  expect_true(all(plot_data$estimate >= 0 & plot_data$estimate <= 1))
  expect_true(all(plot_data$conf.low >= 0 & plot_data$conf.low <= 1))
  expect_true(all(plot_data$conf.high >= 0 & plot_data$conf.high <= 1))
})

test_that("cuminc_plot validates competing-risk coding and plot options", {
  skip_if_not_installed("cmprsk")
  df <- competing_test_data()

  expect_error(
    cuminc_plot(df, time, status, Censored, censor_code = Censored),
    "must differ"
  )
  expect_error(
    cuminc_plot(df, time, status, Unknown, censor_code = Censored),
    "not observed"
  )
  expect_error(
    cuminc_plot(df, time, status, Relapse, censor_code = Unknown),
    "not observed"
  )
  binary <- transform(df, status = ifelse(status == "Relapse", 1, 0))
  expect_error(
    cuminc_plot(binary, time, status, 1, censor_code = 0),
    "competing-event"
  )
  expect_error(
    cuminc_plot(df, time, status, Relapse, censor_code = Censored,
                break_time_by = -1),
    "positive"
  )
  expect_error(
    cuminc_plot(df, time, status, Relapse, censor_code = Censored,
                conf.int = "yes"),
    "TRUE or FALSE"
  )
})

test_that("cuminc_plot uses complete cases in required variables", {
  skip_if_not_installed("cmprsk")
  df <- competing_test_data()
  df$time[1] <- NA_real_
  df$treatment[2] <- NA

  out <- cuminc_plot(
    df, time, status, Relapse,
    censor_code = Censored,
    by = treatment
  )

  fit <- attr(out, "fit")
  expect_s3_class(fit, "cuminc")
  expect_true(nrow(attr(out, "plot_data")) > 0)
})
