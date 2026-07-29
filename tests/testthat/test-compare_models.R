test_that("compare_models reports multi_reg statistics without refitting", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    x1 = c(1, 2, 1, 3, 4, 2, 5, 1, 4, 2, 3, 5),
    x2 = factor(c("A", "B", "A", "B", "B", "A", "B", "A", "B", "A", "A", "B"))
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = x1, approach = logit)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = logit)
  fit0 <- m0$models[[1]]
  fit1 <- m1$models[[1]]

  res <- compare_models(
    m0,
    m1,
    model_names = c("Clinical score", "Clinical score + group"),
    primary_exposure = x1,
    format = tibble
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "compare_models")
  expect_null(res$table)
  expect_equal(res$table_body$model, c("Clinical score", "Clinical score + group"))
  expect_equal(res$table_body$n, c(stats::nobs(fit0), stats::nobs(fit1)))
  expect_equal(res$table_body$AIC, c(stats::AIC(fit0), stats::AIC(fit1)))
  expect_equal(res$table_body$BIC, c(stats::BIC(fit0), stats::BIC(fit1)))
  expect_equal(res$table_body$logLik, c(as.numeric(stats::logLik(fit0)), as.numeric(stats::logLik(fit1))))
  expect_true(is.na(res$table_body$LR_chisq[1]))
  expect_true(is.finite(res$table_body$LR_chisq[2]))
  expect_equal(sum(res$table_body$best_AIC), 1)
  expect_true("Best AIC" %in% names(res$table_display))
  expect_true("Primary estimate" %in% names(res$table_display))
})

test_that("compare_models reports Cox events and concordance from cox_reg objects", {
  skip_if_not_installed("survival")

  df <- data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
  m0 <- cox_reg(data = df, time = time, event = status, exposures = trt)
  m1 <- cox_reg(data = df, time = time, event = status, exposures = trt, adjust_for = c(age, karno))
  fit0 <- m0$models[[1]]
  fit1 <- m1$models[[1]]

  res <- compare_models(
    m0,
    m1,
    model_names = c("Treatment", "Treatment + clinical covariates"),
    primary_exposure = trt,
    nested = TRUE,
    format = tibble
  )

  expect_equal(res$table_body$n, c(fit0$n, fit1$n))
  expect_equal(res$table_body$events, c(fit0$nevent, fit1$nevent))
  expect_equal(res$table_body$AIC, c(stats::AIC(fit0), stats::AIC(fit1)))
  expect_equal(res$table_body$BIC, c(stats::BIC(fit0), stats::BIC(fit1)))
  expect_equal(res$table_body$logLik, c(as.numeric(stats::logLik(fit0)), as.numeric(stats::logLik(fit1))))
  expect_equal(
    res$table_body$concordance,
    unname(c(summary(fit0)$concordance[1], summary(fit1)$concordance[1]))
  )
  expect_equal(
    res$table_body$primary_estimate,
    c(exp(stats::coef(fit0)[["trtTest treatment"]]), exp(stats::coef(fit1)[["trtTest treatment"]])),
    tolerance = 1e-8
  )
  expect_true("Events" %in% names(res$table_display))
  expect_true("Concordance" %in% names(res$table_display))
})

test_that("compare_models supports list input and formatted tables", {
  skip_if_not_installed("flextable")

  df <- data.frame(
    y = c(2, 3, 5, 6, 8, 9, 11, 12),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(0, 1, 0, 1, 0, 1, 0, 1)
  )

  m0 <- stats::lm(y ~ x1, data = df)
  m1 <- stats::lm(y ~ x1 + x2, data = df)

  res <- compare_models(
    list(base = m0, adjusted = m1),
    nested = FALSE
  )

  expect_s3_class(res$table, "flextable")
  expect_equal(res$table_body$model, c("base", "adjusted"))
  expect_true(all(is.na(res$table_body$LR_chisq)))
  expect_false("Events" %in% names(res$table_display))
})

test_that("compare_models rejects gtregression objects with multiple fitted models", {
  df <- data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )

  multi_exposure <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, prior)
  )

  expect_error(
    compare_models(multi_exposure, multi_exposure),
    "must contain one fitted model"
  )
})
