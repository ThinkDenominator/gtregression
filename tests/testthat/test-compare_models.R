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
  expect_equal(res$table_body$model_terms, c("x1", "x1 + x2"))
  expect_equal(res$table_display$Variables, c("x1", "x1 + x2"))
  expect_equal(res$table_body$n, c(stats::nobs(fit0), stats::nobs(fit1)))
  expect_equal(res$table_body$AIC, c(stats::AIC(fit0), stats::AIC(fit1)))
  expect_equal(res$table_body$BIC, c(stats::BIC(fit0), stats::BIC(fit1)))
  expect_equal(res$table_body$logLik, c(as.numeric(stats::logLik(fit0)), as.numeric(stats::logLik(fit1))))
  expect_equal(res$comparison_status$status, "Same analysis sample")
  expect_equal(unique(res$table_body$comparison_status), "Same analysis sample")
  expect_true(res$table_body$nested_comparison[2])
  expect_false(any(grepl("Different analysis sample", res$comparison_warnings, fixed = TRUE)))
  expect_false(any(grepl("Non-nested comparison", res$comparison_warnings, fixed = TRUE)))
  expect_true(any(grepl("Comparison status: Same analysis sample", res$footnotes, fixed = TRUE)))
  expect_true(any(grepl("Nested-model status: sequential models are nested", res$footnotes, fixed = TRUE)))
  expect_true(is.na(res$table_body$LR_chisq[1]))
  expect_true(is.finite(res$table_body$LR_chisq[2]))
  expect_equal(sum(res$table_body$best_AIC), 1)
  expect_true("Best AIC" %in% names(res$table_display))
  expect_true("Variables" %in% names(res$table_display))
  expect_true("Primary estimate" %in% names(res$table_display))
})

test_that("compare_models uses object names by default", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    x1 = c(1, 2, 1, 3, 4, 2, 5, 1, 4, 2, 3, 5),
    x2 = factor(c("A", "B", "A", "B", "B", "A", "B", "A", "B", "A", "A", "B"))
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = x1, approach = logit)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = logit)

  default_names <- compare_models(m0, m1, format = tibble)
  explicit_names <- compare_models(
    m0,
    m1,
    model_names = c("Clinical", "Clinical plus group"),
    format = tibble
  )

  expect_equal(default_names$table_body$model, c("m0", "m1"))
  expect_equal(explicit_names$table_body$model, c("Clinical", "Clinical plus group"))
})

test_that("compare_models flags different analysis samples without hiding statistics", {
  df <- data.frame(
    y = c(2.1, 3.4, 4.8, 6.2, 7.4, 8.9, 10.5, 11.7, 12.8, 14.1),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(0, 1, 0, 1, NA, 0, 1, NA, 1, 0)
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = x1, approach = linear)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = linear)

  res <- compare_models(
    m0,
    m1,
    primary_exposure = x1,
    format = tibble
  )

  expect_equal(res$comparison_status$status, "Different analysis sample")
  expect_equal(unique(res$table_body$comparison_status), "Different analysis sample")
  expect_true(res$table_body$nested_comparison[2])
  expect_true(length(unique(res$table_body$n)) > 1)
  expect_true(all(is.finite(res$table_body$AIC)))
  expect_true(all(is.finite(res$table_body$BIC)))
  expect_true(all(is.finite(res$table_body$logLik)))
  expect_true(is.finite(res$table_body$primary_estimate[2]))
  expect_true(is.finite(res$table_body$primary_pct_change[2]))
  expect_true(length(res$comparison_warnings) >= 1)
  expect_match(res$comparison_warnings[1], "Different analysis sample: Models were fitted")
  expect_false(any(grepl("Non-nested comparison", res$comparison_warnings, fixed = TRUE)))
  expect_true(any(grepl("Comparison status: Different analysis sample", res$footnotes, fixed = TRUE)))
  expect_true(any(grepl("Nested-model status: sequential models are nested", res$footnotes, fixed = TRUE)))
})

test_that("compare_models only warns about non-nested models when needed", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    x1 = c(1, 2, 1, 3, 4, 2, 5, 1, 4, 2, 3, 5),
    x2 = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
    x3 = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = logit)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x3), approach = logit)

  res <- compare_models(m0, m1, format = tibble)

  expect_equal(res$comparison_status$status, "Same analysis sample")
  expect_false(res$table_body$nested_comparison[2])
  expect_false(any(grepl("Different analysis sample", res$comparison_warnings, fixed = TRUE)))
  expect_true(any(grepl("Non-nested comparison", res$comparison_warnings, fixed = TRUE)))
  expect_true(any(grepl("Nested-model status: one or more sequential model comparisons are not nested", res$footnotes, fixed = TRUE)))
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
  expect_equal(res$table_body$model_terms, c("trt", "trt + age + karno"))
  expect_false(any(grepl("Surv", res$table_display$Variables, fixed = TRUE)))
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
    y = c(2.1, 3.4, 4.8, 6.2, 7.4, 8.9, 10.5, 11.7, 12.8, 14.1),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0)
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = x1, approach = linear)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = linear)

  res <- compare_models(
    list(base = m0, adjusted = m1),
    nested = FALSE
  )

  expect_s3_class(res$table, "flextable")
  expect_equal(res$table_body$model, c("base", "adjusted"))
  expect_true(all(is.na(res$table_body$LR_chisq)))
  expect_false("Events" %in% names(res$table_display))
})

test_that("compare_models renders caution notes prominently in flextable output", {
  skip_if_not_installed("flextable")

  df <- data.frame(
    y = c(2.1, 3.4, 4.8, 6.2, 7.4, 8.9, 10.5, 11.7, 12.8, 14.1),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(0, 1, 0, 1, NA, 0, 1, NA, 1, 0)
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = x1, approach = linear)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = linear)

  res <- compare_models(m0, m1, primary_exposure = x1)

  expect_s3_class(res$table, "flextable")
  expect_match(res$table$footer$dataset$Model[1], "Comparison status: Different analysis sample")
  expect_match(res$table$footer$dataset$Model[2], "Nested-model status: sequential models are nested")
  expect_match(res$table$footer$dataset$Model[3], "Different analysis sample: Models were fitted")
})

test_that("compare_models renders caution notes without raw HTML in gt output", {
  skip_if_not_installed("gt")

  df <- data.frame(
    y = c(2.1, 3.4, 4.8, 6.2, 7.4, 8.9, 10.5, 11.7, 12.8, 14.1),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(0, 1, 0, 1, NA, 0, 1, NA, 1, 0)
  )

  m0 <- multi_reg(data = df, outcome = y, exposures = x1, approach = linear)
  m1 <- multi_reg(data = df, outcome = y, exposures = c(x1, x2), approach = linear)

  res <- compare_models(m0, m1, primary_exposure = x1, format = gt)
  source_notes <- unlist(res$table$`_source_notes`, recursive = TRUE, use.names = FALSE)

  expect_true(any(grepl("Comparison status: Different analysis sample", source_notes, fixed = TRUE)))
  expect_true(any(grepl("Different analysis sample: Models were fitted", source_notes, fixed = TRUE)))
  expect_false(any(grepl("<div", source_notes, fixed = TRUE)))
  expect_false(any(grepl("background-color", source_notes, fixed = TRUE)))
})

test_that("compare_models rejects raw fitted model objects", {
  df <- data.frame(
    y = c(2, 3, 5, 6, 8, 9, 11, 12),
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(0, 1, 0, 1, 0, 1, 0, 1)
  )

  m0 <- stats::lm(y ~ x1, data = df)
  m1 <- stats::lm(y ~ x1 + x2, data = df)

  expect_error(
    compare_models(m0, m1),
    "gtregression objects"
  )
})

test_that("compare_models supports multivariable Cox and parametric survival outputs", {
  skip_if_not_installed("survival")

  df <- data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes")),
      celltype = factor(
        celltype,
        levels = c("squamous", "smallcell", "adeno", "large"),
        labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
      )
    )

  cox_m0 <- cox_reg(data = df, time = time, event = status, exposures = trt)
  cox_m1 <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, age, karno, celltype, prior),
    multivariable = TRUE
  )
  cox_res <- compare_models(cox_m0, cox_m1, primary_exposure = trt, format = tibble)

  expect_equal(cox_res$table_body$model_type, c("Cox regression", "Cox regression"))
  expect_equal(cox_res$table_body$model_terms, c("trt", "trt + age + karno + celltype + prior"))
  expect_equal(cox_res$table_body$events, c(cox_m0$models[[1]]$nevent, cox_m1$models[[1]]$nevent))
  expect_true(is.finite(cox_res$table_body$primary_estimate[2]))

  aft_m0 <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = trt,
    distribution = weibull
  )
  aft_m1 <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, age, karno, celltype, prior),
    distribution = weibull,
    multivariable = TRUE
  )
  aft_res <- compare_models(aft_m0, aft_m1, primary_exposure = trt, format = tibble)

  expect_equal(aft_res$table_body$model_type, c("Parametric survival", "Parametric survival"))
  expect_equal(aft_res$table_body$model_terms, c("trt", "trt + age + karno + celltype + prior"))
  expect_equal(aft_res$table_body$events, c(128, 128))
  expect_true(is.finite(aft_res$table_body$primary_estimate[2]))
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
