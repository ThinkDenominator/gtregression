fg_test_data <- function() {
  set.seed(913)
  n <- 240
  data.frame(
    time = rexp(n, 0.07),
    status = sample(c(0, 1, 2), n, replace = TRUE,
                    prob = c(0.35, 0.40, 0.25)),
    treatment = factor(rep(c("Control", "Intervention"), each = n / 2)),
    age = rnorm(n, 64, 10),
    stage = factor(sample(c("I", "II", "III"), n, replace = TRUE))
  )
}

test_that("competing_risk_reg fits crude Fine-Gray models", {
  skip_if_not_installed("cmprsk")
  df <- fg_test_data()
  attr(df$treatment, "label") <- "Treatment group"

  out <- competing_risk_reg(
    df, time, status,
    event_of_interest = 1,
    exposures = c(treatment, age, stage),
    model_stats = TRUE
  )

  expect_s3_class(out, "competing_risk_reg")
  expect_s3_class(out$table, "flextable")
  expect_true(all(c("exposure", "level", "estimate", "conf.low",
                    "conf.high", "p.value", "ref") %in% names(out$table_body)))
  expect_true(any(out$table_body$ref))
  expect_false(any(out$table_body$estimate[!out$table_body$ref] <= 0))
  expect_identical(out$event_of_interest, "1")
  expect_match(paste(out$footnotes, collapse = " "), "Event of interest: 1")
  expect_match(paste(out$footnotes, collapse = " "), "Competing events")
  expect_true(all(out$model_stats$converged))
  expect_true(all(out$model_stats$events_of_interest > 0))
  level_labels <- out$table_display$Characteristic[!out$table_display$is_header]
  expect_true(all(grepl("^  ", level_labels)))
})

test_that("competing_risk_reg agrees with a direct crr model", {
  skip_if_not_installed("cmprsk")
  df <- fg_test_data()

  out <- competing_risk_reg(
    df, time, status,
    event_of_interest = 1,
    exposures = treatment,
    adjust_for = c(age, stage)
  )
  design <- stats::model.matrix(~ treatment + age + stage, df)
  design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
  direct <- cmprsk::crr(df$time, df$status, design, failcode = 1, cencode = 0)

  estimate <- out$table_body$estimate[
    out$table_body$exposure == "treatment" & !out$table_body$ref
  ]
  expect_equal(estimate, unname(exp(direct$coef["treatmentIntervention"])),
               tolerance = 1e-8)
  expect_true(any(grepl("Adjusted SHR", names(out$table_display), fixed = TRUE)))
  expect_match(paste(out$footnotes, collapse = " "), "Adjusted for")
})

test_that("competing_risk_reg fits one multivariable model", {
  skip_if_not_installed("cmprsk")
  df <- fg_test_data()

  out <- competing_risk_reg(
    df, time, status,
    event_of_interest = 1,
    exposures = c(treatment, age, stage),
    multivariable = TRUE,
    model_stats = TRUE,
    format = "gt"
  )

  expect_length(out$models, 1L)
  expect_s3_class(out$table, "gt_tbl")
  expect_setequal(unique(out$table_body$exposure),
                  c("treatment", "age", "stage"))
  expect_true(is.data.frame(out$model_stats))
  expect_equal(out$model_stats$n, nrow(df))
})

test_that("competing_risk_reg works with downstream output functions", {
  skip_if_not_installed("cmprsk")
  df <- fg_test_data()
  crude <- competing_risk_reg(df, time, status, 1, c(treatment, age))
  adjusted <- competing_risk_reg(
    df, time, status, 1, c(treatment, age),
    multivariable = TRUE
  )

  expect_s3_class(plot_reg(crude), "ggplot")
  expect_s3_class(forest_reg(forest_df(crude)), "gtregression_forest")
  expect_s3_class(forest_reg(forest_df(crude, adjusted)),
                  "gtregression_forest")
  expect_s3_class(merge_tables(crude, adjusted), "gtregression")
  expect_s3_class(modify_table(crude, caption = "Fine-Gray model"),
                  "gtregression")
})

test_that("competing_risk_reg validates analysis inputs", {
  skip_if_not_installed("cmprsk")
  df <- fg_test_data()

  expect_error(
    competing_risk_reg(df, time, status, 0, treatment),
    "must differ"
  )
  expect_error(
    competing_risk_reg(df, time, status, 3, treatment),
    "not observed"
  )
  expect_error(
    competing_risk_reg(df, time, status, 1, treatment,
                       adjust_for = treatment),
    "must not overlap"
  )
  expect_error(
    competing_risk_reg(df, time, status, 1, treatment,
                       adjust_for = age, multivariable = TRUE),
    "not used"
  )
  binary <- transform(df, status = as.integer(status == 1))
  expect_error(
    competing_risk_reg(binary, time, status, 1, treatment),
    "competing-event"
  )
})
