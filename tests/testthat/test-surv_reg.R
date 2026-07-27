lung_surv_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("surv_reg returns crude time-ratio tables", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()
  attr(df$trt, "label") <- "Treatment group"

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, karno),
    distribution = weibull,
    format = gt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "surv_reg")
  expect_s3_class(res, "gt_surv")
  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$source, "surv_reg")
  expect_equal(res$approach, "survreg")
  expect_equal(res$distribution, "weibull")
  expect_false(res$adjusted_mode)
  expect_null(res$adjust_for)
  expect_named(res$models, c("trt", "celltype", "karno"))
  expect_true(all(vapply(res$models, inherits, logical(1), what = "survreg")))
  expect_true("Time Ratio (95% CI)" %in% names(res$table_display))
  expect_true("Treatment group" %in% res$table_display$Characteristic)
  expect_true("Ref." %in% res$table_display[["Time Ratio (95% CI)"]])
  expect_null(res$model_stats)
})

test_that("surv_reg returns adjusted time-ratio tables and model stats", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  res <- surv_reg(
    data = df,
    time = "time",
    event = "status",
    exposures = c("trt", "celltype", "prior"),
    adjust_for = c("age", "karno"),
    distribution = lognormal,
    model_stats = TRUE
  )

  expect_s3_class(res, "ft_surv")
  expect_s3_class(res$table, "flextable")
  expect_true(res$adjusted_mode)
  expect_equal(res$adjust_for, c("age", "karno"))
  expect_equal(res$distribution, "lognormal")
  expect_true("Adjusted Time Ratio (95% CI)" %in% names(res$table_display))
  expect_s3_class(res$model_stats, "data.frame")
  expect_equal(res$model_stats$model, c("trt", "celltype", "prior"))
  expect_true(all(c("distribution", "AIC", "BIC", "logLik", "scale", "events", "n") %in%
                    names(res$model_stats)))
  expect_true(all(res$model_stats$distribution == "lognormal"))
  expect_true(all(is.finite(res$model_stats$AIC)))
  expect_true(all(res$model_stats$events > 0))
})

test_that("surv_reg normalizes common distribution spellings", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, prior),
    distribution = "log-logistic"
  )

  expect_equal(res$distribution, "loglogistic")
  expect_true(all(vapply(res$models, function(x) identical(x$dist, "loglogistic"), logical(1))))
})

test_that("surv_reg validates survival and distribution inputs", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()
  df$bad_event <- 1

  expect_error(
    surv_reg(df, time = "time", event = "bad_event", exposures = "age"),
    "`event` must include at least one censored observation"
  )
  expect_error(
    surv_reg(df, time = "time", event = "status", exposures = "missing_var"),
    "exposure variables were not found"
  )
  expect_error(
    surv_reg(df, time = "time", event = "status", exposures = "age", distribution = gaussian),
    "Invalid distribution"
  )
  expect_error(
    surv_reg(df, time = "time", event = "status", exposures = "age", model_stats = NA),
    "`model_stats` must be TRUE or FALSE"
  )
})

test_that("surv_reg works with plotting and forest helpers", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("forestploter")

  df <- lung_surv_data()
  df$celltype <- factor(
    df$celltype,
    levels = c("squamous", "smallcell", "adeno", "large"),
    labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
  )

  crude <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "karno", "age", "prior"),
    distribution = weibull
  )
  adjusted <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "prior"),
    adjust_for = c("age", "karno"),
    distribution = lognormal
  )

  expect_s3_class(plot_reg(crude, log_x = TRUE), "ggplot")
  expect_s3_class(plot_reg(adjusted, log_x = TRUE), "ggplot")
  expect_s3_class(plot_reg_combine(crude, adjusted, log_x = TRUE), "patchwork")

  df_crude <- forest_df(crude)
  expect_true("Time Ratio (95% CI)" %in% names(df_crude))
  expect_equal(attr(df_crude, "forest_meta")$x_trans, "log")
  expect_equal(attr(df_crude, "forest_meta")$ref_line, 1)

  df_both <- forest_df(crude, adjusted)
  expect_true("Time Ratio (95% CI)" %in% names(df_both))
  expect_true("Adjusted Time Ratio (95% CI)" %in% names(df_both))
  expect_s3_class(forest_reg(df_both), "gtregression_forest")
})
