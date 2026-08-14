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
  expect_true("N" %in% names(res$table_display))
  expect_true("Time Ratio (95% CI)" %in% names(res$table_display))
  expect_true("Treatment group" %in% res$table_display$Characteristic)
  expect_true("Ref." %in% res$table_display[["Time Ratio (95% CI)"]])
  expect_null(res$model_stats)
  expect_equal(
    attr(res$table_display, "row_exposure")[res$table_display$is_header],
    c("trt", "celltype", "karno")
  )
})

test_that("surv_reg crude tables display survival complete-case N consistently", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()
  df$karno[1:3] <- NA

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, karno),
    distribution = weibull
  )

  karno_header <- which(attr(res$table_display, "row_exposure") == "karno" &
                          res$table_display$is_header)[1]

  expect_equal(
    res$table_display$N[karno_header],
    sum(stats::complete.cases(df[, c("time", "status", "karno")]))
  )

  expect_s3_class(modify_table(res, remove_N = TRUE), "surv_reg")
  expect_false("N" %in% names(modify_table(res, remove_N = TRUE)$table_display))
})

test_that("surv_reg explains how to show reference rows when hidden", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  expect_message(
    res <- surv_reg(
      data = df,
      time = time,
      event = status,
      exposures = trt,
      distribution = weibull,
      show_ref = FALSE
    ),
    "To display Ref., use `show_ref = TRUE`.",
    fixed = TRUE
  )

  expect_true(any(res$table_body$ref))
  expect_equal(nrow(res$table_display), 1L)
  expect_false("Ref." %in% res$table_display[["Time Ratio (95% CI)"]])
  expect_false(any(grepl("Ref. = reference category", res$table$`_source_notes`,
                         fixed = TRUE)))
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

test_that("surv_reg supports a single multivariable parametric survival model", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior, age, karno),
    distribution = weibull,
    multivariable = TRUE,
    model_stats = TRUE
  )

  direct <- survival::survreg(
    survival::Surv(time, status) ~ trt + celltype + prior + age + karno,
    data = df,
    dist = "weibull",
    model = TRUE
  )

  expect_s3_class(res, "surv_reg")
  expect_false(res$adjusted_mode)
  expect_true(res$multivariable)
  expect_null(res$adjust_for)
  expect_named(res$models, "multivariable_model")
  expect_equal(stats::nobs(res$models$multivariable_model), stats::nobs(direct))
  expect_equal(stats::coef(res$models$multivariable_model), stats::coef(direct), tolerance = 1e-8)
  expect_equal(res$model_stats$model, "multivariable_model")
  expect_true("Adjusted Time Ratio (95% CI)" %in% names(res$table_display))
  expect_true(all(c("trt", "celltype", "prior", "age", "karno") %in% res$table_body$exposure))
  expect_equal(
    attr(res$table_display, "row_exposure")[res$table_display$is_header],
    c("trt", "celltype", "prior", "age", "karno")
  )
})

test_that("surv_reg supports interaction terms", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  adjusted <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = trt,
    adjust_for = c(age, karno),
    interaction = trt*prior,
    distribution = weibull
  )

  direct_adjusted <- survival::survreg(
    survival::Surv(time, status) ~ trt + age + karno + prior + trt:prior,
    data = df,
    dist = "weibull",
    model = TRUE
  )

  expect_equal(adjusted$interaction, "trt*prior")
  expect_true("Adjusted Time Ratio (95% CI)" %in% names(adjusted$table_display))
  expect_true(any(grepl(" x ", adjusted$table_body$level, fixed = TRUE)))
  expect_equal(stats::coef(adjusted$models$trt), stats::coef(direct_adjusted), tolerance = 1e-8)

  multivariable <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, age, karno),
    interaction = "trt*prior",
    distribution = lognormal,
    multivariable = TRUE
  )

  direct_multi <- survival::survreg(
    survival::Surv(time, status) ~ trt + age + karno + prior + trt:prior,
    data = df,
    dist = "lognormal",
    model = TRUE
  )

  expect_true(multivariable$multivariable)
  expect_equal(multivariable$interaction, "trt*prior")
  expect_true(any(grepl(":", names(stats::coef(multivariable$models$multivariable_model)), fixed = TRUE)))
  expect_equal(stats::coef(multivariable$models$multivariable_model), stats::coef(direct_multi), tolerance = 1e-8)

  expect_error(
    surv_reg(
      data = df,
      time = time,
      event = status,
      exposures = c(trt, prior),
      interaction = trt:prior
    ),
    "Use standard interaction syntax with"
  )
  expect_error(
    surv_reg(
      data = df,
      time = time,
      event = status,
      exposures = c(trt, prior),
      interaction = trt*prior
    ),
    "exposure-by-exposure mode"
  )
})

test_that("surv_reg accepts multivariate alias for multivariable mode", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, age, karno),
    multivariate = TRUE
  )

  expect_true(res$multivariable)
  expect_named(res$models, "multivariable_model")
  expect_true("Adjusted Time Ratio (95% CI)" %in% names(res$table_display))
})

test_that("surv_reg uses exposure-specific complete cases for crude models", {
  skip_if_not_installed("survival")

  df <- data.frame(
    time = c(1, 2, 3, 5, 9, 12, 20, 40),
    status = c(1, 1, 0, 1, 0, 1, 0, 1),
    age = c(40, 44, 50, 52, 60, 62, 70, 72),
    marker = c(1, 0, NA, 1, NA, 0, 1, 0)
  )

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(age, marker),
    distribution = weibull,
    model_stats = TRUE
  )

  direct_age <- survival::survreg(survival::Surv(time, status) ~ age, data = df, dist = "weibull")
  direct_marker <- survival::survreg(survival::Surv(time, status) ~ marker, data = df, dist = "weibull")

  expect_equal(stats::nobs(res$models$age), stats::nobs(direct_age))
  expect_equal(stats::nobs(res$models$marker), stats::nobs(direct_marker))
  expect_gt(stats::nobs(res$models$age), stats::nobs(res$models$marker))
  expect_equal(res$model_stats$n, c(stats::nobs(direct_age), stats::nobs(direct_marker)))
  expect_equal(stats::coef(res$models$age), stats::coef(direct_age), tolerance = 1e-8)
  expect_equal(stats::coef(res$models$marker), stats::coef(direct_marker), tolerance = 1e-8)
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
  expect_error(
    surv_reg(df, time = "time", event = "status", exposures = "age", multivariable = NA),
    "`multivariable` must be TRUE or FALSE"
  )
  expect_error(
    surv_reg(df, time = "time", event = "status", exposures = "age", multivariate = NA),
    "`multivariate` must be TRUE or FALSE"
  )
  expect_error(
    surv_reg(
      df,
      time = "time",
      event = "status",
      exposures = "age",
      adjust_for = "karno",
      multivariable = TRUE
    ),
    "`adjust_for` is not used when `multivariable = TRUE`"
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

test_that("surv_reg supports stratified crude and adjusted survival tables", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  crude <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, karno),
    stratifier = trt,
    distribution = weibull,
    format = gt
  )

  expect_s3_class(crude, "gtregression")
  expect_s3_class(crude, "stratified_surv_reg")
  expect_s3_class(crude, "surv_reg")
  expect_equal(crude$source, "stratified_surv_reg")
  expect_true(crude$stratified)
  expect_equal(crude$show_sample, "events")
  expect_false(crude$adjusted_mode)
  expect_equal(crude$by, "trt")
  expect_equal(crude$distribution, "weibull")
  expect_equal(crude$levels, c("Standard treatment", "Test treatment"))
  expect_false("..N__Standard treatment" %in% names(crude$table_display))
  expect_true("..Events__Standard treatment" %in% names(crude$table_display))
  expect_true("..eff__Standard treatment" %in% names(crude$table_display))
  expect_named(crude$per_stratum, c("Standard treatment", "Test treatment"))

  adjusted <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, prior),
    adjust_for = c(age, karno),
    stratifier = trt,
    distribution = lognormal,
    model_stats = TRUE
  )

  expect_s3_class(adjusted, "stratified_surv_reg")
  expect_true(adjusted$adjusted_mode)
  expect_equal(adjusted$adjust_for, c("age", "karno"))
  expect_equal(adjusted$distribution, "lognormal")
  expect_false("..N__Standard treatment" %in% names(adjusted$table_display))
  expect_true("..Events__Standard treatment" %in% names(adjusted$table_display))
  expect_true("..eff__Standard treatment" %in% names(adjusted$table_display))
  expect_s3_class(adjusted$model_stats, "data.frame")
  expect_true(all(c("stratum", "model", "distribution", "AIC", "events", "n") %in%
                    names(adjusted$model_stats)))
})

test_that("surv_reg explains hidden reference rows in stratified tables", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  expect_message(
    res <- surv_reg(
      data = df,
      time = time,
      event = status,
      exposures = prior,
      stratifier = trt,
      distribution = weibull,
      show_ref = FALSE
    ),
    "To display Ref., use `show_ref = TRUE`.",
    fixed = TRUE
  )

  expect_true(any(vapply(res$per_stratum, function(x) any(x$table_body$ref), logical(1))))
  eff_cols <- startsWith(names(res$table_display), "..eff__")
  expect_false("Ref." %in% unlist(res$table_display[eff_cols]))
  expect_false(any(grepl("Ref. = reference category", res$table$`_source_notes`,
                         fixed = TRUE)))
})

test_that("surv_reg controls displayed sample columns in stratified tables", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  with_n <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = celltype,
    stratifier = trt,
    show_sample = n
  )
  expect_true("..N__Standard treatment" %in% names(with_n$table_display))
  expect_false("..Events__Standard treatment" %in% names(with_n$table_display))

  with_both <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = celltype,
    stratifier = trt,
    show_sample = both
  )
  expect_true("..N__Standard treatment" %in% names(with_both$table_display))
  expect_true("..Events__Standard treatment" %in% names(with_both$table_display))

  with_none <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = celltype,
    stratifier = trt,
    show_sample = none
  )
  expect_false(any(startsWith(names(with_none$table_display), "..N__")))
  expect_false(any(startsWith(names(with_none$table_display), "..Events__")))
  expect_true("..eff__Standard treatment" %in% names(with_none$table_display))

  expect_error(
    surv_reg(df, time = time, event = status, exposures = celltype, stratifier = trt, show_sample = "bad"),
    "show_sample"
  )
})

test_that("surv_reg supports stratified multivariable survival tables", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  res <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, prior, age, karno),
    stratifier = trt,
    distribution = weibull,
    multivariable = TRUE
  )

  expect_s3_class(res, "stratified_surv_reg")
  expect_true(res$multivariable)
  expect_equal(res$show_sample, "events")
  expect_false(res$adjusted_mode)
  expect_named(res$models, c("Standard treatment", "Test treatment"))
  expect_named(res$models[["Standard treatment"]], "multivariable_model")
  expect_false("..N__Standard treatment" %in% names(res$table_display))
  expect_true("..Events__Standard treatment" %in% names(res$table_display))
  expect_true("..eff__Standard treatment" %in% names(res$table_display))
  expect_true(any(trimws(res$table_display$Characteristic) %in% c("Age", "age")))
})

test_that("surv_reg validates stratifier inputs and supports stratified forest data", {
  skip_if_not_installed("survival")

  df <- lung_surv_data()

  expect_error(
    surv_reg(df, time = time, event = status, exposures = trt, stratifier = trt),
    "`stratifier` cannot also be used"
  )
  expect_error(
    surv_reg(df, time = time, event = status, exposures = trt, adjust_for = age, stratifier = age),
    "`stratifier` cannot also be used"
  )
  expect_error(
    surv_reg(df, time = time, event = status, exposures = trt, stratifier = "status"),
    "`stratifier` cannot also be used"
  )
  expect_error(
    surv_reg(df, time = time, event = status, exposures = trt, interaction = trt*prior, stratifier = prior),
    "`stratifier` cannot also be used"
  )

  stratified <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, karno),
    stratifier = trt,
    distribution = weibull
  )

  strat_plot <- plot_reg(stratified)
  expect_s3_class(strat_plot, "ggplot")
  expect_true("stratum" %in% names(strat_plot$data))
  expect_error(plot_reg_combine(stratified, stratified), "does not support stratified")

  forest_data <- forest_df(stratified)
  expect_s3_class(forest_data, "data.frame")
  expect_true(all(paste0(
    "trt = ",
    c("Standard treatment", "Test treatment"),
    "\nTime Ratio (95% CI)"
  ) %in% names(forest_data)))
  expect_false(any(forest_data$Characteristic %in% c(
    "trt = Standard treatment",
    "trt = Test treatment"
  )))
  expect_equal(length(attr(forest_data, "est")), nrow(forest_data))
  expect_equal(attr(forest_data, "forest_meta")$stratifier, "trt")
  expect_true(attr(forest_data, "forest_meta")$side_by_side_strata)
  expect_equal(length(attr(forest_data, "forest_estimates")$est), 2L)
})
test_that("surv_reg displays every level of character predictors", {
  set.seed(413)
  d <- data.frame(
    time = stats::rexp(180),
    event = stats::rbinom(180, 1, 0.7),
    classification = rep(c("Associated", "Elementary", "Combined"), each = 60)
  )

  res <- surv_reg(d, time, event, classification, format = "gt")
  rows <- res$table_body[res$table_body$exposure == "classification", ]

  expect_equal(rows$level, c("Associated", "Combined", "Elementary"))
  expect_equal(sum(rows$ref), 1L)
  expect_equal(nrow(rows), 3L)
})
