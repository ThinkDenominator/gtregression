lung_cox_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("cox_reg returns crude hazard ratio tables", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  attr(df$trt, "label") <- "Treatment group"

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, karno),
    format = gt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "cox_reg")
  expect_s3_class(res, "gt_cox")
  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$source, "cox_reg")
  expect_equal(res$approach, "cox")
  expect_false(res$adjusted_mode)
  expect_null(res$adjust_for)
  expect_equal(res$time, "time")
  expect_equal(res$event, "status")
  expect_named(res$models, c("trt", "celltype", "karno"))
  expect_true(all(vapply(res$models, inherits, logical(1), what = "coxph")))
  expect_true("N" %in% names(res$table_display))
  expect_true("HR (95% CI)" %in% names(res$table_display))
  expect_true("Treatment group" %in% res$table_display$Characteristic)
  expect_true("Ref." %in% res$table_display[["HR (95% CI)"]])
  expect_null(res$model_stats)
  expect_equal(
    attr(res$table_display, "row_exposure")[res$table_display$is_header],
    c("trt", "celltype", "karno")
  )
})

test_that("cox_reg crude tables display survival complete-case N consistently", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  df$karno[1:3] <- NA

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, karno)
  )

  karno_header <- which(attr(res$table_display, "row_exposure") == "karno" &
                          res$table_display$is_header)[1]

  expect_equal(
    res$table_display$N[karno_header],
    sum(stats::complete.cases(df[, c("time", "status", "karno")]))
  )

  expect_s3_class(modify_table(res, remove_N = TRUE), "cox_reg")
  expect_false("N" %in% names(modify_table(res, remove_N = TRUE)$table_display))
})

test_that("cox_reg adjustment notes use display labels", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  attr(df$age, "label") <- "Patient age"
  attr(df$karno, "label") <- "Performance score"

  res <- cox_reg(
    data = df, time = time, event = status, exposures = trt,
    adjust_for = c(age, karno)
  )

  expect_true(any(res$footnotes == "Adjusted for Patient age and Performance score"))
  expect_false(any(res$footnotes == "Adjusted for age and karno"))
  expect_false(any(grepl("^Event variable:", res$footnotes)))
})

test_that("cox_reg explains how to show reference rows when hidden", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  expect_message(
    res <- cox_reg(
      data = df,
      time = time,
      event = status,
      exposures = trt,
      show_ref = FALSE
    ),
    "To display Ref., use `show_ref = TRUE`.",
    fixed = TRUE
  )

  expect_true(any(res$table_body$ref))
  expect_equal(nrow(res$table_display), 1L)
  expect_false("Ref." %in% res$table_display[["HR (95% CI)"]])
  expect_false(any(grepl("Ref. = reference category", res$table$`_source_notes`,
                         fixed = TRUE)))
})

test_that("cox_reg returns adjusted hazard ratio tables and model stats", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  res <- cox_reg(
    data = df,
    time = "time",
    event = "status",
    exposures = c("trt", "celltype", "prior"),
    adjust_for = c("age", "karno"),
    model_stats = TRUE
  )

  expect_s3_class(res, "ft_cox")
  expect_s3_class(res$table, "flextable")
  expect_true(res$adjusted_mode)
  expect_equal(res$adjust_for, c("age", "karno"))
  expect_true("Adjusted HR (95% CI)" %in% names(res$table_display))
  expect_s3_class(res$model_stats, "data.frame")
  expect_equal(res$model_stats$model, c("trt", "celltype", "prior"))
  expect_true(all(c("AIC", "BIC", "logLik", "concordance", "events", "n") %in%
                    names(res$model_stats)))
  expect_true(all(is.finite(res$model_stats$AIC)))
  expect_true(all(res$model_stats$events > 0))
})

test_that("cox_reg supports a single multivariable Cox model", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior, age, karno),
    multivariable = TRUE,
    model_stats = TRUE
  )

  direct <- survival::coxph(
    survival::Surv(time, status) ~ trt + celltype + prior + age + karno,
    data = df,
    model = TRUE
  )

  expect_s3_class(res, "cox_reg")
  expect_false(res$adjusted_mode)
  expect_true(res$multivariable)
  expect_null(res$adjust_for)
  expect_named(res$models, "multivariable_model")
  expect_equal(res$models$multivariable_model$n, direct$n)
  expect_equal(res$models$multivariable_model$nevent, direct$nevent)
  expect_equal(stats::coef(res$models$multivariable_model), stats::coef(direct), tolerance = 1e-8)
  expect_equal(res$model_stats$model, "multivariable_model")
  expect_true("Adjusted HR (95% CI)" %in% names(res$table_display))
  expect_true(all(c("trt", "celltype", "prior", "age", "karno") %in% res$table_body$exposure))
  expect_equal(
    attr(res$table_display, "row_exposure")[res$table_display$is_header],
    c("trt", "celltype", "prior", "age", "karno")
  )
})

test_that("cox_reg supports interaction terms", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  attr(df$trt, "label") <- "Treatment"
  attr(df$prior, "label") <- "Prior therapy"

  adjusted <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = trt,
    adjust_for = c(age, karno),
    interaction = trt*prior
  )

  direct_adjusted <- survival::coxph(
    survival::Surv(time, status) ~ trt + age + karno + prior + trt:prior,
    data = df,
    model = TRUE
  )

  expect_equal(adjusted$interaction, "trt*prior")
  expect_true("Adjusted HR (95% CI)" %in% names(adjusted$table_display))
  expect_true(any(grepl(" x ", adjusted$table_body$level, fixed = TRUE)))
  expect_true(any(adjusted$table_body$level == "Treatment: Test treatment x Prior therapy: Yes"))
  expect_equal(stats::coef(adjusted$models$trt), stats::coef(direct_adjusted), tolerance = 1e-8)

  multivariable <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, age, karno),
    interaction = "trt*prior",
    multivariable = TRUE
  )

  direct_multi <- survival::coxph(
    survival::Surv(time, status) ~ trt + age + karno + prior + trt:prior,
    data = df,
    model = TRUE
  )

  expect_true(multivariable$multivariable)
  expect_equal(multivariable$interaction, "trt*prior")
  expect_true(any(grepl(":", names(stats::coef(multivariable$models$multivariable_model)), fixed = TRUE)))
  expect_equal(stats::coef(multivariable$models$multivariable_model), stats::coef(direct_multi), tolerance = 1e-8)

  expect_error(
    cox_reg(
      data = df,
      time = time,
      event = status,
      exposures = c(trt, prior),
      interaction = trt:prior
    ),
    "Use standard interaction syntax with"
  )
  expect_error(
    cox_reg(
      data = df,
      time = time,
      event = status,
      exposures = c(trt, prior),
      interaction = trt*prior
    ),
    "exposure-by-exposure mode"
  )
})

test_that("cox_reg accepts multivariate alias for multivariable mode", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, age, karno),
    multivariate = TRUE
  )

  expect_true(res$multivariable)
  expect_named(res$models, "multivariable_model")
  expect_true("Adjusted HR (95% CI)" %in% names(res$table_display))
})

test_that("cox_reg validates survival inputs", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  df$bad_event <- 1

  expect_error(
    cox_reg(df, time = "time", event = "bad_event", exposures = "age"),
    "`event` must include at least one censored observation"
  )
  expect_error(
    cox_reg(df, time = "time", event = "status", exposures = "missing_var"),
    "exposure variables were not found"
  )
  expect_error(
    cox_reg(df, time = "time", event = "status", exposures = "age", model_stats = NA),
    "`model_stats` must be TRUE or FALSE"
  )
  expect_error(
    cox_reg(df, time = "time", event = "status", exposures = "age", multivariable = NA),
    "`multivariable` must be TRUE or FALSE"
  )
  expect_error(
    cox_reg(df, time = "time", event = "status", exposures = "age", multivariate = NA),
    "`multivariate` must be TRUE or FALSE"
  )
  expect_error(
    cox_reg(
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

test_that("cox_reg accepts zero follow-up times like survival::coxph", {
  skip_if_not_installed("survival")

  df <- data.frame(
    time = c(0, 2, 4, 6, 8, 10, 12, 14),
    status = c(1, 1, 0, 1, 0, 1, 0, 1),
    group = factor(c("A", "B", "A", "B", "A", "B", "A", "B"))
  )

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = group
  )

  direct <- survival::coxph(survival::Surv(time, status) ~ group, data = df)
  direct_hr <- unname(exp(stats::coef(direct)[["groupB"]]))
  got_hr <- res$table_body$estimate[res$table_body$exposure == "group" &
                                      res$table_body$level == "B"]

  expect_s3_class(res, "cox_reg")
  expect_equal(got_hr, direct_hr, tolerance = 1e-8)
})

test_that("cox_reg uses exposure-specific complete cases for crude models", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  df$marker <- df$karno
  df$marker[seq_len(20)] <- NA

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(age, marker),
    model_stats = TRUE
  )

  direct_age <- survival::coxph(survival::Surv(time, status) ~ age, data = df)
  direct_marker <- survival::coxph(survival::Surv(time, status) ~ marker, data = df)

  expect_equal(res$models$age$n, direct_age$n)
  expect_equal(res$models$marker$n, direct_marker$n)
  expect_gt(res$models$age$n, res$models$marker$n)
  expect_equal(res$model_stats$n, c(direct_age$n, direct_marker$n))
  expect_equal(stats::coef(res$models$age), stats::coef(direct_age), tolerance = 1e-8)
  expect_equal(stats::coef(res$models$marker), stats::coef(direct_marker), tolerance = 1e-8)
})

test_that("cox_reg accepts two-level factor exposures after complete-case filtering", {
  skip_if_not_installed("survival")

  df <- data.frame(
    time_90 = c(0, 1, 3, 5, 9, 12, 20, 40, 60, 90),
    cs_event = c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    within_admission = factor(
      c("No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
      levels = c("No", "Yes")
    )
  )

  res <- cox_reg(
    data = df,
    time = time_90,
    event = cs_event,
    exposures = within_admission
  )

  direct <- survival::coxph(
    survival::Surv(time_90, cs_event) ~ within_admission,
    data = df
  )
  direct_hr <- unname(exp(stats::coef(direct)[["within_admissionYes"]]))
  got_hr <- res$table_body$estimate[
    res$table_body$exposure == "within_admission" &
      res$table_body$level == "Yes"
  ]

  expect_s3_class(res, "cox_reg")
  expect_equal(got_hr, direct_hr, tolerance = 1e-8)
})

test_that("cox_reg rejects negative follow-up times", {
  skip_if_not_installed("survival")

  df <- data.frame(
    time = c(-1, 2, 4, 6),
    status = c(1, 1, 0, 1),
    age = c(50, 60, 55, 70)
  )

  expect_error(
    cox_reg(df, time = time, event = status, exposures = age),
    "`time` must contain non-negative follow-up times"
  )
})

test_that("cox_reg aligns correctly when merged with descriptive tables", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()
  df$celltype <- factor(
    df$celltype,
    levels = c("squamous", "smallcell", "adeno", "large"),
    labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
  )
  attr(df$trt, "label") <- "Treatment group"
  attr(df$celltype, "label") <- "Cancer cell type"
  attr(df$karno, "label") <- "Karnofsky performance score"
  attr(df$age, "label") <- "Age"
  attr(df$prior, "label") <- "Prior therapy"

  desc <- descriptive_table(
    data = df,
    exposures = c("time", "status", "celltype", "karno", "age", "prior"),
    by = trt,
    statistic = c(time = median, karno = mean, age = mean),
    percent = column,
    show_overall = last
  )
  crude <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "karno", "age", "prior")
  )
  adjusted <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "prior"),
    adjust_for = c("age", "karno")
  )

  merged <- merge_tables(
    desc,
    modify_table(crude, header_labels = c(estimate = "Crude HR")),
    modify_table(adjusted, header_labels = c(estimate = "Adjusted HR")),
    spanners = c("Baseline profile", "Crude Cox", "Adjusted Cox")
  )

  out <- merged$table_display
  crude_col <- grep("^HR|^Crude", names(out), value = TRUE)[1]
  adj_col <- grep("^Adjusted", names(out), value = TRUE)[1]
  ch <- trimws(out$Characteristic)

  expect_match(out[ch == "Cancer cell type", crude_col], "^$")
  expect_match(out[ch == "Squamous", crude_col], "Ref\\.")
  expect_match(out[ch == "Small cell", crude_col], "2\\.72")
  expect_match(out[ch == "Adenocarcinoma", crude_col], "3\\.15")
  expect_match(out[ch == "Large cell", crude_col], "1\\.26")
  expect_match(out[ch == "Age", crude_col], "1\\.01")
  expect_match(out[ch == "No", crude_col], "Ref\\.")
  expect_match(out[ch == "Yes", crude_col], "0\\.87")
  expect_match(out[ch == "Test treatment", crude_col], "1\\.02")

  expect_match(out[ch == "Cancer cell type", adj_col], "^$")
  expect_match(out[ch == "Squamous", adj_col], "Ref\\.")
  expect_match(out[ch == "Small cell", adj_col], "2\\.06")
  expect_match(out[ch == "Adenocarcinoma", adj_col], "3\\.23")
  expect_match(out[ch == "Large cell", adj_col], "1\\.38")
  expect_match(out[ch == "No", adj_col], "Ref\\.")
  expect_match(out[ch == "Yes", adj_col], "0\\.96")
  expect_match(out[ch == "Test treatment", adj_col], "1\\.21")
})

test_that("cox_reg works with plotting and forest helpers", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("forestploter")

  df <- lung_cox_data()
  df$celltype <- factor(
    df$celltype,
    levels = c("squamous", "smallcell", "adeno", "large"),
    labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
  )
  attr(df$trt, "label") <- "Treatment group"
  attr(df$celltype, "label") <- "Cancer cell type"
  attr(df$karno, "label") <- "Karnofsky performance score"
  attr(df$age, "label") <- "Age"
  attr(df$prior, "label") <- "Prior therapy"

  crude <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "karno", "age", "prior")
  )
  adjusted <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c("trt", "celltype", "prior"),
    adjust_for = c("age", "karno")
  )

  expect_s3_class(plot_reg(crude, log_x = TRUE), "ggplot")
  expect_s3_class(plot_reg(adjusted, log_x = TRUE), "ggplot")
  expect_s3_class(plot_reg_combine(crude, adjusted, log_x = TRUE), "patchwork")

  df_crude <- forest_df(crude)
  expect_true("HR (95% CI)" %in% names(df_crude))
  expect_equal(attr(df_crude, "forest_meta")$x_trans, "log")
  expect_equal(attr(df_crude, "forest_meta")$ref_line, 1)

  df_both <- forest_df(crude, adjusted)
  expect_true("HR (95% CI)" %in% names(df_both))
  expect_true("Adjusted HR (95% CI)" %in% names(df_both))
  expect_s3_class(forest_reg(df_both), "gtregression_forest")

  desc <- descriptive_table(
    data = df,
    exposures = c("time", "status", "celltype", "karno", "age", "prior"),
    by = trt,
    statistic = c(time = median, karno = mean, age = mean),
    percent = column,
    show_overall = last
  )
  df_desc <- forest_df(crude, adjusted, desc = desc)
  display_cols <- setdiff(names(df_desc), c("se_uni", "se_adj"))
  ch <- trimws(df_desc$Characteristic)

  expect_equal(df_desc[ch == "Karnofsky performance score", "Overall"], "58.6 (20.0)")
  expect_equal(df_desc[ch == "Age", "Standard treatment"], "57.5 (10.8)")
  expect_equal(df_desc[ch == "Small cell", "Standard treatment"], "30 (43.5%)")
  expect_equal(df_desc[ch == "Yes", "Overall"], "40 (29.2%)")
  expect_false(any(is.na(df_desc[display_cols])))
  expect_true(all(nchar(df_desc[[" "]]) >= 20))
  expect_true(all(nchar(df_desc[["  "]]) >= 20))
  expect_s3_class(forest_reg(df_desc), "gtregression_forest")
})

test_that("cox_reg supports stratified crude and adjusted Cox tables", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  crude <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, karno),
    stratifier = trt,
    format = gt
  )

  expect_s3_class(crude, "gtregression")
  expect_s3_class(crude, "stratified_cox_reg")
  expect_s3_class(crude, "cox_reg")
  expect_equal(crude$source, "stratified_cox_reg")
  expect_true(crude$stratified)
  expect_equal(crude$show_sample, "events")
  expect_false(crude$adjusted_mode)
  expect_equal(crude$by, "trt")
  expect_equal(crude$levels, c("Standard treatment", "Test treatment"))
  expect_false("..N__Standard treatment" %in% names(crude$table_display))
  expect_true("..Events__Standard treatment" %in% names(crude$table_display))
  expect_true("..eff__Standard treatment" %in% names(crude$table_display))
  expect_true("..p__Test treatment" %in% names(crude$table_display))
  expect_named(crude$per_stratum, c("Standard treatment", "Test treatment"))

  adjusted <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, prior),
    adjust_for = c(age, karno),
    stratifier = trt,
    model_stats = TRUE
  )

  expect_s3_class(adjusted, "stratified_cox_reg")
  expect_true(adjusted$adjusted_mode)
  expect_equal(adjusted$adjust_for, c("age", "karno"))
  expect_false("..N__Standard treatment" %in% names(adjusted$table_display))
  expect_true("..Events__Standard treatment" %in% names(adjusted$table_display))
  expect_true("..eff__Standard treatment" %in% names(adjusted$table_display))
  expect_s3_class(adjusted$model_stats, "data.frame")
  expect_true(all(c("stratum", "model", "AIC", "events", "n") %in% names(adjusted$model_stats)))
})

test_that("cox_reg explains hidden reference rows in stratified tables", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  expect_message(
    res <- cox_reg(
      data = df,
      time = time,
      event = status,
      exposures = prior,
      stratifier = trt,
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

test_that("cox_reg controls displayed sample columns in stratified tables", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  with_n <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = celltype,
    stratifier = trt,
    show_sample = n
  )
  expect_true("..N__Standard treatment" %in% names(with_n$table_display))
  expect_false("..Events__Standard treatment" %in% names(with_n$table_display))

  with_both <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = celltype,
    stratifier = trt,
    show_sample = both
  )
  expect_true("..N__Standard treatment" %in% names(with_both$table_display))
  expect_true("..Events__Standard treatment" %in% names(with_both$table_display))

  with_none <- cox_reg(
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
    cox_reg(df, time = time, event = status, exposures = celltype, stratifier = trt, show_sample = "bad"),
    "show_sample"
  )
})

test_that("cox_reg supports stratified multivariable Cox tables", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  res <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, prior, age, karno),
    stratifier = trt,
    multivariable = TRUE
  )

  expect_s3_class(res, "stratified_cox_reg")
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

test_that("cox_reg validates stratifier inputs and supports stratified forest data", {
  skip_if_not_installed("survival")

  df <- lung_cox_data()

  expect_error(
    cox_reg(df, time = time, event = status, exposures = trt, stratifier = trt),
    "`stratifier` cannot also be used"
  )
  expect_error(
    cox_reg(df, time = time, event = status, exposures = trt, adjust_for = age, stratifier = age),
    "`stratifier` cannot also be used"
  )
  expect_error(
    cox_reg(df, time = time, event = status, exposures = trt, stratifier = "time"),
    "`stratifier` cannot also be used"
  )
  expect_error(
    cox_reg(df, time = time, event = status, exposures = trt, interaction = trt*prior, stratifier = prior),
    "`stratifier` cannot also be used"
  )

  stratified <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, karno),
    stratifier = trt
  )

  strat_plot <- plot_reg(stratified)
  expect_s3_class(strat_plot, "ggplot")
  expect_true("stratum" %in% names(strat_plot$data))
  expect_error(plot_reg_combine(stratified, stratified), "does not support stratified")

  forest_data <- forest_df(stratified)
  expect_s3_class(forest_data, "data.frame")
  expect_true(all(paste0(
    c("Standard treatment", "Test treatment"),
    "\nHR (95% CI)"
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
test_that("cox_reg displays every level of character predictors", {
  set.seed(412)
  d <- data.frame(
    time = stats::rexp(180),
    event = stats::rbinom(180, 1, 0.7),
    classification = rep(c("Associated", "Elementary", "Combined"), each = 60)
  )

  res <- cox_reg(d, time, event, classification, format = "gt")
  rows <- res$table_body[res$table_body$exposure == "classification", ]

  expect_equal(rows$level, c("Associated", "Combined", "Elementary"))
  expect_equal(sum(rows$ref), 1L)
  expect_equal(nrow(rows), 3L)
})
