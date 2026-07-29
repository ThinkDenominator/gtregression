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
  expect_true("HR (95% CI)" %in% names(res$table_display))
  expect_true("Treatment group" %in% res$table_display$Characteristic)
  expect_true("Ref." %in% res$table_display[["HR (95% CI)"]])
  expect_null(res$model_stats)
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
