test_that("merge_tables works with two compatible gtsummary tables", {
  skip_if_not_installed("gtsummary")
  library(gtsummary)
  data(trial)

  desc <- tbl_summary(trial, by = trt)
  uni <- tbl_uvregression(trial, method = lm, y = age, show_header = FALSE)
  merged <- merge_tables(desc, uni)

  expect_s3_class(merged, "tbl_merge")
  expect_true("Table 1" %in% merged$call_list$tbl_merge$tab_spanner)
})

test_that("merge_tables works with three gtsummary tables and custom spanners", {
  skip_if_not_installed("gtsummary")
  library(gtsummary)
  data(trial)

  desc <- tbl_summary(trial, by = trt)
  uni <- tbl_uvregression(trial, method = lm, y = age, show_header = FALSE)
  multi <- tbl_regression(lm(age ~ trt + grade, data = trial))

  merged <- merge_tables(desc, uni, multi, spanners = c("Summary", "Crude", "Adjusted"))
  expect_s3_class(merged, "tbl_merge")
  expect_equal(length(merged$call_list$tbl_merge$tbls), 3)
  expect_equal(merged$call_list$tbl_merge$tab_spanner, c("Summary", "Crude", "Adjusted"))
})

test_that("merge_tables throws error when fewer than 2 tables are passed", {
  skip_if_not_installed("gtsummary")
  library(gtsummary)
  data(trial)

  desc <- tbl_summary(trial, by = trt)
  expect_error(merge_tables(desc), "At least two gtsummary tables")
})

test_that("merge_tables throws error for non-gtsummary input", {
  skip_if_not_installed("gtsummary")
  library(gtsummary)
  data(trial)

  desc <- tbl_summary(trial, by = trt)
  expect_error(merge_tables(desc, mtcars), "must be gtsummary table objects")
})

test_that("merge_tables throws error when spanners length doesn't match number of tables", {
  skip_if_not_installed("gtsummary")
  library(gtsummary)
  data(trial)

  desc <- tbl_summary(trial, by = trt)
  uni <- tbl_uvregression(trial, method = lm, y = age, show_header = FALSE)

  expect_error(
    merge_tables(desc, uni, spanners = c("Only one")),
    "must match the number of tables"
  )
})
