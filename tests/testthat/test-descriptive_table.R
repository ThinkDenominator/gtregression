test_that("descriptive_table works with basic categorical inputs", {
  df <- data.frame(
    group = factor(c("A", "A", "B", "B")),
    status = factor(c("No", "Yes", "No", "Yes")),
    risk = factor(c("Low", "High", "High", NA), levels = c("Low", "High"))
  )

  tbl <- descriptive_table(
    data = df,
    exposures = c("status", "risk"),
    by = "group"
  )

  expect_s3_class(tbl, "descriptive_table")
  expect_s3_class(tbl$table, "gt_tbl")
  expect_equal(tbl$by, "group")
  expect_equal(tbl$levels, c("A", "B"))
  expect_equal(tbl$table_display$A[tbl$table_display$Characteristic == "  Yes"], "1 (50.0%)")
  expect_equal(tbl$table_display$B[tbl$table_display$Characteristic == "  Low"], "0 (0.0%)")
  expect_true("(Missing)" %in% trimws(tbl$table_display$Characteristic))
  expect_match(tbl$footnotes[1], "percentages are by column")
})

test_that("descriptive_table handles row percentages and overall position", {
  df <- data.frame(
    group = factor(c("A", "A", "B", "B", "B"), levels = c("A", "B")),
    status = factor(c("No", "Yes", "No", "No", "Yes"))
  )

  tbl <- descriptive_table(
    data = df,
    exposures = "status",
    by = "group",
    percent = "row",
    show_overall = "last"
  )

  expect_named(tbl$table_display, c("Characteristic", "is_header", "A", "B", "Overall"))
  expect_equal(tbl$table_display$A[tbl$table_display$Characteristic == "  No"], "1 (33.3%)")
  expect_equal(tbl$table_display$B[tbl$table_display$Characteristic == "  No"], "2 (66.7%)")
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "  No"], "3")
  expect_match(tbl$footnotes[1], "percentages are by row")
})

test_that("descriptive_table falls back to column percentages without by", {
  df <- data.frame(status = factor(c("No", "Yes", "No")))

  expect_warning(
    tbl <- descriptive_table(df, "status", percent = "rows"),
    "`percent = \"row\"` requires `by`"
  )

  expect_named(tbl$table_display, c("Characteristic", "is_header", "Overall"))
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "  No"], "2 (66.7%)")
})

test_that("descriptive_table handles dichotomous single-row values", {
  df <- data.frame(
    named = factor(c("No", "Yes", "No", "Yes")),
    formula = factor(c("Low", "High", "High", "Low"))
  )

  tbl_named <- descriptive_table(
    df,
    "named",
    show_dichotomous = "single_row",
    value = list(named = "Yes")
  )
  tbl_formula <- descriptive_table(
    df,
    "formula",
    show_dichotomous = "single_row",
    value = list(formula ~ "High")
  )

  expect_equal(tbl_named$table_display$Characteristic, c("named", "  Yes"))
  expect_equal(tbl_formula$table_display$Characteristic, c("formula", "  High"))
})

test_that("descriptive_table chooses sensible default single-row levels", {
  df <- data.frame(
    logical_var = c(TRUE, FALSE, TRUE),
    numeric_var = c(0, 1, 1),
    char_var = c("a", "b", "a")
  )

  tbl_logical <- descriptive_table(df, "logical_var", show_dichotomous = "single_row")
  tbl_numeric <- descriptive_table(df, "numeric_var", show_dichotomous = "single_row")
  tbl_char <- descriptive_table(df, "char_var", show_dichotomous = "single_row")

  expect_equal(tbl_logical$table_display$Characteristic, c("logical_var", "  TRUE"))
  expect_equal(tbl_numeric$table_display$Characteristic, c("numeric_var", "  1"))
  expect_equal(tbl_char$table_display$Characteristic, c("char_var", "  b"))
})

test_that("descriptive_table handles continuous summaries", {
  df <- data.frame(
    mean_var = c(1, 2, 3, 4),
    median_var = c(1, 2, 10, NA),
    mode_var = c(1, 1, 2, 3),
    count_var = c(1, 2, NA, 4),
    empty_var = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  tbl <- descriptive_table(
    df,
    c("mean_var", "median_var", "mode_var", "count_var", "empty_var"),
    statistic = c(
      mean_var = "mean",
      median_var = "median",
      mode_var = "mode",
      count_var = "count"
    )
  )

  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "mean_var"], "2.5 (1.3)")
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "median_var"], "2.0 (1.5-6.0)")
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "mode_var"], "1.0")
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "count_var"], "N = 3")
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "empty_var"], "")
  expect_match(tbl$footnotes[2], "Continuous summaries")
})

test_that("descriptive_table supports flextable and theme options", {
  skip_if_not_installed("flextable")

  df <- data.frame(
    group = factor(c("A", "A", "B")),
    status = factor(c("No", "Yes", "No"))
  )

  tbl <- descriptive_table(
    df,
    "status",
    by = "group",
    format = "flextable",
    theme = "striped",
    show_overall = "first"
  )

  expect_s3_class(tbl, "ft_desc")
  expect_s3_class(tbl$table, "flextable")
  expect_named(tbl$table_display, c("Characteristic", "is_header", "Overall", "A", "B"))
})

test_that("descriptive_table accepts custom theme primitives", {
  df <- data.frame(status = factor(c("No", "Yes", "No")))

  tbl <- descriptive_table(df, "status", theme = c("HEADER_SHADED", "compact"))

  expect_s3_class(tbl, "gt_desc")
  expect_equal(tbl$table_display$Overall[tbl$table_display$Characteristic == "  No"], "2 (66.7%)")
})

test_that("descriptive_table can hide missing rows", {
  df <- data.frame(status = factor(c("No", "Yes", NA)))

  tbl <- descriptive_table(df, "status", show_missing = "no")

  expect_false("(Missing)" %in% trimws(tbl$table_display$Characteristic))
})

test_that("descriptive_table gives clear errors for invalid inputs", {
  df <- data.frame(x = 1:3, y = 4:6, group = c("A", "B", "A"))

  expect_error(descriptive_table(df, character(0)), "`exposures` must be")
  expect_error(descriptive_table(df, 1), "`exposures` must be")
  expect_error(descriptive_table(df, "x", by = c("group", "y")), "`by` must be")
  expect_error(descriptive_table(df, "not_in_data"), "Variables not found")
  expect_error(descriptive_table(df, "x", digits = -1), "`digits` must be")
  expect_error(descriptive_table(df, "x", statistic = c(x = "range")), "Unsupported statistic")
  expect_error(descriptive_table(df, "x", statistic = c("mean")), "`statistic` must be")
})
