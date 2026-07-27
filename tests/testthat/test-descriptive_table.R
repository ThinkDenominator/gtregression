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
  expect_s3_class(tbl$table, "flextable")
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

  expect_equal(tbl_named$table_display$Characteristic, "named")
  expect_equal(tbl_named$table_display$Overall, "2 (50.0%)")
  expect_equal(tbl_formula$table_display$Characteristic, "formula")
  expect_equal(tbl_formula$table_display$Overall, "2 (50.0%)")
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

  expect_equal(tbl_logical$table_display$Characteristic, "logical_var")
  expect_equal(tbl_numeric$table_display$Characteristic, "numeric_var")
  expect_equal(tbl_char$table_display$Characteristic, "char_var")
  expect_equal(tbl_logical$table_display$Overall, "2 (66.7%)")
  expect_equal(tbl_numeric$table_display$Overall, "2 (66.7%)")
  expect_equal(tbl_char$table_display$Overall, "1 (33.3%)")
})

test_that("descriptive_table single-row dichotomous variables are compact and ordered", {
  df <- data.frame(
    group = factor(c("A", "A", "B", "B")),
    smoke = factor(c("No", "Yes", "No", "Yes"), levels = c("No", "Yes")),
    ht = factor(c("No", "No", "Yes", "Yes"), levels = c("No", "Yes"))
  )

  tbl <- descriptive_table(
    df,
    exposures = c("smoke", "ht"),
    by = "group",
    show_dichotomous = "single_row",
    value = list(smoke = "Yes", ht = "Yes"),
    show_overall = "last",
    percent = "row"
  )

  expect_equal(tbl$table_display$Characteristic, c("smoke", "ht"))
  expect_true(all(tbl$table_display$is_header))
  expect_false(any(grepl("^\\s", tbl$table_display$Characteristic)))
  expect_equal(tbl$table_display$A, c("1 (50.0%)", "0 (0.0%)"))
  expect_equal(tbl$table_display$B, c("1 (50.0%)", "2 (100.0%)"))
  expect_equal(tbl$table_display$Overall, c("2", "2"))
})

test_that("descriptive_table can treat numeric ordinal variables as categorical", {
  df <- data.frame(
    group = factor(c("A", "A", "B", "B", "B")),
    ordinal_score = c(0, 1, 0, 2, 2),
    continuous_score = c(10, 12, 15, 18, 21)
  )

  default_tbl <- descriptive_table(
    df,
    exposures = c("ordinal_score", "continuous_score"),
    by = "group",
    show_overall = "last"
  )
  ordinal_tbl <- descriptive_table(
    df,
    exposures = c("ordinal_score", "continuous_score"),
    by = "group",
    statistic = c(ordinal_score = categorical),
    show_overall = "last"
  )
  ordinal_tbl_quoted <- descriptive_table(
    df,
    exposures = c("ordinal_score", "continuous_score"),
    by = "group",
    statistic = c(ordinal_score = "categorical", continuous_score = "mean"),
    show_overall = "last"
  )
  ordinal_tbl_typo <- descriptive_table(
    df,
    exposures = c("ordinal_score", "continuous_score"),
    by = "group",
    statistic = c(ordinal_score = catagorical),
    show_overall = "last"
  )

  expect_equal(
    default_tbl$table_display$Overall[
      default_tbl$table_display$Characteristic == "ordinal_score"
    ],
    "1.0 (0.0-2.0)"
  )
  expect_true("ordinal_score" %in% ordinal_tbl$table_display$Characteristic)
  expect_true(all(c("  0", "  1", "  2") %in% ordinal_tbl$table_display$Characteristic))
  expect_equal(
    ordinal_tbl$table_display$Overall[
      ordinal_tbl$table_display$Characteristic == "  2"
    ],
    "2 (40.0%)"
  )
  expect_equal(
    ordinal_tbl_quoted$table_display$Overall[
      ordinal_tbl_quoted$table_display$Characteristic == "  2"
    ],
    "2 (40.0%)"
  )
  expect_equal(
    ordinal_tbl_typo$table_display$Overall[
      ordinal_tbl_typo$table_display$Characteristic == "  2"
    ],
    "2 (40.0%)"
  )
  expect_equal(
    ordinal_tbl_quoted$table_display$Overall[
      ordinal_tbl_quoted$table_display$Characteristic == "continuous_score"
    ],
    "15.2 (4.4)"
  )
  expect_match(ordinal_tbl$footnotes[2], "Continuous variables shown as Median")
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

  mean_all <- descriptive_table(
    df,
    c("mean_var", "median_var"),
    statistic = mean
  )
  expect_equal(mean_all$table_display$Overall[mean_all$table_display$Characteristic == "mean_var"], "2.5 (1.3)")
  expect_match(mean_all$footnotes[2], "Continuous variables shown as Mean")
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

  body_bottom_borders <- tbl$table$body$styles$cells$border.width.bottom$data
  expect_true(all(body_bottom_borders[1, ] == 0))
  expect_true(all(body_bottom_borders[nrow(body_bottom_borders), ] > 0))
})

test_that("descriptive_table accepts custom theme primitives", {
  df <- data.frame(status = factor(c("No", "Yes", "No")))

  tbl <- descriptive_table(df, "status", theme = c("HEADER_SHADED", "compact"))

  expect_s3_class(tbl, "ft_desc")
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
  expect_error(descriptive_table(df, "x", statistic = c("mean", "median")), "`statistic` must be")
  expect_error(descriptive_table(df, "x", statistic = c(y = categorical)), "must also be included")
})
