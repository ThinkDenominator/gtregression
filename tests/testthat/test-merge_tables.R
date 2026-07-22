birthwt_merge_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
      ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes"))
    )
}

test_that("merge_tables combines native gtregression objects", {
  df <- birthwt_merge_data()

  desc_tbl <- descriptive_table(
    data = df,
    exposures = c("age", "lwt", "race", "smoke", "ht"),
    by = "low",
    show_overall = "last",
    format = gt
  )
  uni_tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "lwt", "race", "smoke", "ht"),
    approach = logit,
    format = gt
  )

  merged <- merge_tables(
    desc_tbl,
    uni_tbl,
    spanners = c("Descriptive", "Univariable"),
    theme = shaded
  )

  expect_s3_class(merged, "gtregression")
  expect_s3_class(merged, "merged_table")
  expect_s3_class(merged, "gt_merge")
  expect_s3_class(merged$table, "gt_tbl")
  expect_equal(merged$spanners, c("Descriptive", "Univariable"))
  expect_equal(merged$engine, "gt")
  expect_equal(merged$part_sources, c("descriptive_table", "uni_reg"))
  expect_true(all(c("Characteristic", "is_header") %in% names(merged$table_display)))
  expect_true(any(trimws(merged$table_display$Characteristic) == "smoke"))
  expect_true(any(grepl("OR", names(merged$table_display), fixed = TRUE)))
  expect_true(any(grepl("percentages are by column", merged$footnotes)))
  expect_true(any(grepl("OR = Odds Ratio", merged$footnotes, fixed = TRUE)))
})

test_that("merge_tables aligns univariable and adjusted multivariable tables", {
  df <- birthwt_merge_data()

  uni_tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    approach = logit
  )
  multi_tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  merged <- merge_tables(uni_tbl, multi_tbl, spanners = c("Crude", "Adjusted"))

  expect_equal(merged$spanners, c("Crude", "Adjusted"))
  expect_equal(merged$part_sources, c("uni_reg", "multi_reg"))
  expect_true(any(trimws(merged$table_display$Characteristic) == "Yes"))
  expect_true(any(grepl("Adjusted.OR", names(merged$table_display), fixed = TRUE)))
  expect_true(any(grepl("Adjusted for age and lwt", merged$footnotes, fixed = TRUE)))
})

test_that("merge_tables supports three-table merges and default spanners", {
  df <- birthwt_merge_data()

  desc_tbl <- descriptive_table(df, exposures = c("age", "smoke"), by = "low")
  uni_tbl <- uni_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit)
  multi_tbl <- multi_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit)

  merged <- merge_tables(desc_tbl, uni_tbl, multi_tbl)

  expect_s3_class(merged, "merged_table")
  expect_equal(merged$spanners, c("Table 1", "Table 2", "Table 3"))
  expect_equal(merged$part_sources, c("descriptive_table", "uni_reg", "multi_reg"))
  expect_true(ncol(merged$table_display) > ncol(desc_tbl$table_display))
})

test_that("merge_tables supports flextable output", {
  skip_if_not_installed("flextable")

  df <- birthwt_merge_data()

  uni_tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )
  multi_tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  merged <- merge_tables(uni_tbl, multi_tbl, theme = striped)

  expect_s3_class(merged, "ft_merge")
  expect_s3_class(merged$table, "flextable")
  expect_equal(merged$engine, "flextable")
})

test_that("merge_tables validates inputs", {
  df <- birthwt_merge_data()

  uni_gt <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit)
  multi_gt <- multi_reg(df, outcome = "low", exposures = "smoke", approach = logit)

  expect_error(merge_tables(uni_gt), "at least two tables")
  expect_error(merge_tables(data.frame(x = 1), multi_gt), "All inputs must be outputs")
  expect_error(merge_tables(uni_gt, multi_gt, spanners = "Only one"), "Length of `spanners`")

  skip_if_not_installed("flextable")
  uni_ft <- uni_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    format = flextable
  )
  expect_error(merge_tables(uni_gt, uni_ft), "same engine")
})

test_that("merge_tables rejects malformed package-like inputs", {
  bad <- structure(
    list(
      table_display = data.frame(Characteristic = "age", is_header = TRUE),
      table = list()
    ),
    class = "gtregression"
  )

  good <- uni_reg(
    data = birthwt_merge_data(),
    outcome = "low",
    exposures = "age",
    approach = logit
  )

  expect_error(merge_tables(bad, good), "same engine")
})
