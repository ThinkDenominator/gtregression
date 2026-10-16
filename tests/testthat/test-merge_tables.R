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

flextable_header_text <- function(x) {
  # version-independent reader (the header storage differs between
  # flextable releases)
  as.vector(.flex_part_text(x, "header"))
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
    theme = shaded,
    format = gt
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

  gt_labels <- as.character(merged$table$`_boxhead`$column_label)
  expect_false(any(grepl("_p[0-9]+$", gt_labels)))
  expect_true(all(c("N", "OR (95% CI)", "p-value") %in% gt_labels))
  expect_true(any(grepl("^Normal BW, N=", gt_labels)))
  expect_true(any(grepl("^Low BW, N=", gt_labels)))
  expect_true(any(grepl("^Overall, N=", gt_labels)))
})

test_that("merge_tables preserves descriptive sample sizes in headers", {
  df <- birthwt_merge_data()
  desc_tbl <- descriptive_table(
    data = df,
    exposures = c("age", "race"),
    by = "low",
    format = flextable
  )
  uni_tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "race"),
    approach = logit,
    format = flextable
  )

  merged <- merge_tables(
    desc_tbl,
    uni_tbl,
    spanners = c("Descriptive", "Univariable")
  )

  descriptive_labels <- unname(merged$column_labels[[1]])
  regression_labels <- unname(merged$column_labels[[2]])

  expect_equal(descriptive_labels, unname(desc_tbl$header_labels))
  expect_true(all(grepl(", N=[0-9]+$", descriptive_labels)))
  expect_false(any(grepl(", N=", regression_labels, fixed = TRUE)))

  header_text <- flextable_header_text(merged$table)
  expect_true(any(grepl("^Normal BW, N=", header_text)))
  expect_true(any(grepl("^Low BW, N=", header_text)))
})

test_that("merge_tables preserves the overall N header", {
  df <- birthwt_merge_data()
  desc_tbl <- descriptive_table(
    data = df,
    exposures = c("age", "race"),
    format = flextable
  )
  uni_tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "race"),
    approach = logit,
    format = flextable
  )

  merged <- merge_tables(desc_tbl, uni_tbl)
  header_text <- flextable_header_text(merged$table)

  expect_true(any(grepl("^Overall, N=[0-9]+$", header_text)))
})

test_that("merge_tables defaults to flextable independently of input format", {
  skip_if_not_installed("flextable")

  df <- birthwt_merge_data()
  uni_gt <- uni_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    format = gt
  )
  multi_gt <- multi_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    format = gt
  )

  merged <- merge_tables(uni_gt, multi_gt)

  expect_equal(merged$format, "flextable")
  expect_s3_class(merged, "ft_merge")
  expect_s3_class(merged$table, "flextable")
})

test_that("merge_tables aligns univariable and adjusted multivariable tables", {
  df <- birthwt_merge_data()

  exposures <- c("ui", "smoke", "ht")
  uni_tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = exposures,
    approach = logit
  )
  multi_tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = exposures,
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  merged <- merge_tables(uni_tbl, multi_tbl, spanners = c("Crude", "Adjusted"))

  expect_equal(merged$spanners, c("Crude", "Adjusted"))
  expect_equal(merged$part_sources, c("uni_reg", "multi_reg"))
  expect_true(any(trimws(merged$table_display$Characteristic) == "Yes"))
  expect_true(any(grepl("Adjusted.OR", names(merged$table_display), fixed = TRUE)))
  expect_true(any(grepl("Adjusted for age and lwt", merged$footnotes, fixed = TRUE)))
  expect_equal(
    trimws(merged$table_display$Characteristic[merged$table_display$is_header]),
    exposures
  )
})

test_that("merge_tables carries stored multivariable footnotes unchanged", {
  df <- birthwt_merge_data()
  attr(df$age, "label") <- "Maternal age"
  attr(df$lwt, "label") <- "Maternal weight"
  uni_tbl <- uni_reg(df, outcome = "low", exposures = c("smoke", "ht"), approach = logit)
  multi_tbl <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )
  merged <- merge_tables(uni_tbl, multi_tbl, spanners = c("Crude", "Adjusted"))

  expect_true(any(multi_tbl$footnotes == "Adjusted for Maternal age and Maternal weight"))
  expect_true(all(multi_tbl$footnotes %in% merged$footnotes))
  expect_equal(
    merged$footnotes[grepl("^Adjusted for ", merged$footnotes)],
    multi_tbl$footnotes[grepl("^Adjusted for ", multi_tbl$footnotes)]
  )
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

test_that("merged flextables do not draw a rule after the first exposure", {
  skip_if_not_installed("flextable")

  df <- birthwt_merge_data()
  attr(df$age, "label") <- "Maternal age"
  attr(df$lwt, "label") <- "Maternal weight"
  attr(df$race, "label") <- "Maternal race"

  desc_tbl <- descriptive_table(
    df,
    exposures = c("age", "lwt", "race"),
    by = "low"
  )
  uni_tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "race"),
    approach = logit
  )
  multi_tbl <- multi_reg(
    df,
    outcome = "low",
    exposures = c("lwt", "race"),
    adjust_for = "age",
    approach = logit
  )

  merged <- merge_tables(desc_tbl, uni_tbl, multi_tbl)
  borders <- merged$table$body$styles$cells
  bottom <- borders$border.width.bottom$data
  header <- merged$table$header$styles$cells
  header_top <- header$border.width.top$data
  header_bottom <- header$border.width.bottom$data

  expect_true(all(bottom[1, ] == 0))
  expect_true(all(bottom[nrow(bottom), ] > 0))
  expect_true(all(header_top[1, ] > 0))
  expect_true(all(header_bottom[nrow(header_bottom), ] > 0))
})

test_that("merged tables align reference and comparison levels", {
  df <- birthwt_merge_data()
  attr(df$smoke, "label") <- "Death"

  desc_tbl <- suppressMessages(
    descriptive_table(
      df,
      exposures = c("smoke", "race"),
      by = "low",
      show_dichotomous = "all_levels"
    )
  )
  uni_tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "race"),
    approach = logit,
    show_ref = TRUE
  )
  multi_tbl <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "race"),
    approach = logit,
    show_ref = TRUE
  )

  merged <- merge_tables(desc_tbl, uni_tbl, multi_tbl)
  level_labels <- merged$table_display$Characteristic[
    !merged$table_display$is_header
  ]

  expect_true(all(grepl("^  ", level_labels)))
  expect_true(all(c("No", "Yes") %in% trimws(level_labels)))
})

test_that("merge_tables supports multiple adjusted sensitivity models", {
  df <- birthwt_merge_data()
  primary <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  sensitivity_data <- df
  sensitivity_data$low[c(3, 8, 12)] <- NA
  sensitivity_data$low <- factor(
    ifelse(
      is.na(sensitivity_data$low),
      "Low BW",
      as.character(sensitivity_data$low)
    ),
    levels = levels(df$low)
  )
  sensitivity <- multi_reg(
    sensitivity_data,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  merged <- merge_tables(
    primary,
    sensitivity,
    spanners = c("Primary analysis", "Worst-case sensitivity")
  )

  expect_equal(
    merged$spanners,
    c("Primary analysis", "Worst-case sensitivity")
  )
  expect_equal(merged$part_sources, c("multi_reg", "multi_reg"))
  expect_equal(
    trimws(merged$table_display$Characteristic[merged$table_display$is_header]),
    c("smoke", "ht", "ui")
  )
  ui_row <- which(
    trimws(merged$table_display$Characteristic) == "ui" &
      merged$table_display$is_header
  )
  second_model_cols <- grep("_p2$", names(merged$table_display), value = TRUE)
  expect_true(all(merged$table_display[ui_row, second_model_cols] == ""))
  expect_equal(
    sum(grepl("^Adjusted for age and lwt", merged$footnotes)),
    1L
  )
})

test_that("merge_tables supports multiple univariable scenario tables", {
  df <- birthwt_merge_data()
  scenario_a <- uni_reg(
    df, outcome = "low", exposures = c("age", "smoke"), approach = logit
  )
  scenario_b <- uni_reg(
    df, outcome = "low", exposures = c("smoke", "ht"), approach = logit
  )
  scenario_c <- uni_reg(
    df, outcome = "low", exposures = c("age", "ui"), approach = logit
  )

  merged <- merge_tables(
    scenario_a,
    scenario_b,
    scenario_c,
    spanners = c("Observed", "Best case", "Worst case")
  )

  expect_equal(merged$part_sources, rep("uni_reg", 3))
  expect_equal(
    trimws(merged$table_display$Characteristic[merged$table_display$is_header]),
    c("age", "smoke", "ht", "ui")
  )
  expect_equal(merged$spanners, c("Observed", "Best case", "Worst case"))
})

test_that("merge_tables warns for mixed binary rows in descriptive-regression merges", {
  df <- birthwt_merge_data()

  desc_tbl <- descriptive_table(
    df,
    exposures = "smoke",
    by = "low",
    show_dichotomous = "all_levels"
  )
  uni_compact <- suppressMessages(uni_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    show_ref = FALSE
  ))
  multi_compact <- suppressMessages(multi_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    show_ref = FALSE
  ))

  expect_warning(
    suppressMessages(merge_tables(desc_tbl, uni_compact, multi_compact)),
    "show_dichotomous = \"all_levels\".*show_ref = TRUE"
  )

  uni_expanded <- uni_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    show_ref = TRUE
  )
  multi_expanded <- multi_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    show_ref = TRUE
  )

  expect_no_warning(suppressMessages(
    merge_tables(desc_tbl, uni_expanded, multi_expanded)
  ))
})

test_that("merge_tables aligns compact binary regression rows to descriptive levels", {
  df <- birthwt_merge_data()

  desc_tbl <- descriptive_table(
    df,
    exposures = c("age", "smoke"),
    by = "low",
    format = gt
  )
  uni_tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = gt
  )

  compact_uni <- uni_tbl
  display <- uni_tbl$table_display
  body <- uni_tbl$table_body

  smoke_header <- which(display$Characteristic == "smoke" & display$is_header)[1]
  smoke_yes <- which(trimws(display$Characteristic) == "Yes" & !display$is_header)[1]
  age_row <- which(display$Characteristic == "age" & display$is_header)[1]
  effect_cols <- setdiff(names(display), c("Characteristic", "is_header"))

  display[smoke_header, effect_cols] <- display[smoke_yes, effect_cols]
  compact_uni$table_display <- display[c(age_row, smoke_header), , drop = FALSE]
  compact_uni$table_body <- body[
    body$exposure == "age" | (body$exposure == "smoke" & body$level == "Yes"),
    ,
    drop = FALSE
  ]

  merged <- NULL
  expect_warning(
    expect_message(
      merged <- merge_tables(
        desc_tbl,
        compact_uni,
        spanners = c("Descriptive", "Crude")
      ),
      "show_ref = TRUE"
    ),
    "binary display mode mismatch"
  )

  out <- merged$table_display
  or_cols <- grep("OR", names(out), value = TRUE)
  p_cols <- grep("p.value", names(out), value = TRUE)
  smoke_header_row <- which(trimws(out$Characteristic) == "smoke" & (out$is_header %in% TRUE))
  smoke_yes_row <- which(trimws(out$Characteristic) == "Yes" & (out$is_header %in% FALSE))

  expect_length(smoke_header_row, 1L)
  expect_length(smoke_yes_row, 1L)
  expect_equal(out[smoke_header_row, or_cols], "")
  expect_equal(out[smoke_header_row, p_cols], "")
  expect_true(any(nzchar(unlist(out[smoke_yes_row, or_cols], use.names = FALSE))))
  expect_false(any(trimws(out$Characteristic) == "smoke" & (out$is_header %in% FALSE)))
})

test_that("merge_tables keeps compact descriptive and regression binary rows aligned", {
  df <- birthwt_merge_data()

  desc_tbl <- suppressMessages(
    descriptive_table(
      df,
      exposures = c("age", "smoke"),
      by = "low",
      show_dichotomous = "single_row",
      value = list(smoke = "Yes"),
      format = gt
    )
  )
  uni_tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = gt
  )

  compact_uni <- uni_tbl
  display <- uni_tbl$table_display
  body <- uni_tbl$table_body

  smoke_header <- which(display$Characteristic == "smoke" & display$is_header)[1]
  smoke_yes <- which(trimws(display$Characteristic) == "Yes" & !display$is_header)[1]
  age_row <- which(display$Characteristic == "age" & display$is_header)[1]
  effect_cols <- setdiff(names(display), c("Characteristic", "is_header"))

  display[smoke_header, effect_cols] <- display[smoke_yes, effect_cols]
  compact_uni$table_display <- display[c(age_row, smoke_header), , drop = FALSE]
  compact_uni$table_body <- body[
    body$exposure == "age" | (body$exposure == "smoke" & body$level == "Yes"),
    ,
    drop = FALSE
  ]

  merged <- NULL
  expect_no_message(
    merged <- merge_tables(desc_tbl, compact_uni, spanners = c("Descriptive", "Crude"))
  )

  out <- merged$table_display
  smoke_rows <- which(trimws(out$Characteristic) == "smoke")
  or_cols <- grep("OR", names(out), value = TRUE)

  expect_length(smoke_rows, 1L)
  expect_true(out$is_header[smoke_rows])
  expect_true(any(nzchar(unlist(out[smoke_rows, or_cols], use.names = FALSE))))
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

  header_text <- flextable_header_text(merged$table)
  expect_false(any(grepl("_p[0-9]+$", header_text)))
  expect_true(all(c("N", "OR (95% CI)", "p-value", "Adjusted OR (95% CI)") %in%
                    header_text))
})

test_that("merge_tables validates inputs", {
  df <- birthwt_merge_data()

  uni_gt <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit, format = gt)
  multi_gt <- multi_reg(df, outcome = "low", exposures = "smoke", approach = logit, format = gt)

  expect_error(merge_tables(uni_gt), "at least two tables")
  expect_error(merge_tables(data.frame(x = 1), multi_gt), "All inputs must be outputs")
  expect_error(merge_tables(uni_gt, multi_gt, spanners = "Only one"), "Length of `spanners`")
  expect_error(
    merge_tables(uni_gt, multi_gt, spanners = c("Primary", "")),
    "non-empty character label"
  )
  expect_error(
    merge_tables(uni_gt, multi_gt, spanners = c("Primary", NA_character_)),
    "non-empty character label"
  )

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
