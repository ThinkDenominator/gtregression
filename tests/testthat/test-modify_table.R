birthwt_modify_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
    )
}

test_that("modify_table updates package-native univariable gt tables", {
  df <- birthwt_modify_data()

  tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke", "ht"),
    approach = logit,
    format = gt
  )

  modified <- modify_table(
    tbl,
    variable_labels = c(age = "Maternal age", smoke = "Smoking", ht = "Hypertension"),
    level_labels = list(smoke = c(Yes = "Smoker"), ht = c(No = "No hypertension")),
    header_labels = c(estimate = "Crude OR", p.value = "P"),
    caption = "Table 1. Univariable regression",
    remove_N = TRUE,
    caveat = "Interpret with clinical context."
  )

  expect_s3_class(modified, "gtregression")
  expect_s3_class(modified, "uni_reg")
  expect_s3_class(modified$table, "gt_tbl")
  expect_false("N" %in% names(modified$table_display))
  expect_true("Maternal age" %in% modified$table_display$Characteristic)
  expect_true("Smoking" %in% modified$table_display$Characteristic)
  expect_true("  Smoker" %in% modified$table_display$Characteristic)
  expect_true("  No hypertension" %in% modified$table_display$Characteristic)
  expect_equal(modified$caption, "Table 1. Univariable regression")
  expect_true(any(grepl("Interpret with clinical context", modified$footnotes, fixed = TRUE)))
})

test_that("modify_table validates header aliases and preserves visible headers", {
  df <- birthwt_modify_data()
  tbl <- uni_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit, format = gt)

  modified <- modify_table(
    tbl,
    header_labels = c("OR (95% CI)" = "Odds ratio", "p-value" = "P value")
  )

  expect_s3_class(modified$table, "gt_tbl")
  expect_true("OR (95% CI)" %in% names(modified$table_display))
  expect_error(
    modify_table(tbl, header_labels = c(not_a_column = "Nope")),
    "columns not found"
  )
})

test_that("modify_table works for adjusted multivariable tables and footnote options", {
  df <- birthwt_modify_data()

  tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit,
    format = gt
  )

  keep_notes <- modify_table(tbl, caveat = "Adjusted model.")
  renamed <- modify_table(tbl, header_labels = c(estimate = "Adjusted OR", p.value = "P"))
  drop_notes <- modify_table(
    tbl,
    remove_abbreviations = TRUE,
    remove_N_obs = TRUE,
    caveat = "Adjusted model."
  )

  expect_s3_class(keep_notes, "multi_reg")
  expect_s3_class(renamed$table, "gt_tbl")
  expect_true(any(grepl("OR = Odds Ratio", keep_notes$footnotes, fixed = TRUE)))
  expect_true(any(grepl("Adjusted model", keep_notes$footnotes, fixed = TRUE)))
  expect_false(any(grepl("OR = Odds Ratio", drop_notes$footnotes, fixed = TRUE)))
  expect_false(any(grepl("complete observations included", drop_notes$footnotes, fixed = TRUE)))
})

test_that("modify_table works for descriptive and merged tables", {
  df <- birthwt_modify_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "smoke"),
    by = "low",
    show_overall = "last"
  )
  uni <- uni_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit)
  merged <- merge_tables(desc, uni, spanners = c("Descriptive", "Univariable"))

  desc_mod <- modify_table(
    desc,
    variable_labels = c(age = "Age", smoke = "Smoking"),
    level_labels = list(smoke = c(Yes = "Smoker"))
  )
  merged_mod <- modify_table(
    merged,
    variable_labels = c(age = "Age", smoke = "Smoking")
  )

  expect_s3_class(desc_mod, "descriptive_table")
  expect_s3_class(merged_mod, "merged_table")
  expect_true("Age" %in% desc_mod$table_display$Characteristic)
  expect_true("  Smoker" %in% desc_mod$table_display$Characteristic)
  expect_true("Smoking" %in% merged_mod$table_display$Characteristic)
})

test_that("modified input labels still align when tables are merged", {
  skip_if_not_installed("flextable")

  df <- birthwt_modify_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "smoke"),
    by = "low",
    show_overall = "last",
    format = flextable
  )
  desc <- modify_table(desc, variable_labels = c(smoke = "Smoking status"))

  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  merged <- merge_tables(desc, uni, spanners = c("Descriptive", "Crude"))
  merged <- modify_table(merged, variable_labels = c(smoke = "Smoking status"))

  expect_equal(sum(trimws(merged$table_display$Characteristic) == "Smoking status"), 1L)

  header_text <- vapply(
    merged$table$header$content$data,
    function(part) part$txt[1],
    character(1)
  )
  expect_false(any(grepl("_p[0-9]+$", header_text)))
  expect_true(all(c("Descriptive", "Crude", "Low BW", "OR (95% CI)", "p-value") %in%
                    header_text))
})

test_that("modify_table works for flextable outputs", {
  skip_if_not_installed("flextable")

  df <- birthwt_modify_data()
  tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  modified <- modify_table(
    tbl,
    variable_labels = c(smoke = "Smoking"),
    header_labels = c(estimate = "Crude OR"),
    bold_labels = TRUE,
    bold_levels = TRUE,
    caption = "Flextable regression"
  )

  expect_s3_class(modified, "ft_uni")
  expect_s3_class(modified$table, "flextable")
  expect_true("Smoking" %in% modified$table_display$Characteristic)
})

test_that("modify_table validates inputs clearly", {
  df <- birthwt_modify_data()
  tbl <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit)

  expect_error(modify_table(data.frame(x = 1)), "gtregression object")
  expect_error(modify_table(tbl, variable_labels = c("Smoking")), "variable_labels")
  expect_error(modify_table(tbl, level_labels = c(Yes = "Smoker")), "level_labels")
  expect_error(modify_table(tbl, level_labels = list(smoke = c("Smoker"))), "level_labels\\$smoke")
  expect_error(modify_table(tbl, bold_labels = NA), "`bold_labels` must be")
  expect_error(modify_table(tbl, bold_levels = 1), "`bold_levels` must be")
  expect_error(modify_table(tbl, remove_N = NA), "`remove_N` must be")
  expect_error(modify_table(tbl, remove_N_obs = c(TRUE, FALSE)), "`remove_N_obs` must be")
  expect_error(modify_table(tbl, remove_abbreviations = "yes"), "`remove_abbreviations` must be")
  expect_error(modify_table(tbl, caption = NA_character_), "`caption` must be")
  expect_error(modify_table(tbl, caveat = c("a", "b")), "`caveat` must be")
})
