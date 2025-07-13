test_that("modify_table() works correctly with pima_data", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")

  library(gtregression)
  library(gtsummary)
  library(dplyr)

  data("data_PimaIndiansDiabetes", package = "gtregression")

  pima_data <- data_PimaIndiansDiabetes |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese",
        TRUE ~ NA_character_
      ),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older"))
    )

  exposures <- c("bmi", "age_cat")

  uni_rr <- uni_reg(pima_data,
                    outcome = "diabetes",
                    exposures = exposures,
                    approach = "log-binomial")

  tbl_custom <- suppressWarnings(
    modify_table(
    uni_rr,
    variable_labels = c(age_cat = "Age", bmi = "BMI"),
    level_labels = list(age_cat = c(
      `Young` = "Young Adults",
      `Older` = "Older Adults"
    )),
    header_labels = c(estimate = "Unadjusted RR", `p.value` = "P value"),
    caption = "Table 1: Univariate Regression",
    bold_labels = TRUE,
    remove_N = TRUE,
    remove_abbreviations = TRUE,
    caveat = "Interpret results with caution due to missing data."
  )
  )

  expect_s3_class(tbl_custom, "gtsummary")

  labels <- tbl_custom$table_body$label[tbl_custom$table_body$row_type
                                        == "label"]
  expect_true("Age" %in% labels)
  expect_true("BMI" %in% labels)

  levels_updated <- tbl_custom$table_body$label[tbl_custom$table_body$row_type
                                                == "level"]
  expect_true("Young Adults" %in% levels_updated)
  expect_true("Older Adults" %in% levels_updated)

  header_info <- tbl_custom$table_styling$header
  expect_true("Unadjusted RR" %in% header_info$label)
  expect_true("P value" %in% header_info$label)

  # Check that column 'stat_n' is hidden (N column removed)
  expect_true("stat_n" %in% names(tbl_custom$table_body))
  expect_true("N" %in% names(tbl_custom$table_body))

  # Check if caveat added to source note
  caveat_text <- tbl_custom$table_styling$source_note$source_note
  expect_true(any(grepl("Interpret results with caution", caveat_text)))

  # Check that abbreviation footnote has been removed
  abbrev_removed <- is.null(tbl_custom$table_styling$source_note_abbreviation)
  expect_true(abbrev_removed)
})
