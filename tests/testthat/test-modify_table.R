test_that("modify_table() works correctly with pima_data", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")

  library(gtregression)
  library(gtsummary)
  library(dplyr)

  # Load and process data
  data("PimaIndiansDiabetes2", package = "gtregression")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
    mutate(bmi = case_when(
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
    age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
    npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
    npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
    glucose_cat = case_when(
      glucose < 140 ~ "Normal",
      glucose >= 140 ~ "High"
    ),
    glucose_cat = factor(glucose_cat, levels = c("Normal", "High")),
    bp_cat = case_when(
      pressure < 80 ~ "Normal",
      pressure >= 80 ~ "High"
    ),
    bp_cat = factor(bp_cat, levels = c("Normal", "High")),
    triceps_cat = case_when(
      triceps < 23 ~ "Normal",
      triceps >= 23 ~ "High"
    ),
    triceps_cat = factor(triceps_cat, levels = c("Normal", "High")),
    insulin_cat = case_when(
      insulin < 30 ~ "Low",
      insulin >= 30 & insulin < 150 ~ "Normal",
      insulin >= 150 ~ "High"
    ),
    insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High")),
    dpf_cat = case_when(
      pedigree <= 0.2 ~ "Low Genetic Risk",
      pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
      pedigree > 0.5 ~ "High Genetic Risk"
    ),
    dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))
    ) %>%
    mutate(diabetes_cat = case_when(diabetes == 1 ~ "Diabetes positive", TRUE ~ "Diabetes negative")) %>%
    mutate(diabetes_cat = factor(diabetes_cat, levels = c("Diabetes negative", "Diabetes positive")))

  # Define exposures
  exposures <- c("bmi", "age_cat", "npreg_cat", "bp_cat", "triceps_cat", "insulin_cat", "dpf_cat")

  # Run uni_reg
  uni_rr <- uni_reg(pima_data, outcome = "diabetes", exposures = exposures, approach = "log-binomial")

  # Modify the table
  tbl_custom <- modify_table(
    uni_rr,
    variable_labels = c(age_cat = "Age", bmi = "BMI"),
    level_labels = c(`Young` = "Young Adults", `Older` = "Older Adults"),
    header_labels = c(estimate = "Unadjusted RR", `p.value` = "P value"),
    caption = "Table 1: Univariate Regression",
    bold_labels = TRUE
  )

  # Check gtsummary object
  expect_s3_class(tbl_custom, "gtsummary")

  # Check variable labels updated
  labels <- tbl_custom$table_body$label[tbl_custom$table_body$row_type == "label"]
  expect_true("Age" %in% labels)
  expect_true("BMI" %in% labels)

  # Check level labels updated
  levels_updated <- tbl_custom$table_body$label[tbl_custom$table_body$row_type == "level"]
  expect_true("Young Adults" %in% levels_updated)
  expect_true("Older Adults" %in% levels_updated)

  # Check header labels updated
  header_info <- tbl_custom$table_styling$header

  expect_true("Unadjusted RR" %in% header_info$label)
  expect_true("P value" %in% header_info$label)
})
