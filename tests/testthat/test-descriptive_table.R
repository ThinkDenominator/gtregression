test_that("descriptive_table works with basic inputs", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("dplyr")

  data("data_PimaIndiansDiabetes", package = "gtregression")

  pima_data <- data_PimaIndiansDiabetes |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |> # Convert outcome to numeric b                                                                                                                                                                                                                                                                     inary
    mutate(bmi = case_when(
      mass < 25 ~ "Normal",
      mass >= 25 & mass < 30 ~ "Overweight",
      mass >= 30 ~ "Obese",
      TRUE ~ NA_character_),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
      glucose_cat= case_when(glucose<=140~ "Normal", glucose>140~"High"),
      glucose_cat= factor(glucose_cat, levels = c("Normal", "High")),
      bp_cat = case_when(
        pressure < 80 ~ "Normal",
        pressure >= 80 ~ "High"
      ),
      bp_cat= factor(bp_cat, levels = c("Normal", "High")),
      triceps_cat = case_when(
        triceps < 23 ~ "Normal",
        triceps >= 23 ~ "High"
      ),
      triceps_cat= factor(triceps_cat, levels = c("Normal", "High")),
      insulin_cat = case_when(
        insulin < 30 ~ "Low",
        insulin >= 30 & insulin < 150 ~ "Normal",
        insulin >= 150 ~ "High"
      ),
      insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High"))
    ) |>
    mutate(
      dpf_cat = case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
        pedigree > 0.5 ~ "High Genetic Risk"
      )
    ) |>
    mutate(dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))) |>
    mutate(diabetes_cat= case_when(diabetes== 1~ "Diabetes positive", TRUE~ "Diabetes negative")) |>
    mutate(diabetes_cat= factor(diabetes_cat, levels = c("Diabetes negative","Diabetes positive" )))

  expect_warning(
    tbl <- descriptive_table(
      data = pima_data,
      exposures = c("bmi", "age_cat", "npreg_cat", "bp_cat", "triceps_cat",
                    "insulin_cat", "dpf_cat"),
      by = "diabetes_cat"
    ),
    regexp= NA
  )

  expect_s3_class(tbl, "descriptive_table")
})

test_that("descriptive_table works with row percentages", {
  data("data_PimaIndiansDiabetes", package = "gtregression")

  pima_data <- data_PimaIndiansDiabetes |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |> # Convert outcome to numeric b                                                                                                                                                                                                                                                                     inary
    mutate(bmi = case_when(
      mass < 25 ~ "Normal",
      mass >= 25 & mass < 30 ~ "Overweight",
      mass >= 30 ~ "Obese",
      TRUE ~ NA_character_),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
      glucose_cat= case_when(glucose<=140~ "Normal", glucose>140~"High"),
      glucose_cat= factor(glucose_cat, levels = c("Normal", "High")),
      bp_cat = case_when(
        pressure < 80 ~ "Normal",
        pressure >= 80 ~ "High"
      ),
      bp_cat= factor(bp_cat, levels = c("Normal", "High")),
      triceps_cat = case_when(
        triceps < 23 ~ "Normal",
        triceps >= 23 ~ "High"
      ),
      triceps_cat= factor(triceps_cat, levels = c("Normal", "High")),
      insulin_cat = case_when(
        insulin < 30 ~ "Low",
        insulin >= 30 & insulin < 150 ~ "Normal",
        insulin >= 150 ~ "High"
      ),
      insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High"))
    ) |>
    mutate(
      dpf_cat = case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
        pedigree > 0.5 ~ "High Genetic Risk"
      )
    ) |>
    mutate(dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))) |>
    mutate(diabetes_cat= case_when(diabetes== 1~ "Diabetes positive", TRUE~ "Diabetes negative")) |>
    mutate(diabetes_cat= factor(diabetes_cat, levels = c("Diabetes negative","Diabetes positive" )))

  expect_error(
    tbl <- descriptive_table(
      data = pima_data,
      exposures = c("bmi", "age_cat", "npreg_cat", "bp_cat", "triceps_cat"),
      by = "diabetes_cat",
      percent = "row",
      show_overall = "last"
    ),
    regex= NA
  )
  expect_s3_class(tbl, "descriptive_table")
})

test_that("descriptive_table handles single_row correctly", {
  data("data_PimaIndiansDiabetes", package = "gtregression")

  pima_data <- data_PimaIndiansDiabetes |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |> # Convert outcome to numeric b                                                                                                                                                                                                                                                                     inary
    mutate(bmi = case_when(
      mass < 25 ~ "Normal",
      mass >= 25 & mass < 30 ~ "Overweight",
      mass >= 30 ~ "Obese",
      TRUE ~ NA_character_),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
      glucose_cat= case_when(glucose<=140~ "Normal", glucose>140~"High"),
      glucose_cat= factor(glucose_cat, levels = c("Normal", "High")),
      bp_cat = case_when(
        pressure < 80 ~ "Normal",
        pressure >= 80 ~ "High"
      ),
      bp_cat= factor(bp_cat, levels = c("Normal", "High")),
      triceps_cat = case_when(
        triceps < 23 ~ "Normal",
        triceps >= 23 ~ "High"
      ),
      triceps_cat= factor(triceps_cat, levels = c("Normal", "High")),
      insulin_cat = case_when(
        insulin < 30 ~ "Low",
        insulin >= 30 & insulin < 150 ~ "Normal",
        insulin >= 150 ~ "High"
      ),
      insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High"))
    ) |>
    mutate(
      dpf_cat = case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
        pedigree > 0.5 ~ "High Genetic Risk"
      )
    ) |>
    mutate(dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))) |>
    mutate(diabetes_cat= case_when(diabetes== 1~ "Diabetes positive", TRUE~ "Diabetes negative")) |>
    mutate(diabetes_cat= factor(diabetes_cat, levels = c("Diabetes negative","Diabetes positive" )))

  tbl <- suppressWarnings(descriptive_table(
    data = pima_data,
    exposures = c("bp_cat", "triceps_cat"),
    by = "diabetes_cat",
    show_dichotomous = "single_row",
    value = list(bp_cat ~ "High", triceps_cat ~ "High")

  ))

  expect_s3_class(tbl, "descriptive_table")
})

test_that("descriptive_table errors with missing variable", {
  data("data_PimaIndiansDiabetes", package = "gtregression")

  pima_data <- data_PimaIndiansDiabetes |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |> # Convert outcome to numeric b                                                                                                                                                                                                                                                                     inary
    mutate(bmi = case_when(
      mass < 25 ~ "Normal",
      mass >= 25 & mass < 30 ~ "Overweight",
      mass >= 30 ~ "Obese",
      TRUE ~ NA_character_),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
      glucose_cat= case_when(glucose<=140~ "Normal", glucose>140~"High"),
      glucose_cat= factor(glucose_cat, levels = c("Normal", "High")),
      bp_cat = case_when(
        pressure < 80 ~ "Normal",
        pressure >= 80 ~ "High"
      ),
      bp_cat= factor(bp_cat, levels = c("Normal", "High")),
      triceps_cat = case_when(
        triceps < 23 ~ "Normal",
        triceps >= 23 ~ "High"
      ),
      triceps_cat= factor(triceps_cat, levels = c("Normal", "High")),
      insulin_cat = case_when(
        insulin < 30 ~ "Low",
        insulin >= 30 & insulin < 150 ~ "Normal",
        insulin >= 150 ~ "High"
      ),
      insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High"))
    ) |>
    mutate(
      dpf_cat = case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
        pedigree > 0.5 ~ "High Genetic Risk"
      )
    ) |>
    mutate(dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))) |>
    mutate(diabetes_cat= case_when(diabetes== 1~ "Diabetes positive", TRUE~ "Diabetes negative")) |>
    mutate(diabetes_cat= factor(diabetes_cat, levels = c("Diabetes negative","Diabetes positive" )))

  expect_error(
    descriptive_table(data = pima_data, exposures = c("not_in_data")),
    "not found in the data"
  )
})
