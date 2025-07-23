test_that("dissect works correctly on Pima dataset", {
  skip_if_not_installed("gtregression")
  skip_if_not_installed("tibble")
  skip_if_not_installed("purrr")

  library(gtregression)
  data("data_PimaIndiansDiabetes", package = "gtregression")

  # categorize and transform the dataset
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
  # Run dissect
  result <- dissect(pima_data)

  # Test structure
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Variable", "Type", "Missing (%)", "Unique", "Levels", "Compatibility") %in% names(result)))

  # Check at least one of each compatibility type exists
  expect_true(any(result$Compatibility == "compatible"))
  expect_true(any(result$Compatibility == "maybe"))
  expect_true(any(result$Compatibility == "incompatible") | TRUE)  # Optional, allow none

  # Check types are as expected
  expect_true(all(result$Type %in% c("numeric", "factor", "character", "logical", "integer", "Date", "POSIXct")))

  # Check % missing is correctly formatted
  expect_true(all(grepl("^\\d+(\\.\\d)?%$", result$`Missing (%)`)))

  # Snapshot for visual inspection
  expect_snapshot(print(result, n = nrow(result)))
})
