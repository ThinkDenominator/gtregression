# Load required packages
library(testthat)
library(gtregression)
library(MASS)
library(dplyr)
library(gtsummary)
# Sample dataset
data("PimaIndiansDiabetes2", package = "mlbench")

pima_data <- PimaIndiansDiabetes2 %>%
  mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>% # Convert outcome to numeric binary
  mutate(
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
  )

test_that("stratified_multi_reg returns a gtsummary tbl_merge object", {
  result <- stratified_multi_reg(
    data = pima_data,
    outcome = "diabetes",
    exposures = c("bmi", "age_cat", "npreg_cat"),
    stratifier = "glucose_cat",
    approach = "robpoisson"  # Removed glm
  )

  expect_s3_class(result, "tbl_merge")
  expect_true("gtsummary" %in% class(result))
})

test_that("stratified_multi_reg excludes NA values in stratifier", {
  pima_data$glucose_cat[1:5] <- NA  # Introduce NA values in stratifier

  result <- stratified_multi_reg(
    data = pima_data,
    outcome = "diabetes",
    exposures = c("bmi", "age_cat", "npreg_cat"),
    stratifier = "glucose_cat",
    approach = "robpoisson"  # Removed glm
  )

  expect_s3_class(result, "tbl_merge")  # Should still return valid output
})

test_that("stratified_multi_reg runs successfully with robpoisson", {
  result <- stratified_multi_reg(
    data = pima_data,
    outcome = "diabetes",
    exposures = c("bmi", "age_cat", "npreg_cat"),
    stratifier = "glucose_cat",
    approach = "robpoisson"
  )

  expect_s3_class(result, "tbl_merge")
})
