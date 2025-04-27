# Load required packages
library(gtregression)
library(testthat)
library(ggplot2)

# Upload and prepare Pima Indians Dataset
data("PimaIndiansDiabetes2", package = "gtregression")

pima_data <- PimaIndiansDiabetes2 %>%
  dplyr::mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) %>%
  dplyr::mutate(
    bmi = dplyr::case_when(
      mass < 25 ~ "Normal",
      mass >= 25 & mass < 30 ~ "Overweight",
      mass >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
    age_cat = dplyr::case_when(
      age < 30 ~ "Young",
      age >= 30 & age < 50 ~ "Middle-aged",
      age >= 50 ~ "Older"
    ),
    age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
    npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
    npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
    glucose_cat = dplyr::case_when(
      glucose < 140 ~ "Normal",
      glucose >= 140 ~ "High"
    ),
    glucose_cat = factor(glucose_cat, levels = c("Normal", "High")),
    bp_cat = dplyr::case_when(
      pressure < 80 ~ "Normal",
      pressure >= 80 ~ "High"
    ),
    bp_cat = factor(bp_cat, levels = c("Normal", "High")),
    triceps_cat = dplyr::case_when(
      triceps < 23 ~ "Normal",
      triceps >= 23 ~ "High"
    ),
    triceps_cat = factor(triceps_cat, levels = c("Normal", "High")),
    insulin_cat = dplyr::case_when(
      insulin < 30 ~ "Low",
      insulin >= 30 & insulin < 150 ~ "Normal",
      insulin >= 150 ~ "High"
    ),
    insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High")),
    dpf_cat = dplyr::case_when(
      pedigree <= 0.2 ~ "Low Genetic Risk",
      pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
      pedigree > 0.5 ~ "High Genetic Risk"
    ),
    dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk", "Moderate Genetic Risk", "High Genetic Risk"))
  )

# Define exposures
exposures <- c(
  "bmi", "age_cat", "npreg_cat", "glucose_cat",
  "bp_cat", "triceps_cat", "insulin_cat", "dpf_cat"
)

# Fit models
uni_rr_robpos <- uni_reg(pima_data, outcome = "diabetes", exposures = exposures, approach = "robpoisson")
multi_rr_full_model <- multi_reg(pima_data, outcome = "diabetes", exposures = exposures, approach = "robpoisson")

# ---------- Start Tests ----------

test_that("plot_reg works with default settings", {
  p <- plot_reg(uni_rr_robpos)
  expect_s3_class(p, "ggplot")
})

test_that("plot_reg works with custom y-order and log-scale", {
  p <- plot_reg(
    tbl = uni_rr_robpos,
    order_y = exposures,
    log_x = TRUE,
    show_ref = TRUE
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_reg_combine works with default settings", {
  p <- plot_reg_combine(
    tbl_uni = uni_rr_robpos,
    tbl_multi = multi_rr_full_model
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_reg_combine works with custom y-order and log-scale", {
  p <- plot_reg_combine(
    tbl_uni = uni_rr_robpos,
    tbl_multi = multi_rr_full_model,
    order_y = exposures,
    log_x = TRUE,
    show_ref = TRUE
  )
  expect_s3_class(p, "ggplot")
})
