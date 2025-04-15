library(testthat)
library(gtregression)
library(dplyr)
library(mlbench)
library(MASS)

test_that("select_models works for valid approaches and directions", {
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = factor(case_when(mass < 25 ~ "Normal",
                             mass >= 25 & mass < 30 ~ "Overweight",
                             mass >= 30 ~ "Obese")),
      age_cat = factor(case_when(age < 30 ~ "Young",
                                 age < 50 ~ "Middle-aged",
                                 TRUE ~ "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High", "Low")),
      glucose_cat = factor(ifelse(glucose >= 140, "High", "Normal")),
      dpf_cat = factor(ifelse(pedigree >= 0.5, "High", "Low")),
      insulin_cat = factor(ifelse(insulin >= 100, "High", "Normal"))
    )

  outcome <- "diabetes"
  exposures <- c("bmi", "age_cat", "npreg_cat", "glucose_cat", "insulin_cat", "dpf_cat")

  approaches <- c("logit", "robpoisson")
  directions <- c("forward", "backward", "both")

  for (appr in approaches) {
    for (dir in directions) {
      result <- select_models(data = pima_data, outcome = outcome,
                              exposures = exposures, approach = appr, direction = dir)
      expect_true("results_table" %in% names(result))
      expect_true("best_model" %in% names(result))
      expect_s3_class(result$results_table, "tbl_df")
      expect_s3_class(result$best_model, "glm")
    }
  }
})

test_that("select_models works for linear regression and returns adjusted R2", {
  set.seed(123)
  df <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = sample(letters[1:3], 100, replace = TRUE),
    x3 = sample(LETTERS[1:2], 100, replace = TRUE)
  )
  df$x2 <- factor(df$x2)
  df$x3 <- factor(df$x3)

  result <- select_models(df, outcome = "y",
                          exposures = c("x1", "x2", "x3"),
                          approach = "linear", direction = "forward")
  expect_true("adj_r2" %in% colnames(result$results_table))
})

test_that("select_models validates outcome types appropriately", {
  df <- data.frame(
    y_bin = sample(c(0, 1), 100, replace = TRUE),
    y_cont = rnorm(100),
    y_count = rpois(100, lambda = 2),
    x = sample(letters[1:3], 100, replace = TRUE)
  )
  df$x <- factor(df$x)

  # Should pass (no error expected)
  expect_error(select_models(df, outcome = "y_bin", exposures = "x", approach = "logit"), NA)
  expect_error(select_models(df, outcome = "y_count", exposures = "x", approach = "poisson"), NA)
  expect_error(select_models(df, outcome = "y_cont", exposures = "x", approach = "linear"), NA)

  # Should fail (wrong outcome type for given model)
  expect_error(select_models(df, outcome = "y_bin", exposures = "x", approach = "poisson"))
  expect_error(select_models(df, outcome = "y_cont", exposures = "x", approach = "logit"))
  expect_error(select_models(df, outcome = "y_count", exposures = "x", approach = "linear"))
})

test_that("select_models supports negative binomial regression", {
  data("quine", package = "MASS")
  quine <- quine %>%
    mutate(across(c(Eth, Sex, Age, Lrn), as.factor))

  result <- select_models(quine, outcome = "Days",
                          exposures = c("Eth", "Sex", "Age", "Lrn"),
                          approach = "negbin", direction = "forward")

  expect_true("results_table" %in% names(result))
  expect_s3_class(result$best_model, "glm")  # MASS::glm.nb returns class 'glm'
})
