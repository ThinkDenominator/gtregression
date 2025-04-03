library(testthat)
library(gtregression)  # Your package
library(mlbench)
library(MASS)
library(dplyr)

test_that("identify_confounder works with MH and change methods across approaches", {
  # Load and prepare Pima dataset
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = factor(case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese"
      ), levels = c("Normal", "Overweight", "Obese")),
      age_cat = factor(case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ), levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"),
                         levels = c("Low parity", "High parity")),
      glucose_cat = factor(ifelse(glucose >= 140, "High", "Normal"),
                           levels = c("Normal", "High"))
    )

  outcome <- "diabetes"
  exposure <- "glucose_cat"
  potential_confounders <- c("bmi", "age_cat", "npreg_cat", "bp_cat")

  # Test both methods
  methods <- c("mh", "change")
  approaches <- c("robpoisson", "log-binomial")

  for (method in methods) {
    for (approach in approaches) {
      if (method == "mh") {
        # Only test one confounder at a time
        conf_mh <- identify_confounder(
          data = pima_data,
          outcome = outcome,
          exposure = exposure,
          potential_confounder = "bmi",
          approach = approach,
          method = method
        )

        expect_type(conf_mh, "list")
        expect_named(conf_mh, c("crude", "mantel_haenszel", "percent_change", "is_confounder", "effect_modification", "stratum_estimates"))
        expect_true(is.numeric(conf_mh$crude))
        expect_true(is.logical(conf_mh$is_confounder))

      } else {
        # Change-in-estimate method supports multiple confounders
        conf_tbl <- identify_confounder(
          data = pima_data,
          outcome = outcome,
          exposure = exposure,
          potential_confounder = potential_confounders,
          approach = approach,
          method = method
        )

        expect_s3_class(conf_tbl, "tbl_df")
        expect_true(all(c("covariate", "crude_est", "adjusted_est", "pct_change", "is_confounder") %in% names(conf_tbl)))
        expect_true(nrow(conf_tbl) >= 1)
      }
    }
  }

  # Also test for count data using Negative Binomial regression
  data("quine", package = "MASS")

  conf_tbl_nb <- identify_confounder(
    data = quine,
    outcome = "Days",
    exposure = "Sex",
    potential_confounder = c("Eth", "Age", "Lrn"),
    approach = "negbin",
    method = "change"
  )

  expect_s3_class(conf_tbl_nb, "tbl_df")
  expect_true(all(c("covariate", "crude_est", "adjusted_est", "pct_change", "is_confounder") %in% names(conf_tbl_nb)))
})
