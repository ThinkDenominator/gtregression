library(testthat)
library(gtregression)
library(mlbench)
library(MASS)
library(dplyr)

test_that("identify_confounder works with MH and change methods across approaches", {
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

  methods <- c("mh", "change")
  approaches <- c("robpoisson", "log-binomial")

  for (method in methods) {
    for (approach in approaches) {
      if (method == "mh") {
        # Only works with one confounder
        conf_mh <- identify_confounder(
          data = pima_data,
          outcome = outcome,
          exposure = exposure,
          potential_confounder = "bmi",
          approach = approach,
          method = method
        )

        expect_type(conf_mh, "list")
        expect_named(conf_mh, c(
          "crude", "mantel_haenszel", "percent_change", "is_confounder",
          "effect_modification", "stratum_estimates", "skipped_strata", "reason"
        ), ignore.order = TRUE)

        expect_true(is.numeric(conf_mh$crude) || is.na(conf_mh$crude))
        expect_true(is.logical(conf_mh$is_confounder) || is.na(conf_mh$is_confounder))
        expect_true(is.character(conf_mh$skipped_strata) || length(conf_mh$skipped_strata) == 0)

      } else {
        # One confounder: expect list
        conf_change_list <- identify_confounder(
          data = pima_data,
          outcome = outcome,
          exposure = exposure,
          potential_confounder = "bmi",
          approach = approach,
          method = method
        )

        expect_type(conf_change_list, "list")
        expect_named(conf_change_list, c("crude", "adjusted", "percent_change", "is_confounder"), ignore.order = TRUE)
        expect_true(is.numeric(conf_change_list$crude))

        # Multiple confounders: expect tibble
        conf_change_tbl <- identify_confounder(
          data = pima_data,
          outcome = outcome,
          exposure = exposure,
          potential_confounder = potential_confounders,
          approach = approach,
          method = method
        )

        expect_s3_class(conf_change_tbl, "tbl_df")
        expect_true(all(c("covariate", "crude_est", "adjusted_est", "pct_change", "is_confounder") %in% names(conf_change_tbl)))
        expect_true(nrow(conf_change_tbl) >= 1)
      }
    }
  }

  # Count data with negbin + change
  data("quine", package = "MASS")
  quine <- quine %>% mutate(across(c(Eth, Sex, Age, Lrn), as.factor))

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
