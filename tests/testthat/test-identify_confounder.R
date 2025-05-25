test_that("identify_confounder works across approaches using change-in-estimate method", {
  # Load and prepare data
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 %>%
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = dplyr::case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese"
      ),
      bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
      age_cat = dplyr::case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      npreg_cat = factor(ifelse(pregnant > 2, "High parity", "Low parity"),
                         levels = c("Low parity", "High parity")),
      glucose_cat = factor(ifelse(glucose >= 140, "High", "Normal"),
                           levels = c("Normal", "High"))
    )

  outcome <- "diabetes"
  exposure <- "glucose_cat"
  potential_confounders <- c("bmi", "age_cat", "npreg_cat", "bp_cat")
  approaches <- c("robpoisson", "log-binomial")

  for (approach in approaches) {
    # One confounder - return list
    conf_list <- identify_confounder(
      data = pima_data,
      outcome = outcome,
      exposure = exposure,
      potential_confounder = "bmi",
      approach = approach
    )

    expect_type(conf_list, "list")
    expect_named(conf_list, c("crude", "adjusted", "percent_change", "is_confounder"), ignore.order = TRUE)
    expect_true(is.numeric(conf_list$crude))

    # Multiple confounders - return tibble
    conf_tbl <- identify_confounder(
      data = pima_data,
      outcome = outcome,
      exposure = exposure,
      potential_confounder = potential_confounders,
      approach = approach
    )

    expect_s3_class(conf_tbl, "tbl_df")
    expect_true(all(c("covariate", "crude_est", "adjusted_est", "pct_change", "is_confounder") %in% names(conf_tbl)))
    expect_true(nrow(conf_tbl) >= 1)
  }

  # Test on count outcome with negative binomial
  data("quine", package = "MASS")
  quine_data <- quine %>%
    dplyr::mutate(across(c(Eth, Sex, Age, Lrn), as.factor))

  conf_tbl_nb <- identify_confounder(
    data = quine_data,
    outcome = "Days",
    exposure = "Sex",
    potential_confounder = c("Eth", "Age", "Lrn"),
    approach = "negbin"
  )

  expect_s3_class(conf_tbl_nb, "tbl_df")
  expect_true(all(c("covariate", "crude_est", "adjusted_est", "pct_change", "is_confounder") %in% names(conf_tbl_nb)))
})
