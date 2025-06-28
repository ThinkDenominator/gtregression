test_that("plot_reg_combine works with various options", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("mlbench")
  skip_if_not_installed("dplyr")

  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
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
      dpf_cat = factor(dpf_cat, levels = c("Low Genetic Risk",
                                           "Moderate Genetic Risk",
                                           "High Genetic Risk"))
    )

  exposures <- c("bmi", "age_cat", "npreg_cat", "glucose_cat",
                 "bp_cat", "triceps_cat", "insulin_cat", "dpf_cat")

  tbl_uni <- gtregression::uni_reg(
    pima_data,
    outcome = "diabetes", exposures = exposures, approach = "robpoisson"
  )
  tbl_multi <- gtregression::multi_reg(
    pima_data,
    outcome = "diabetes", exposures = exposures, approach = "robpoisson"
  )

  p1 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi)
  expect_s3_class(p1, "patchwork")

  p2 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi, order_y = exposures)
  expect_s3_class(p2, "patchwork")

  p3 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi, log_x = TRUE)
  expect_s3_class(p3, "patchwork")

  p4 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi,
                                       order_y = exposures, log_x = TRUE)
  expect_s3_class(p4, "patchwork")
})
