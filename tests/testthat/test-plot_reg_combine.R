test_that("plot_reg_combine works with various options", {
  local_edition(3)

  skip_if_not_installed("patchwork")
  skip_if_not_installed("gtregression")
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

  # baseline
  p1 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi)
  expect_true(inherits(p1, "patchwork"))

  # order_y
  p2 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi, order_y = exposures)
  expect_true(inherits(p2, "patchwork"))

  # log10 scaling (non-linear approaches only)
  p3 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi, log_x = TRUE)
  expect_true(inherits(p3, "patchwork"))

  # order_y + log10
  p4 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi,
                                       order_y = exposures, log_x = TRUE)
  expect_true(inherits(p4, "patchwork"))

  # show_ref toggle
  p5 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi, show_ref = FALSE)
  expect_true(inherits(p5, "patchwork"))

  # custom colors + base size (exercise scales/theme paths)
  p6 <- gtregression::plot_reg_combine(
    tbl_uni, tbl_multi,
    point_color = "#1b9e77", errorbar_color = "#4d4d4d",
    sig_color = "#d95f02", sig_errorbar_color = "#7570b3",
    base_size = 11
  )
  expect_true(inherits(p6, "patchwork"))

  # explicit x-limits/breaks (both panels)
  p7 <- gtregression::plot_reg_combine(
    tbl_uni, tbl_multi,
    xlim_uni = c(0.5, 8), breaks_uni = c(0.5, 1, 2, 4, 8),
    xlim_multi = c(0.5, 8), breaks_multi = c(0.5, 1, 2, 4, 8)
  )
  expect_true(inherits(p7, "patchwork"))

  # mismatched exposures (alignment by row key; multi missing a variable)
  exposures_miss <- c("bmi", "age_cat", "glucose_cat", "bp_cat")  # subset
  tbl_multi_miss <- gtregression::multi_reg(
    pima_data,
    outcome = "diabetes", exposures = exposures_miss, approach = "robpoisson"
  )
  p8 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi_miss)
  expect_true(inherits(p8, "patchwork"))

  # linear approach branch (ref line = 0; p.value path; log_x ignored)
  pima_lin <- pima_data
  pima_lin$mass <- as.numeric(pima_lin$mass)
  exposures_lin <- c("age", "glucose", "triceps")

  tbl_uni_lin <- gtregression::uni_reg(
    pima_lin, outcome = "mass", exposures = exposures_lin, approach = "linear"
  )
  tbl_mul_lin <- gtregression::multi_reg(
    pima_lin, outcome = "mass", exposures = exposures_lin, approach = "linear"
  )
  p9 <- gtregression::plot_reg_combine(tbl_uni_lin, tbl_mul_lin, log_x = TRUE, alpha = 0.5)
  expect_true(inherits(p9, "patchwork"))

  # explicit ref_line override (non-default)
  p10 <- gtregression::plot_reg_combine(tbl_uni, tbl_multi, ref_line = 1.2)
  expect_true(inherits(p10, "patchwork"))
})
