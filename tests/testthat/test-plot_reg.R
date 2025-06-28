test_that("plot_reg works with default settings and correct X-axis labels", {
  local_edition(3)

  library(ggplot2)
  library(mlbench)

  data("PimaIndiansDiabetes2", package = "mlbench")

  pima_data <- PimaIndiansDiabetes2 |>
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = dplyr::case_when(
        mass < 25 ~ "Normal",
        mass < 30 ~ "Overweight",
        TRUE ~ "Obese"
      ),
      age_cat = dplyr::case_when(
        age < 30 ~ "Young",
        age < 50 ~ "Middle-aged",
        TRUE ~ "Older"
      ),
      npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
      glucose_cat = dplyr::case_when(glucose < 140 ~ "Normal", TRUE ~ "High"),
      bp_cat = dplyr::case_when(pressure < 80 ~ "Normal", TRUE ~ "High"),
      triceps_cat = dplyr::case_when(triceps < 23 ~ "Normal", TRUE ~ "High"),
      insulin_cat = dplyr::case_when(
        insulin < 30 ~ "Low",
        insulin < 150 ~ "Normal",
        TRUE ~ "High"
      ),
      dpf_cat = dplyr::case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree <= 0.5 ~ "Moderate Genetic Risk",
        TRUE ~ "High Genetic Risk"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(bmi, age_cat, npreg_cat, glucose_cat, bp_cat, triceps_cat,
          insulin_cat, dpf_cat),
        ~ factor(.x)
      )
    )

  exposures <- c("bmi", "age_cat", "npreg_cat", "glucose_cat", "bp_cat",
                 "triceps_cat", "insulin_cat", "dpf_cat")

  approaches_labels <- list(
    "logit" = "Odds Ratio",
    "log-binomial" = "Risk Ratio",
    "robpoisson" = "Risk Ratio",
    "linear" = "Coefficient"
  )

  for (approach in names(approaches_labels)) {
    outcome_var <- if (approach == "linear") "mass" else "diabetes"

    tbl_uni <- gtregression::uni_reg(pima_data,
                                     outcome = outcome_var,
                                     exposures = exposures,
                                     approach = approach)
    p <- gtregression::plot_reg(tbl_uni)

    expect_s3_class(p, "ggplot")

    # Extract x-axis label from plot
    x_label <- p$labels$x
    expected_label <- approaches_labels[[approach]]

    expect_match(x_label, expected_label, fixed = TRUE)
  }

  # Also test plot_reg_combine()
  tbl_uni <- gtregression::uni_reg(pima_data,
                                   outcome = "diabetes",
                                   exposures = exposures,
                                   approach = "robpoisson")
  tbl_multi <- gtregression::multi_reg(pima_data,
                                       outcome = "diabetes",
                                       exposures = exposures,
                                       approach = "robpoisson")

  p1 <- gtregression::plot_reg(tbl_uni)
  expect_s3_class(p1, "ggplot")

  p2 <- gtregression::plot_reg(tbl_uni,
                               order_y = exposures,
                               log_x = TRUE, s\
                               how_ref = TRUE)
  expect_s3_class(p2, "ggplot")

  p3 <- gtregression::plot_reg_combine(tbl_uni = tbl_uni, tbl_multi = tbl_multi)
  expect_s3_class(p3, "ggplot")

  p4 <- gtregression::plot_reg_combine(
    tbl_uni = tbl_uni, tbl_multi = tbl_multi,
    order_y = exposures, log_x = TRUE, show_ref = TRUE
  )
  expect_s3_class(p4, "ggplot")
})
