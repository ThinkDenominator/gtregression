test_that("save_table(), save_plot(), and save_docx() work correctly", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gt")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("gtregression")

  library(gtregression)
  library(gtsummary)
  library(ggplot2)
  library(dplyr)

  # Prepare Data
  data("data_PimaIndiansDiabetes", package = "gtregression")
  pima_data <- data_PimaIndiansDiabetes |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |>
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
    ) |>
    mutate(diabetes_cat = case_when(diabetes == 1 ~ "Diabetes positive", TRUE ~ "Diabetes negative")) |>
    mutate(diabetes_cat = factor(diabetes_cat, levels = c("Diabetes negative", "Diabetes positive")))

  exposures <- c("bmi", "age_cat", "npreg_cat", "bp_cat", "triceps_cat", "insulin_cat", "dpf_cat")

  # Univariate regression
  uni_rr <- uni_reg(pima_data, outcome = "diabetes", exposures = exposures, approach = "log-binomial")
  # Modify table
  tbl_custom <- modify_table(
    uni_rr,
    variable_labels = c(age_cat = "Age", bmi = "BMI"),
    level_labels = c(`Young` = "Young Adults", `Older` = "Older Adults"),
    header_labels = c(estimate = "Risk Ratio", `p.value` = "P-value"),
    caption = "Table 1: Univariate Regression",
    bold_labels = TRUE
  )

  # Plot
  p <- plot_reg(tbl_custom)

  # Save Table
  save_table(tbl_custom, filename = "regression_results", format = "docx")
  expect_true(file.exists("regression_results.docx"))

  # Save Plots with suppressed warnings
  suppressWarnings(save_plot(p, filename = "plot_png", format = "png"))
  suppressWarnings(save_plot(p, filename = "plot_pdf", format = "pdf"))
  suppressWarnings(save_plot(p, filename = "plot_jpg", format = "jpg"))

  expect_true(file.exists("plot_png.png"))
  expect_true(file.exists("plot_pdf.pdf"))
  expect_true(file.exists("plot_jpg.jpg"))

  # Save DOCX report with suppressed warnings
  suppressWarnings(save_docx(
    tables = list(uni_rr, uni_rr),
    plots = list(p),
    filename = "final_report.docx",
    titles = c(
      "Table 1: Univariate Regression",
      "Table 2: Univariate Regression (Duplicate)",
      "Figure 1: Plot"
    )
  ))
  expect_true(file.exists("final_report.docx"))

  # Clean up
  file.remove(
    "regression_results.docx",
    "plot_png.png",
    "plot_pdf.pdf",
    "plot_jpg.jpg",
    "final_report.docx"
  )
})
