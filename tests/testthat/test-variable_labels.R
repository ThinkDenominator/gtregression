labelled_birthwt_data <- function() {
  df <- data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
      ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes"))
    )

  attr(df$smoke, "label") <- "Smoking during pregnancy"
  attr(df$ht, "label") <- "Hypertension"
  attr(df$age, "label") <- "Maternal age"
  attr(df$race, "label") <- "Maternal race"
  df
}

test_that("variable label attributes are used in descriptive and regression displays", {
  df <- labelled_birthwt_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "race", "smoke", "ht"),
    by = "low",
    show_dichotomous = "single_row"
  )
  uni <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "race", "smoke", "ht"),
    approach = logit
  )
  multi <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "race", "smoke", "ht"),
    approach = logit
  )

  expect_true("Maternal age" %in% desc$table_display$Characteristic)
  expect_true("Smoking during pregnancy" %in% desc$table_display$Characteristic)
  expect_true("Maternal race" %in% uni$table_display$Characteristic)
  expect_true("Hypertension" %in% multi$table_display$Characteristic)

  expect_true("age" %in% desc$table_body$var)
  expect_true("smoke" %in% uni$table_body$exposure)
  expect_equal(uni$variable_labels[["smoke"]], "Smoking during pregnancy")
  expect_equal(multi$variable_labels[["ht"]], "Hypertension")
})

test_that("variable label attributes remain compatible with modify_table, plots, merge, and forest_df", {
  df <- labelled_birthwt_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "race", "smoke", "ht"),
    by = "low"
  )
  uni <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "race", "smoke", "ht"),
    approach = logit
  )
  multi <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "race", "smoke", "ht"),
    approach = logit
  )

  modified <- modify_table(
    uni,
    variable_labels = c(smoke = "Smoker"),
    level_labels = list(smoke = c(Yes = "Smoked"))
  )
  expect_true("Smoker" %in% modified$table_display$Characteristic)
  expect_true("  Smoked" %in% modified$table_display$Characteristic)

  p <- plot_reg(uni, show_ref = FALSE)
  expect_true("Smoking during pregnancy" %in% p$data$label)
  expect_true("Hypertension" %in% p$data$label)

  merged <- merge_tables(desc, uni, multi)
  expect_true("Maternal age" %in% merged$table_display$Characteristic)
  expect_true("Smoking during pregnancy" %in% merged$table_display$Characteristic)

  forest <- forest_df(uni, multi, desc = desc)
  expect_true("Maternal age" %in% forest$Characteristic)
  expect_true("Smoking during pregnancy" %in% forest$Characteristic)
})

test_that("variable label attributes are used in stratified outputs", {
  df <- labelled_birthwt_data()

  strata_uni <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke", "ht"),
      stratifier = "race",
      approach = logit
    )
  )
  strata_multi <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke", "ht"),
      stratifier = "race",
      approach = logit
    )
  )

  expect_true("Maternal age" %in% strata_uni$table_display$Characteristic)
  expect_true("Smoking during pregnancy" %in% strata_uni$table_display$Characteristic)
  expect_true("Hypertension" %in% strata_multi$table_display$Characteristic)
  expect_equal(strata_uni$variable_labels[["smoke"]], "Smoking during pregnancy")
})
