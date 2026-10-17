birthwt_modify_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
    )
}

lung_modify_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes")),
      celltype = factor(
        celltype,
        levels = c("squamous", "smallcell", "adeno", "large"),
        labels = c("Squamous", "Small cell", "Adenocarcinoma", "Large cell")
      )
    )
}

test_that("modify_table updates package-native univariable gt tables", {
  df <- birthwt_modify_data()

  tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke", "ht"),
    approach = logit,
    format = gt
  )

  modified <- modify_table(
    tbl,
    variable_labels = c(age = "Maternal age", smoke = "Smoking", ht = "Hypertension"),
    level_labels = list(smoke = c(Yes = "Smoker"), ht = c(No = "No hypertension")),
    header_labels = c(estimate = "Crude OR", p.value = "P"),
    caption = "Table 1. Univariable regression",
    remove_N = TRUE,
    caveat = "Interpret with clinical context."
  )

  expect_s3_class(modified, "gtregression")
  expect_s3_class(modified, "uni_reg")
  expect_s3_class(modified$table, "gt_tbl")
  expect_false("N" %in% names(modified$table_display))
  expect_true("Maternal age" %in% modified$table_display$Characteristic)
  expect_true("Smoking" %in% modified$table_display$Characteristic)
  expect_true("  Smoker" %in% modified$table_display$Characteristic)
  expect_true("  No hypertension" %in% modified$table_display$Characteristic)
  expect_equal(modified$caption, "Table 1. Univariable regression")
  expect_true(any(grepl("Interpret with clinical context", modified$footnotes, fixed = TRUE)))
})

test_that("modify_table validates header aliases and preserves visible headers", {
  df <- birthwt_modify_data()
  tbl <- uni_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit, format = gt)

  modified <- modify_table(
    tbl,
    header_labels = c("OR (95% CI)" = "Odds ratio", "p-value" = "P value")
  )

  expect_s3_class(modified$table, "gt_tbl")
  expect_true("OR (95% CI)" %in% names(modified$table_display))
  expect_error(
    modify_table(tbl, header_labels = c(not_a_column = "Nope")),
    "columns not found"
  )
})

test_that("modify_table works for adjusted multivariable tables and footnote options", {
  df <- birthwt_modify_data()

  tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt"),
    approach = logit,
    format = gt
  )

  keep_notes <- modify_table(tbl, caveat = "Adjusted model.")
  renamed <- modify_table(tbl, header_labels = c(estimate = "Adjusted OR", p.value = "P"))
  drop_notes <- modify_table(
    tbl,
    remove_abbreviations = TRUE,
    remove_N_obs = TRUE,
    caveat = "Adjusted model."
  )

  expect_s3_class(keep_notes, "multi_reg")
  expect_s3_class(renamed$table, "gt_tbl")
  expect_true(any(grepl("OR = Odds Ratio", keep_notes$footnotes, fixed = TRUE)))
  expect_true(any(grepl("Adjusted model", keep_notes$footnotes, fixed = TRUE)))
  relabelled <- modify_table(
    tbl,
    variable_labels = c(age = "Maternal age", lwt = "Maternal weight")
  )
  expect_true(any(relabelled$footnotes == "Adjusted for Maternal age and Maternal weight"))
  expect_false(any(relabelled$footnotes == "Adjusted for age and lwt"))
  custom_adjustment <- modify_table(
    tbl,
    remove_adjustment_note = TRUE,
    caveat = "Adjusted for baseline maternal characteristics."
  )
  hidden_adjustment <- modify_table(tbl, remove_adjustment_note = TRUE)
  expect_true(any(custom_adjustment$footnotes == "Adjusted for baseline maternal characteristics."))
  expect_false(any(custom_adjustment$footnotes == "Adjusted for age and lwt"))
  expect_false(any(grepl("^Adjusted for ", hidden_adjustment$footnotes)))
  expect_false(any(grepl("OR = Odds Ratio", drop_notes$footnotes, fixed = TRUE)))
  expect_false(any(grepl("complete observations included", drop_notes$footnotes, fixed = TRUE)))
})

test_that("modify_table works for Cox and survival regression tables", {
  skip_if_not_installed("survival")
  skip_if_not_installed("flextable")

  df <- lung_modify_data()

  cox_tbl <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior),
    adjust_for = c(age, karno),
    format = gt
  )
  cox_mod <- modify_table(
    cox_tbl,
    variable_labels = c(trt = "Treatment group", celltype = "Cancer cell type"),
    header_labels = c(estimate = "Adjusted HR", p.value = "P")
  )

  aft_tbl <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior),
    adjust_for = c(age, karno),
    distribution = weibull,
    format = flextable
  )
  aft_mod <- modify_table(
    aft_tbl,
    variable_labels = c(trt = "Treatment group", celltype = "Cancer cell type"),
    header_labels = c(estimate = "Adjusted time ratio", p.value = "P")
  )

  expect_s3_class(cox_mod, "cox_reg")
  expect_s3_class(cox_mod$table, "gt_tbl")
  expect_true("Treatment group" %in% cox_mod$table_display$Characteristic)
  expect_true("Cancer cell type" %in% cox_mod$table_display$Characteristic)
  expect_s3_class(aft_mod, "surv_reg")
  expect_s3_class(aft_mod$table, "flextable")
  expect_true("Treatment group" %in% aft_mod$table_display$Characteristic)
  expect_true("Cancer cell type" %in% aft_mod$table_display$Characteristic)

  cox_no_adjustment_note <- modify_table(
    cox_tbl,
    remove_adjustment_note = TRUE,
    caveat = "Adjusted survival model."
  )
  aft_no_adjustment_note <- modify_table(
    aft_tbl,
    remove_adjustment_note = TRUE,
    caveat = "Adjusted parametric survival model."
  )
  expect_false(any(grepl("^Adjusted for ", cox_no_adjustment_note$footnotes)))
  expect_false(any(grepl("^Adjusted for ", aft_no_adjustment_note$footnotes)))
  expect_true(any(cox_no_adjustment_note$footnotes == "Adjusted survival model."))
  expect_true(any(aft_no_adjustment_note$footnotes == "Adjusted parametric survival model."))
})

test_that("modify_table keeps new-model adjustment notes aligned with labels", {
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("nnet")

  fine_gray <- competing_risk_reg(
    data_competing_risk,
    time = follow_up_months,
    event = outcome,
    event_of_interest = Relapse,
    censor_code = Censored,
    exposures = treatment,
    adjust_for = c(age, stage)
  )
  fine_gray_modified <- modify_table(
    fine_gray,
    variable_labels = c(age = "Participant age", stage = "Cancer stage")
  )

  set.seed(76)
  ordinal_data <- data.frame(
    recovery = ordered(
      sample(c("Poor", "Partial", "Complete"), 180, replace = TRUE),
      levels = c("Poor", "Partial", "Complete")
    ),
    treatment = factor(rep(c("Standard", "Intervention"), each = 90)),
    age = stats::rnorm(180, 60, 9)
  )
  ordinal_model <- ordinal_reg(
    ordinal_data, recovery, treatment, adjust_for = age
  )
  ordinal_modified <- modify_table(
    ordinal_model,
    variable_labels = c(age = "Participant age")
  )

  multinomial_model <- multinomial_reg(
    iris, Species, Sepal.Length, adjust_for = Sepal.Width
  )
  multinomial_modified <- modify_table(
    multinomial_model,
    variable_labels = c(Sepal.Width = "Sepal width")
  )

  expect_true(any(
    fine_gray_modified$footnotes ==
      "Adjusted for Participant age and Cancer stage"
  ))
  expect_false(any(fine_gray_modified$footnotes == "Adjusted for age and stage"))
  expect_true(any(ordinal_modified$footnotes == "Adjusted for Participant age"))
  expect_true(any(multinomial_modified$footnotes == "Adjusted for Sepal width"))

  hidden <- modify_table(fine_gray, remove_adjustment_note = TRUE)
  expect_false(any(grepl("^Adjusted for ", hidden$footnotes)))
})

test_that("modify_table refreshes matching and clustering notes", {
  skip_if_not_installed("survival")
  skip_if_not_installed("lme4")

  matched_model <- conditional_logit_reg(
    data_infertility,
    outcome = case,
    exposures = spontaneous,
    matched_set = stratum
  )
  matched_modified <- modify_table(
    matched_model,
    variable_labels = c(stratum = "Matched study set")
  )

  set.seed(19)
  clustered <- data.frame(
    clinic = factor(rep(seq_len(20), each = 10)),
    treatment = factor(rep(c("Standard", "Intervention"), 100)),
    age = stats::rnorm(200, 60, 8)
  )
  clinic_effect <- stats::rnorm(20, 0, 1.5)
  clustered$score <- 40 +
    2 * (clustered$treatment == "Intervention") +
    0.15 * clustered$age + clinic_effect[clustered$clinic] +
    stats::rnorm(200)
  multilevel_model <- multilevel_reg(
    clustered,
    outcome = score,
    exposures = treatment,
    adjust_for = age,
    group = clinic
  )
  multilevel_modified <- modify_table(
    multilevel_model,
    variable_labels = c(age = "Participant age", clinic = "Study clinic")
  )

  expect_true(any(
    matched_modified$footnotes ==
      "Matched by set identifier: Matched study set."
  ))
  expect_true(any(
    multilevel_modified$footnotes ==
      "Random-intercept model clustered by Study clinic."
  ))
  expect_true(any(
    multilevel_modified$footnotes == "Adjusted for Participant age"
  ))
})

test_that("modify_table preserves stratified Cox sample-column controls", {
  skip_if_not_installed("survival")

  df <- lung_modify_data()

  tbl <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, prior),
    stratifier = trt,
    show_sample = both,
    format = gt
  )

  modified <- modify_table(
    tbl,
    variable_labels = c(celltype = "Cancer cell type", prior = "Prior therapy"),
    header_labels = c(estimate = "Crude HR"),
    remove_N = TRUE,
    caveat = "Stratified Cox table."
  )

  expect_s3_class(modified, "stratified_cox_reg")
  expect_s3_class(modified$table, "gt_tbl")
  expect_false(any(startsWith(names(modified$table_display), "..N__")))
  expect_true(any(startsWith(names(modified$table_display), "..Events__")))
  expect_true("Cancer cell type" %in% modified$table_display$Characteristic)
  expect_true(any(grepl("Stratified Cox table", modified$footnotes, fixed = TRUE)))
  expect_error(
    modify_table(tbl, header_labels = c(p.value = "P")),
    "columns not found in the stratified table"
  )
})

test_that("modify_table preserves stratified survival sample-column controls", {
  skip_if_not_installed("survival")
  skip_if_not_installed("flextable")

  df <- lung_modify_data()

  tbl <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(celltype, prior),
    adjust_for = c(age, karno),
    stratifier = trt,
    distribution = weibull,
    show_sample = both,
    format = flextable
  )

  modified <- modify_table(
    tbl,
    variable_labels = c(celltype = "Cancer cell type", prior = "Prior therapy"),
    header_labels = c(estimate = "Adjusted time ratio"),
    remove_N = TRUE
  )

  expect_s3_class(modified, "stratified_surv_reg")
  expect_s3_class(modified$table, "flextable")
  expect_false(any(startsWith(names(modified$table_display), "..N__")))
  expect_true(any(startsWith(names(modified$table_display), "..Events__")))
  expect_true("Cancer cell type" %in% modified$table_display$Characteristic)
})

test_that("modify_table works for descriptive and merged tables", {
  df <- birthwt_modify_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "smoke"),
    by = "low",
    show_overall = "last"
  )
  uni <- uni_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit)
  merged <- merge_tables(desc, uni, spanners = c("Descriptive", "Univariable"))

  desc_mod <- modify_table(
    desc,
    variable_labels = c(age = "Age", smoke = "Smoking"),
    level_labels = list(smoke = c(Yes = "Smoker"))
  )
  merged_mod <- modify_table(
    merged,
    variable_labels = c(age = "Age", smoke = "Smoking")
  )

  expect_s3_class(desc_mod, "descriptive_table")
  expect_s3_class(merged_mod, "merged_table")
  expect_true("Age" %in% desc_mod$table_display$Characteristic)
  expect_true("  Smoker" %in% desc_mod$table_display$Characteristic)
  expect_true("Smoking" %in% merged_mod$table_display$Characteristic)
})

test_that("modified input labels still align when tables are merged", {
  skip_if_not_installed("flextable")

  df <- birthwt_modify_data()

  desc <- descriptive_table(
    data = df,
    exposures = c("age", "smoke"),
    by = "low",
    show_overall = "last",
    format = flextable
  )
  desc <- modify_table(desc, variable_labels = c(smoke = "Smoking status"))

  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  merged <- merge_tables(desc, uni, spanners = c("Descriptive", "Crude"))
  merged <- modify_table(merged, variable_labels = c(smoke = "Smoking status"))

  expect_equal(sum(trimws(merged$table_display$Characteristic) == "Smoking status"), 1L)

  # version-independent reader (the header storage differs between
  # flextable releases)
  header_text <- as.vector(.flex_part_text(merged$table, "header"))
  expect_false(any(grepl("_p[0-9]+$", header_text)))
  expect_true(all(c("Descriptive", "Crude", "OR (95% CI)", "p-value") %in%
                    header_text))
  expect_true(any(grepl("^Low BW, N=[0-9]+$", header_text)))
})

test_that("modify_table works for flextable outputs", {
  skip_if_not_installed("flextable")

  df <- birthwt_modify_data()
  tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  modified <- modify_table(
    tbl,
    variable_labels = c(smoke = "Smoking"),
    header_labels = c(estimate = "Crude OR"),
    bold_labels = TRUE,
    bold_levels = TRUE,
    italic_labels = TRUE,
    italic_levels = TRUE,
    caption = "Flextable regression"
  )

  expect_s3_class(modified, "ft_uni")
  expect_s3_class(modified$table, "flextable")
  expect_true("Smoking" %in% modified$table_display$Characteristic)
})

test_that("modify_table preserves existing publication notes", {
  df <- birthwt_modify_data()
  tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = "age",
    approach = logit,
    format = flextable
  )

  modified <- modify_table(tbl, caption = "Adjusted model")

  expect_true(any(grepl("Abbreviations:", modified$footnotes)))
  expect_true(any(grepl("Ref\\. = reference category", modified$footnotes)))
  expect_true(any(grepl("complete observations", modified$footnotes)))
  expect_true(any(grepl("Adjusted for", modified$footnotes)))
})

test_that("modify_table validates inputs clearly", {
  df <- birthwt_modify_data()
  tbl <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit)

  expect_error(modify_table(data.frame(x = 1)), "gtregression object")
  expect_error(modify_table(tbl, variable_labels = c("Smoking")), "variable_labels")
  expect_error(modify_table(tbl, level_labels = c(Yes = "Smoker")), "level_labels")
  expect_error(modify_table(tbl, level_labels = list(smoke = c("Smoker"))), "level_labels\\$smoke")
  expect_error(modify_table(tbl, bold_labels = NA), "`bold_labels` must be")
  expect_error(modify_table(tbl, bold_levels = 1), "`bold_levels` must be")
  expect_error(modify_table(tbl, italic_labels = 1), "`italic_labels` must be")
  expect_error(modify_table(tbl, italic_levels = NA), "`italic_levels` must be")
  expect_error(modify_table(tbl, remove_N = NA), "`remove_N` must be")
  expect_error(modify_table(tbl, remove_N_obs = c(TRUE, FALSE)), "`remove_N_obs` must be")
  expect_error(modify_table(tbl, remove_abbreviations = "yes"), "`remove_abbreviations` must be")
  expect_error(modify_table(tbl, caption = NA_character_), "`caption` must be")
  expect_error(modify_table(tbl, caveat = c("a", "b")), "`caveat` must be")
})
