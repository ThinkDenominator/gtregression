birthwt_strata_uni_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
      ptl_cat = ifelse(ptl > 0, "Yes", "No"),
      ftv_cat = dplyr::case_when(
        ftv == 0 ~ "None",
        ftv == 1 ~ "One",
        ftv >= 2 ~ "Two or more"
      )
    ) |>
    dplyr::mutate(
      ptl_cat = factor(ptl_cat, levels = c("No", "Yes")),
      ftv_cat = factor(ftv_cat, levels = c("None", "One", "Two or more"))
    )
}

test_that("stratified_uni_reg returns a gtregression object for logit models", {
  df <- birthwt_strata_uni_data()

  res <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "lwt", "smoke", "ht"),
      stratifier = "race",
      approach = logit,
      format = gt
    )
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "stratified_uni_reg")
  expect_s3_class(res, "gt_strata_uni")
  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$approach, "logit")
  expect_equal(res$format, "gt")
  expect_equal(res$source, "stratified_uni_reg")
  expect_equal(res$by, "race")
  expect_equal(res$levels, c("White", "Black", "Other"))
  expect_named(
    res,
    c("table", "table_display", "per_stratum", "models",
      "model_summaries", "reg_check", "by", "levels", "approach",
      "format", "source")
  )
  expect_named(res$per_stratum, c("White", "Black", "Other"))
  expect_true(all(vapply(res$per_stratum, inherits, logical(1), what = "uni_reg")))
  expect_true(all(c("Characteristic", "is_header", "..N__White",
                    "..eff__White", "..p__White") %in% names(res$table_display)))
})

test_that("stratified_uni_reg preserves factor level order and excludes missing strata", {
  df <- birthwt_strata_uni_data()
  df$race <- factor(df$race, levels = c("Other", "White", "Black"))
  df$race[1:3] <- NA

  res <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke"),
      stratifier = "race",
      approach = "logit"
    )
  )

  expect_equal(res$levels, c("Other", "White", "Black"))
  expect_named(res$per_stratum, c("Other", "White", "Black"))
  expect_true(all(!grepl("NA", names(res$table_display), fixed = TRUE)))
})

test_that("stratified_uni_reg accepts bare and quoted options", {
  df <- birthwt_strata_uni_data()

  bare <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke"),
      stratifier = "race",
      approach = logit,
      format = gt,
      theme = clinical
    )
  )
  quoted <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke"),
      stratifier = "race",
      approach = "logit",
      format = "gt",
      theme = "clinical"
    )
  )

  expect_equal(bare$approach, quoted$approach)
  expect_equal(bare$format, quoted$format)
  expect_equal(bare$table_display, quoted$table_display)
})

test_that("stratified_uni_reg supports flextable output", {
  skip_if_not_installed("flextable")

  df <- birthwt_strata_uni_data()

  res <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke"),
      stratifier = "race",
      approach = logit,
      format = flextable
    )
  )

  expect_s3_class(res, "ft_strata_uni")
  expect_s3_class(res$table, "flextable")
  expect_equal(res$format, "flextable")
})

test_that("stratified_uni_reg returns diagnostics for linear models", {
  df <- birthwt_strata_uni_data()

  res <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "bwt",
      exposures = c("age", "lwt", "smoke"),
      stratifier = "race",
      approach = linear
    )
  )

  expect_s3_class(res, "stratified_uni_reg")
  expect_true(any(grepl("..eff__White", names(res$table_display), fixed = TRUE)))
  expect_equal(res$approach, "linear")
  expect_true(all(vapply(res$reg_check, is.list, logical(1))))
  expect_true("age" %in% names(res$reg_check$White))
  expect_true("Test" %in% names(res$reg_check$White$age))
})

test_that("stratified_uni_reg validates missing variables and outcome types", {
  df <- birthwt_strata_uni_data()

  expect_error(
    stratified_uni_reg(
      df,
      outcome = "low",
      exposures = "not_here",
      stratifier = "race",
      approach = logit
    ),
    "exposure variables were not found"
  )
  expect_error(
    stratified_uni_reg(
      df,
      outcome = "low",
      exposures = "age",
      stratifier = "not_here",
      approach = logit
    ),
    "Stratifier not found"
  )
  expect_error(
    stratified_uni_reg(
      df,
      outcome = "bwt",
      exposures = "age",
      stratifier = "race",
      approach = logit
    ),
    "requires either a factor variable"
  )
})

test_that("stratified_uni_reg errors when no valid strata remain", {
  df <- birthwt_strata_uni_data()
  df$race <- NA_character_

  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_uni_reg(
          df,
          outcome = "low",
          exposures = c("age", "lwt"),
          stratifier = "race",
          approach = logit
        )
      )
    ),
    "No valid models across strata"
  )
})
