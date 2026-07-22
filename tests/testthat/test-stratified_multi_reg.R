birthwt_strata_multi_data <- function() {
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

test_that("stratified_multi_reg returns a gtregression object for full logit models", {
  df <- birthwt_strata_multi_data()

  res <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "lwt", "smoke"),
      stratifier = "race",
      approach = logit,
      format = gt
    )
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "stratified_multi_reg")
  expect_s3_class(res, "gt_strata_multi")
  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$approach, "logit")
  expect_equal(res$format, "gt")
  expect_equal(res$source, "stratified_multi_reg")
  expect_equal(res$by, "race")
  expect_equal(res$levels, c("White", "Black", "Other"))
  expect_named(
    res,
    c("table", "table_display", "per_stratum", "models",
      "model_summaries", "reg_check", "by", "levels", "approach",
      "format", "source")
  )
  expect_named(res$models, c("White", "Black", "Other"))
  expect_named(res$models$White, "multivariable_model")
  expect_true(all(c("Characteristic", "is_header", "..eff__White",
                    "..p__White") %in% names(res$table_display)))
})

test_that("stratified_multi_reg adjust_for fits one adjusted model per exposure", {
  df <- birthwt_strata_multi_data()

  res <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "low",
      exposures = c("smoke", "ht", "ui"),
      stratifier = "race",
      adjust_for = c("age", "lwt"),
      approach = logit,
      theme = clinical
    )
  )

  expect_s3_class(res, "stratified_multi_reg")
  expect_named(res$models$White, c("smoke", "ht", "ui"))
  expect_named(res$model_summaries$White, c("smoke", "ht", "ui"))
  expect_true(res$per_stratum$White$adjusted_mode)
  expect_true(res$per_stratum$Black$adjusted_mode)
  expect_true(all(c("smoke", "ht", "ui") %in% res$table_display$Characteristic))
})

test_that("stratified_multi_reg preserves factor strata order and excludes missing strata", {
  df <- birthwt_strata_multi_data()
  df$race <- factor(df$race, levels = c("Other", "White", "Black"))
  df$race[1:3] <- NA

  res <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke"),
      stratifier = "race",
      approach = "logit"
    )
  )

  expect_equal(res$levels, c("Other", "White", "Black"))
  expect_named(res$models, c("Other", "White", "Black"))
  expect_true(all(!grepl("NA", names(res$table_display), fixed = TRUE)))
})

test_that("stratified_multi_reg accepts bare and quoted options", {
  df <- birthwt_strata_multi_data()

  bare <- suppressMessages(
    stratified_multi_reg(
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
    stratified_multi_reg(
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

test_that("stratified_multi_reg supports flextable output", {
  skip_if_not_installed("flextable")

  df <- birthwt_strata_multi_data()

  res <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "low",
      exposures = c("smoke", "ht"),
      stratifier = "race",
      adjust_for = c("age", "lwt"),
      approach = logit,
      format = flextable
    )
  )

  expect_s3_class(res, "ft_strata_multi")
  expect_s3_class(res$table, "flextable")
  expect_equal(res$format, "flextable")
})

test_that("stratified_multi_reg passes interactions to stratum-specific models", {
  df <- birthwt_strata_multi_data()

  res <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "low",
      exposures = "smoke",
      stratifier = "race",
      adjust_for = c("age", "lwt"),
      interaction = "smoke*ht",
      approach = logit
    )
  )

  expect_s3_class(res, "stratified_multi_reg")
  expect_named(res$models$White, "smoke")
  expect_match(
    paste(deparse(stats::formula(res$models$White$smoke)), collapse = " "),
    "smoke \\* ht"
  )
})

test_that("stratified_multi_reg returns diagnostics for linear models", {
  df <- birthwt_strata_multi_data()

  res <- suppressMessages(
    stratified_multi_reg(
      data = df,
      outcome = "bwt",
      exposures = c("age", "lwt", "smoke"),
      stratifier = "race",
      approach = linear
    )
  )

  expect_s3_class(res, "stratified_multi_reg")
  expect_equal(res$approach, "linear")
  expect_true("..eff__White" %in% names(res$table_display))
  expect_true("multivariable_model" %in% names(res$reg_check$White))
  expect_true("Test" %in% names(res$reg_check$White$multivariable_model))
})

test_that("stratified_multi_reg validates invalid inputs through stratum failures", {
  df <- birthwt_strata_multi_data()

  expect_error(
    stratified_multi_reg(
      df,
      outcome = "low",
      exposures = "age",
      stratifier = "not_here",
      approach = logit
    ),
    "Stratifier not found"
  )
  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_multi_reg(
          df,
          outcome = "low",
          exposures = "not_here",
          stratifier = "race",
          approach = logit
        )
      )
    ),
    "No valid models across strata"
  )
  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_multi_reg(
          df,
          outcome = "bwt",
          exposures = "age",
          stratifier = "race",
          approach = logit
        )
      )
    ),
    "No valid models across strata"
  )
})

test_that("stratified_multi_reg validates adjustment and interaction inputs", {
  df <- birthwt_strata_multi_data()

  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_multi_reg(
          df,
          outcome = "low",
          exposures = "smoke",
          stratifier = "race",
          adjust_for = "smoke",
          approach = logit
        )
      )
    ),
    "No valid models across strata"
  )
  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_multi_reg(
          df,
          outcome = "low",
          exposures = "smoke",
          stratifier = "race",
          adjust_for = "low",
          approach = logit
        )
      )
    ),
    "No valid models across strata"
  )
  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_multi_reg(
          df,
          outcome = "low",
          exposures = "smoke",
          stratifier = "race",
          interaction = "smoke:ht",
          approach = logit
        )
      )
    ),
    "No valid models across strata"
  )
})

test_that("stratified_multi_reg errors when no valid strata remain", {
  df <- birthwt_strata_multi_data()
  df$race <- NA_character_

  expect_error(
    suppressWarnings(
      suppressMessages(
        stratified_multi_reg(
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
