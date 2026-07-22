birthwt_multi_data <- function() {
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

test_that("multi_reg returns a gtregression object for default logit models", {
  df <- birthwt_multi_data()

  res <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "lwt", "race", "smoke"),
    approach = logit,
    format = gt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "multi_reg")
  expect_s3_class(res, "gt_multi")
  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$approach, "logit")
  expect_equal(res$format, "gt")
  expect_equal(res$source, "multi_reg")
  expect_named(
    res,
    c("table", "table_body", "table_display", "models",
      "model_summaries", "reg_check", "approach", "format", "source",
      "adjusted_mode", "adjust_for", "exposures", "interaction")
  )
  expect_false(res$adjusted_mode)
  expect_null(res$adjust_for)
  expect_equal(res$exposures, c("age", "lwt", "race", "smoke"))
  expect_null(res$interaction)
  expect_named(res$models, "multivariable_model")
  expect_s3_class(res$models$multivariable_model, "glm")
  expect_s3_class(res$model_summaries$multivariable_model, "summary.glm")
  expect_true(all(c("exposure", "level", "estimate", "conf.low",
                    "conf.high", "p.value", "ref") %in% names(res$table_body)))
  expect_true(all(c("Characteristic", "Adjusted OR (95% CI)", "p-value",
                    "is_header") %in% names(res$table_display)))
  expect_true(any(res$table_body$ref))
  expect_true("Regression diagnostics available only for 'linear' models." %in% res$reg_check)
})

test_that("multi_reg adjusted mode fits one model per exposure", {
  df <- birthwt_multi_data()

  res <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    adjust_for = c("age", "lwt", "race"),
    approach = "logit",
    theme = clinical
  )

  expect_s3_class(res, "multi_reg")
  expect_named(res$models, c("smoke", "ht", "ui"))
  expect_named(res$model_summaries, c("smoke", "ht", "ui"))
  expect_named(res$reg_check, c("smoke", "ht", "ui"))
  expect_true(res$adjusted_mode)
  expect_equal(res$adjust_for, c("age", "lwt", "race"))
  expect_equal(res$exposures, c("smoke", "ht", "ui"))
  expect_equal(unique(res$table_body$exposure), c("smoke", "ht", "ui"))
  expect_true(all(vapply(res$models, inherits, logical(1), what = "glm")))
})

test_that("multi_reg accepts bare and quoted output options", {
  df <- birthwt_multi_data()

  bare <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = gt,
    theme = minimal
  )
  quoted <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = "logit",
    format = "gt",
    theme = "minimal"
  )

  expect_equal(bare$approach, quoted$approach)
  expect_equal(bare$format, quoted$format)
  expect_equal(bare$table_display, quoted$table_display)
})

test_that("multi_reg supports flextable output", {
  skip_if_not_installed("flextable")

  df <- birthwt_multi_data()

  res <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  expect_s3_class(res, "ft_multi")
  expect_s3_class(res$table, "flextable")
  expect_equal(res$format, "flextable")
})

test_that("multi_reg supports interaction terms in logit models", {
  df <- birthwt_multi_data()

  res <- multi_reg(
    data = df,
    outcome = "low",
    exposures = "smoke",
    adjust_for = c("age", "lwt"),
    interaction = "smoke*ht",
    approach = logit
  )

  expect_s3_class(res, "multi_reg")
  expect_named(res$models, "smoke")
  expect_match(
    paste(deparse(stats::formula(res$models$smoke)), collapse = " "),
    "smoke \\* ht"
  )
  expect_equal(res$table_body$level, c("No", "Yes"))
})

test_that("multi_reg returns diagnostics for linear models", {
  df <- birthwt_multi_data()

  res <- multi_reg(
    data = df,
    outcome = "bwt",
    exposures = c("age", "lwt", "race"),
    approach = linear
  )

  expect_s3_class(res, "multi_reg")
  expect_s3_class(res$models$multivariable_model, "lm")
  expect_true("Adjusted Beta (95% CI)" %in% names(res$table_display))
  expect_type(res$reg_check, "list")
  expect_named(res$reg_check, "multivariable_model")
  expect_true("Test" %in% names(res$reg_check$multivariable_model))
})

test_that("multi_reg validates required variables and outcome types", {
  df <- birthwt_multi_data()

  expect_error(
    multi_reg(df, outcome = "not_here", exposures = "age", approach = logit),
    "Outcome variable not found"
  )
  expect_error(
    multi_reg(df, outcome = "low", exposures = "not_here", approach = logit),
    "exposure variables were not found"
  )
  expect_error(
    multi_reg(df, outcome = "bwt", exposures = "age", approach = logit),
    "requires either a factor variable"
  )
  expect_error(
    multi_reg(df, outcome = "low", exposures = "age", approach = linear),
    "Linear regression requires a continuous outcome"
  )
})

test_that("multi_reg validates adjustment and interaction inputs", {
  df <- birthwt_multi_data()

  expect_error(
    multi_reg(
      df,
      outcome = "low",
      exposures = "smoke",
      adjust_for = "smoke",
      approach = logit
    ),
    "must not overlap"
  )
  expect_error(
    multi_reg(
      df,
      outcome = "low",
      exposures = "smoke",
      adjust_for = "low",
      approach = logit
    ),
    "Outcome variable cannot be included"
  )
  expect_error(
    multi_reg(
      df,
      outcome = "low",
      exposures = "smoke",
      interaction = "smoke:ht",
      approach = logit
    ),
    "not ':'"
  )
  expect_error(
    multi_reg(
      df,
      outcome = "low",
      exposures = "smoke",
      interaction = "age*lwt",
      approach = logit
    ),
    "exposure must be part"
  )
})
