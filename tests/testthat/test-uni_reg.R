test_that("uni_reg returns a gtregression object for binary logistic models", {
  df <- mtcars
  df$am <- as.integer(df$am)
  df$cyl <- factor(df$cyl)

  res <- uni_reg(
    data = df,
    outcome = "am",
    exposures = c("mpg", "cyl"),
    approach = logit,
    format = gt
  )

  expect_s3_class(res, "gtregression")
  expect_s3_class(res, "uni_reg")
  expect_s3_class(res, "gt_uni")
  expect_s3_class(res$table, "gt_tbl")
  expect_equal(res$approach, "logit")
  expect_equal(res$format, "gt")
  expect_equal(res$source, "uni_reg")
  expect_named(
    res,
    c("table", "table_body", "table_display", "models",
      "model_summaries", "reg_check", "approach", "format", "source")
  )
  expect_true(all(c("exposure", "level", "estimate", "conf.low",
                    "conf.high", "p.value", "ref") %in% names(res$table_body)))
  expect_true(any(res$table_body$ref))
  expect_true(all(c("Characteristic", "OR (95% CI)", "p-value", "N",
                    "is_header") %in% names(res$table_display)))
})

test_that("uni_reg supports logbinomial and old hyphenated alias", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 1, 0)
  )

  res_new <- uni_reg(df, outcome = "y", exposures = "x", approach = logbinomial)
  res_old <- uni_reg(df, outcome = "y", exposures = "x", approach = "log-binomial")

  expect_equal(res_new$approach, "logbinomial")
  expect_equal(res_old$approach, "logbinomial")
  expect_named(res_new$table_display, c("Characteristic", "N", "RR (95% CI)", "p-value", "is_header"), ignore.order = TRUE)
})

test_that("uni_reg supports poisson and negative binomial count models", {
  df <- data.frame(
    count = c(0, 1, 2, 3, 1, 4, 2, 5, 3, 6),
    exposure = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  )

  pois <- uni_reg(df, outcome = "count", exposures = "exposure", approach = poisson)
  nb <- uni_reg(df, outcome = "count", exposures = "exposure", approach = negbin)

  expect_equal(pois$approach, "poisson")
  expect_equal(nb$approach, "negbin")
  expect_true("IRR (95% CI)" %in% names(pois$table_display))
  expect_true("IRR (95% CI)" %in% names(nb$table_display))
})

test_that("uni_reg supports robust poisson when risks is available", {
  skip_if_not_installed("risks")

  df <- data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0),
    x = c(0, 0, 1, 1, 1, 0, 1, 0)
  )

  res <- uni_reg(df, outcome = "y", exposures = "x", approach = robpoisson)

  expect_equal(res$approach, "robpoisson")
  expect_true("RR (95% CI)" %in% names(res$table_display))
})

test_that("uni_reg returns diagnostics for linear model", {
  df <- mtcars
  df$cyl <- factor(df$cyl)

  res <- uni_reg(
    data = df,
    outcome = "mpg",
    exposures = c("hp", "cyl"),
    approach = linear
  )

  expect_s3_class(res, "uni_reg")
  expect_type(res$reg_check, "list")
  expect_named(res$reg_check, c("hp", "cyl"))
  expect_match(res$reg_check$hp$Test[1], "Breusch-Pagan")
  expect_true("Beta (95% CI)" %in% names(res$table_display))
})

test_that("uni_reg supports flextable output", {
  skip_if_not_installed("flextable")

  df <- mtcars
  df$am <- as.integer(df$am)

  res <- uni_reg(
    data = df,
    outcome = "am",
    exposures = "mpg",
    approach = logit,
    format = flextable
  )

  expect_s3_class(res, "ft_uni")
  expect_s3_class(res$table, "flextable")
  expect_equal(res$format, "flextable")
})

test_that("uni_reg errors clearly when all models fail", {
  df <- mtcars
  df$onelevel <- factor(rep("only", nrow(df)))

  expect_error(
    suppressWarnings(
      uni_reg(df, outcome = "mpg", exposures = "onelevel", approach = linear)
    ),
    "Exposure with <2 levels"
  )
})

test_that("uni_reg handles bad outcome type gracefully", {
  df <- mtcars
  df$mpg_char <- as.character(df$mpg)

  expect_error(
    uni_reg(df, outcome = "mpg_char", exposures = "hp", approach = linear),
    "requires a continuous"
  )
})

test_that("uni_reg accessors return current components", {
  df <- mtcars
  df$am <- as.integer(df$am)

  res <- uni_reg(
    data = df,
    outcome = "am",
    exposures = "mpg",
    approach = logit
  )

  expect_s3_class(res$models[[1]], "glm")
  expect_s3_class(res$model_summaries[[1]], "summary.glm")
  expect_s3_class(res$table, "gt_tbl")
  expect_identical(res$engine, res$format)
})
