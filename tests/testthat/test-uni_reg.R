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
      "model_summaries", "model_stats", "variable_labels", "reg_check",
      "approach", "format", "source")
  )
  expect_null(res$model_stats)
  expect_true(all(c("exposure", "level", "estimate", "conf.low",
                    "conf.high", "p.value", "ref") %in% names(res$table_body)))
  expect_true(any(res$table_body$ref))
  expect_true(all(c("Characteristic", "OR (95% CI)", "p-value", "N",
                    "is_header") %in% names(res$table_display)))
  expect_true("Ref." %in% res$table_display[["OR (95% CI)"]])
  expect_true(any(grepl("Ref. = reference category", res$table$`_source_notes`,
                        fixed = TRUE)))
})

test_that("uni_reg optionally returns model-fit statistics", {
  df <- mtcars
  df$am <- as.integer(df$am)
  df$cyl <- factor(df$cyl)

  res <- uni_reg(
    data = df,
    outcome = "am",
    exposures = c("mpg", "cyl"),
    approach = logit,
    model_stats = TRUE
  )

  expect_s3_class(res, "uni_reg")
  expect_s3_class(res$model_stats, "data.frame")
  expect_equal(res$model_stats$model, c("mpg", "cyl"))
  expect_true(all(c("AIC", "BIC", "logLik", "deviance", "null_deviance",
                    "pseudo_r2", "r_squared", "adj_r_squared", "n") %in%
                    names(res$model_stats)))
  expect_true(all(is.finite(res$model_stats$AIC)))
  expect_true(all(is.finite(res$model_stats$BIC)))
  expect_true(all(is.na(res$model_stats$r_squared)))
  expect_error(
    uni_reg(df, outcome = "am", exposures = "mpg", model_stats = NA),
    "`model_stats` must be TRUE or FALSE"
  )
})

test_that("uni_reg matches direct model fits with exposure-specific complete cases", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0),
    y_cont = c(1.1, 1.4, 1.8, 2.2, 2.5, 3.0, 3.4, 3.9),
    age = c(40, 44, 50, 52, 60, 62, 70, 72),
    marker = c(1, 0, NA, 1, NA, 0, 1, 0)
  )

  logit_res <- uni_reg(
    data = df,
    outcome = y,
    exposures = c(age, marker),
    approach = logit,
    model_stats = TRUE
  )
  direct_logit_age <- stats::glm(y ~ age, data = df, family = stats::binomial("logit"))
  direct_logit_marker <- stats::glm(y ~ marker, data = df, family = stats::binomial("logit"))

  expect_equal(stats::coef(logit_res$models$age), stats::coef(direct_logit_age), tolerance = 1e-8)
  expect_equal(stats::coef(logit_res$models$marker), stats::coef(direct_logit_marker), tolerance = 1e-8)
  expect_equal(logit_res$model_stats$n, c(stats::nobs(direct_logit_age), stats::nobs(direct_logit_marker)))
  expect_gt(stats::nobs(logit_res$models$age), stats::nobs(logit_res$models$marker))

  linear_res <- uni_reg(
    data = df,
    outcome = y_cont,
    exposures = c(age, marker),
    approach = linear,
    model_stats = TRUE
  )
  direct_lm_age <- stats::lm(y_cont ~ age, data = df)
  direct_lm_marker <- stats::lm(y_cont ~ marker, data = df)

  expect_equal(stats::coef(linear_res$models$age), stats::coef(direct_lm_age), tolerance = 1e-8)
  expect_equal(stats::coef(linear_res$models$marker), stats::coef(direct_lm_marker), tolerance = 1e-8)
  expect_equal(linear_res$model_stats$n, c(stats::nobs(direct_lm_age), stats::nobs(direct_lm_marker)))
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

test_that("uni_reg supports Firth logistic regression", {
  skip_if_not_installed("logistf")

  df <- data_endometrial
  df$HG <- factor(df$HG, levels = c(0, 1),
                  labels = c("Low grade", "High grade"))
  df$NV <- factor(df$NV, levels = c(0, 1),
                  labels = c("Absent", "Present"))

  res <- uni_reg(df, outcome = HG, exposures = c(NV, PI, EH), approach = firth)

  expect_s3_class(res, "uni_reg")
  expect_equal(res$approach, "firth")
  expect_s3_class(res$models$NV, "logistf")
  expect_true("OR (95% CI)" %in% names(res$table_display))
  expect_true(any(res$table_body$ref))
  expect_true(any(is.finite(res$table_body$estimate[!res$table_body$ref])))
  expect_equal(unname(table(df$HG, df$NV)["Low grade", "Present"]), 0)
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

  stats_res <- uni_reg(
    data = df,
    outcome = "mpg",
    exposures = c("hp", "cyl"),
    approach = linear,
    model_stats = TRUE
  )
  expect_true(all(is.na(stats_res$model_stats$pseudo_r2)))
  expect_true(all(is.finite(stats_res$model_stats$r_squared)))
  expect_true(all(is.finite(stats_res$model_stats$adj_r_squared)))
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
  expect_s3_class(res$table, "flextable")
  expect_equal(res$format, "flextable")
  expect_identical(res$engine, res$format)
})
