collinearity_birthwt_data <- function() {
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

test_that("check_collinearity returns VIF table for numeric predictors", {
  fit <- multi_reg(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear
  )

  out <- check_collinearity(fit, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Variable", "VIF", "Interpretation"))
  expect_equal(out$Variable, c("hp", "wt"))
  expect_true(all(is.finite(out$VIF)))
  expect_true(all(out$VIF >= 1))
  expect_true(all(out$Interpretation %in% c("No collinearity", "Moderate", "High")))
})

test_that("check_collinearity handles factor predictors with adjusted GVIF values", {
  df <- mtcars
  df$cyl <- factor(df$cyl)
  df$gear <- factor(df$gear)

  fit <- multi_reg(
    data = df,
    outcome = "mpg",
    exposures = c("cyl", "gear"),
    approach = linear
  )

  out <- check_collinearity(fit, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Variable, c("cyl", "gear"))
  expect_true(all(is.finite(out$VIF) | is.na(out$VIF)))
  expect_true(all(out$VIF >= 1 | is.na(out$VIF)))
})

test_that("check_collinearity returns list for adjusted multivariable models", {
  df <- collinearity_birthwt_data()

  fit <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt", "race"),
    approach = logit
  )

  out <- check_collinearity(fit, format = tibble)

  expect_type(out, "list")
  expect_equal(names(out), c("smoke", "ht"))
  expect_true(all(vapply(out, inherits, logical(1), what = "tbl_df")))
  expect_true(all(c("age", "lwt", "race") %in% out$smoke$Variable))
})

test_that("check_collinearity handles stratified multivariable models", {
  df <- collinearity_birthwt_data()

  fit <- stratified_multi_reg(
    data = df,
    outcome = "bwt",
    exposures = c("age", "lwt", "smoke"),
    stratifier = "race",
    approach = linear
  )

  out <- check_collinearity(fit, format = tibble)

  expect_type(out, "list")
  expect_equal(names(out), levels(df$race))
  expect_true(all(vapply(out, inherits, logical(1), what = "tbl_df")))
  expect_true(all(vapply(out, function(x) all(c("Variable", "VIF", "Interpretation") %in% names(x)),
                         logical(1))))
})

test_that("check_collinearity rejects univariable and unsupported objects clearly", {
  uni <- uni_reg(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear
  )

  expect_error(
    check_collinearity(uni),
    "VIF is not applicable for univariate models"
  )
  expect_error(
    check_collinearity(stats::lm(mpg ~ hp + wt, data = mtcars)),
    "Input must be a gtregression model object"
  )

  bad <- list(source = "unknown", models = list(stats::lm(mpg ~ hp + wt, data = mtcars)))
  class(bad) <- c("gtregression", "list")
  expect_error(
    check_collinearity(bad),
    "Input must be a fitted model"
  )
})

test_that("check_collinearity validates model list and inner model types", {
  fit <- multi_reg(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear
  )

  missing_models <- fit
  missing_models$models <- NULL
  expect_error(
    check_collinearity(missing_models),
    "Model list not found"
  )

  bad_inner <- fit
  bad_inner$models <- list(multivariable_model = structure(list(), class = "not_a_model"))
  expect_error(
    check_collinearity(bad_inner),
    "Unsupported model type"
  )
})

test_that("check_collinearity returns empty tibble when fewer than two predictors are present", {
  one <- multi_reg(
    data = mtcars,
    outcome = "mpg",
    exposures = "hp",
    approach = linear
  )

  out <- check_collinearity(one, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("Variable", "VIF", "Interpretation"))
})

test_that("check_collinearity reports when VIF cannot be computed", {
  fit_model <- list(terms = stats::terms(y ~ x1 + x2))
  class(fit_model) <- "lm"
  obj <- list(
    source = "multi_reg",
    models = list(multivariable_model = fit_model)
  )
  class(obj) <- c("gtregression", "multi_reg", "list")

  out <- check_collinearity(obj, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$Variable, NA_character_)
  expect_true(is.na(out$VIF))
  expect_equal(out$Interpretation, "VIF could not be computed")
})

test_that("check_collinearity handles simulated car::vif vector and GVIF matrix outputs", {
  fit <- multi_reg(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear
  )

  old <- options(gtregression.vif_fun = function(model) c(hp = 1.5, wt = 6.2))
  on.exit(options(old), add = TRUE)
  vector_out <- check_collinearity(fit, format = tibble)

  expect_equal(vector_out$Variable, c("hp", "wt"))
  expect_equal(vector_out$VIF, c(1.5, 6.2))
  expect_equal(vector_out$Interpretation, c("No collinearity", "High"))

  options(gtregression.vif_fun = function(model) {
    out <- matrix(c(4, 1, 9, 2), ncol = 2, byrow = TRUE)
    colnames(out) <- c("GVIF", "Df")
    rownames(out) <- c("hp", "wt")
    out
  })
  gvif_out <- check_collinearity(fit, format = tibble)

  expect_equal(gvif_out$Variable, c("hp", "wt"))
  expect_equal(gvif_out$VIF, c(2, round(9^(1 / 4), 2)))

  options(gtregression.vif_fun = function(model) {
    out <- matrix(c(2, 3), ncol = 1)
    rownames(out) <- c("hp", "wt")
    out
  })
  matrix_out <- check_collinearity(fit, format = tibble)

  expect_equal(matrix_out$Variable, c("hp", "wt"))
  expect_equal(matrix_out$VIF, c(2, 3))
})

test_that("check_collinearity handles legacy multi_reg_nbin source", {
  data("quine", package = "MASS")
  fit_model <- MASS::glm.nb(Days ~ Eth + Sex, data = quine)
  obj <- list(
    source = "multi_reg_nbin",
    models = list(multivariable_model = fit_model)
  )
  class(obj) <- c("gtregression", "multi_reg_nbin", "list")

  out <- check_collinearity(obj, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Eth", "Sex") %in% out$Variable))
})

test_that("check_collinearity preserves nested adjusted stratified output", {
  df <- collinearity_birthwt_data()
  fit <- stratified_multi_reg(
    data = df,
    outcome = "bwt",
    exposures = c("smoke", "ht"),
    stratifier = "race",
    adjust_for = c("age", "lwt"),
    approach = linear
  )

  out <- check_collinearity(fit, format = tibble)

  expect_type(out, "list")
  expect_true(is.list(out$White))
  expect_true(all(c("smoke", "ht") %in% names(out$White)))
  expect_s3_class(out$White$smoke, "tbl_df")
})

test_that("check_collinearity can return gt and flextable outputs", {
  fit <- multi_reg(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear
  )

  gt_out <- check_collinearity(fit, format = gt)
  ft_out <- check_collinearity(fit, format = flextable)

  expect_s3_class(gt_out, "gt_tbl")
  expect_s3_class(ft_out, "flextable")
  expect_equal(names(gt_out$`_data`), c("Variable", "VIF", "Interpretation"))
  expect_match(
    as.character(gt_out$`_source_notes`[[1]]),
    "Screening aid only"
  )
})

test_that("check_collinearity formats nested list outputs without flattening", {
  df <- collinearity_birthwt_data()
  fit <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = c("age", "lwt", "race"),
    approach = logit
  )

  out <- check_collinearity(fit, format = gt)

  expect_type(out, "list")
  expect_equal(names(out), c("smoke", "ht"))
  expect_s3_class(out$smoke, "gt_tbl")
  expect_s3_class(out$ht, "gt_tbl")
})
