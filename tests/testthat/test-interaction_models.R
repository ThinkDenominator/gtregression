birthwt_interaction_data <- function() {
  data("data_birthwt", package = "gtregression")

  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
    )
}

test_that("interaction_models returns focused result for logit LRT", {
  df <- birthwt_interaction_data()

  result <- interaction_models(
    data = df,
    outcome = "low",
    exposure = "smoke",
    effect_modifier = "race",
    covariates = c("age", "lwt"),
    approach = logit,
    test = LRT,
    format = tibble
  )

  expect_s3_class(result, "interaction_models_result")
  expect_named(
    result,
    c(
      "summary", "model_no_interaction", "model_with_interaction",
      "robust_no_interaction", "robust_with_interaction",
      "formula_no_interaction", "formula_with_interaction",
      "interaction_terms", "comparison", "p_value", "alpha",
      "has_interaction", "decision", "interpretation", "test",
      "approach", "source"
    )
  )
  expect_s3_class(result$summary, "tbl_df")
  expect_equal(result$test, "Likelihood Ratio Test")
  expect_true(is.numeric(result$p_value) || is.na(result$p_value))
  expect_true(result$decision %in%
                c("interaction", "no_interaction", "not_estimable"))
  expect_true(length(result$interaction_terms) >= 1)
})

test_that("interaction_models supports Wald test and verbose message", {
  df <- birthwt_interaction_data()

  expect_message(
    result <- interaction_models(
      data = df,
      outcome = "low",
      exposure = "smoke",
      effect_modifier = "race",
      approach = "logit",
      test = "Wald",
      verbose = TRUE
    ),
    "interaction"
  )

  expect_equal(result$test, "Wald Test")
  expect_s3_class(result$summary, "tbl_df")
})

test_that("interaction_models can add gt and flextable viewing tables", {
  df <- birthwt_interaction_data()

  gt_result <- interaction_models(
    data = df,
    outcome = "low",
    exposure = "smoke",
    effect_modifier = "race",
    approach = logit,
    format = gt
  )
  ft_result <- interaction_models(
    data = df,
    outcome = "low",
    exposure = "smoke",
    effect_modifier = "race",
    approach = logit,
    format = flextable
  )

  expect_s3_class(gt_result$summary, "tbl_df")
  expect_s3_class(gt_result$table, "gt_tbl")
  expect_s3_class(ft_result$table, "flextable")
  expect_match(
    as.character(gt_result$table$`_source_notes`[[1]]),
    "Screening aid only"
  )
})

test_that("interaction_models supports robpoisson and robust coefficient tables", {
  df <- birthwt_interaction_data()

  result <- interaction_models(
    data = df,
    outcome = "low",
    exposure = "smoke",
    effect_modifier = "race",
    approach = robpoisson,
    test = Wald
  )

  expect_equal(result$approach, "robpoisson")
  expect_s3_class(result$robust_no_interaction, "coeftest")
  expect_s3_class(result$robust_with_interaction, "coeftest")
  expect_s3_class(result$model_no_interaction, "glm")
})

test_that("interaction_models supports linear models with covariates", {
  set.seed(2)
  n <- 150
  df <- data.frame(
    y = rnorm(n),
    x = factor(sample(c("A", "B"), n, TRUE)),
    z = factor(sample(c("Low", "High"), n, TRUE)),
    age = rnorm(n)
  )

  result <- interaction_models(
    data = df,
    outcome = "y",
    exposure = "x",
    effect_modifier = "z",
    covariates = "age",
    approach = linear,
    test = LRT
  )

  expect_s3_class(result$model_no_interaction, "lm")
  expect_equal(result$summary$approach, "linear")
  expect_match(deparse(result$formula_with_interaction), "x:z")
})

test_that("interaction_models supports poisson and negative binomial models", {
  data("quine", package = "MASS")
  quine_data <- quine |>
    dplyr::mutate(dplyr::across(c(Eth, Sex, Age, Lrn), as.factor))

  pois <- interaction_models(
    data = quine_data,
    outcome = "Days",
    exposure = "Sex",
    effect_modifier = "Eth",
    covariates = "Age",
    approach = poisson
  )
  nb <- interaction_models(
    data = quine_data,
    outcome = "Days",
    exposure = "Sex",
    effect_modifier = "Eth",
    covariates = "Age",
    approach = negbin
  )

  expect_s3_class(pois$model_no_interaction, "glm")
  expect_s3_class(nb$model_no_interaction, "negbin")
  expect_true(is.numeric(pois$p_value) || is.na(pois$p_value))
  expect_true(is.numeric(nb$p_value) || is.na(nb$p_value))
})

test_that("interaction_models classifies detected interactions using alpha", {
  df <- birthwt_interaction_data()

  result <- interaction_models(
    data = df,
    outcome = "low",
    exposure = "smoke",
    effect_modifier = "race",
    approach = logit,
    alpha = 0.99
  )

  expect_true(result$has_interaction)
  expect_equal(result$decision, "interaction")
  expect_match(result$interpretation, "Evidence of interaction")
})

test_that("interaction_models handles not-estimable comparison", {
  model1 <- lm(mpg ~ wt, data = mtcars)
  comp <- gtregression:::.interaction_compare_models(
    model1,
    model1,
    test = "LRT",
    approach = "linear"
  )

  expect_true(is.na(comp$p_value))
})

test_that("interaction_models validates inputs clearly", {
  df <- birthwt_interaction_data()

  expect_error(
    interaction_models(list(), "low", "smoke", effect_modifier = "race"),
    "`data` must be a data frame"
  )
  expect_error(
    interaction_models(df, c("low", "bwt"), "smoke", effect_modifier = "race"),
    "`outcome` must be a single character variable name"
  )
  expect_error(
    interaction_models(df, "low", c("smoke", "ht"), effect_modifier = "race"),
    "`exposure` must be a single character variable name"
  )
  expect_error(
    interaction_models(df, "low", "smoke", effect_modifier = c("race", "ht")),
    "`effect_modifier` must be a single character variable name"
  )
  expect_error(
    interaction_models(df, "low", "smoke", effect_modifier = "race",
                       covariates = c("age", NA_character_)),
    "`covariates` must be NULL or a character vector"
  )
  expect_error(
    interaction_models(df, "low", "smoke", effect_modifier = "race",
                       alpha = 1),
    "`alpha` must be a single number between 0 and 1"
  )
  expect_error(
    interaction_models(df, "low", "smoke", effect_modifier = "race",
                       verbose = NA),
    "`verbose` must be TRUE or FALSE"
  )
  expect_error(
    interaction_models(df, "missing", "smoke", effect_modifier = "race"),
    "Variables not found: missing"
  )
  expect_error(
    interaction_models(df, "low", "smoke", effect_modifier = "race",
                       approach = "bad"),
    "not a valid approach"
  )
})

test_that("interaction_models errors when no complete cases are available", {
  df <- birthwt_interaction_data()
  df$smoke <- NA

  expect_error(
    interaction_models(
      data = df,
      outcome = "low",
      exposure = "smoke",
      effect_modifier = "race",
      approach = logit
    ),
    "No complete cases available"
  )
})
