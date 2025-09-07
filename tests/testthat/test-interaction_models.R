# Interaction models: branch & error coverage

test_that("interaction_models (logit, LRT) returns expected structure and messages", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("dplyr")

  library(dplyr)
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      age_cat = factor(ifelse(age < 30, "Young",
                              ifelse(age < 50, "Middle-aged", "Older")),
                       levels = c("Young", "Middle-aged", "Older")),
      glucose_cat = factor(ifelse(glucose < 140, "Normal", "High"),
                           levels = c("Normal", "High"))
    )

  expect_message(
    res <- interaction_models(
      data = pima,
      outcome = "diabetes",
      exposure = "age_cat",
      effect_modifier = "glucose_cat",
      approach = "logit",
      test = "LRT",
      verbose = TRUE
    ),
    "Interaction Term Assessment", fixed = TRUE
  )

  expect_named(res, c("model_no_interaction","model_with_interaction","p_value","test"))
  expect_equal(res$test, "Likelihood Ratio Test")
  expect_true(is.numeric(res$p_value) || is.na(res$p_value))
})

test_that("interaction_models (logit, Wald) hits Wald branch", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("dplyr")

  library(dplyr); data("PimaIndiansDiabetes2", package = "mlbench")
  pima <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      age_cat = factor(ifelse(age < 30, "Young",
                              ifelse(age < 50, "Middle-aged", "Older")),
                       levels = c("Young", "Middle-aged", "Older")),
      glucose_cat = factor(ifelse(glucose < 140, "Normal", "High"),
                           levels = c("Normal", "High"))
    )

  res <- interaction_models(
    data = pima,
    outcome = "diabetes",
    exposure = "age_cat",
    effect_modifier = "glucose_cat",
    approach = "logit",
    test = "Wald",
    verbose = FALSE
  )
  expect_equal(res$test, "Wald Test")
})

test_that("interaction_models (robpoisson) uses robust variance path", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("sandwich")

  library(dplyr); data("PimaIndiansDiabetes2", package = "mlbench")
  pima <- PimaIndiansDiabetes2 |>
    mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      age_cat = factor(ifelse(age < 30, "Young",
                              ifelse(age < 50, "Middle-aged", "Older")),
                       levels = c("Young", "Middle-aged", "Older")),
      glucose_cat = factor(ifelse(glucose < 140, "Normal", "High"),
                           levels = c("Normal", "High"))
    )

  res <- interaction_models(
    data = pima,
    outcome = "diabetes",
    exposure = "age_cat",
    effect_modifier = "glucose_cat",
    approach = "robpoisson",
    test = "LRT",
    verbose = FALSE
  )
  expect_named(res, c("model_no_interaction","model_with_interaction","p_value","test"))
  expect_equal(res$test, "Likelihood Ratio Test")
})

test_that("interaction_models (negbin, default LRT) covers negbin anova branch", {
  skip_if_not_installed("MASS")

  set.seed(1)
  n <- 250
  f1 <- factor(sample(c("A","B","C"), n, TRUE))
  f2 <- factor(sample(c("X","Y"),     n, TRUE))
  mu <- exp(0.2 + 0.3*(f1=="B") + 0.6*(f1=="C") + 0.4*(f2=="Y") + 0.5*(f1=="B" & f2=="Y"))
  y  <- rpois(n, mu)
  df <- data.frame(y = y, f1 = f1, f2 = f2)

  suppressWarnings({
    res <- interaction_models(
      data = df,
      outcome = "y",
      exposure = "f1",
      effect_modifier = "f2",
      approach = "negbin",
      verbose = FALSE
    )
  })

  expect_named(res, c("model_no_interaction","model_with_interaction","p_value","test"))
  expect_equal(res$test, "Likelihood Ratio Test")
  expect_true(is.numeric(res$p_value) || is.na(res$p_value))
})

test_that("interaction_models (linear) covers lm branch with covariates", {
  set.seed(2)
  n <- 150
  f1 <- factor(sample(c("A","B"), n, TRUE))
  f2 <- factor(sample(c("X","Y"), n, TRUE))
  z  <- rnorm(n)
  y  <- 1 + 0.5*(f1=="B") + 0.2*(f2=="Y") + 0.7*(f1=="B" & f2=="Y") + 0.3*z + rnorm(n, sd=0.5)
  df <- data.frame(y=y, f1=f1, f2=f2, z=z)

  res <- interaction_models(
    data = df,
    outcome = "y",
    exposure = "f1",
    effect_modifier = "f2",
    covariates = "z",
    approach = "linear",
    test = "LRT",
    verbose = FALSE
  )
  expect_named(res, c("model_no_interaction","model_with_interaction","p_value","test"))
  expect_equal(res$test, "Likelihood Ratio Test")
})

test_that("interaction_models errors when model fitting fails (missing predictor)", {
  set.seed(3)
  df <- data.frame(y = rbinom(20, 1, 0.5), a = factor(sample(c("A","B"), 20, TRUE)))
  expect_error(
    interaction_models(
      data = df,
      outcome = "y",
      exposure = "NON_EXISTENT",
      effect_modifier = "a",
      approach = "logit",
      verbose = FALSE
    ),
    "Model fitting failed for one or both models",
    fixed = TRUE
  )
})

test_that("interaction_models errors on invalid approach", {
  set.seed(4)
  df <- data.frame(
    y  = rbinom(50, 1, 0.5),
    e  = factor(sample(c("A","B"), 50, TRUE)),
    m  = factor(sample(c("X","Y"), 50, TRUE))
  )
  expect_error(
    interaction_models(
      data = df,
      outcome = "y",
      exposure = "e",
      effect_modifier = "m",
      approach = "invalid",
      verbose = FALSE
    ),
    "invalid  is not a valid approach for interaction_models",
    fixed = TRUE
  )
})
