test_that(".fit_uni_model returns correct model class", {
  skip_if_not_installed("risks")

  df <- data_SynthDiabetes |>
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      age_cat = dplyr::case_when(
        age < 30 ~ "Young",
        age >= 30 & age < 50 ~ "Middle-aged",
        age >= 50 ~ "Older"
      ),
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older"))
    ) |>
    dplyr::filter(!is.na(diabetes), !is.na(age_cat), !is.na(mass))

  # logit
  m1 <- .fit_uni_model(df, outcome = "diabetes", exposure = "age_cat", approach = "logit")
  expect_s3_class(m1, "glm")
  expect_equal(family(m1)$family, "binomial")

  # logbinomial
  m2 <- .fit_uni_model(df, outcome = "diabetes", exposure = "age_cat", approach = "logbinomial")
  expect_s3_class(m2, "glm")
  expect_equal(family(m2)$link, "log")

  # poisson
  df$count_outcome <- round(df$glucose / 10)
  m3 <- .fit_uni_model(df, outcome = "count_outcome", exposure = "age_cat", approach = "poisson")
  expect_s3_class(m3, "glm")
  expect_equal(family(m3)$family, "poisson")

  # linear
  m4 <- .fit_uni_model(df, outcome = "mass", exposure = "age_cat", approach = "linear")
  expect_s3_class(m4, "lm")

  # robpoisson
  m5 <- .fit_uni_model(df, outcome = "diabetes", exposure = "age_cat", approach = "robpoisson")
  expect_true(any(class(m5) %in% c("riskratio", "risks")))

  # negbin

  m6 <- .fit_uni_model(df, outcome = "glucose", exposures = "age_cat", approach = "negbin")
  expect_s3_class(m6, "negbin")
  expect_s3_class(m6, "glm")


})

test_that(".fit_uni_model handles model fitting failure gracefully", {
  df <- data_SynthDiabetes |>
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      constant = factor("only", levels = "only")
    )

  expect_warning(
    model <- .fit_uni_model(df, outcome = "diabetes", exposure = "constant", approach = "logit"),
    regexp = "Model failed for"
  )
  expect_null(model)
})

test_that(".fit_uni_model supports Firth logistic regression when logistf is available", {
  skip_if_not_installed("logistf")

  df <- data_endometrial
  df$HG <- factor(df$HG, levels = c(0, 1))
  df$NV <- factor(df$NV, levels = c(0, 1))

  model <- .fit_uni_model(df, outcome = "HG", exposures = "NV", approach = "firth")

  expect_s3_class(model, "logistf")
  expect_true("gtregression_model_frame" %in% names(attributes(model)))
})
