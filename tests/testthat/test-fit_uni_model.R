test_that(".fit_uni_model returns correct model class (PimaIndiansDiabetes2)", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("risks")

  data("PimaIndiansDiabetes2", package = "mlbench")

  df <- PimaIndiansDiabetes2 |>
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

  # log-binomial
  m2 <- .fit_uni_model(df, outcome = "diabetes", exposure = "age_cat", approach = "log-binomial")
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

})

test_that(".fit_uni_model handles model fitting failure gracefully", {
  data("PimaIndiansDiabetes2", package = "mlbench")

  df <- PimaIndiansDiabetes2 |>
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
