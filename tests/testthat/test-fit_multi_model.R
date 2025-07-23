test_that(".fit_multi_model returns correct model class for each approach", {
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
      age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
      bmi_cat = dplyr::case_when(
        mass < 25 ~ "Normal",
        mass >= 25 & mass < 30 ~ "Overweight",
        mass >= 30 ~ "Obese"
      ),
      bmi_cat = factor(bmi_cat, levels = c("Normal", "Overweight", "Obese"))
    ) |>
    dplyr::filter(!is.na(diabetes), !is.na(age_cat), !is.na(bmi_cat), !is.na(mass), !is.na(glucose))

  exposures <- c("age_cat", "bmi_cat")

  # logit
  m_logit <- .fit_multi_model(df, outcome = "diabetes", exposures = exposures, approach = "logit")
  expect_s3_class(m_logit, "glm")
  expect_equal(family(m_logit)$family, "binomial")

  # log-binomial
  m_logbin <- .fit_multi_model(df, outcome = "diabetes", exposures = exposures, approach = "log-binomial")
  expect_s3_class(m_logbin, "glm")
  expect_equal(family(m_logbin)$link, "log")

  # poisson (using count outcome)
  df$count_outcome <- round(df$glucose / 10)
  m_pois <- .fit_multi_model(df, outcome = "count_outcome", exposures = exposures, approach = "poisson")
  expect_s3_class(m_pois, "glm")
  expect_equal(family(m_pois)$family, "poisson")

  # linear
  m_lm <- .fit_multi_model(df, outcome = "mass", exposures = exposures, approach = "linear")
  expect_s3_class(m_lm, "lm")

  # robpoisson
  m_rob <- .fit_multi_model(df, outcome = "diabetes", exposures = exposures, approach = "robpoisson")
  expect_s3_class(m_rob, "risks")
  expect_true("robpoisson" %in% class(m_rob))

  # negbin
  df <- df[complete.cases(df[, c("glucose", exposures)]), ]

  m_negbin <- .fit_multi_model(df, outcome = "glucose",
                               exposures = exposures, approach = "negbin")
  expect_s3_class(m_negbin, "glm")
})

test_that(".fit_multi_model returns NULL and warns on failure", {
  df_fail <- data.frame(
    outcome = c(1, 0, 1, 0),
    x1 = factor("only", levels = "only"),  # one-level factor
    x2 = factor("only", levels = "only")
  )

  expect_warning({
    model <- .fit_multi_model(df_fail, outcome = "outcome", exposures = c("x1", "x2"), approach = "logit")
    expect_null(model)
  }, regexp = "Model failed for")
})

