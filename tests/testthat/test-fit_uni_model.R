test_that(".fit_uni_model returns glm or riskratio object for binary outcomes",
          {
  skip_if_not_installed("mlbench")
  data("PimaIndiansDiabetes2", package = "mlbench")

  pima <- PimaIndiansDiabetes2 |>
    dplyr::mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |>
    tidyr::drop_na()

  model_logit <- .fit_uni_model(pima,
                              outcome = "diabetes",
                              exposure = "glucose",
                              approach = "logit")
  model_rr <- .fit_uni_model(pima,
                           outcome = "diabetes",
                           exposure = "glucose",
                           approach = "robpoisson")

  expect_s3_class(model_logit, "glm")
  expect_true(inherits(model_rr, "robpoisson") || inherits(model_rr, "risks"))

  expect_true(any(grepl("glucose", names(coef(model_logit)))))
})

test_that(".fit_uni_model throws error on invalid outcome type", {
  data("birthwt", package = "MASS")
  expect_warning(
    .fit_uni_model(birthwt,
                   outcome = "bwt",
                   exposure = "age",
                   approach = "logit"),
    regexp = "y values must be 0 <= y <= 1"
  )
})
