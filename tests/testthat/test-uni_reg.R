test_that("uni_reg returns a gtsummary object and works with  binary data", {
  skip_if_not_installed("mlbench")
  skip_if_not_installed("gtsummary")

  data("PimaIndiansDiabetes2", package = "mlbench")
  pima <- dplyr::mutate(
    PimaIndiansDiabetes2,
    diabetes = ifelse(diabetes == "pos", 1, 0)
  ) |>
    tidyr::drop_na()

  res <- uni_reg(
    data = pima,
    outcome = "diabetes",
    exposures = c("age", "glucose"),
    approach = "logit"
  )

  expect_s3_class(res, "uni_reg")
  expect_s3_class(res, "gtsummary")
  expect_true(nrow(res$table_body) > 0)
})

test_that("uni_reg returns diagnostics for linear model", {
  data("birthwt", package = "MASS")
  bw <- dplyr::mutate(birthwt, race = factor(race))

  res <- uni_reg(
    data = bw,
    outcome = "bwt",
    exposures = c("age", "race"),
    approach = "linear"
  )

  expect_s3_class(res, "uni_reg")
  expect_type(res$reg_check, "list")
  expect_match(res$reg_check[[1]]$Test[1], "Breusch-Pagan")
})

test_that("uni_reg throws error if all models fail", {
  data("birthwt", package = "MASS")
  bw <- dplyr::mutate(birthwt, onelevel = factor(rep("only", nrow(birthwt))))

  expect_error(suppressMessages(
    uni_reg(bw, outcome = "bwt",
            exposures = c("onelevel"),
            approach = "linear"),
    regexp = "All models failed"
  )
  )
})



test_that("uni_reg handles bad outcome type gracefully", {
  data("birthwt", package = "MASS")
  bw <- dplyr::mutate(birthwt, bwt_char = as.character(bwt))

  expect_error(
    uni_reg(bw, outcome = "bwt_char",
            exposures = c("age"),
            approach = "linear"),
    regexp = "requires a continuous"
  )
})

test_that("uni_reg$ accessors return correct components", {
  skip_if_not_installed("mlbench")
  data("PimaIndiansDiabetes2", package = "mlbench")
  pima <- tidyr::drop_na(PimaIndiansDiabetes2)
  pima$diabetes <- as.integer(pima$diabetes == "pos")

  res <- uni_reg(
    data = pima,
    outcome = "diabetes",
    exposures = c("glucose"),
    approach = "logit"
  )

  expect_s3_class(res$models[[1]], "glm")
  expect_s3_class(res$model_summaries[[1]], "summary.glm")
  expect_equal(res$table, res)
})

