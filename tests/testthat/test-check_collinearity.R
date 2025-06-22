test_that("check_collinearity works correctly for multivariable models", {
  library(gtregression)
  library(dplyr)
  library(mlbench)

  data(PimaIndiansDiabetes2, package = "mlbench")
  pima <- PimaIndiansDiabetes2 |>
    filter(!is.na(diabetes)) |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0))

  # multivariable linear model
  multi_lm <- multi_reg(
    data = pima,
    outcome = "glucose",
    exposures = c("age", "mass", "pressure"),
    approach = "linear"
  )

  vif_tbl <- check_collinearity(multi_lm)
  expect_s3_class(vif_tbl, "tbl_df")
  expect_true(all(c("Variable", "VIF", "Interpretation") %in% names(vif_tbl)))
  expect_equal(nrow(vif_tbl), 3)
})

test_that("check_collinearity throws error for univariate models", {
  data(PimaIndiansDiabetes2, package = "mlbench")
  pima <- PimaIndiansDiabetes2 |>
    filter(!is.na(diabetes)) |>
    mutate(diabetes = ifelse(diabetes == "pos", 1, 0))

  uni_lm <- uni_reg(
    data = pima,
    outcome = "glucose",
    exposures = c("age", "mass", "pressure"),
    approach = "linear"
  )

  expect_error(
    check_collinearity(uni_lm),
    "VIF is not applicable for univariate models"
  )
})
