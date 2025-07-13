test_that(".validate_approach accepts valid inputs", {
  expect_invisible(.validate_approach("logit", context = "uni_reg"))
  expect_invisible(.validate_approach("negbin", context = "multi_reg_nbin"))
})

test_that(".validate_approach throws error for invalid input", {
    expect_error(
      .validate_approach("R", context = "uni_reg"),
      regexp = "^R  is not a valid approach for uni_reg\\.",
      perl = TRUE
    )
  expect_error(
    .validate_approach("logit", context = "unknown_context"),
    regexp = "The function 'unknown_context' is not recognized",
    fixed = TRUE
  )
})

test_that(".validate_outcome_by_approach validates outcome type", {
  binary <- c(0, 1, 1, 0)
  count <- c(1, 2, 3, 0)
  non_cont <- c(0, 1, 2)
  decimal <- c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)

  expect_silent(.validate_outcome_by_approach(binary, "logit"))
  expect_silent(.validate_outcome_by_approach(count, "poisson"))
  expect_silent(.validate_outcome_by_approach(decimal, "linear"))
  expect_silent(.validate_outcome_by_approach(binary, "poisson"))


  expect_error(
    .validate_outcome_by_approach(non_cont, "logit"),
    regexp = "This approach requires either a factor variable."
  )

})

test_that(".validate_uni_inputs checks all validation layers", {
  df <- data.frame(x = c(1, 2), y = c(0, 1))
  expect_silent(.validate_uni_inputs(df, outcome = "y", exposures = "x", approach = "logit"))

  expect_error(
    .validate_uni_inputs(df, outcome = "missing", exposures = "x", approach = "logit"),
    regexp = "Outcome variable not found in the dataset."
  )
  expect_error(
    .validate_uni_inputs(df, outcome = "y", exposures = "badvar", approach = "logit"),
    regexp = "One or more exposure variables were not found in the dataset"
  )
})

test_that(".validate_nb_multi_inputs rejects insufficient data", {
  df <- data.frame(y = c(2, 3), x1 = c(1, 1), x2 = c(2, 3))  # x1 has <2 unique values

  expect_error(
    .validate_nb_multi_inputs(df, outcome = "y", exposures = c("x1", "x2")),
    regexp = "Exposure(s) has less than 2 unique values: x1", fixed= TRUE
  )
})
