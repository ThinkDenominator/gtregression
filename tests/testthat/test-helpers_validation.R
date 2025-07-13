test_that(".validate_approach accepts valid inputs", {
  expect_invisible(.validate_approach("logit", context = "uni_reg"))
  expect_invisible(.validate_approach("negbin", context = "multi_reg_nbin"))
})

test_that(".validate_approach throws error for invalid input", {
  expect_error(.validate_approach("foo", context = "uni_reg"),
               regexp = "Invalid approach")
  expect_error(.validate_approach("logit", context = "unknown_context"),
               regexp = "Unknown context")
})

test_that(".validate_outcome_by_approach validates outcome type", {
  binary <- c(0, 1, 1, 0)
  count <- c(1, 2, 3, 0)
  cont  <- c(1.2, 3.4, 5.6, 2.1)

  expect_silent(.validate_outcome_by_approach(binary, "logit"))
  expect_silent(.validate_outcome_by_approach(count, "poisson"))
  expect_error(.validate_outcome_by_approach(cont, "linear"),
               regexp = "Linear regression requires a continuous outcome.")
  expect_error(.validate_outcome_by_approach(cont, "logit"),
               regexp = "binary outcome")
  expect_error(.validate_outcome_by_approach(binary, "poisson"),
               regexp = "Count outcome required")
})

test_that(".validate_uni_inputs checks all validation layers", {
  df <- data.frame(x = c(1, 2), y = c(0, 1))
  expect_silent(.validate_uni_inputs(df, outcome = "y", exposures = "x", approach = "logit"))
  expect_error(.validate_uni_inputs(df, outcome = "missing", exposures = "x", approach = "logit"),
               regexp = "Outcome variable not found")
  expect_error(.validate_uni_inputs(df, outcome = "y", exposures = "badvar", approach = "logit"),
               regexp = "One or more exposure variables")
})

test_that(".validate_nb_multi_inputs rejects insufficient data", {
  df <- data.frame(y = c(2, 3), x1 = c(1, 1), x2 = c(2, 3))  # x1 has no variation

  expect_error(.validate_nb_multi_inputs(df, outcome = "y", exposures = c("x1", "x2")),
               regexp = "Negative binomial requires a count outcome.")
})
