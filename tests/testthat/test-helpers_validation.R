test_that(".validate_approach accepts valid inputs for each context", {
  expect_invisible(.validate_approach("logit",         "uni_reg"))
  expect_invisible(.validate_approach("negbin",        "multi_reg"))
  expect_invisible(.validate_approach("robpoisson",    "interaction_models"))
  expect_invisible(.validate_approach("log-binomial",  "check_convergence"))
  expect_invisible(.validate_approach("linear",        "identify_confounder"))
  expect_invisible(.validate_approach("poisson",       "select_models"))
})

test_that(".validate_approach errors for invalid approach and unknown context", {
  # Allow variable whitespace in the error ("R  is ...")
  expect_error(
    .validate_approach("R", "uni_reg"),
    regexp = "^R\\s+is not a valid approach for\\s+uni_reg\\.",
    perl   = TRUE
  )
  expect_error(
    .validate_approach("logit", "unknown_context"),
    regexp = "The function 'unknown_context' is not recognized",
    fixed  = TRUE
  )
})

test_that(".validate_exposures passes with good inputs", {
  df <- data.frame(
    num = c(1, 2, 3, 4),
    fac = factor(c("a", "b", "a", "b")),
    chr = c("x", "y", "x", "y")
  )
  expect_invisible(.validate_exposures(df, c("num", "fac", "chr")))
})

test_that(".validate_exposures aggregates all error messages", {
  df <- data.frame(
    allNA  = c(NA, NA, NA),
    fac1   = factor(c("a", "a", NA)),  # < 2 levels after NA drop
    const  = c(5, 5, 5),               # no numeric variation
    when   = as.Date(c("2020-01-01", NA, "2020-01-03"))  # unsupported type branch
  )

  msg <- tryCatch(
    { .validate_exposures(df, names(df)); "" },
    error = function(e) e$message
  )

  expect_true(nzchar(msg))
  needles <- c("all missing", "<2 levels", "no variation", "unsupported types")
  expect_true(all(vapply(needles, function(p) grepl(p, msg, ignore.case = TRUE), logical(1))))
})

test_that(".validate_outcome_by_approach: success cases", {
  bin_01     <- c(0,1,1,0,1)
  bin_12     <- c(1,2,2,1,1)        # also allowed
  counts     <- c(0,1,2,3,5)
  continuous <- c(1.2, 2.4, 3.1, 4.0, 5.6)

  expect_silent(.validate_outcome_by_approach(bin_01, "logit"))
  expect_silent(.validate_outcome_by_approach(bin_12, "logit"))
  expect_silent(.validate_outcome_by_approach(bin_01, "robpoisson"))
  expect_silent(.validate_outcome_by_approach(bin_12, "log-binomial"))
  expect_silent(.validate_outcome_by_approach(counts, "poisson"))
  expect_silent(.validate_outcome_by_approach(counts, "negbin"))
  expect_silent(.validate_outcome_by_approach(continuous, "linear"))
})

test_that(".validate_outcome_by_approach: failure cases", {
  all_na   <- c(NA, NA, NA)
  nonbin   <- c(0, 1, 2)      # not binary
  noncount <- c(1.5, 2.3, 3)  # not all integers / may include decimals
  notcont  <- c(0, 1, 1, 0)   # not continuous (>2 uniques required)

  expect_error(.validate_outcome_by_approach(all_na, "logit"),
               "All values in the outcome variable are missing", fixed = TRUE)

  expect_error(.validate_outcome_by_approach(nonbin, "logit"),
               regexp = "requires either a factor variable|numeric variable coded as 0 and 1",
               ignore.case = TRUE)

  expect_error(.validate_outcome_by_approach(noncount, "poisson"),
               "Poisson regression requires a count outcome", fixed = TRUE)
  expect_error(.validate_outcome_by_approach(noncount, "negbin"),
               "Negative binomial requires a count outcome", fixed = TRUE)

  expect_error(.validate_outcome_by_approach(notcont, "linear"),
               "Linear regression requires a continuous outcome", fixed = TRUE)
})

test_that(".validate_uni_inputs wires validations correctly", {
  df <- data.frame(y = c(0,1,1,0), x = c(1,2,3,4))
  expect_silent(.validate_uni_inputs(df, outcome = "y", exposures = "x", approach = "logit"))

  expect_error(.validate_uni_inputs(df, outcome = "missing", exposures = "x", approach = "logit"),
               "Outcome variable not found", fixed = TRUE)
  expect_error(.validate_uni_inputs(df, outcome = "y", exposures = "bad", approach = "logit"),
               "One or more exposure variables were not found", fixed = TRUE)

  # Exposure invalid (<2 levels) → bubbles up from .validate_exposures()
  df_bad <- data.frame(y = c(0,1,1,0), x = factor(c("a","a","a","a")))
  expect_error(.validate_uni_inputs(df_bad, outcome = "y", exposures = "x", approach = "logit"),
               "<2 levels", fixed = FALSE)
})

test_that(".validate_multi_inputs wires validations correctly", {
  df <- data.frame(y = c(0,1,1,0), x = c(1,2,3,4), z = factor(c("a","b","a","b")))
  expect_silent(.validate_multi_inputs(df, outcome = "y", exposures = c("x","z"), approach = "logit"))

  expect_error(.validate_multi_inputs(df, outcome = "missing", exposures = c("x","z"), approach = "logit"),
               "Outcome variable not found", fixed = TRUE)
  expect_error(.validate_multi_inputs(df, outcome = "y", exposures = c("x","bad"), approach = "logit"),
               "One or more exposure variables were not found", fixed = TRUE)

  # Exposure invalid (<2 levels) → bubbles up from .validate_exposures()
  df_bad <- data.frame(y = c(0,1,1,0), x = factor(c("a","a","a","a")), z = factor(c("a","b","a","b")))
  expect_error(.validate_multi_inputs(df_bad, outcome = "y", exposures = c("x","z"), approach = "logit"),
               "<2 levels", fixed = FALSE)
})
# End of file
