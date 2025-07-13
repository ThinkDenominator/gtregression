test_that(".get_effect_label returns correct labels", {
  expect_equal(.get_effect_label("logit"), "**OR**")
  expect_equal(.get_effect_label("poisson"), "**IRR**")
  expect_equal(.get_effect_label("linear"), "**Beta**")
  expect_equal(.get_effect_label("log-binomial"), "**RR**")  # default
})

test_that(".get_effect_label_adjusted returns correct labels", {
  expect_equal(.get_effect_label_adjusted("logit"), "**Adjusted OR**")
  expect_equal(.get_effect_label_adjusted("poisson"), "**Adjusted IRR**")
  expect_equal(.get_effect_label_adjusted("linear"), "**Adjusted Beta**")
  expect_equal(.get_effect_label_adjusted("log-binomial"), "**Adjusted RR**")  # default
})

test_that(".get_abbreviation returns expected descriptions", {
  expect_equal(.get_abbreviation("logit"), "OR = Odds Ratio")
  expect_equal(.get_abbreviation("log-binomial"), "RR = Relative Risk")
  expect_equal(.get_abbreviation("poisson"), "IRR = Incidence Rate Ratio")
  expect_equal(.get_abbreviation("linear"), "Beta = Linear Regression Coefficient, CI = Confidence Interval")
  expect_equal(.get_abbreviation("unknown"), "RR = Relative Risk")
})

test_that(".get_remove_abbreviation returns expected strings", {
  expect_equal(.get_remove_abbreviation("logit"), "OR = Odds Ratio")
  expect_equal(.get_remove_abbreviation("log-binomial"), "RR = Relative Risk")
  expect_equal(.get_remove_abbreviation("poisson"), "IRR = Incidence Rate Ratio")
  expect_equal(.get_remove_abbreviation("linear"), "CI = Confidence Interval")
  expect_equal(.get_remove_abbreviation("unknown"), "")
})
