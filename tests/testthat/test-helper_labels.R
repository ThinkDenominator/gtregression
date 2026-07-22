test_that(".get_effect_label returns correct labels", {
  expect_equal(.get_effect_label("logit"), "OR (95% CI)")
  expect_equal(.get_effect_label("poisson"), "IRR (95% CI)")
  expect_equal(.get_effect_label("linear"), "Beta (95% CI)")
  expect_equal(.get_effect_label("logbinomial"), "RR (95% CI)")
  expect_equal(.get_effect_label("robpoisson"), "RR (95% CI)")
  expect_equal(.get_effect_label("negbin"), "IRR (95% CI)")
})

test_that(".abbrev_note returns expected descriptions", {
  expect_equal(.abbrev_note("logit"), "Abbreviations: OR = Odds Ratio; CI = Confidence Interval.")
  expect_equal(.abbrev_note("logbinomial"), "Abbreviations: RR = Risk Ratio; CI = Confidence Interval.")
  expect_equal(.abbrev_note("poisson"), "Abbreviations: IRR = Incidence Rate Ratio; CI = Confidence Interval.")
  expect_equal(.abbrev_note("linear"), "Abbreviations: Beta = Linear regression coefficient; CI = Confidence Interval.")
  expect_equal(.abbrev_note("unknown"), "Abbreviations: CI = Confidence Interval.")
})
