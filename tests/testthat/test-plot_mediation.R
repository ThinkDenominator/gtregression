test_that("plot_mediation returns a ggplot object", {
  med <- mediation_analysis(
    data = data_diabetes_mediation,
    exposure = obesity,
    mediator = glucose,
    outcome = diabetes,
    covariates = c(age, blood_pressure),
    outcome_approach = logit,
    sims = 20,
    seed = 123
  )

  p <- plot_mediation(med)
  expect_s3_class(p, "ggplot")
  expect_equal(p$layers[[2]]$data$label, c("Obesity", "Plasma glucose", "Diabetes"))

  p_no_estimates <- plot_mediation(med, show_estimates = FALSE, base_size = 11)
  expect_s3_class(p_no_estimates, "ggplot")
})

test_that("plot_mediation validates inputs", {
  expect_error(plot_mediation(list()), "`mediation_object` must be returned", fixed = TRUE)

  med <- mediation_analysis(
    data = data_diabetes_mediation,
    exposure = obesity,
    mediator = glucose,
    outcome = diabetes,
    outcome_approach = logit,
    sims = 20,
    seed = 123
  )

  expect_error(plot_mediation(med, show_estimates = NA), "`show_estimates`", fixed = TRUE)
  expect_error(plot_mediation(med, base_size = 0), "`base_size`", fixed = TRUE)
})
