birthwt_confounder_data <- function() {
  data("data_birthwt", package = "gtregression")

  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
    )
}

test_that("identify_confounder returns a focused summary for multiple pairs", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = c("race", "ht"),
    approach = logit,
    method = change
  )

  expect_s3_class(result, "identify_confounder_result")
  expect_s3_class(result$summary, "tbl_df")
  expect_named(
    result$summary,
    c(
      "exposure", "candidate", "crude_est", "adjusted_est", "mh_est",
      "percent_change", "percent_change_model", "percent_change_mh",
      "is_confounder", "interaction_p", "is_effect_modifier",
      "decision", "recommendation"
    )
  )
  expect_equal(nrow(result$summary), 2)
  expect_true(all(result$summary$decision %in%
                    c("confounder", "effect_modification", "no_evidence",
                      "not_estimable", "invalid")))
  expect_type(result$details, "list")
  expect_true("smoke__race" %in% names(result$details))
})

test_that("identify_confounder keeps detailed fields for single-pair calls", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "race",
    approach = "logit"
  )

  expect_type(result, "list")
  expect_true(all(c("crude", "adjusted", "percent_change", "is_confounder",
                    "summary", "table") %in% names(result)))
  expect_true(is.numeric(result$crude))
  expect_s3_class(result$summary, "tbl_df")
  expect_equal(nrow(result$summary), 1)
})

test_that("identify_confounder supports Mantel-Haenszel method", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "race",
    approach = logit,
    method = mh
  )

  expect_equal(result$mh_status, "calculated")
  expect_equal(result$mh_method, "mh_or")
  expect_true(is.numeric(result$mh_estimate))
  expect_true(is.numeric(result$percent_change_mh))
  expect_equal(result$summary$mh_est, round(result$mh_estimate, 3))
})

test_that("identify_confounder reports MH ineligibility clearly", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "age",
    potential_confounder = "smoke",
    approach = logit,
    method = mh
  )

  expect_true(is.na(result$mh_estimate))
  expect_match(result$mh_status, "MH requires binary outcome")
  expect_true(is.na(result$summary$mh_est))
  expect_equal(result$decision, "not_estimable")
})

test_that("identify_confounder supports both method and risk-ratio MH", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "race",
    approach = logbinomial,
    method = both
  )

  expect_equal(result$mh_method, "mh_rr")
  expect_true(is.numeric(result$mh_estimate))
  expect_true(is.logical(result$is_confounder) || is.na(result$is_confounder))
})

test_that("identify_confounder supports count outcomes with change method", {
  data("quine", package = "MASS")
  quine_data <- quine |>
    dplyr::mutate(dplyr::across(c(Eth, Sex, Age, Lrn), as.factor))

  result <- identify_confounder(
    data = quine_data,
    outcome = "Days",
    exposure = "Sex",
    potential_confounder = c("Eth", "Age"),
    approach = negbin
  )

  expect_s3_class(result$summary, "tbl_df")
  expect_equal(nrow(result$summary), 2)
  expect_true(all(c("crude_est", "adjusted_est", "percent_change") %in%
                    names(result$summary)))
})

test_that("identify_confounder supports additional model approaches", {
  df <- birthwt_confounder_data()

  linear_result <- identify_confounder(
    data = df,
    outcome = "bwt",
    exposure = "age",
    potential_confounder = "smoke",
    approach = linear
  )
  expect_true(is.numeric(linear_result$crude))
  expect_equal(linear_result$mh_status, "MH is available only for binary outcome models.")

  rob_result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "race",
    approach = robpoisson
  )
  expect_true(is.numeric(rob_result$crude))

  data("quine", package = "MASS")
  quine_data <- quine |>
    dplyr::mutate(dplyr::across(c(Eth, Sex, Age, Lrn), as.factor))
  poisson_result <- identify_confounder(
    data = quine_data,
    outcome = "Days",
    exposure = "Sex",
    potential_confounder = "Eth",
    approach = poisson
  )
  expect_true(is.numeric(poisson_result$crude))
})

test_that("identify_confounder reports effect modification when estimate spread is large", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "race",
    approach = logit,
    emm_test = estimate,
    emm_threshold = 0
  )

  expect_equal(result$decision, "effect_modification")
  expect_true(result$is_effect_modifier)
  expect_match(result$recommendation, "Report stratum-specific effects")
})

test_that("identify_confounder handles invalid and not-estimable combinations", {
  df <- birthwt_confounder_data()

  invalid <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "smoke",
    approach = logit
  )
  expect_equal(invalid$decision, "invalid")
  expect_match(invalid$recommendation, "Choose different variables")

  missing_df <- df
  missing_df$smoke <- NA
  expect_error(
    identify_confounder(
      data = missing_df,
      outcome = "low",
      exposure = "smoke",
      potential_confounder = "race",
      approach = logit
    ),
    "No complete cases available"
  )
})

test_that("identify_confounder builds flextable output", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = "race",
    approach = logit,
    format = flextable,
    theme = clinical
  )

  expect_s3_class(result$table, "flextable")
})

test_that("identify_confounder display table includes caveat", {
  df <- birthwt_confounder_data()

  result <- identify_confounder(
    data = df,
    outcome = "low",
    exposure = "smoke",
    potential_confounder = c("race", "ht"),
    approach = logit,
    method = both
  )

  display_cols <- names(result$table$`_data`)

  expect_true("Interaction p" %in% display_cols)
  expect_true("Confounder?" %in% display_cols)
  expect_true("Effect modifier?" %in% display_cols)
  expect_true("is_confounder" %in% names(result$summary))
  expect_true("is_effect_modifier" %in% names(result$summary))
  expect_match(
    as.character(result$table$`_source_notes`[[1]]),
    "Screening aid only"
  )
})

test_that("identify_confounder validates inputs clearly", {
  df <- birthwt_confounder_data()

  expect_error(
    identify_confounder(list(), "low", "smoke", "race"),
    "`data` must be a data.frame"
  )
  expect_error(
    identify_confounder(df, c("low", "bwt"), "smoke", "race"),
    "`outcome` must be a single character string"
  )
  expect_error(
    identify_confounder(df, "missing", "smoke", "race"),
    "Outcome variable not found"
  )
  expect_error(
    identify_confounder(df, "low", character(0), "race"),
    "`exposure` must be a character vector"
  )
  expect_error(
    identify_confounder(df, "low", "missing", "race"),
    "exposure variables were not found"
  )
  expect_error(
    identify_confounder(df, "low", "smoke", "missing"),
    "potential confounder variables were not found"
  )
  expect_error(
    identify_confounder(df, "low", "smoke", "race", approach = "bad"),
    "not a valid approach"
  )
})
