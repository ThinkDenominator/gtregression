ordinal_test_data <- function() {
  set.seed(1401)
  n <- 500
  treatment <- factor(
    sample(c("Control", "Intervention"), n, replace = TRUE),
    levels = c("Control", "Intervention")
  )
  age <- stats::rnorm(n, 60, 10)
  sex <- factor(sample(c("Female", "Male"), n, replace = TRUE))
  latent <- 0.75 * (treatment == "Intervention") - 0.025 * age +
    0.2 * (sex == "Male") + stats::rlogis(n)
  recovery <- cut(
    latent,
    breaks = c(-Inf, -1.8, -0.8, 0.2, Inf),
    labels = c("Poor", "Fair", "Good", "Excellent"),
    ordered_result = TRUE
  )
  data.frame(recovery, treatment, age, sex)
}

test_that("ordinal_reg fits crude proportional-odds models", {
  df <- ordinal_test_data()
  out <- ordinal_reg(
    df, recovery, c(treatment, age),
    assumption_test = FALSE,
    model_stats = TRUE
  )
  direct <- ordinal::clm(recovery ~ treatment, data = df, Hess = TRUE)
  estimate <- out$table_body$estimate[
    out$table_body$exposure == "treatment" & !out$table_body$ref
  ]

  expect_s3_class(out, "ordinal_reg")
  expect_s3_class(out$table, "flextable")
  expect_equal(
    estimate,
    unname(exp(direct$beta["treatmentIntervention"])),
    tolerance = 1e-8
  )
  expect_identical(out$outcome_levels, levels(df$recovery))
  expect_equal(nrow(out$model_stats), 2L)
  expect_true(all(out$model_stats$convergence == 0L))
  expect_null(out$assumption_tests)
})

test_that("ordinal_reg supports adjusted and multivariable models", {
  df <- ordinal_test_data()
  attr(df$age, "label") <- "Participant age"

  adjusted <- ordinal_reg(
    df, recovery, treatment,
    adjust_for = c(age, sex),
    assumption_test = TRUE
  )
  joint <- ordinal_reg(
    df, recovery, c(treatment, age, sex),
    multivariable = TRUE,
    assumption_test = TRUE,
    format = gt
  )

  expect_identical(unique(adjusted$table_body$exposure), "treatment")
  expect_match(paste(adjusted$footnotes, collapse = " "), "Participant age")
  expect_s3_class(joint$table, "gt_tbl")
  expect_setequal(
    unique(joint$table_body$exposure), c("treatment", "age", "sex")
  )
  expect_true(all(c(
    "model", "term", "statistic", "p.value", "proportional_odds"
  ) %in% names(joint$assumption_tests)))
})

test_that("ordinal_reg uses reference contrasts for ordered predictors", {
  df <- ordinal_test_data()
  df$age_band <- ordered(
    cut(df$age, c(-Inf, 55, 65, Inf),
        labels = c("Under 55", "55 to 64", "65 or older"))
  )
  out <- ordinal_reg(
    df, recovery, age_band,
    assumption_test = FALSE
  )

  expect_identical(out$table_body$level, levels(df$age_band))
  expect_equal(sum(out$table_body$ref), 1L)
})

test_that("ordinal_reg validates outcome ordering and model roles", {
  df <- ordinal_test_data()
  unordered <- transform(df, recovery = factor(recovery, ordered = FALSE))
  binary <- transform(
    df,
    recovery = ordered(ifelse(recovery == "Excellent", "High", "Lower"))
  )

  expect_error(
    ordinal_reg(unordered, recovery, treatment),
    "ordered factor"
  )
  expect_error(
    ordinal_reg(binary, recovery, treatment),
    "at least three"
  )
  expect_error(
    ordinal_reg(df, recovery, treatment, adjust_for = treatment),
    "must not overlap"
  )
})

test_that("ordinal_reg works with downstream table and plot workflows", {
  df <- ordinal_test_data()
  crude <- ordinal_reg(
    df, recovery, c(treatment, age), assumption_test = FALSE
  )
  adjusted <- ordinal_reg(
    df, recovery, c(treatment, age),
    multivariable = TRUE,
    assumption_test = FALSE
  )

  expect_s3_class(plot_reg(crude), "ggplot")
  expect_s3_class(forest_reg(forest_df(crude)), "gtregression_forest")
  expect_s3_class(merge_tables(crude, adjusted), "gtregression")
  expect_s3_class(modify_table(crude, caption = "Ordinal model"),
                  "gtregression")
})
