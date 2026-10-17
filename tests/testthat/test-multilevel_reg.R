multilevel_test_data <- function() {
  set.seed(819)
  clusters <- 24
  cluster_size <- 12
  n <- clusters * cluster_size
  data <- data.frame(
    clinic = factor(rep(seq_len(clusters), each = cluster_size)),
    treatment = factor(rep(c("Control", "Intervention"), length.out = n)),
    age = rnorm(n, 60, 9),
    sex = factor(sample(c("Female", "Male"), n, replace = TRUE))
  )
  random_intercept <- rnorm(clusters, 0, 2)
  linear_predictor <- -0.8 + 0.7 * (data$treatment == "Intervention") +
    0.015 * data$age + random_intercept[data$clinic] / 2
  data$score <- 42 + 2.2 * (data$treatment == "Intervention") +
    0.18 * data$age + random_intercept[data$clinic] + rnorm(n, 0, 1.5)
  data$response <- rbinom(n, 1, stats::plogis(linear_predictor))
  data
}

test_that("multilevel_reg fits a publication-ready linear random-intercept model", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()
  attr(df$clinic, "label") <- "Study clinic"

  out <- multilevel_reg(
    df,
    outcome = score,
    exposures = c(treatment, age),
    group = clinic,
    model_stats = TRUE
  )
  direct <- lme4::lmer(
    score ~ treatment + age + (1 | clinic), data = df, REML = FALSE
  )

  expect_s3_class(out, "multilevel_reg")
  expect_s3_class(out$table, "flextable")
  expect_equal(
    out$table_body$estimate[out$table_body$exposure == "age"],
    unname(lme4::fixef(direct)["age"]),
    tolerance = 1e-8
  )
  expect_equal(out$model_stats$n, nrow(df))
  expect_equal(out$model_stats$clusters, nlevels(df$clinic))
  expect_true(out$model_stats$ICC > 0 && out$model_stats$ICC < 1)
  expect_identical(out$model_stats$ICC_scale, "observed")
  expect_match(paste(out$footnotes, collapse = " "), "Study clinic")
})

test_that("multilevel_reg fits a binary random-intercept model", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()

  out <- multilevel_reg(
    df,
    outcome = response,
    exposures = c(treatment, age),
    group = clinic,
    approach = logit,
    model_stats = TRUE,
    format = gt
  )
  direct <- lme4::glmer(
    response ~ treatment + age + (1 | clinic),
    data = df,
    family = stats::binomial()
  )
  treatment_or <- out$table_body$estimate[
    out$table_body$exposure == "treatment" & !out$table_body$ref
  ]

  expect_equal(
    treatment_or,
    unname(exp(lme4::fixef(direct)["treatmentIntervention"])),
    tolerance = 1e-8
  )
  expect_s3_class(out$table, "gt_tbl")
  expect_identical(out$model_stats$ICC_scale, "latent logistic")
  expect_true(out$model_stats$residual_variance > 3.28)
})

test_that("multilevel_reg supports adjustment variables without reporting them", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()
  attr(df$sex, "label") <- "Participant sex"

  out <- multilevel_reg(
    df, score, treatment, clinic,
    adjust_for = c(age, sex)
  )

  expect_identical(unique(out$table_body$exposure), "treatment")
  expect_false(any(out$table_body$exposure %in% c("age", "sex")))
  expect_match(paste(out$footnotes, collapse = " "), "Participant sex")
})

test_that("multilevel_reg distinguishes exposure names that share prefixes", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()
  df$age_group <- cut(df$age, c(-Inf, 55, 65, Inf),
                      labels = c("Under 55", "55 to 64", "65 or older"))

  out <- multilevel_reg(
    df, score, c(age, age_group), clinic
  )

  expect_equal(sum(out$table_body$exposure == "age"), 1L)
  expect_equal(sum(out$table_body$exposure == "age_group"), 3L)
  expect_identical(
    out$table_body$level[out$table_body$exposure == "age_group"],
    levels(df$age_group)
  )
})

test_that("multilevel_reg works with table and plot workflows", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()
  linear <- multilevel_reg(df, score, c(treatment, age), clinic)
  logistic <- multilevel_reg(df, response, c(treatment, age), clinic,
                             approach = logit)

  expect_s3_class(plot_reg(linear), "ggplot")
  expect_s3_class(plot_reg(logistic), "ggplot")
  expect_s3_class(forest_reg(forest_df(linear)), "gtregression_forest")
  expect_s3_class(merge_tables(linear, linear), "gtregression")
  expect_s3_class(modify_table(linear, caption = "Multilevel model"),
                  "gtregression")
})

test_that("multilevel_reg validates model roles and outcomes", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()

  expect_error(
    multilevel_reg(df, score, treatment, treatment),
    "must not overlap"
  )
  expect_error(
    multilevel_reg(df, treatment, age, clinic, approach = linear),
    "numeric outcome"
  )
  expect_error(
    multilevel_reg(df, score, age, clinic, approach = logit),
    "exactly two levels|coded as"
  )
  one_cluster <- transform(df, clinic = factor("one"))
  expect_error(
    multilevel_reg(one_cluster, score, age, clinic),
    "at least two"
  )
})

test_that("multilevel_reg reports singular random-intercept fits", {
  skip_if_not_installed("lme4")
  df <- multilevel_test_data()
  set.seed(923)
  df$independent_score <- rnorm(nrow(df))

  expect_warning(
    multilevel_reg(df, independent_score, age, clinic, model_stats = TRUE),
    "singular fit"
  )
  out <- suppressWarnings(
    multilevel_reg(df, independent_score, age, clinic, model_stats = TRUE)
  )
  expect_true(out$singular)
  expect_true(out$model_stats$singular)
})
