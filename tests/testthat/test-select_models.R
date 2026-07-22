select_birthwt_data <- function() {
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

test_that("select_models works for logit approaches and all directions", {
  df <- select_birthwt_data()
  exposures <- c("age", "lwt", "race", "smoke", "ht")

  for (direction in c("forward", "backward", "both")) {
    result <- select_models(
      data = df,
      outcome = "low",
      exposures = exposures,
      approach = logit,
      direction = direction,
      format = tibble
    )

    expect_type(result, "list")
    expect_named(result, c("results_table", "best_model", "all_models", "direction"))
    expect_equal(result$direction, direction)
    expect_s3_class(result$results_table, "tbl_df")
    expect_s3_class(result$best_model, "glm")
    expect_true(all(c("model_id", "formula", "n_predictors", "AIC", "BIC",
                      "logLik", "deviance", "selected_vars") %in%
                      names(result$results_table)))
    expect_equal(length(result$all_models), nrow(result$results_table))
    expect_equal(
      attr(result$best_model, "formula_str"),
      result$results_table$formula[which.min(result$results_table$AIC)]
    )
  }
})

test_that("select_models supports logbinomial and robpoisson binary models", {
  df <- select_birthwt_data()

  logbin <- select_models(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logbinomial,
    direction = forward,
    format = tibble
  )
  rob <- select_models(
    data = df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = robpoisson,
    direction = forward,
    format = tibble
  )

  expect_s3_class(logbin$best_model, "glm")
  expect_s3_class(rob$best_model, "glm")
  expect_true(all(is.finite(logbin$results_table$AIC)))
  expect_true(all(is.finite(rob$results_table$AIC)))
})

test_that("select_models works for linear regression and returns adjusted R-squared", {
  result <- select_models(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt", "cyl"),
    approach = linear,
    direction = both,
    format = tibble
  )

  expect_s3_class(result$best_model, "lm")
  expect_true("adj_r2" %in% names(result$results_table))
  expect_false("selected_vars" %in% names(result$results_table))
  expect_true(all(is.finite(result$results_table$adj_r2)))
})

test_that("select_models supports negative binomial regression", {
  data("quine", package = "MASS")
  quine_data <- quine |>
    dplyr::mutate(dplyr::across(c(Eth, Sex, Age, Lrn), as.factor))

  pois <- select_models(
    data = quine_data,
    outcome = "Days",
    exposures = c("Eth", "Sex", "Age", "Lrn"),
    approach = poisson,
    direction = forward,
    format = tibble
  )
  result <- select_models(
    data = quine_data,
    outcome = "Days",
    exposures = c("Eth", "Sex", "Age", "Lrn"),
    approach = negbin,
    direction = backward,
    format = tibble
  )

  expect_s3_class(pois$best_model, "glm")
  expect_true(all(is.finite(pois$results_table$AIC)))
  expect_true(inherits(result$best_model, "negbin"))
  expect_s3_class(result$results_table, "tbl_df")
  expect_true(all(is.finite(result$results_table$AIC)))
})

test_that("select_models validates inputs clearly", {
  df <- select_birthwt_data()

  expect_error(
    select_models(list(y = 1), outcome = "y", exposures = "x"),
    "`data` must be a data frame"
  )
  expect_error(
    select_models(df, outcome = c("low", "bwt"), exposures = "age"),
    "`outcome` must be"
  )
  expect_error(
    select_models(df, outcome = "low", exposures = character(0)),
    "`exposures` must be"
  )
  expect_error(
    select_models(df, outcome = "low", exposures = "missing"),
    "Variables not found: missing"
  )
  expect_error(
    select_models(df, outcome = "missing", exposures = "age"),
    "Variables not found: missing"
  )
  expect_error(
    select_models(df, outcome = "low", exposures = "age", approach = "notreal"),
    "not a valid approach"
  )
  expect_error(
    select_models(df, outcome = "low", exposures = "age", direction = "sideways"),
    "must be one of"
  )
  expect_error(
    select_models(df, outcome = "bwt", exposures = "age", approach = logit),
    "This approach requires either a factor variable"
  )
})

test_that("select_models keeps best model selected variables consistent", {
  df <- mtcars
  result <- select_models(
    data = df,
    outcome = "mpg",
    exposures = c("hp", "wt", "qsec"),
    approach = linear,
    direction = forward,
    format = tibble
  )
  best_row <- which.min(result$results_table$AIC)

  expect_equal(attr(result$best_model, "formula_str"),
               result$results_table$formula[best_row])
  expect_equal(length(attr(result$best_model, "selected_vars")),
               result$results_table$n_predictors[best_row])
  expect_true(all(names(result$all_models) == paste0("model_", seq_along(result$all_models))))
})

test_that("select_models can add gt and flextable viewing tables", {
  gt_result <- select_models(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear,
    direction = forward,
    format = gt
  )
  ft_result <- select_models(
    data = mtcars,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear,
    direction = forward,
    format = flextable
  )

  expect_s3_class(gt_result$results_table, "tbl_df")
  expect_s3_class(gt_result$table, "gt_tbl")
  expect_s3_class(ft_result$table, "flextable")
  expect_equal(
    gt_result$table$`_formats`[[1]]$func$default(c(FALSE, TRUE, FALSE)),
    c("No", "Yes", "No")
  )
  expect_equal(
    gt_result$results_table$model_id[which.min(gt_result$results_table$AIC)],
    max(gt_result$results_table$model_id)
  )
  expect_match(
    as.character(gt_result$table$`_source_notes`[[1]]),
    "Selection direction: forward"
  )
  expect_match(
    as.character(gt_result$table$`_source_notes`[[2]]),
    "Screening aid only"
  )
})
