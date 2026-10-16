matched_case_control_data <- function() {
  set.seed(42)
  sets <- 1:40
  out <- data.frame(
    matched_set = rep(sets, each = 3),
    case = rep(c(1, 0, 0), length(sets)),
    severity = factor(
      sample(c("Low", "Medium", "High"), length(sets) * 3, replace = TRUE),
      levels = c("Low", "Medium", "High")
    ),
    age = rep(seq(35, 74), each = 3) + rep(c(0, -2, 2), length(sets))
  )
  out$exposed <- factor(
    rbinom(nrow(out), 1, ifelse(out$case == 1, 0.65, 0.35)),
    levels = c(0, 1), labels = c("No", "Yes")
  )
  out
}

test_that("conditional_logit_reg returns matched odds-ratio tables", {
  df <- matched_case_control_data()
  result <- conditional_logit_reg(
    df,
    outcome = case,
    exposures = c(exposed, age),
    matched_set = matched_set,
    model_stats = TRUE,
    format = gt
  )

  expect_s3_class(result, "gtregression")
  expect_s3_class(result, "conditional_logit_reg")
  expect_s3_class(result$table, "gt_tbl")
  expect_equal(names(result$models), c("exposed", "age"))
  expect_equal(result$model_stats$matched_sets, c(40, 40))
  expect_equal(result$model_stats$cases, c(40, 40))
  expect_equal(result$model_stats$controls, c(80, 80))
  expect_true("Matched OR (95% CI)" %in% names(result$table_display))
  expect_true(any(grepl("Matched by set identifier", result$footnotes)))
})

test_that("conditional_logit_reg supports adjusted and joint models", {
  df <- matched_case_control_data()
  adjusted <- conditional_logit_reg(
    df,
    outcome = case,
    exposures = exposed,
    matched_set = matched_set,
    adjust_for = age
  )
  joint <- conditional_logit_reg(
    df,
    outcome = case,
    exposures = c(exposed, severity, age),
    matched_set = matched_set,
    multivariable = TRUE
  )

  expect_true("Adjusted matched OR (95% CI)" %in% names(adjusted$table_display))
  expect_true(any(grepl("Adjusted for age", adjusted$footnotes, fixed = TRUE)))
  expect_equal(names(joint$models), "multivariable_model")
  expect_true(any(grepl("other exposures", joint$footnotes, fixed = TRUE)))
})

test_that("conditional_logit_reg keeps multilevel references when compacting binary rows", {
  df <- matched_case_control_data()
  expect_message(
    result <- conditional_logit_reg(
      df,
      outcome = case,
      exposures = c(exposed, severity),
      matched_set = matched_set,
      multivariable = TRUE,
      show_ref = FALSE
    ),
    "Reference rows are retained",
    fixed = TRUE
  )

  expect_equal(sum(result$table_display$Characteristic == "exposed"), 1L)
  expect_true("Low" %in% trimws(result$table_display$Characteristic))
  expect_true("Ref." %in% result$table_display[["Adjusted matched OR (95% CI)"]])
})

test_that("conditional_logit_reg validates matched sets and overlap", {
  df <- matched_case_control_data()
  df$case <- 1L

  expect_error(
    conditional_logit_reg(df, case, exposed, matched_set),
    "No informative matched sets"
  )
  expect_error(
    conditional_logit_reg(matched_case_control_data(), case, exposed, exposed),
    "matched_set.*exposure"
  )
})

test_that("conditional logistic outputs work with downstream helpers", {
  df <- matched_case_control_data()
  crude <- conditional_logit_reg(
    df, case, c(exposed, severity), matched_set, format = gt
  )
  adjusted <- conditional_logit_reg(
    df,
    case,
    c(exposed, severity),
    matched_set,
    multivariable = TRUE,
    format = gt
  )

  expect_s3_class(plot_reg(crude), "ggplot")
  forest_data <- forest_df(crude, adjusted)
  expect_true(all(c("Matched OR (95% CI)", "Adjusted matched OR (95% CI)") %in%
                    names(forest_data)))
  expect_s3_class(forest_reg(forest_data, quiet = TRUE), "gtregression_forest")

  merged <- merge_tables(
    crude,
    adjusted,
    spanners = c("Matched crude", "Matched adjusted"),
    format = gt
  )
  expect_s3_class(merged, "merged_table")

  modified <- modify_table(crude, caption = "Matched analysis")
  expect_equal(modified$caption, "Matched analysis")
})
