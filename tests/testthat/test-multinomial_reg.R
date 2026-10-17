multinomial_test_data <- function() {
  data <- iris
  data$Species <- factor(
    data$Species,
    levels = c("setosa", "versicolor", "virginica")
  )
  data$Wide.Sepal <- factor(
    ifelse(data$Sepal.Width >= stats::median(data$Sepal.Width), "Yes", "No"),
    levels = c("No", "Yes")
  )
  data
}

test_that("multinomial_reg fits crude models and preserves outcome comparisons", {
  df <- multinomial_test_data()
  out <- multinomial_reg(
    df,
    outcome = Species,
    exposures = c(Sepal.Length, Wide.Sepal),
    model_stats = TRUE
  )
  direct <- nnet::multinom(
    Species ~ Sepal.Length, data = df, trace = FALSE, Hess = TRUE
  )
  estimate <- out$per_outcome$versicolor$table_body$estimate[
    out$per_outcome$versicolor$table_body$exposure == "Sepal.Length"
  ]

  expect_s3_class(out, "multinomial_reg")
  expect_s3_class(out$table, "flextable")
  expect_identical(names(out$per_outcome), c("versicolor", "virginica"))
  expect_equal(
    estimate,
    unname(exp(stats::coef(direct)["versicolor", "Sepal.Length"])),
    tolerance = 1e-7
  )
  expect_equal(nrow(out$model_stats), 2L)
  expect_identical(out$outcome_reference, "setosa")
  expect_true(all(c("outcome_level", "outcome_reference") %in%
                    names(out$table_body)))
})

test_that("multinomial_reg supports adjusted and joint models", {
  df <- multinomial_test_data()
  attr(df$Sepal.Width, "label") <- "Sepal width"

  adjusted <- multinomial_reg(
    df, Species, Sepal.Length,
    adjust_for = Sepal.Width
  )
  joint <- multinomial_reg(
    df, Species, c(Sepal.Length, Sepal.Width, Wide.Sepal),
    multivariable = TRUE,
    format = gt
  )

  expect_s3_class(joint$table, "gt_tbl")
  expect_identical(
    unique(adjusted$per_outcome$versicolor$table_body$exposure),
    "Sepal.Length"
  )
  expect_match(paste(adjusted$footnotes, collapse = " "), "Sepal width")
  expect_setequal(
    unique(joint$per_outcome$versicolor$table_body$exposure),
    c("Sepal.Length", "Sepal.Width", "Wide.Sepal")
  )
})

test_that("multinomial outcome reference can be changed reproducibly", {
  df <- multinomial_test_data()
  df$Species <- stats::relevel(df$Species, ref = "virginica")
  out <- multinomial_reg(df, Species, Sepal.Length)

  expect_identical(out$outcome_reference, "virginica")
  expect_identical(names(out$per_outcome), c("setosa", "versicolor"))
  expect_true(all(vapply(
    out$per_outcome,
    function(x) grepl("vs virginica", paste(x$footnotes, collapse = " ")),
    logical(1)
  )))
})

test_that("multinomial_reg validates nominal outcomes and model roles", {
  df <- multinomial_test_data()
  ordered <- transform(df, Species = ordered(Species))
  binary <- subset(df, Species != "virginica")
  binary$Species <- droplevels(binary$Species)

  expect_error(
    multinomial_reg(ordered, Species, Sepal.Length),
    "unordered factor"
  )
  expect_error(
    multinomial_reg(binary, Species, Sepal.Length),
    "at least three"
  )
  expect_error(
    multinomial_reg(
      df, Species, Sepal.Length, adjust_for = Sepal.Length
    ),
    "must not overlap"
  )
})

test_that("multinomial comparison objects work with plots and forest tables", {
  df <- multinomial_test_data()
  out <- multinomial_reg(
    df, Species, c(Sepal.Length, Wide.Sepal),
    multivariable = TRUE
  )
  comparison <- out$per_outcome$versicolor

  expect_s3_class(plot_reg(comparison), "ggplot")
  expect_s3_class(forest_reg(forest_df(comparison)), "gtregression_forest")
  expect_s3_class(modify_table(out, caption = "Multinomial model"),
                  "gtregression")
  expect_error(plot_reg(out), "multiple outcome comparisons")
  expect_error(forest_df(out), "multiple outcome comparisons")
})
