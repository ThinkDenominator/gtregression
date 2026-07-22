test_that(".reg_check_linear works as expected", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- gtregression:::.reg_check_linear(model, exposure = "hp")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_named(result, c("Exposure", "Test", "Statistic", "Interpretation"))
  expect_equal(unique(result$Exposure), "hp")
  expect_equal(
    result$Test,
    c("Breusch-Pagan", "Shapiro-Wilk", "RESET", "Cook's Distance")
  )
  expect_true(all(nzchar(result$Statistic)))
  expect_true(all(nzchar(result$Interpretation)))
})

test_that(".reg_check_linear validates inputs", {
  data(mtcars)
  model <- lm(mpg ~ wt, data = mtcars)

  expect_error(
    gtregression:::.reg_check_linear(model = mtcars, exposure = "wt"),
    "`model` must be a fitted linear model"
  )
  expect_error(
    gtregression:::.reg_check_linear(model = model, exposure = c("wt", "hp")),
    "`exposure` must be a single non-empty character string"
  )
  expect_error(
    gtregression:::.reg_check_linear(model = model, exposure = ""),
    "`exposure` must be a single non-empty character string"
  )
  expect_error(
    gtregression:::.reg_check_linear(model = model, exposure = NA_character_),
    "`exposure` must be a single non-empty character string"
  )
})

test_that(".reg_check_linear returns stable output when diagnostics are unavailable", {
  small_data <- data.frame(y = c(1, 2), x = c(0, 1))
  small_model <- lm(y ~ x, data = small_data)

  result <- gtregression:::.reg_check_linear(small_model, exposure = "x")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true("not available" %in% result$Statistic)
  expect_true(any(grepl("could not be calculated", result$Interpretation)))
})

test_that(".reg_check_linear handles influential observations", {
  influential_data <- data.frame(
    y = c(1, 2, 3, 4, 5, 60),
    x = c(1, 2, 3, 4, 5, 6)
  )
  model <- lm(y ~ x, data = influential_data)

  result <- gtregression:::.reg_check_linear(model, exposure = "x")
  cook_row <- result[result$Test == "Cook's Distance", ]

  expect_match(cook_row$Statistic, "obs > 4/n", fixed = TRUE)
  expect_match(cook_row$Interpretation, "Influential points detected")
})
