test_that(".reg_check_linear works as expected", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- gtregression:::.reg_check_linear(model, exposure = "hp")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true(all(c("Exposure", "Test", "Statistic", "Interpretation") %in% names(result)))
  expect_equal(unique(result$Exposure), "hp")
})
