test_that("data_endometrial is available for Firth logistic examples", {
  expect_s3_class(data_endometrial, "data.frame")
  expect_equal(nrow(data_endometrial), 79)
  expect_equal(names(data_endometrial), c("NV", "PI", "EH", "HG"))
  expect_equal(unname(table(data_endometrial$HG, data_endometrial$NV)["0", "1"]), 0)
})
