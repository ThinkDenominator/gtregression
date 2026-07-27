lung_ph_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("check_ph works with direct coxph models", {
  skip_if_not_installed("survival")

  df <- lung_ph_data()
  fit <- survival::coxph(survival::Surv(time, status) ~ trt + age + prior, data = df)

  out <- check_ph(fit, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Model", "Term", "Test", "Chi.square", "df", "p.value",
                    "Interpretation") %in% names(out)))
  expect_equal(unique(out$Model), "cox_model")
  expect_true("GLOBAL" %in% out$Term)
  expect_true(all(is.finite(out$p.value)))
  expect_true(all(out$Interpretation %in%
                    c("Possible PH violation", "No evidence of PH violation")))
})

test_that("check_ph works with cox_reg objects", {
  skip_if_not_installed("survival")

  df <- lung_ph_data()

  cox_fit <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior),
    adjust_for = c(age, karno)
  )

  out <- check_ph(cox_fit, transform = rank, format = tibble)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("trt", "celltype", "prior") %in% out$Model))
  expect_equal(sum(out$Term == "GLOBAL"), 3)
  expect_true(all(out$Test %in% c("Term", "Global")))
})

test_that("check_ph returns formatted tables", {
  skip_if_not_installed("survival")
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")

  df <- lung_ph_data()
  fit <- survival::coxph(survival::Surv(time, status) ~ trt + age, data = df)

  expect_s3_class(check_ph(fit), "flextable")
  expect_s3_class(check_ph(fit, format = gt), "gt_tbl")
})

test_that("check_ph validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_ph_data()
  fit <- survival::coxph(survival::Surv(time, status) ~ trt + age, data = df)

  expect_error(
    check_ph(stats::lm(mpg ~ hp, data = mtcars)),
    "`model` must be"
  )
  expect_error(
    check_ph(fit, alpha = 2),
    "`alpha` must be"
  )
  expect_error(
    check_ph(fit, transform = invalid_transform),
    "km|rank|identity"
  )
})
