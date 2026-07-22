birthwt_convergence_data <- function() {
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

test_that("check_convergence runs univariable and multivariable logit models", {
  df <- birthwt_convergence_data()
  exposures <- c("age", "lwt", "race", "smoke")

  result_uni <- check_convergence(
    data = df,
    exposures = exposures,
    outcome = "low",
    approach = logit,
    format = tibble
  )
  result_multi <- check_convergence(
    data = df,
    exposures = exposures,
    outcome = "low",
    approach = "logit",
    multivariate = TRUE,
    format = tibble
  )

  expect_s3_class(result_uni, "data.frame")
  expect_named(result_uni, c("Exposure", "Model", "Converged", "Max.prob."))
  expect_equal(result_uni$Exposure, exposures)
  expect_true(all(result_uni$Model == "logit"))
  expect_true(all(result_uni$Converged))
  expect_true(all(is.finite(result_uni$Max.prob.)))

  expect_s3_class(result_multi, "data.frame")
  expect_equal(nrow(result_multi), 1L)
  expect_equal(result_multi$Exposure, paste(exposures, collapse = " + "))
  expect_true(result_multi$Converged)
})

test_that("check_convergence supports logbinomial and robpoisson approaches", {
  df <- birthwt_convergence_data()

  logbin <- check_convergence(
    data = df,
    exposures = c("age", "smoke"),
    outcome = "low",
    approach = logbinomial,
    format = tibble
  )
  rob <- suppressWarnings(check_convergence(
    data = df,
    exposures = c("age", "smoke"),
    outcome = "low",
    approach = robpoisson,
    format = tibble
  ))

  expect_s3_class(logbin, "data.frame")
  expect_true(all(logbin$Model == "logbinomial"))
  expect_true(all(logbin$Exposure == c("age", "smoke")))
  expect_s3_class(rob, "data.frame")
  expect_true(all(rob$Model == "robpoisson"))
  expect_true(all(is.na(rob$Converged) | rob$Converged %in% c(TRUE, FALSE)))
})

test_that("check_convergence supports poisson and negative binomial count models", {
  data("quine", package = "MASS")
  quine_data <- quine |>
    dplyr::mutate(Days = as.numeric(Days))
  exposures <- c("Eth", "Sex", "Age", "Lrn")

  pois <- check_convergence(
    data = quine_data,
    exposures = exposures,
    outcome = "Days",
    approach = poisson,
    format = tibble
  )
  nb <- check_convergence(
    data = quine_data,
    exposures = exposures,
    outcome = "Days",
    approach = negbin,
    multivariate = TRUE,
    format = tibble
  )

  expect_s3_class(pois, "data.frame")
  expect_equal(nrow(pois), length(exposures))
  expect_true(all(pois$Converged))
  expect_true(all(is.finite(pois$Max.prob.)))

  expect_s3_class(nb, "data.frame")
  expect_equal(nrow(nb), 1L)
  expect_true(nb$Converged)
  expect_true(is.finite(nb$Max.prob.))
})

test_that("check_convergence validates inputs clearly", {
  df <- birthwt_convergence_data()

  expect_error(
    check_convergence(list(y = 1), exposures = "x", outcome = "y"),
    "`data` must be a data frame"
  )
  expect_error(
    check_convergence(df, exposures = character(0), outcome = "low"),
    "`exposures` must be"
  )
  expect_error(
    check_convergence(df, exposures = "age", outcome = c("low", "bwt")),
    "`outcome` must be"
  )
  expect_error(
    check_convergence(df, exposures = "missing", outcome = "low"),
    "Variables not found: missing"
  )
  expect_error(
    check_convergence(df, exposures = "age", outcome = "missing"),
    "Variables not found: missing"
  )
  expect_error(
    check_convergence(df, exposures = "age", outcome = "low", multivariate = NA),
    "`multivariate` must be"
  )
  expect_error(
    check_convergence(df, exposures = "age", outcome = "low", approach = linear),
    "linear  is not a valid approach"
  )
  expect_error(
    check_convergence(df, exposures = "age", outcome = "bwt", approach = logit),
    "This approach requires either a factor variable"
  )
})

test_that("check_convergence handles empty data and fitting failures", {
  df <- birthwt_convergence_data()

  empty <- check_convergence(
    data = df[0, ],
    exposures = c("age", "smoke"),
    outcome = "low",
    approach = logit,
    format = tibble
  )
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("Exposure", "Model", "Converged", "Max.prob."))

  broken_data <- data.frame(
    diabetes = c(0, 1, 0, 1),
    all_one = factor("yes", levels = "yes")
  )

  result <- check_convergence(
    broken_data,
    exposures = "all_one",
    outcome = "diabetes",
    approach = "logit",
    format = tibble
  )

  expect_false(result$Converged)
  expect_true(is.na(result$Max.prob.))

  result_multi <- suppressWarnings(check_convergence(
    broken_data,
    exposures = "all_one",
    outcome = "diabetes",
    approach = "logbinomial",
    multivariate = TRUE,
    format = tibble
  ))

  expect_false(result_multi$Converged)
  expect_equal(result_multi$Exposure, "all_one")
  expect_true(is.na(result_multi$Max.prob.))

  expect_s3_class(
    suppressWarnings(check_convergence(
      broken_data,
      exposures = "all_one",
      outcome = "diabetes",
      approach = "logbinomial",
      multivariate = TRUE
    )),
    "flextable"
  )
})

test_that("check_convergence warns for robpoisson fitted values above one", {
  df <- data.frame(
    outcome = c(0, 1, 1, 1, 1, 1),
    x = c(0, 1, 2, 3, 4, 5)
  )

  expect_warning(
    suppressMessages(check_convergence(
      data = df,
      exposures = "x",
      outcome = "outcome",
      approach = robpoisson,
      format = tibble
    )),
    "predicted fitted value exceeds 1"
  )
})

test_that("check_convergence can return gt and flextable outputs", {
  df <- birthwt_convergence_data()

  gt_out <- check_convergence(
    data = df,
    exposures = c("age", "smoke"),
    outcome = "low",
    approach = logit,
    format = gt
  )
  ft_out <- check_convergence(
    data = df,
    exposures = c("age", "smoke"),
    outcome = "low",
    approach = logit,
    format = flextable
  )

  expect_s3_class(gt_out, "gt_tbl")
  expect_s3_class(ft_out, "flextable")
  expect_equal(names(gt_out$`_data`), c("Exposure", "Model", "Converged", "Max.prob."))
  expect_match(
    as.character(gt_out$`_source_notes`[[1]]),
    "Screening aid only"
  )
  expect_error(
    check_convergence(df, "age", "low", format = "bad"),
    "should be one of"
  )
})
