birthwt_plot_combine_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
      ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes"))
    )
}

test_that("plot_reg_combine returns patchwork for current regression objects", {
  df <- birthwt_plot_combine_data()
  exposures <- c("smoke", "age", "ht", "lwt", "ui")

  tbl_uni <- uni_reg(df, outcome = "low", exposures = exposures, approach = logit)
  tbl_multi <- multi_reg(df, outcome = "low", exposures = exposures, approach = logit)

  p <- plot_reg_combine(tbl_uni, tbl_multi)

  expect_s3_class(p, "patchwork")
  expect_equal(p$patches$annotation$caption, "Ref. = reference category.")
  expect_equal(p[[1]]$data$exposure[p[[1]]$data$is_header], exposures)
  expect_equal(p[[2]]$data$exposure[p[[2]]$data$is_header], exposures)
})

test_that("plot_reg_combine supports multi_reg adjust_for mode", {
  df <- birthwt_plot_combine_data()

  tbl_uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    approach = logit
  )
  tbl_multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  p <- plot_reg_combine(
    tbl_uni,
    tbl_multi,
    order_y = c("smoke", "ht", "ui"),
    log_x = TRUE,
    show_ref = FALSE,
    point_color = "#1b9e77",
    errorbar_color = "#4d4d4d",
    sig_color = "#d95f02",
    sig_errorbar_color = "#7570b3",
    xlim_uni = c(0.2, 20),
    breaks_uni = c(0.5, 1, 2, 4, 8),
    xlim_multi = c(0.2, 20),
    breaks_multi = c(0.5, 1, 2, 4, 8)
  )

  expect_s3_class(p, "patchwork")
  expect_equal(p$patches$annotation$caption, "Adjusted for age and lwt")
  expect_true("smoke" %in% p[[1]]$data$label)
  expect_true("ht" %in% p[[1]]$data$label)
  expect_true(any(p[[1]]$data$label == "smoke" & p[[1]]$data$is_header))
  expect_true(any(grepl("<b>smoke</b>", p[[1]]$data$label_clean, fixed = TRUE)))
  expect_equal(p[[1]]$coordinates$limits$x, c(0.2, 20))
  expect_equal(p[[1]]$scales$get_scales("x")$breaks, c(0.5, 1, 2, 4, 8))
  expect_equal(
    plot_reg_combine(tbl_uni, tbl_multi, show_adjustment_note = FALSE)$patches$annotation$caption,
    "Ref. = reference category."
  )
  expect_equal(
    plot_reg_combine(tbl_uni, tbl_multi, caption = "Custom note")$patches$annotation$caption,
    "Custom note"
  )

  p_default_breaks <- plot_reg_combine(tbl_uni, tbl_multi, log_x = TRUE)
  expect_true(length(p_default_breaks[[1]]$scales$get_scales("x")$breaks) > 1L)
  expect_true(1 %in% p_default_breaks[[1]]$scales$get_scales("x")$breaks)
})

test_that("plot_reg_combine supports survival multivariable counterparts", {
  skip_if_not_installed("survival")

  df <- data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )

  cox_crude <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior)
  )
  cox_multi <- cox_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior, age, karno),
    multivariable = TRUE
  )
  surv_crude <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior)
  )
  surv_multi <- surv_reg(
    data = df,
    time = time,
    event = status,
    exposures = c(trt, celltype, prior, age, karno),
    multivariable = TRUE
  )

  p_cox <- plot_reg_combine(cox_crude, cox_multi)
  p_surv <- plot_reg_combine(surv_crude, surv_multi)

  expect_s3_class(p_cox, "patchwork")
  expect_s3_class(p_surv, "patchwork")
  expect_match(p_cox[[2]]$labels$x, "Adjusted Hazard Ratio", fixed = TRUE)
  expect_match(p_surv[[2]]$labels$x, "Adjusted Time Ratio", fixed = TRUE)
  expect_equal(
    p_cox$patches$annotation$caption,
    "Adjusted for the other variables in the model. Ref. = reference category."
  )
})

test_that("plot_reg_combine supports mismatched exposures and linear models", {
  df <- birthwt_plot_combine_data()

  tbl_uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke", "ht"),
    approach = logit
  )
  tbl_multi_subset <- multi_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit
  )
  p_subset <- plot_reg_combine(tbl_uni, tbl_multi_subset)
  expect_s3_class(p_subset, "patchwork")

  tbl_uni_linear <- uni_reg(
    df,
    outcome = "bwt",
    exposures = c("age", "lwt", "smoke"),
    approach = linear
  )
  tbl_multi_linear <- multi_reg(
    df,
    outcome = "bwt",
    exposures = c("age", "lwt", "smoke"),
    approach = linear
  )
  p_linear <- plot_reg_combine(tbl_uni_linear, tbl_multi_linear, log_x = TRUE, alpha = 0.5)
  expect_s3_class(p_linear, "patchwork")
})

test_that("plot_reg_combine validates object classes and sources", {
  df <- birthwt_plot_combine_data()

  tbl_uni <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit)
  tbl_multi <- multi_reg(df, outcome = "low", exposures = "smoke", approach = logit)

  expect_error(plot_reg_combine(mtcars, tbl_multi), "`tbl_uni` must be")
  expect_error(plot_reg_combine(tbl_uni, mtcars), "`tbl_multi` must be")
  expect_error(plot_reg_combine(tbl_multi, tbl_multi), "must come from uni_reg")
  expect_error(plot_reg_combine(tbl_uni, tbl_uni), "must come from multi_reg")
})
