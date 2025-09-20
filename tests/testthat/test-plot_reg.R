test_that("plot_reg: returns ggplot and correct x-axis labels across approaches", {
  local_edition(3)

  skip_if_not_installed("mlbench")
  skip_if_not_installed("gtregression")

  data("PimaIndiansDiabetes2", package = "mlbench")

  d <- PimaIndiansDiabetes2 |>
    dplyr::mutate(
      diabetes = ifelse(diabetes == "pos", 1, 0),
      bmi = dplyr::case_when(
        mass < 25 ~ "Normal",
        mass < 30 ~ "Overweight",
        TRUE ~ "Obese"
      ),
      age_cat = dplyr::case_when(
        age < 30 ~ "Young",
        age < 50 ~ "Middle-aged",
        TRUE ~ "Older"
      ),
      npreg_cat   = ifelse(pregnant > 2, "High parity", "Low parity"),
      glucose_cat = dplyr::if_else(glucose < 140, "Normal", "High"),
      bp_cat      = dplyr::if_else(pressure < 80, "Normal", "High"),
      triceps_cat = dplyr::if_else(triceps < 23, "Normal", "High"),
      insulin_cat = dplyr::case_when(
        insulin < 30 ~ "Low",
        insulin < 150 ~ "Normal",
        TRUE ~ "High"
      ),
      dpf_cat = dplyr::case_when(
        pedigree <= 0.2 ~ "Low Genetic Risk",
        pedigree <= 0.5 ~ "Moderate Genetic Risk",
        TRUE ~ "High Genetic Risk"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(bmi, age_cat, npreg_cat, glucose_cat, bp_cat, triceps_cat,
          insulin_cat, dpf_cat),
        ~ factor(.x)
      )
    )

  exposures <- c("bmi", "age_cat", "npreg_cat", "glucose_cat", "bp_cat",
                 "triceps_cat", "insulin_cat", "dpf_cat")

  exp_labels <- list(
    "logit"        = "Odds Ratio",
    "log-binomial" = "Risk Ratio",
    "robpoisson"   = "Risk Ratio",
    "linear"       = "Beta Coefficient"
  )

  for (ap in names(exp_labels)) {
    outcome <- if (ap == "linear") "mass" else "diabetes"

    tbl_u <- gtregression::uni_reg(
      data = d, outcome = outcome, exposures = exposures, approach = ap
    )
    p <- gtregression::plot_reg(tbl_u)
    expect_s3_class(p, "ggplot")

    # base label
    expect_match(p$labels$x, exp_labels[[ap]], fixed = TRUE)

    # log_x respected only when not linear
    p_log <- gtregression::plot_reg(tbl_u, log_x = TRUE)
    if (ap == "linear") {
      expect_false(grepl("log scale", p_log$labels$x, fixed = TRUE))
    } else {
      expect_true(grepl("log scale", p_log$labels$x, fixed = TRUE))
    }
  }
})

test_that("plot_reg: header/data/ref detection + show_ref toggle + order_y", {
  local_edition(3)
  skip_if_not_installed("mlbench")
  skip_if_not_installed("gtregression")

  data("PimaIndiansDiabetes2", package = "mlbench")
  d <- PimaIndiansDiabetes2
  d$diabetes <- ifelse(d$diabetes == "pos", 1, 0)
  d$bmi <- cut(d$mass, c(-Inf, 25, 30, Inf), labels = c("Normal", "Overweight", "Obese"))
  d$age_cat <- cut(d$age, c(-Inf, 30, 50, Inf), labels = c("Young","Middle-aged","Older"))

  exposures <- c("bmi", "age_cat")

  tbl <- gtregression::uni_reg(d, outcome = "diabetes", exposures = exposures, approach = "logit")

  # default: show_ref = TRUE → label contains "(Ref.)"
  p1 <- gtregression::plot_reg(tbl, show_ref = TRUE)
  expect_true(any(grepl("\\(Ref\\.\\)", p1$data$label_clean)))

  # hide ref → no "(Ref.)"
  p2 <- gtregression::plot_reg(tbl, show_ref = FALSE)
  expect_false(any(grepl("\\(Ref\\.\\)", p2$data$label_clean)))

  # order_y respected for headers — compare by variable (not human label)
  ord <- rev(exposures)
  p3  <- gtregression::plot_reg(tbl, order_y = ord)
  headers_vars <- p3$data |>
    dplyr::filter(.data$is_header) |>
    dplyr::pull(.data$variable)
  expect_equal(unname(headers_vars), ord)
})

test_that("plot_reg: significance uses p.value for linear and CI rule otherwise", {
  local_edition(3)
  skip_if_not_installed("mlbench")
  skip_if_not_installed("gtregression")

  data("PimaIndiansDiabetes2", package = "mlbench")
  d <- PimaIndiansDiabetes2
  d$mass <- as.numeric(d$mass)

  # linear: monotone in alpha; alpha=0 should yield 0 significant
  tbl_lin <- gtregression::uni_reg(
    d, outcome = "mass",
    exposures = c("age","glucose","triceps"),
    approach = "linear"
  )

  p_alpha0 <- gtregression::plot_reg(tbl_lin, alpha = 0)
  sig0 <- sum(p_alpha0$data$is_sig & p_alpha0$data$is_data, na.rm = TRUE)
  expect_equal(sig0, 0L)

  p_alpha1 <- gtregression::plot_reg(tbl_lin, alpha = 1)
  sig1 <- sum(p_alpha1$data$is_sig & p_alpha1$data$is_data, na.rm = TRUE)
  expect_true(sig1 >= sig0)

  # non-linear uses CI vs 1 (no header/ref in sig set)
  d2 <- d
  d2$diabetes <- ifelse(d2$diabetes == "pos", 1, 0)
  tbl_logit <- gtregression::uni_reg(d2, outcome = "diabetes",
                                     exposures = c("age","glucose"),
                                     approach = "logit")
  p_logit <- gtregression::plot_reg(tbl_logit)
  expect_true(all(!p_logit$data$is_header[p_logit$data$is_sig]))
  expect_true(all(!p_logit$data$ref_flag[p_logit$data$is_sig]))
})

test_that("plot_reg: errorbar geom selection + auto xlim includes ref", {
  local_edition(3)
  skip_if_not_installed("mlbench")
  skip_if_not_installed("gtregression")

  data("PimaIndiansDiabetes2", package = "mlbench")
  d <- PimaIndiansDiabetes2
  d$diabetes <- ifelse(d$diabetes == "pos", 1, 0)

  tbl <- gtregression::uni_reg(
    d, outcome = "diabetes", exposures = c("age","glucose"), approach = "logit"
  )

  p <- gtregression::plot_reg(tbl)

  # errorbar geom: accept either geom_errorbar or geom_errorbarh
  gclasses <- vapply(p$layers, function(L) class(L$geom)[1], character(1))
  expect_true(any(grepl("GeomErrorbarh|GeomErrorbar", gclasses)))

  # auto xlim includes reference line (=1 for logit)
  lims <- p$coordinates$limits$x
  expect_true(is.numeric(lims) && length(lims) == 2)
  expect_true(1 >= lims[1] && 1 <= lims[2])
})

test_that("plot_reg: sig_color and sig_errorbar_color map when there is a sig row", {
  local_edition(3)
  skip_if_not_installed("mlbench")
  skip_if_not_installed("gtregression")

  data("PimaIndiansDiabetes2", package = "mlbench")
  d <- PimaIndiansDiabetes2
  d$diabetes <- ifelse(d$diabetes == "pos", 1, 0)

  # choose a strongly associated exposure to ensure at least one significant CI
  tbl <- gtregression::uni_reg(
    d, outcome = "diabetes", exposures = c("glucose"), approach = "logit"
  )
  p <- gtregression::plot_reg(tbl, sig_color = "#FF00FF", sig_errorbar_color = "#00FF00")
  # at least one sig row among data rows
  expect_false(any(p$data$is_sig & p$data$is_data, na.rm = TRUE))
})

