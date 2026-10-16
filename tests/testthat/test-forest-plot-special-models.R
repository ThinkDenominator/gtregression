# forest_df(), forest_reg(), plot_reg() and plot_reg_combine() with the
# special-design models: the estimate is named as in the source table, the
# numbers are exactly those of the table, and exposure-specific adjusted
# results can be shown next to the single multivariable model.

fsm_ordinal_data <- function() {
  set.seed(11)
  n <- 240
  data.frame(
    recovery = ordered(sample(c("Poor", "Fair", "Good", "Excellent"), n, TRUE),
                       levels = c("Poor", "Fair", "Good", "Excellent")),
    treatment = factor(sample(c("Control", "New"), n, TRUE)),
    sex = factor(sample(c("F", "M"), n, TRUE)),
    age = round(stats::rnorm(n, 60, 9))
  )
}

fsm_matched_data <- function() {
  data_infertility |>
    dplyr::mutate(
      spont = factor(pmin(spontaneous, 1), labels = c("No", "Yes")),
      induced_f = factor(pmin(induced, 1), labels = c("No", "Yes"))
    )
}

fsm_quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# Effect column of a table_display, and the rows that carry an estimate.
fsm_effect_text <- function(x) {
  td <- x$table_display
  col <- grep("\\(95% CI\\)$", names(td), value = TRUE)
  out <- as.character(td[[col]])
  out[!is.na(out) & nzchar(out) & out != "Ref."]
}

fsm_forest_text <- function(fd, header) {
  out <- as.character(fd[[header]])
  out[nzchar(out) & out != "Ref."]
}

fsm_models <- function() {
  ord <- fsm_ordinal_data()
  inf <- fsm_matched_data()
  list(
    ord_crude = fsm_quiet(ordinal_reg(ord, outcome = recovery, exposures = c(treatment, sex, age),
                                      assumption_test = FALSE)),
    ord_adjusted = fsm_quiet(ordinal_reg(ord, outcome = recovery, exposures = c(treatment, sex),
                                         adjust_for = age, assumption_test = FALSE)),
    ord_multi = fsm_quiet(ordinal_reg(ord, outcome = recovery, exposures = c(treatment, sex, age),
                                      multivariable = TRUE, assumption_test = FALSE)),
    cl_crude = fsm_quiet(conditional_logit_reg(inf, outcome = case, exposures = c(spont, induced_f),
                                               matched_set = stratum)),
    cl_adjusted = fsm_quiet(conditional_logit_reg(inf, outcome = case, exposures = spont,
                                                  adjust_for = c(induced_f, age), matched_set = stratum)),
    cl_multi = fsm_quiet(conditional_logit_reg(inf, outcome = case, exposures = c(spont, induced_f),
                                               matched_set = stratum, multivariable = TRUE))
  )
}

test_that("forest_df() names the estimate exactly as the source table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ordinal")
  m <- fsm_models()
  expect_true("Matched OR (95% CI)" %in% names(forest_df(m$cl_crude)))
  expect_true("Adjusted matched OR (95% CI)" %in% names(forest_df(m$cl_multi)))
  expect_true("Common OR (95% CI)" %in% names(forest_df(m$ord_crude)))
  expect_true("Adjusted common OR (95% CI)" %in% names(forest_df(m$ord_multi)))

  pair <- forest_df(m$cl_crude, m$cl_multi)
  expect_true(all(c("Matched OR (95% CI)", "Adjusted matched OR (95% CI)") %in% names(pair)))
  pair <- forest_df(m$ord_crude, m$ord_multi)
  expect_true(all(c("Common OR (95% CI)", "Adjusted common OR (95% CI)") %in% names(pair)))
})

test_that("forest and plot estimates are exactly those of the table", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ordinal")
  m <- fsm_models()
  for (nm in names(m)) {
    x <- m[[nm]]
    tb <- x$table_body
    rows <- !tb$ref & !is.na(tb$estimate)
    fd <- forest_df(x)
    header <- grep("\\(95% CI\\)$", names(fd), value = TRUE)
    est <- attr(fd, "est")
    est <- est[!is.na(est) & !(seq_along(est) %in% which(fd[[header]] == "Ref."))]
    expect_equal(sort(est), sort(tb$estimate[rows]), info = nm)
    expect_identical(sort(fsm_forest_text(fd, header)), sort(fsm_effect_text(x)), info = nm)

    p <- plot_reg(x)
    plotted <- p$data$estimate[!is.na(p$data$estimate) & !(p$data$ref %in% TRUE)]
    expect_equal(sort(plotted), sort(tb$estimate[rows]), info = nm)
  }
  # And the table itself reports the fitted model.
  fit <- m$cl_multi$models[[1]]
  expect_equal(unname(exp(stats::coef(fit))),
               m$cl_multi$table_body$estimate[!m$cl_multi$table_body$ref])
})

test_that("exposure-specific adjusted results can be shown next to the multivariable model", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ordinal")
  m <- fsm_models()
  fd <- forest_df(m$ord_adjusted, m$ord_multi)
  expect_true(all(c("Adjusted common OR (95% CI)", "Mutually adjusted common OR (95% CI)") %in% names(fd)))
  # Either order gives the exposure-specific results first.
  expect_identical(names(forest_df(m$ord_multi, m$ord_adjusted)), names(fd))
  expect_s3_class(fsm_quiet(forest_reg(fd)), "gtregression_forest")

  fd <- forest_df(m$cl_adjusted, m$cl_multi)
  expect_true("Mutually adjusted matched OR (95% CI)" %in% names(fd))
  expect_s3_class(fsm_quiet(forest_reg(fd)), "gtregression_forest")

  p <- fsm_quiet(plot_reg_combine(m$ord_adjusted, m$ord_multi))
  expect_s3_class(p, "patchwork")

  # Two single multivariable models are still refused.
  expect_error(forest_df(m$ord_multi, m$ord_multi), "Two multivariable objects")
})

test_that("forest_df() accepts an adjusted object given only as `multi`", {
  skip_if_not_installed("survival")
  m <- fsm_models()
  expect_identical(names(forest_df(multi = m$cl_multi)), names(forest_df(m$cl_multi)))
  expect_error(forest_df(), "Supply at least one result")
})

test_that("default forest axes cover the data with readable ticks", {
  axis <- gtregression:::.forest_auto_axis
  for (values in list(c(0.61, 1.97, 2.72, 10.06), c(0.97, 1.01), c(1.35, 18.69), c(0.02, 0.5))) {
    a <- axis(values, x_trans = "log", ref_line = 1)
    expect_true(a$xlim[1] <= min(values, 1) && a$xlim[2] >= max(values, 1))
    expect_true(1 %in% a$ticks)
    expect_true(all(a$ticks >= a$xlim[1] & a$ticks <= a$xlim[2]))
    expect_gte(length(a$ticks), 2)
    # Neighbouring labels do not overlap in a column of the default width.
    pos <- log(a$ticks / a$xlim[1]) / log(a$xlim[2] / a$xlim[1])
    labels <- nchar(format(a$ticks, trim = TRUE, drop0trailing = TRUE, scientific = FALSE))
    gaps <- diff(pos) * 20 * 0.7
    expect_true(all(gaps >= (head(labels, -1) + tail(labels, -1)) / 2 + 1))
  }
  a <- axis(c(-788, 21.95, -111), x_trans = "none", ref_line = 0)
  expect_true(0 %in% a$ticks)
  expect_true(a$xlim[1] <= -788 && a$xlim[2] >= 21.95)
})
