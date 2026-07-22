birthwt_plot_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, levels = c(1, 2, 3),
                    labels = c("White", "Black", "Other")),
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
      ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes")),
      ftv_cat = dplyr::case_when(
        ftv == 0 ~ "None",
        ftv == 1 ~ "One",
        ftv >= 2 ~ "Two or more"
      ),
      ftv_cat = factor(ftv_cat, levels = c("None", "One", "Two or more"))
    )
}

test_that("plot_reg returns ggplot and correct x-axis labels", {
  df <- birthwt_plot_data()

  tbl_logit <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("age", "lwt", "smoke", "ht"),
    approach = logit
  )
  tbl_linear <- uni_reg(
    data = df,
    outcome = "bwt",
    exposures = c("age", "lwt", "smoke"),
    approach = linear
  )

  p_logit <- plot_reg(tbl_logit)
  p_linear <- plot_reg(tbl_linear)

  expect_s3_class(p_logit, "ggplot")
  expect_s3_class(p_linear, "ggplot")
  expect_match(p_logit$labels$x, "Odds Ratio", fixed = TRUE)
  expect_match(p_linear$labels$x, "Beta Coefficient", fixed = TRUE)

  expect_true(grepl("log scale", plot_reg(tbl_logit, log_x = TRUE)$labels$x, fixed = TRUE))
  expect_false(grepl("log scale", plot_reg(tbl_linear, log_x = TRUE)$labels$x, fixed = TRUE))
})

test_that("plot_reg supports multi_reg adjusted mode", {
  df <- birthwt_plot_data()

  tbl <- multi_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht", "ui"),
    adjust_for = c("age", "lwt"),
    approach = logit
  )

  p <- plot_reg(tbl)

  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "Adjusted Odds Ratio", fixed = TRUE)
  expect_equal(p$labels$caption, "Adjusted for age and lwt")
  expect_null(plot_reg(tbl, show_adjustment_note = FALSE)$labels$caption)
  expect_equal(plot_reg(tbl, caption = "Custom note")$labels$caption, "Custom note")
  expect_true(all(c("smoke", "ht", "ui") %in% p$data$exposure))
})

test_that("plot_reg handles reference rows, ordering, significance, and x limits", {
  df <- birthwt_plot_data()

  tbl <- uni_reg(
    data = df,
    outcome = "low",
    exposures = c("smoke", "ht", "age"),
    approach = logit
  )

  p_ref <- plot_reg(tbl, show_ref = TRUE)
  p_no_ref <- plot_reg(tbl, show_ref = FALSE)

  expect_true(any(grepl("(ref)", p_ref$data$label_clean, fixed = TRUE)))
  expect_false(any(grepl("(ref)", p_no_ref$data$label_clean, fixed = TRUE)))

  p_ordered <- plot_reg(tbl, order_y = c("age", "smoke", "ht"))
  expect_equal(p_ordered$data$exposure[p_ordered$data$is_header], c("age", "smoke", "ht"))

  p_breaks <- plot_reg(tbl, breaks = c(0.5, 1, 2, 4), xlim = c(0.25, 6))
  expect_s3_class(p_breaks, "ggplot")

  expect_true(all(!p_ref$data$is_header[p_ref$data$is_sig]))
  expect_true(all(!p_ref$data$ref[p_ref$data$is_sig]))

  lims <- p_ref$coordinates$limits$x
  expect_true(is.numeric(lims) && length(lims) == 2)
  expect_true(1 >= lims[1] && 1 <= lims[2])

  gclasses <- vapply(p_ref$layers, function(layer) class(layer$geom)[1], character(1))
  expect_true(any(grepl("GeomErrorbarh|GeomErrorbar", gclasses)))
})

test_that("plot_reg uses p values for linear significance", {
  df <- birthwt_plot_data()

  tbl <- uni_reg(
    data = df,
    outcome = "bwt",
    exposures = c("age", "lwt"),
    approach = linear
  )

  p_alpha0 <- plot_reg(tbl, alpha = 0)
  p_alpha1 <- plot_reg(tbl, alpha = 1)

  sig0 <- sum(p_alpha0$data$is_sig & p_alpha0$data$is_data, na.rm = TRUE)
  sig1 <- sum(p_alpha1$data$is_sig & p_alpha1$data$is_data, na.rm = TRUE)

  expect_equal(sig0, 0L)
  expect_true(sig1 >= sig0)
})

test_that("plot_reg validates unsupported inputs", {
  df <- birthwt_plot_data()

  stratified <- suppressMessages(
    stratified_uni_reg(
      data = df,
      outcome = "low",
      exposures = c("age", "smoke"),
      stratifier = "race",
      approach = logit
    )
  )

  expect_error(plot_reg(mtcars), "gtregression object")
  expect_error(plot_reg(stratified), "does not support stratified")
})

test_that("plot_reg validates malformed gtregression objects", {
  good_body <- data.frame(
    exposure = "age",
    level = "age",
    estimate = 1,
    conf.low = 0.8,
    conf.high = 1.2,
    p.value = 0.5,
    ref = FALSE
  )
  good_display <- data.frame(Characteristic = "age", is_header = TRUE)

  no_source <- structure(
    list(table_body = good_body, table_display = good_display, approach = "logit"),
    class = "gtregression"
  )
  no_body <- structure(
    list(table_display = good_display, source = "uni_reg", approach = "logit"),
    class = "gtregression"
  )
  no_display <- structure(
    list(table_body = good_body, source = "uni_reg", approach = "logit"),
    class = "gtregression"
  )
  bad_body <- structure(
    list(
      table_body = good_body[, setdiff(names(good_body), "ref"), drop = FALSE],
      table_display = good_display,
      source = "uni_reg",
      approach = "logit"
    ),
    class = "gtregression"
  )
  bad_display <- structure(
    list(
      table_body = good_body,
      table_display = data.frame(Characteristic = "age"),
      source = "uni_reg",
      approach = "logit"
    ),
    class = "gtregression"
  )

  expect_error(plot_reg(no_source), "source")
  expect_error(plot_reg(no_body), "table_body")
  expect_error(plot_reg(no_display), "table_display")
  expect_error(plot_reg(bad_body), "requires `table_body`")
  expect_error(plot_reg(bad_display), "requires `table_display`")
})

test_that("plot_reg includes non-factor extra rows when display has only headers", {
  body <- data.frame(
    exposure = c("age", "age"),
    level = c("age", "age:smokeYes"),
    estimate = c(1.1, 1.3),
    conf.low = c(0.9, 1.0),
    conf.high = c(1.4, 1.8),
    p.value = c(0.2, 0.04),
    ref = c(FALSE, FALSE)
  )
  display <- data.frame(Characteristic = "age", is_header = TRUE)
  tbl <- structure(
    list(
      table_body = body,
      table_display = display,
      source = "uni_reg",
      approach = "logit"
    ),
    class = "gtregression"
  )

  p <- plot_reg(tbl)

  expect_true("age:smokeYes" %in% p$data$label)
})
