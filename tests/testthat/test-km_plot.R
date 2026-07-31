lung_km_data <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, levels = c(1, 2),
                   labels = c("Standard treatment", "Test treatment")),
      prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
    )
}

test_that("km_plot returns an ungrouped ggplot when risk table is hidden", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    risk_table = FALSE
  )

  expect_s3_class(res, "ggplot")
  expect_s3_class(attr(res, "fit"), "survfit")
  expect_s3_class(attr(res, "plot_data"), "data.frame")
  expect_s3_class(attr(res, "risk_table"), "data.frame")
  expect_true(all(c("time", "survival", "conf.low", "conf.high", "strata") %in%
                    names(attr(res, "plot_data"))))
  expect_equal(unique(as.character(attr(res, "plot_data")$strata)), "Overall")
  expect_true(is.na(attr(res, "logrank_p")))
})

test_that("km_plot returns grouped curves with risk table and log-rank p-value", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  df <- lung_km_data()
  attr(df$trt, "label") <- "Treatment group"

  res <- km_plot(
    data = df,
    time = "time",
    event = "status",
    by = "trt",
    break_time_by = 200,
    title = "Kaplan-Meier survival by treatment"
  )

  expect_s3_class(res, "patchwork")
  expect_s3_class(attr(res, "fit"), "survfit")
  expect_true(is.finite(attr(res, "logrank_p")))
  expect_true(all(c("Standard treatment", "Test treatment") %in%
                    unique(as.character(attr(res, "plot_data")$strata))))
  expect_true(all(c(0, 200, 400) %in% attr(res, "risk_table")$time))
})

test_that("km_plot builds without dropping confidence interval rows", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    by = trt,
    risk_table = FALSE,
    break_time_by = 200
  )

  expect_warning(ggplot2::ggplot_build(res), NA)
})

test_that("km_plot supports y-axis limits and shaded confidence intervals", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    by = trt,
    risk_table = FALSE,
    ylim = c(50, 100),
    conf.int = TRUE
  )

  expect_s3_class(res, "ggplot")
  expect_equal(.km_normalize_ylim(c(50, 100), y_percent = TRUE), c(0.5, 1))
  expect_equal(.km_normalize_ylim(c(0.5, 1), y_percent = FALSE), c(0.5, 1))
  expect_true(any(vapply(
    res$layers,
    function(layer) inherits(layer$geom, "GeomRibbon"),
    logical(1)
  )))
  expect_warning(ggplot2::ggplot_build(res), NA)
})

test_that("km_plot draws log-rank p-value inside the graph", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    by = trt,
    risk_table = FALSE,
    p_value_position = c(100, 0.12)
  )

  last_layer <- ggplot2::layer_data(res, length(res$layers))
  expect_true(any(grepl("Log-rank p", last_layer$label, fixed = TRUE)))
  expect_equal(last_layer$x[1], 100)
  expect_equal(last_layer$y[1], 0.12)
})

test_that("km_plot supports clean themes and raw probability y-axis", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    risk_table = FALSE,
    y_percent = FALSE,
    theme = classic
  )

  expect_s3_class(res, "ggplot")
  expect_s3_class(res$theme$panel.grid.major, "element_blank")
  expect_s3_class(res$theme$panel.grid.minor, "element_blank")

  res_grid <- km_plot(
    data = df,
    time = time,
    event = status,
    risk_table = FALSE,
    theme = "minimal",
    grid = TRUE
  )

  expect_s3_class(res_grid, "ggplot")
})

test_that("km_plot supports publication panel styling controls", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  df <- lung_km_data()

  res <- km_plot(
    data = df,
    time = time,
    event = status,
    by = trt,
    risk_table = FALSE,
    title = "A. Treatment group",
    subtitle = "Lung cancer trial",
    caption = "Log-rank test shown inside the panel.",
    title_size = 10,
    title_face = plain,
    legend_position = none
  )

  expect_s3_class(res, "ggplot")
  expect_equal(res$labels$title, "A. Treatment group")
  expect_equal(res$labels$subtitle, "Lung cancer trial")
  expect_equal(res$labels$caption, "Log-rank test shown inside the panel.")
  expect_equal(res$theme$plot.title$size, 10)
  expect_equal(res$theme$plot.title$face, "plain")
  expect_equal(res$theme$legend.position, "none")
})

test_that("km_plot validates inputs", {
  skip_if_not_installed("survival")

  df <- lung_km_data()
  df$bad_event <- 0
  df$one_group <- "A"

  expect_error(
    km_plot(data = "not data", time = time, event = status),
    "`data` must be"
  )
  expect_error(
    km_plot(data = df, time = missing_time, event = status),
    "`time` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = missing_event),
    "`event` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, by = one_group),
    "`by` must contain at least two"
  )
  expect_error(
    km_plot(data = df, time = time, event = bad_event),
    "`event` must include at least one event"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, break_time_by = 0),
    "`break_time_by` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, xlim = c(10, 1)),
    "`xlim` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, ylim = c(100, 50)),
    "`ylim` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, ylim = c(-10, 100)),
    "`ylim` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, p_value_position = c(10, 2)),
    "`p_value_position` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, title = c("A", "B")),
    "`title` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, title_size = 0),
    "`title_size` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, title_face = heavy),
    "`title_face` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, legend_position = middle),
    "`legend_position` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, risk_table = NA),
    "`risk_table` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, y_percent = NA),
    "`y_percent` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, grid = NA),
    "`grid` must be"
  )
  expect_error(
    km_plot(data = df, time = time, event = status, theme = ugly),
    "`theme` must be"
  )
})
