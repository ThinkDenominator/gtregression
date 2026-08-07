birthwt_save_data <- function() {
  data_birthwt |>
    dplyr::mutate(
      smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
      ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
      low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
    )
}

test_that("normalize save paths validates input and appends extensions", {
  path <- .normalize_save_path(file.path(tempdir(), "my-table"), "html")
  bare_path <- .normalize_save_path("my-table", "html")
  default_paths <- c(
    .normalize_save_path("table", "html"),
    .normalize_save_path("plot", "png"),
    .normalize_save_path("forest", "pdf"),
    .normalize_save_path("report.docx", "docx")
  )

  expect_match(path, "\\.html$")
  expect_equal(normalizePath(dirname(bare_path)), normalizePath(tempdir()))
  expect_equal(basename(bare_path), "my-table.html")
  expect_true(all(normalizePath(dirname(default_paths)) == normalizePath(tempdir())))
  expect_error(.normalize_save_path(character(0), "html"), "`filename` must be")
  expect_error(.normalize_save_path(NA_character_, "html"), "`filename` must be")
  expect_error(.normalize_save_path("x", ""), "`ext` must be")
})

test_that("resolve_table_object accepts package tables and raw rendered tables", {
  df <- birthwt_save_data()

  tbl <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit, format = gt)
  merged <- merge_tables(
    uni_reg(df, outcome = "low", exposures = "smoke", approach = logit, format = gt),
    multi_reg(df, outcome = "low", exposures = "smoke", approach = logit, format = gt)
  )

  expect_s3_class(.resolve_table_object(tbl), "gt_tbl")
  expect_s3_class(.resolve_table_object(merged), "gt_tbl")
  expect_s3_class(.resolve_table_object(tbl$table), "gt_tbl")
  expect_error(.resolve_table_object(data.frame(x = 1)), "`tbl` must be")
})

test_that("save_table writes gt html files and returns normalized paths", {
  skip_if_not_installed("gt")

  df <- birthwt_save_data()
  tbl <- uni_reg(df, outcome = "low", exposures = c("age", "smoke"), approach = logit, format = gt)
  filename <- file.path(tempdir(), paste0("gtregression-table-", Sys.getpid()))

  out <- suppressWarnings(save_table(tbl, filename = filename, format = html))

  expect_true(file.exists(out))
  expect_match(out, "\\.html$")
  unlink(out)
})

test_that("save_table writes flextable docx and html files", {
  skip_if_not_installed("flextable")

  df <- birthwt_save_data()
  tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke"),
    approach = logit,
    format = flextable
  )

  file_docx <- file.path(tempdir(), paste0("gtregression-flex-", Sys.getpid(), ".docx"))
  file_html <- file.path(tempdir(), paste0("gtregression-flex-", Sys.getpid(), ".html"))

  out_docx <- save_table(
    tbl,
    filename = file_docx,
    format = docx,
    orientation = auto,
    fit_width = TRUE,
    font_size = 9,
    min_font_size = 8
  )
  out_html <- save_table(tbl, filename = file_html, format = html)

  expect_true(file.exists(out_docx))
  expect_true(file.exists(out_html))
  expect_error(save_table(tbl, filename = file.path(tempdir(), "bad-flex"), format = pdf),
               "PDF is not directly supported")

  unlink(c(out_docx, out_html))
})

test_that("docx flextables are fitted without unsafe font shrinking", {
  skip_if_not_installed("flextable")

  wide_df <- as.data.frame(matrix(seq_len(100), nrow = 5))
  ft <- flextable::autofit(flextable::flextable(wide_df))
  fitted <- .fit_flextable_docx_width(
    ft,
    table_width = 6.5,
    fit_width = TRUE,
    font_size = 9,
    min_font_size = 8
  )
  unfitted <- .fit_flextable_docx_width(ft, table_width = NULL, font_size = 9)
  natural <- .fit_flextable_docx_width(ft, table_width = 6.5, fit_width = FALSE)

  expect_lte(sum(flextable::flextable_dim(fitted)$widths), 6.5)
  expect_s3_class(unfitted, "flextable")
  expect_gt(sum(flextable::flextable_dim(natural)$widths),
            sum(flextable::flextable_dim(fitted)$widths))
  expect_error(.fit_flextable_docx_width(ft, table_width = 0), "`table_width`")
  expect_error(.fit_flextable_docx_width(ft, min_font_size = 10, font_size = 9),
               "`min_font_size`")
})

test_that("docx orientation auto prefers landscape for wide flextables", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  wide_df <- as.data.frame(matrix(seq_len(80), nrow = 4))
  names(wide_df) <- paste0("long_column_", seq_along(wide_df))
  ft <- flextable::autofit(flextable::flextable(wide_df))
  prepared <- .prepare_flextable_docx(
    ft,
    orientation = "auto",
    fit_width = TRUE,
    font_size = 9,
    min_font_size = 8
  )

  expect_equal(prepared$orientation, "landscape")
  expect_equal(prepared$page_width, 9)
  expect_s3_class(prepared$table, "flextable")
  expect_error(.prepare_flextable_docx(ft, fit_width = NA), "`fit_width`")
})

test_that("save_plot writes files and validates inputs", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  filename <- file.path(tempdir(), paste0("gtregression-plot-", Sys.getpid()))

  out <- save_plot(p, filename = filename, format = png, width = 4, height = 3, dpi = 72)

  expect_true(file.exists(out))
  expect_match(out, "\\.png$")
  expect_error(save_plot(mtcars), "`plot` must be")
  expect_error(save_plot(p, width = 0), "`width` must be")
  expect_error(save_plot(p, height = NA_real_), "`height` must be")
  expect_error(save_plot(p, dpi = -1), "`dpi` must be")

  unlink(out)
})

test_that("save_forest writes forest outputs and validates inputs", {
  skip_if_not_installed("forestploter")

  df <- birthwt_save_data()
  uni <- uni_reg(
    df,
    outcome = "low",
    exposures = c("age", "smoke", "ht"),
    approach = logit
  )
  multi <- multi_reg(
    df,
    outcome = "low",
    exposures = c("smoke", "ht"),
    adjust_for = "age",
    approach = logit
  )
  forest <- forest_reg(uni = uni, multi = multi, quiet = TRUE)

  file_pdf <- file.path(tempdir(), paste0("gtregression-forest-", Sys.getpid()))
  file_png <- file.path(tempdir(), paste0("gtregression-forest-", Sys.getpid(), "-png"))
  file_bare <- paste0("gtregression-forest-temp-", Sys.getpid())

  out_pdf <- save_forest(forest, filename = file_pdf, format = pdf,
                         width = 7, height = 5, padding = 0.1)
  out_png <- save_forest(forest, filename = file_png, format = png,
                         width = 7, height = 5, auto_size = FALSE, dpi = 72)
  out_bare <- save_forest(forest, filename = file_bare, format = pdf,
                          width = 7, height = 5)

  expect_true(file.exists(out_pdf))
  expect_true(file.exists(out_png))
  expect_true(file.exists(out_bare))
  expect_match(out_pdf, "\\.pdf$")
  expect_match(out_png, "\\.png$")
  expect_equal(normalizePath(dirname(out_bare)), normalizePath(tempdir()))

  expect_error(save_plot(forest), "Use `save_forest\\(\\)`")
  expect_error(save_forest(mtcars), "`forest` must be")
  expect_error(save_forest(forest, width = 0), "`width` must be")
  expect_error(save_forest(forest, height = NA_real_), "`height` must be")
  expect_error(save_forest(forest, scale = 0), "`scale` must be")
  expect_error(save_forest(forest, auto_size = NA), "`auto_size` must be")
  expect_error(save_forest(forest, padding = -1), "`padding` must be")
  expect_error(save_forest(forest, padding = 3, width = 4, height = 4), "`padding` is too large")
  expect_error(save_forest(forest, dpi = -1), "`dpi` must be")

  unlink(c(out_pdf, out_png, out_bare))
})

test_that("save_docx accepts single flextable table and single plot", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  df <- birthwt_save_data()
  tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    format = flextable
  )
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  filename <- file.path(tempdir(), paste0("gtregression-report-", Sys.getpid()))

  out <- save_docx(
    tables = tbl,
    plots = p,
    filename = filename,
    titles = c("Table 1", "Figure 1"),
    plot_width = 4,
    plot_height = 3
  )

  expect_true(file.exists(out))
  expect_match(out, "\\.docx$")

  unlink(out)
})

test_that("save_docx validates inputs and rejects gt tables for Word export", {
  skip_if_not_installed("officer")

  df <- birthwt_save_data()
  gt_tbl <- uni_reg(df, outcome = "low", exposures = "smoke", approach = logit, format = gt)

  expect_error(save_docx(), "at least one table or plot")
  expect_error(save_docx(tables = data.frame(x = 1)), "`tables` must be")
  expect_error(save_docx(plots = data.frame(x = 1)), "`plots` must be")
  expect_error(save_docx(plots = list(data.frame(x = 1))), "must be ggplot2 objects")
  expect_error(save_docx(plots = ggplot2::ggplot(), table_width = 0), "`table_width`")
  expect_error(save_docx(plots = ggplot2::ggplot(), plot_width = 0), "`plot_width` must be")
  expect_error(save_docx(plots = ggplot2::ggplot(), plot_height = NA_real_), "`plot_height` must be")
  expect_error(save_docx(tables = gt_tbl), "gt tables")

  skip_if_not_installed("flextable")
  ft_tbl <- uni_reg(
    df,
    outcome = "low",
    exposures = "smoke",
    approach = logit,
    format = flextable
  )
  expect_warning(
    out <- save_docx(
      tables = list(ft_tbl),
      filename = file.path(tempdir(), paste0("gtregression-warning-", Sys.getpid())),
      titles = c("One", "Too many")
    ),
    "Length of `titles`"
  )
  expect_true(file.exists(out))
  unlink(out)
})
