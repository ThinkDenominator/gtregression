test_that("choice-style arguments accept bare values in descriptive_table", {
  df <- data.frame(
    group = factor(c("A", "A", "B", "B")),
    status = factor(c("No", "Yes", "No", NA))
  )

  tbl <- descriptive_table(
    df,
    exposures = status,
    by = group,
    percent = row,
    show_missing = no,
    show_overall = first,
    format = flextable,
    theme = shaded
  )

  expect_s3_class(tbl, "descriptive_table")
  expect_s3_class(tbl$table, "flextable")
  expect_equal(tbl$format, "flextable")
  expect_named(tbl$table_display, c("Characteristic", "is_header", "Overall", "A", "B"))
  expect_false("(Missing)" %in% trimws(tbl$table_display$Characteristic))
})

test_that("choice-style arguments still accept objects", {
  df <- data.frame(status = factor(c("No", "Yes", "No")))
  pct <- "column"
  overall <- "last"
  fmt <- "gt"
  thm <- c("header_shaded", "compact")

  tbl <- descriptive_table(
    df,
    exposures = "status",
    percent = pct,
    show_overall = overall,
    format = fmt,
    theme = thm
  )

  expect_s3_class(tbl, "gt_desc")
  expect_equal(tbl$format, "gt")
})

test_that("regression table options accept bare values", {
  df <- mtcars
  df$am <- factor(df$am)

  tbl <- uni_reg(
    data = df,
    outcome = mpg,
    exposures = hp,
    approach = linear,
    theme = minimal
  )

  expect_s3_class(tbl, "uni_reg")
  expect_equal(tbl$approach, "linear")
  expect_equal(tbl$format, "flextable")
  expect_s3_class(tbl$table, "flextable")
})

test_that("logbinomial is accepted bare and old hyphen spelling remains an alias", {
  df <- data.frame(
    y = c(0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 1, 0)
  )

  tbl_bare <- uni_reg(
    data = df,
    outcome = "y",
    exposures = "x",
    approach = logbinomial,
    format = gt
  )
  tbl_old <- uni_reg(
    data = df,
    outcome = "y",
    exposures = "x",
    approach = "log-binomial",
    format = gt
  )

  expect_s3_class(tbl_bare, "uni_reg")
  expect_equal(tbl_bare$approach, "logbinomial")
  expect_equal(tbl_old$approach, "logbinomial")
})

test_that("selection and diagnostics options accept bare values", {
  df <- mtcars

  sel <- select_models(
    data = df,
    outcome = "mpg",
    exposures = c("hp", "wt"),
    approach = linear,
    direction = forward
  )

  expect_type(sel, "list")

  conv <- check_convergence(
    data = data.frame(y = c(0, 1, 0, 1), x = c(1, 2, 3, 4)),
    exposures = "x",
    outcome = "y",
    approach = logit,
    format = tibble
  )

  expect_s3_class(conv, "data.frame")
  expect_equal(conv$Model, "logit")
})

test_that("interaction test option accepts bare values", {
  df <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(1, 1, 2, 2, 3, 3),
    z = factor(c("A", "B", "A", "B", "A", "B"))
  )

  res <- interaction_models(
    data = df,
    outcome = "y",
    exposure = "x",
    effect_modifier = "z",
    approach = linear,
    test = LRT,
    verbose = FALSE
  )

  expect_type(res, "list")
  expect_equal(res$test, "Likelihood Ratio Test")
})
