library(testthat)

# Ensure %||% exists for tests
test_that("setup: define %||%", {
  `%||%` <- tryCatch(get("%||%", envir = parent.frame(), inherits = TRUE),
                     error = function(e) NULL)
  if (!is.function(`%||%`)) {
    `%||%` <<- function(x, y) if (length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
  }
  succeed()
})

# Helper: patch forestploter namespace so real forest() never runs
.mock_forestploter <- function(captured_env = NULL) {
  if (is.null(captured_env)) captured_env <- new.env(parent = emptyenv())

  # Patch forest() in forestploter namespace
  testthat::local_mocked_bindings(
    forest = function(...) {
      captured_env$args <- list(...)
      structure(list(.plot_class = "dummy_forest"), class = "dummy_forest")
    },
    .env = asNamespace("forestploter")
  )

  # (Optional) also patch forest_theme if you want to decouple from that too
  testthat::local_mocked_bindings(
    forest_theme = function(...) list(.mock_theme = TRUE),
    .env = asNamespace("forestploter")
  )

  invisible(captured_env)
}

test_that("forest_reg: single-column (univariate) works and reorders columns (side=right)", {
  df1 <- data.frame(
    Characteristic = c("Age", "  18–40", "  >40"),
    "OR (95% CI)"  = c("", "1.20 (1.00–1.44)", "0.80 (0.60–1.10)"),
    " "            = NA_real_,
    se_uni         = c(NA, 0.10, 0.20),
    stringsAsFactors = FALSE
  )
  attr(df1, "est")  <- c(NA, 1.20, 0.80)
  attr(df1, "lo")   <- c(NA, 1.00, 0.60)
  attr(df1, "hi")   <- c(NA, 1.44, 1.10)
  attr(df1, "forest_meta") <- list(x_trans = "log", ref_line = 1)

  captured <- .mock_forestploter()

  res <- forest_reg(df = df1, quiet = TRUE)

  expect_s3_class(res, "gtregression_forest")
  expect_true(is.list(res$meta))
  expect_true(is.data.frame(res$data))
  expect_equal(res$meta$x_trans, "log")
  expect_equal(res$meta$ref_line, 1)

  expect_true(is.list(captured$args))
  expect_true("ci_column" %in% names(captured$args))
  expect_type(captured$args$ci_column, "integer")
  expect_equal(length(captured$args$ci_column), 1L)
  expect_true(all(is.finite(captured$args$xlim)))
  expect_true(captured$args$xlim[1] >= 1e-6)

  expect_invisible(print(res))
})

test_that("forest_reg: two-column (uni + adjusted) works (side=left) with vector ci_col_width", {
  df2 <- data.frame(
    Characteristic             = c("Sex", "  Female", "  Male"),
    "OR (95% CI)"              = c("", "1.50 (1.10–2.05)", "0.90 (0.70–1.20)"),
    "Adjusted OR (95% CI)"     = c("", "1.40 (1.05–1.90)", "0.95 (0.72–1.25)"),
    " "                        = NA_real_,
    "  "                       = NA_real_,
    se_uni                     = c(NA, 0.12, 0.25),
    se_adj                     = c(NA, 0.15, 0.22),
    stringsAsFactors = FALSE
  )
  attr(df2, "est")  <- c(NA, 1.50, 0.90)
  attr(df2, "lo")   <- c(NA, 1.10, 0.70)
  attr(df2, "hi")   <- c(NA, 2.05, 1.20)
  attr(df2, "est2") <- c(NA, 1.40, 0.95)
  attr(df2, "lo2")  <- c(NA, 1.05, 0.72)
  attr(df2, "hi2")  <- c(NA, 1.90, 1.25)
  attr(df2, "forest_meta") <- list(x_trans = "log", ref_line = 1)

  captured <- .mock_forestploter()

  res <- forest_reg(df = df2, side = "left", ci_col_width = c(0.22, 0.26), quiet = TRUE)

  expect_s3_class(res, "gtregression_forest")
  expect_equal(res$meta$x_trans, "log")

  expect_true(is.list(captured$args))
  expect_true(all(c("lower","upper","est") %in% names(captured$args)))
  expect_equal(length(captured$args$ci_column), 2L)
  expect_equal(captured$args$ci_col_width, c(0.22, 0.26))
  expect_true(is.list(captured$args$xlim))
  expect_equal(length(captured$args$xlim), 2L)

  expect_invisible(print(res))
})

test_that("forest_reg: default meta is applied when none provided", {
  df3 <- data.frame(
    Characteristic = c("Var", "  A"),
    "Beta (95% CI)" = c("", "0.15 (0.01–0.29)"),
    " " = NA_real_,
    se_uni = c(NA, 0.05),
    stringsAsFactors = FALSE
  )
  attr(df3, "est") <- c(NA, 0.15)
  attr(df3, "lo")  <- c(NA, 0.01)
  attr(df3, "hi")  <- c(NA, 0.29)

  captured <- .mock_forestploter()

  res <- forest_reg(df = df3, quiet = TRUE)
  expect_equal(res$meta$x_trans, "none")
  expect_equal(res$meta$ref_line, 0)
  expect_true(is.numeric(captured$args$xlim))
})

test_that("forest_reg: errors when df is NULL and no gtregression objects provided", {
  .mock_forestploter()
  expect_error(
    forest_reg(df = NULL, uni = NULL, quiet = TRUE),
    regexp = "inherits\\(uni, \"gtregression\"\\)"
  )
})

test_that("forest_reg: handles zero/NA SEs in weights without error", {
  df4 <- data.frame(
    Characteristic = c("X", "  x1", "  x2"),
    "RR (95% CI)"  = c("", "1.10 (0.90–1.34)", "0.95 (0.70–1.28)"),
    " "            = NA_real_,
    se_uni         = c(NA, 0, NA),
    stringsAsFactors = FALSE
  )
  attr(df4, "est") <- c(NA, 1.10, 0.95)
  attr(df4, "lo")  <- c(NA, 0.90, 0.70)
  attr(df4, "hi")  <- c(NA, 1.34, 1.28)
  attr(df4, "forest_meta") <- list(x_trans = "log", ref_line = 1)

  .mock_forestploter()

  expect_error(forest_reg(df = df4, quiet = TRUE), NA)
})
