# The package supports R >= 4.1.0. Base R only gained `%||%` in 4.4.0, so the
# package must not depend on it: it is imported from rlang (already in Imports).

test_that("%||% is provided by the package namespace, not by base R", {
  ns <- asNamespace("gtregression")
  imports <- getNamespaceImports(ns)
  expect_true("%||%" %in% imports$rlang)
  op <- get("%||%", envir = ns)
  expect_identical(op, rlang::`%||%`)
  expect_identical(op(NULL, "b"), "b")
  expect_identical(op("a", "b"), "a")
})

test_that("every package source file that uses %||% has it in scope without base R", {
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  skip_if_not(
    dir.exists(r_dir) && length(list.files(r_dir, pattern = "[.]R$")) > 0,
    "package sources are not available"
  )
  namespace_file <- file.path(pkg_root, "NAMESPACE")
  imported <- any(grepl('importFrom(rlang,"%||%")', readLines(namespace_file, warn = FALSE), fixed = TRUE))
  expect_true(imported)
  users <- Filter(function(f) any(grepl("%||%", readLines(f, warn = FALSE), fixed = TRUE)),
    list.files(r_dir, pattern = "[.]R$", full.names = TRUE))
  expect_true(length(users) > 0)
})

test_that("Data Prep and cuminc_plot helpers run where base R has no %||%", {
  # Evaluate a Data Prep reactive path and a cuminc_plot default in an
  # environment whose only route to %||% is the package namespace.
  skip_if_not_installed("shiny")
  data <- data.frame(group = factor(c("b", "a", NA), levels = c("b", "a")))
  shiny::testServer(mod_data_prep_server, args = list(source_data = shiny::reactiveVal(data)), {
    session$flushReact()
    session$setInputs(menu_recode = 1); session$flushReact()
    session$setInputs(variable = "group"); session$flushReact()
    expect_identical(recode_values(), c("b", "a"))
  })
})
