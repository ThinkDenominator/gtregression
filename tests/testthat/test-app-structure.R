# Data > Structure always describes the current analysis dataset.

test_that("Structure follows the analysis dataset through Import, Prepare, Undo, Redo and Reset", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- data.frame(
    age = c(9, 25, 40, NA, 18, 65),
    smoking = factor(c("Y", "N", "Y", NA, "N", "Y"), levels = c("N", "Y")),
    status = c("no data", "yes", "yes", NA, "no data", "unknown"),
    stringsAsFactors = FALSE
  )
  data_env <- new.env(parent = emptyenv())
  data_env$study <- start
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  suppressWarnings(shiny::testServer(app_env$server, {
    html <- function(id) paste(as.character(output[[id]]$html), collapse = "")
    check <- function(state) {
      expect_identical(structure_info(), gt_data_structure(analysis_data()))
      overview <- html("structure_overview")
      expect_match(overview, state, fixed = TRUE)
      table <- html("structure_table")
      for (v in names(analysis_data())) expect_match(table, paste0("data-variable=\"", v, "\""), fixed = TRUE)
      expect_identical(
        lengths(regmatches(table, gregexpr("data-variable=", table, fixed = TRUE))),
        ncol(analysis_data())
      )
    }
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    # Import
    check(">original<")
    expect_match(html("structure_overview"), "6 rows", fixed = TRUE)
    # Prepare: keep two variables
    session$setInputs(`data_prep-menu_keep` = 1); session$flushReact()
    session$setInputs(`data_prep-keep_vars` = c("age", "smoking")); session$setInputs(`data_prep-apply` = 1); session$flushReact()
    check("prepared &middot; 1 change")
    expect_false(grepl("data-variable=\"status\"", html("structure_table"), fixed = TRUE))
    # Prepare: filter rows (age >= 18)
    session$setInputs(`data_prep-menu_filter` = 1); session$flushReact()
    session$setInputs(`data_prep-variable1` = "age", `data_prep-operator1` = ">=", `data_prep-value1` = "18", `data_prep-filter_second` = FALSE)
    session$setInputs(`data_prep-apply` = 2); session$flushReact()
    check("prepared &middot; 2 changes")
    expect_match(html("structure_overview"), "4 rows", fixed = TRUE)
    # Undo, Redo, Reset
    session$setInputs(`data_prep-undo` = 1); session$flushReact()
    check("prepared &middot; 1 change")
    expect_match(html("structure_overview"), "6 rows", fixed = TRUE)
    session$setInputs(`data_prep-redo` = 1); session$flushReact()
    check("prepared &middot; 2 changes")
    session$setInputs(`data_prep-reset` = 1); session$setInputs(`data_prep-confirm_reset` = 1); session$flushReact()
    check(">original<")
    expect_identical(analysis_data(), start)
    # Detail keeps the factor's level order.
    session$setInputs(structure_variable = "smoking"); session$flushReact()
    detail <- html("structure_detail")
    expect_lt(regexpr(">N<", detail, fixed = TRUE)[[1]], regexpr(">Y<", detail, fixed = TRUE)[[1]])
    expect_match(detail, "Factor &middot; 2 levels", fixed = TRUE)
    session$setInputs(structure_variable = "not_a_variable"); session$flushReact()
    expect_match(html("structure_detail"), "Select a variable", fixed = TRUE)
  }))
})

test_that("the shell's Structure tab uses the structure outputs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = new.env(parent = emptyenv()))
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  html <- htmltools::renderTags(app_env$ui)$html
  for (id in c("structure_overview", "structure_table", "structure_detail", "data_code", "gta-structure-code")) {
    expect_match(html, paste0("id=\"", id, "\""), fixed = TRUE, info = id)
  }
  expect_false(grepl("id=\"data_summary\"", html, fixed = TRUE))
})
