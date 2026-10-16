test_that("RStudio Addin registration names an exported binding", {
  addin_file <- system.file(
    "rstudio", "addins.dcf",
    package = "gtregression"
  )

  expect_true(nzchar(addin_file))
  expect_true(file.exists(addin_file))

  registration <- read.dcf(addin_file)
  expect_identical(unname(registration[1, "Name"]), "GT Regression app")
  expect_identical(
    unname(registration[1, "Binding"]),
    "gtregression_addin"
  )
  expect_identical(tolower(unname(registration[1, "Interactive"])), "true")
  expect_true(
    unname(registration[1, "Binding"]) %in%
      getNamespaceExports("gtregression")
  )
})

test_that("gtregression_addin fails clearly outside RStudio", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("rstudioapi")
  skip_if(
    rstudioapi::isAvailable(),
    "This assertion applies only outside an available RStudio session."
  )

  expect_error(
    gtregression_addin(),
    "must be run from an available RStudio session",
    fixed = TRUE
  )
})

test_that("both launchers build the one canonical app and differ only in container", {
  expect_true(is.function(gtregression:::gtx_shiny_app_dir))
  expect_identical(
    gtregression:::gtx_shiny_app_dir(),
    system.file("shiny", package = "gtregression")
  )

  canonical <- paste(deparse(body(gtregression:::gtx_gui_app)), collapse = "\n")
  app_launcher <- paste(deparse(body(gtregression_app)), collapse = "\n")
  addin_launcher <- paste(deparse(body(gtregression_addin)), collapse = "\n")
  expect_match(canonical, "gtx_shiny_app(mode = \"app\", data_env = globalenv())", fixed = TRUE)
  # gtregression_app(): canonical app in the browser.
  expect_match(app_launcher, "gtx_shiny_app_dir()", fixed = TRUE)
  expect_match(app_launcher, "gtx_gui_app()", fixed = TRUE)
  expect_match(app_launcher, "shiny::runApp(app", fixed = TRUE)
  # Addin: the same canonical app in a 1200 x 800 RStudio dialog.
  expect_match(addin_launcher, "gtx_gui_app()", fixed = TRUE)
  expect_match(addin_launcher, "shiny::dialogViewer", fixed = TRUE)
  expect_match(addin_launcher, "width = 1200", fixed = TRUE)
  expect_match(addin_launcher, "height = 800", fixed = TRUE)
  expect_match(addin_launcher, "shiny::runGadget", fixed = TRUE)
  expect_match(addin_launcher, "stopOnCancel = TRUE", fixed = TRUE)
  # Neither launcher builds its own application.
  for (launcher in c(app_launcher, addin_launcher)) {
    expect_false(grepl("gtx_shiny_app(", launcher, fixed = TRUE))
    expect_false(grepl("sys.source", launcher, fixed = TRUE))
  }
})

test_that("Addin context loads an environment dataset into the shared server", {
  skip_if_not_installed("shiny")
  data_env <- new.env(parent = emptyenv())
  data_env$mydata <- mtcars
  original <- data_env$mydata

  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "addin", data_env = data_env)
  sys.source(app_file, envir = app_env)

  expect_true(app_env$gtx_addin_mode)
  addin_ui <- htmltools::renderTags(app_env$ui)$html
  expect_match(addin_ui, "environment_dataset_input", fixed = TRUE)
  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(
      session$setInputs(
        environment_dataset_name = "mydata",
        load_environment_data = 1
      )
    )
    session$flushReact()

    expect_identical(rv$data, original)
    expect_identical(rv$data_name, "mydata")
    expect_identical(rv$data_origin, "R Environment")
    expect_identical(rv$data_reference, "mydata")
    expect_match(rv$data_code, "existing R object mydata", fixed = TRUE)
    expect_match(analysis_code_prefix(), "analysis_data <- mydata", fixed = TRUE)
  })

  expect_identical(data_env$mydata, original)
})

test_that("the Addin app factory requires and retains explicit context", {
  skip_if_not_installed("shiny")
  data_env <- new.env(parent = emptyenv())
  data_env$mydata <- mtcars

  expect_error(
    gtregression:::gtx_shiny_app(mode = "addin"),
    "requires an explicit data environment",
    fixed = TRUE
  )
  expect_s3_class(
    gtregression:::gtx_shiny_app(mode = "addin", data_env = data_env),
    "shiny.appobj"
  )
})

test_that("ordinary app source defaults to web mode", {
  skip_if_not_installed("shiny")
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  sys.source(app_file, envir = app_env)

  expect_identical(app_env$gtx_app_mode, "web")
  expect_false(app_env$gtx_addin_mode)
  web_ui <- htmltools::renderTags(app_env$ui)$html
  expect_false(grepl("environment_dataset_input", web_ui, fixed = TRUE))
  expect_false(grepl("data_source", web_ui, fixed = TRUE))
})
