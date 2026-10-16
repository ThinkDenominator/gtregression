# The app never stops on an error, and a variable chosen for two roles is
# explained with a one-click fix instead of an error.

test_that("error helpers turn errors into plain messages", {
  friendly <- gtregression:::gtx_friendly_error
  expect_match(friendly(simpleError("subscript out of bounds")), "only one role")
  expect_match(friendly(simpleError("object 'smoke' not found")), "'smoke' was not found")
  expect_identical(friendly(simpleError("The stratifier `race` cannot be used.")),
                   "The stratifier 'race' cannot be used.")
  expect_match(friendly(simpleError("")), "could not be completed")

  conflicts <- gtregression:::gtx_role_conflicts(
    list(Outcome = "low", Stratifier = "race", Unused = NULL),
    list(
      list(id = "e", label = "Exposures", value = c("race", "smoke", "low")),
      list(id = "a", label = "Adjust for", value = c("smoke", "age"))
    )
  )
  expect_identical(conflicts$variable, c("race", "low", "smoke"))
  expect_identical(conflicts$id, c("e", "e", "a"))
  expect_identical(
    gtregression:::gtx_role_conflict_text(conflicts),
    c("'race' is the stratifier and is also selected in Exposures.",
      "'low' is the outcome and is also selected in Exposures.",
      "'smoke' is selected in both Exposures and Adjust for.")
  )
  expect_length(gtregression:::gtx_role_clashes(list(Outcome = "low", Stratifier = "race")), 0)
  expect_match(gtregression:::gtx_role_clashes(list(Outcome = "low", Stratifier = "low")),
               "both Outcome and Stratifier")
})

test_that("guarded observers survive errors and keep return() and req()", {
  skip_if_not_installed("shiny")
  safe_observe_event <- gtregression:::gtx_safe_observe_event
  safe_observe <- gtregression:::gtx_safe_observe
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(a = 0, b = 0)
    safe_observe_event(input$go, {
      if (input$go == 1) return(NULL)
      rv$a <- input$go
    })
    safe_observe_event(input$bad, stop("subscript out of bounds"))
    safe_observe_event(input$wait, {
      shiny::req(FALSE)
      rv$b <- 1
    })
    safe_observe({
      if (isTRUE(input$bad2 > 0)) stop("boom")
    })
  }
  shiny::testServer(server, {
    session$setInputs(go = 1)
    expect_identical(rv$a, 0)
    expect_no_error(suppressMessages(session$setInputs(bad = 1)))
    expect_no_error(session$setInputs(wait = 1))
    expect_identical(rv$b, 0)
    expect_no_error(suppressMessages(session$setInputs(bad2 = 1)))
    session$setInputs(go = 2)
    expect_identical(rv$a, 2)
  })
})

role_app_env <- function(data) {
  data_env <- new.env(parent = emptyenv())
  data_env$study <- data
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  app_env
}

role_birthwt <- function() {
  data <- gtregression::data_birthwt
  data$race <- factor(data$race, levels = 1:3, labels = c("White", "Black", "Other"))
  data$smoke <- factor(data$smoke, levels = 0:1, labels = c("No", "Yes"))
  data
}

test_that("every app observer is guarded", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_env <- role_app_env(role_birthwt())
  expect_identical(app_env$observeEvent, gtregression:::gtx_safe_observe_event)
  expect_identical(app_env$observe, gtregression:::gtx_safe_observe)
  expect_identical(app_env$downloadHandler, gtregression:::gtx_safe_download_handler)
  app_code <- readLines(system.file("shiny", "app.R", package = "gtregression"))
  expect_false(any(grepl("shiny::observeEvent\\(|shiny::observe\\(", app_code)))
  expect_false(any(grepl("gtx_validate_roles\\(", app_code[!grepl("<- function", app_code)])))
})

test_that("a stratifier that is also an exposure is explained, fixed and run", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  app_env <- role_app_env(role_birthwt())

  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    session$setInputs(
      reg_outcome = "low", reg_exposures = c("age", "smoke", "race"), reg_adjust = "lwt",
      reg_approach = "logit", reg_show_ref = TRUE, reg_model_stats = TRUE,
      reg_interaction = "", reg_stratifier = "smoke", reg_strata_multi = FALSE
    )

    # The panel warns before anything is run.
    expect_identical(strata_conflicts()$conflicts$variable, "smoke")
    expect_match(as.character(output$reg_strata_conflict$html), "Remove 'smoke' from Exposures", fixed = TRUE)

    # Running does not stop the app; it asks for the fix instead.
    expect_no_error(suppressMessages(suppressWarnings(session$setInputs(run_strat = 1))))
    expect_null(rv$strat)
    fix <- role_fix()
    expect_identical(fix$conflicts$variable, "smoke")
    expect_identical(fix$conflicts$id, "reg_exposures")
    expect_identical(fix$run, "run_strat")

    # Adjustment variables are checked too when stratified multivariable is on.
    session$setInputs(reg_exposures = c("age", "race"), reg_adjust = c("lwt", "smoke"), reg_strata_multi = TRUE)
    expect_no_error(suppressMessages(suppressWarnings(session$setInputs(run_strat = 2))))
    expect_identical(role_fix()$conflicts$id, "reg_adjust")

    # The one-click fix clears the pending fix.
    expect_no_error(session$setInputs(gtx_role_fix = 1))
    expect_null(role_fix())

    # Once the selections are distinct the stratified table is produced.
    session$setInputs(reg_adjust = "lwt")
    expect_null(strata_conflicts())
    suppressMessages(suppressWarnings(capture.output(session$setInputs(run_strat = 3))))
    expect_false(is.null(rv$strat))

    # Stratifier equal to the outcome cannot be fixed by removal.
    session$setInputs(reg_stratifier = "low")
    expect_no_error(suppressMessages(suppressWarnings(session$setInputs(run_strat = 4))))
    expect_null(role_fix())
  })
})

test_that("role conflicts in other workspaces never stop the app", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  app_env <- role_app_env(role_birthwt())

  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()

    session$setInputs(reg_outcome = "low", reg_exposures = c("low", "age"), reg_adjust = character(0),
                      reg_approach = "logit", reg_show_ref = TRUE, reg_model_stats = TRUE,
                      reg_interaction = "")
    expect_no_error(suppressMessages(session$setInputs(run_uni = 1)))
    expect_null(rv$uni)
    expect_identical(role_fix()$run, "run_uni")

    session$setInputs(reg_exposures = c("age", "smoke"), reg_adjust = c("smoke", "lwt"))
    expect_no_error(suppressMessages(session$setInputs(run_multi = 1)))
    expect_identical(role_fix()$conflicts$id, "reg_adjust")

    session$setInputs(desc_exposures = c("age", "low"), desc_by = "low")
    expect_no_error(suppressMessages(session$setInputs(run_desc = 1)))
    expect_identical(role_fix()$conflicts$id, "desc_exposures")

    session$setInputs(surv_time = "age", surv_event = "low", surv_exposures = c("age", "smoke"),
                      surv_adjust = character(0), surv_stratifier = "", surv_multivariable = FALSE,
                      surv_show_ref = TRUE)
    expect_no_error(suppressMessages(session$setInputs(run_cox = 1)))
    expect_identical(role_fix()$conflicts$variable, "age")
    expect_identical(role_fix()$run, "run_cox")

    session$setInputs(km_by = "low")
    expect_no_error(suppressMessages(session$setInputs(run_km = 1)))
    expect_null(rv$km)

    session$setInputs(trial_outcome = "low", trial_exposure = "smoke", trial_adjust = c("smoke", "age"))
    expect_no_error(suppressMessages(session$setInputs(run_trial = 1)))
    expect_identical(role_fix()$conflicts$variable, "smoke")
  })
})
