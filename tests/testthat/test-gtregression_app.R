test_that("gtregression app is bundled with the package", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")

  expect_true(nzchar(app_file))
  expect_true(file.exists(app_file))
  expect_true("gtregression_app" %in% getNamespaceExports("gtregression"))
})

test_that("app organises visualisation tools and available combinations", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '"visual_tool"', fixed = TRUE)
  expect_match(app_code, '"merge_table_choices"', fixed = TRUE)
  expect_match(app_code, '"plot_content_controls"', fixed = TRUE)
  expect_match(app_code, '"forest_content_controls"', fixed = TRUE)
  expect_match(app_code, '"Descriptive + regression crude + adjusted"', fixed = TRUE)
  expect_match(app_code, "gtregression::plot_reg_combine(pair[[1]], pair[[2]]", fixed = TRUE)
  expect_match(app_code, "gtregression::forest_df(pair[[1]], pair[[2]], desc = desc)", fixed = TRUE)
  expect_match(app_code, "Run both the exposure models and multivariable model", fixed = TRUE)
})

test_that("app treats stratified plots and forests as single sources", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, "is_stratified_result <- function", fixed = TRUE)
  expect_match(app_code, "pair_can_combine <- function", fixed = TRUE)
  expect_match(app_code, "Cox exposure models (stratified)", fixed = TRUE)
  expect_match(app_code, "plot_reg() creates strata-specific panels", fixed = TRUE)
  expect_match(app_code, "forest_reg() places strata side by side", fixed = TRUE)
  expect_match(app_code, "combined_ready <- pair_can_combine(rv$uni, rv$multi)", fixed = TRUE)
  expect_match(app_code, "cox_combined_ready <- pair_can_combine(rv$cox_exposure, rv$cox_multi)", fixed = TRUE)
  expect_match(app_code, "survreg_combined_ready <- pair_can_combine(rv$survreg_exposure, rv$survreg_multi)", fixed = TRUE)
  expect_match(app_code, "special_combined_choices <- reactive", fixed = TRUE)
  expect_match(app_code, '"Special exposure + multivariable" = "special_combined"', fixed = TRUE)
  expect_match(app_code, 'paste0("special_combined::", categories)', fixed = TRUE)
  expect_match(app_code, "special_combined_pair(input$plot_layout)", fixed = TRUE)
  expect_match(app_code, "special_combined_code_names(plot_layout)", fixed = TRUE)
  expect_match(app_code, 'sub("^special_all", "special_combined"', fixed = TRUE)
})

test_that("app exposes broad imports and essential forest controls", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '".rds"', fixed = TRUE)
  expect_match(app_code, '".dta"', fixed = TRUE)
  expect_match(app_code, '".sav"', fixed = TRUE)
  expect_match(app_code, '".sas7bdat"', fixed = TRUE)
  expect_match(app_code, 'gtregression::import_data(', fixed = TRUE)
  expect_match(app_code, 'format = ext', fixed = TRUE)
  expect_match(app_code, '".tsv"', fixed = TRUE)
  expect_match(app_code, '".txt"', fixed = TRUE)
  expect_match(app_code, 'numericInput("forest_ci_width"', fixed = TRUE)
  expect_match(app_code, 'textInput("forest_xlim"', fixed = TRUE)
  expect_match(app_code, 'textInput("forest_ticks"', fixed = TRUE)
  expect_match(app_code, 'downloadButton(paste0(id, "_html"), "HTML")', fixed = TRUE)
})

test_that("survival workspace separates Cox and parametric workflows", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '"surv_mode"', fixed = TRUE)
  expect_match(app_code, '"Cox regression" = "cox"', fixed = TRUE)
  expect_match(app_code, '"Parametric survival" = "survreg"', fixed = TRUE)
  expect_match(app_code, "input.surv_mode == 'cox'", fixed = TRUE)
  expect_match(app_code, "input.surv_mode == 'survreg'", fixed = TRUE)
  expect_match(app_code, 'id = "surv_output_tab"', fixed = TRUE)
  expect_match(app_code, '"Model Result",', fixed = TRUE)
  expect_match(app_code, 'actionButton("run_cox", "Run Cox regression"', fixed = TRUE)
  expect_match(app_code, 'actionButton("run_survreg", "Run parametric survival regression"', fixed = TRUE)
  expect_match(app_code, 'id = "cox_result_tab"', fixed = TRUE)
  expect_match(app_code, 'id = "survreg_result_tab"', fixed = TRUE)
  expect_match(app_code, 'gtx_table_output("cox_exposure_table")', fixed = TRUE)
  expect_match(app_code, 'gtx_table_output("cox_multi_table")', fixed = TRUE)
  expect_match(app_code, 'gtx_table_output("survreg_exposure_table")', fixed = TRUE)
  expect_match(app_code, 'gtx_table_output("survreg_multi_table")', fixed = TRUE)
  expect_match(app_code, 'uiOutput("km_group_input")', fixed = TRUE)
  expect_match(app_code, 'identical(input$surv_mode, "survreg")', fixed = TRUE)
})

test_that("survival workspace provides a competing-risk workflow", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '"Competing risks" = "competing"', fixed = TRUE)
  expect_match(app_code, 'uiOutput("competing_event_codes")', fixed = TRUE)
  expect_match(app_code, '"competing_censor_code", "Censoring code"', fixed = TRUE)
  expect_match(app_code, '"competing_event_of_interest", "Event of interest"', fixed = TRUE)
  expect_match(app_code, 'actionButton("run_competing", "Run Fine-Gray regression"', fixed = TRUE)
  expect_match(app_code, 'actionButton("run_cuminc", "Draw cumulative incidence"', fixed = TRUE)
  expect_match(app_code, '"competing_risk_reg"', fixed = TRUE)
  expect_match(app_code, '"cuminc_plot"', fixed = TRUE)
  expect_match(app_code, '"Show Gray\'s test p-value"', fixed = TRUE)
  expect_match(app_code, 'gtx_table_downloads(output, "competing"', fixed = TRUE)
  expect_match(app_code, 'gtx_plot_downloads(output, "cuminc"', fixed = TRUE)
  expect_match(app_code, 'competing = "Fine-Gray competing-risk model"', fixed = TRUE)
  expect_match(app_code, 'competing = "competing_result"', fixed = TRUE)
  expect_match(app_code, '"# 6a. Competing-risk analysis"', fixed = TRUE)
})

test_that("survival results are retained by model role", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, "rv$cox_exposure <- result", fixed = TRUE)
  expect_match(app_code, "rv$cox_multi <- result", fixed = TRUE)
  expect_match(app_code, "rv$survreg_exposure <- result", fixed = TRUE)
  expect_match(app_code, "rv$survreg_multi <- result", fixed = TRUE)
  expect_match(app_code, '"Cox exposure models" = rv$cox_exposure', fixed = TRUE)
  expect_match(app_code, '"Cox multivariable model" = rv$cox_multi', fixed = TRUE)
  expect_match(app_code, '"Parametric exposure models" = rv$survreg_exposure', fixed = TRUE)
  expect_match(app_code, '"Parametric multivariable model" = rv$survreg_multi', fixed = TRUE)
  expect_match(app_code, '"cox_multi_result" else "cox_exposure_result"', fixed = TRUE)
  expect_match(app_code, '"survreg_multi_result" else "survreg_exposure_result"', fixed = TRUE)
  expect_match(app_code, '"Cox exposure + multivariable" = "cox_combined"', fixed = TRUE)
  expect_match(app_code, '"Parametric exposure + multivariable" = "survreg_combined"', fixed = TRUE)
  expect_match(app_code, "cox_combined = list(rv$cox_exposure, rv$cox_multi)", fixed = TRUE)
  expect_match(app_code, "survreg_combined = list(rv$survreg_exposure, rv$survreg_multi)", fixed = TRUE)
  expect_match(app_code, "forest_data <- forest_df(cox_exposure_result, cox_multi_result", fixed = TRUE)
  expect_match(app_code, "forest_data <- forest_df(survreg_exposure_result, survreg_multi_result", fixed = TRUE)
})

test_that("special-model tables are retained and available to merge", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(
    app_code,
    '"Special exposure models" = rv$special_exposure',
    fixed = TRUE
  )
  expect_match(
    app_code,
    'out[[tables_label]] <- list(result = result$per_outcome[[category]]',
    fixed = TRUE
  )
  expect_match(
    app_code,
    'available_merge_code_names <- reactive({',
    fixed = TRUE
  )
  expect_match(
    app_code,
    'code = paste0(name, "$per_outcome[[", shQuote(category), "]]")',
    fixed = TRUE
  )
  expect_match(
    app_code,
    "merge_objects <- available_merge_code_names()[merge_labels]",
    fixed = TRUE
  )
})

test_that("advanced tools use a guided contextual workflow", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '"advanced_tool"', fixed = TRUE)
  expect_match(app_code, '"Select models" = "selection"', fixed = TRUE)
  expect_match(app_code, '"Compare models" = "comparison"', fixed = TRUE)
  expect_match(app_code, '"Confounder" = "confounder"', fixed = TRUE)
  expect_match(app_code, '"Interaction" = "interaction"', fixed = TRUE)
  expect_match(app_code, '"Convergence" = "convergence"', fixed = TRUE)
  expect_match(app_code, '"Collinearity" = "collinearity"', fixed = TRUE)
  expect_match(app_code, '"compare_approach"', fixed = TRUE)
  expect_match(app_code, '"compare_outcome"', fixed = TRUE)
  expect_match(app_code, '"compare_exposures_"', fixed = TRUE)
  expect_match(app_code, '"compare_adjust_"', fixed = TRUE)
  expect_match(app_code, '"compare_interaction_a_"', fixed = TRUE)
  expect_match(app_code, '"compare_name_"', fixed = TRUE)
  expect_match(app_code, '"compare_add_model"', fixed = TRUE)
  expect_match(app_code, '"adv_collinearity_model"', fixed = TRUE)
  expect_match(app_code, 'uiOutput("advanced_downloads")', fixed = TRUE)
  expect_match(app_code, 'current_approach <- input$adv_approach %||% "logit"', fixed = TRUE)
  expect_match(app_code, 'isTRUE(current_approach %in% c("cox", "survreg"))', fixed = TRUE)
  expect_match(app_code, 'observeEvent(input$advanced_tool', fixed = TRUE)
  expect_match(app_code, 'Fit and compare models', fixed = TRUE)
  expect_match(app_code, 'fit_one <- function(specification)', fixed = TRUE)
  expect_match(app_code, '"multi_reg", data = data', fixed = TRUE)
  expect_match(app_code, '"cox_reg", data = data', fixed = TRUE)
  expect_match(app_code, '"surv_reg", data = data', fixed = TRUE)
  expect_match(app_code, 'gtx_table_downloads(output, "advanced"', fixed = TRUE)
  expect_match(app_code, 'updateTabsetPanel(session, "advanced_output_tab"', fixed = TRUE)
})

test_that("app errors are contained without pausing the Shiny session", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, "run_safely <- function", fixed = TRUE)
  expect_match(app_code, "shiny.silent.error = function(e) NULL", fixed = TRUE)
  expect_match(app_code, 'type = "error"', fixed = TRUE)
  expect_match(app_code, "clear_advanced_error", fixed = TRUE)
  expect_match(app_code, 'run_safely("Model selection"', fixed = TRUE)
  expect_match(app_code, 'run_safely("Confounder assessment"', fixed = TRUE)
  expect_match(app_code, 'run_safely("Interaction assessment"', fixed = TRUE)
  expect_match(app_code, '"Convergence diagnostics",\n      gtx_capture', fixed = TRUE)
  expect_match(app_code, '"Collinearity diagnostics",\n      gtx_capture', fixed = TRUE)
  expect_match(app_code, "This result could not be displayed", fixed = TRUE)
})

test_that("the complete app workflow can be downloaded as an R script", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, 'tabPanel(\n    "Session Code"', fixed = TRUE)
  expect_match(app_code, 'downloadButton("download_session_script", "Download complete R script"', fixed = TRUE)
  expect_match(app_code, 'complete_workflow_code <- reactive({', fixed = TRUE)
  expect_match(app_code, '"# Generated by the gtregression app"', fixed = TRUE)
  expect_match(app_code, '"library(gtregression)"', fixed = TRUE)
  expect_match(app_code, 'output$full_workflow_code <- renderText(complete_workflow_code())', fixed = TRUE)
  expect_match(app_code, 'output$download_session_script <- downloadHandler(', fixed = TRUE)
  expect_match(app_code, 'filename = function() "gtregression-session.R"', fixed = TRUE)
  expect_match(app_code, 'writeLines(complete_workflow_code(), file, useBytes = TRUE)', fixed = TRUE)
})

test_that("app keeps table modification and display-label controls discoverable", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '"Modify table" = "modify"', fixed = TRUE)
  expect_match(app_code, 'actionButton("run_modify_table", "Apply table changes"', fixed = TRUE)
  expect_match(app_code, 'gtregression::modify_table(', fixed = TRUE)
  expect_match(app_code, 'remove_adjustment_note = isTRUE(input$modify_remove_adjustment_note)', fixed = TRUE)
  expect_match(app_code, 'tabPanel(\n            "Modified Table"', fixed = TRUE)
  expect_match(app_code, "Level-label format:", fixed = TRUE)
})

test_that("app defaults regression plots to a linear axis and supports advanced screening roles", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, 'checkboxInput("plot_log_x", "Use a log x-axis", FALSE)', fixed = TRUE)
  expect_match(app_code, 'checkboxInput("reg_model_stats", "Store model statistics", value = TRUE)', fixed = TRUE)
  expect_match(app_code, 'model_stats = input$reg_model_stats', fixed = TRUE)
  expect_match(app_code, 'checkboxGroupInput("conf_exposures", "Exposures of interest"', fixed = TRUE)
  expect_match(app_code, 'covariate_label = "Covariates included in both interaction models"', fixed = TRUE)
  expect_match(app_code, 'exposure = exposures,', fixed = TRUE)
})

test_that("app exposes the complete plot_model_fit API and records it in session code", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, '"Auto (recommended)" = "auto"', fixed = TRUE)
  expect_match(app_code, '"Normal Q-Q" = "qq"', fixed = TRUE)
  expect_match(app_code, '"Scale-location" = "scale_location"', fixed = TRUE)
  expect_match(app_code, '"Observed vs predicted" = "observed_predicted"', fixed = TRUE)
  expect_match(app_code, 'numericInput("fit_base_size", "Base font size"', fixed = TRUE)
  expect_match(app_code, 'base_size = input$fit_base_size', fixed = TRUE)
  expect_match(app_code, '"# 7a. Model-fit diagnostic"', fixed = TRUE)
})

test_that("app keeps data preview compact and descriptive statistics visible", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, "utils::head(rv$data, 10)", fixed = TRUE)
  expect_match(app_code, "First 10 rows only.", fixed = TRUE)
  expect_match(app_code, 'textInput("desc_statistic", "Numeric statistic override"', fixed = TRUE)
  expect_match(app_code, "If x-axis labels overlap, enter fewer ticks", fixed = TRUE)
})

test_that("app provides guided special-model workflows", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, 'tabPanel(\n    "Special Models"', fixed = TRUE)
  expect_match(app_code, '"Ordered outcome" = "ordinal"', fixed = TRUE)
  expect_match(app_code, '"Unordered outcome" = "multinomial"', fixed = TRUE)
  expect_match(app_code, '"Clustered data" = "multilevel"', fixed = TRUE)
  expect_match(app_code, '"Matched case-control" = "matched"', fixed = TRUE)
  expect_match(app_code, 'ordinal = "ordinal_reg"', fixed = TRUE)
  expect_match(app_code, 'multinomial = "multinomial_reg"', fixed = TRUE)
  expect_match(app_code, 'multilevel = "multilevel_reg"', fixed = TRUE)
  expect_match(app_code, 'matched = "conditional_logit_reg"', fixed = TRUE)
  expect_match(app_code, 'actionButton("run_special", "Run selected model"', fixed = TRUE)
  expect_match(app_code, 'gtx_table_downloads(output, "special_exposure"', fixed = TRUE)
  expect_match(app_code, 'gtx_table_downloads(output, "special_multi"', fixed = TRUE)
  expect_match(app_code, '"Exposure Models"', fixed = TRUE)
  expect_match(app_code, '"Multivariable Model"', fixed = TRUE)
})

test_that("special models retain reference, code, and visualisation guidance", {
  skip_on_cran()
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_code <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_match(app_code, 'uiOutput("special_reference_inputs")', fixed = TRUE)
  expect_match(app_code, 'Outcome reference:', fixed = TRUE)
  expect_match(app_code, 'Set its level order in Data Prep', fixed = TRUE)
  expect_match(app_code, 'gtx_code_assign(result_name, special_function()', fixed = TRUE)
  expect_match(app_code, 'special_exposure_result', fixed = TRUE)
  expect_match(app_code, 'special_multi_result', fixed = TRUE)
  expect_match(app_code, 'keep <- !grepl("^(special_(exposure|multi)(?:::|$)|competing$)"', fixed = TRUE)
  expect_match(app_code, 'Model-fit plots are available for standard regression', fixed = TRUE)
  expect_match(app_code, 'rv$special_exposure_code <- code_snapshot', fixed = TRUE)
  expect_match(app_code, 'rv$special_multi_code <- code_snapshot', fixed = TRUE)
  expect_match(app_code, '"special_stats_result"', fixed = TRUE)
  expect_match(app_code, '"special_assumptions_result"', fixed = TRUE)
  expect_match(app_code, '"Result to inspect"', fixed = TRUE)
  expect_match(app_code, 'Model statistics were not stored for this result', fixed = TRUE)
  expect_match(app_code, 'No proportional-odds assumption result is available', fixed = TRUE)
})

test_that("special-model retained results drive diagnostics and combinations", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  sys.source(app_file, envir = app_env)

  shiny::testServer(app_env$server, {
    exposure <- structure(
      list(
        outcome = "recovery",
        model_stats = data.frame(model = "exposure"),
        assumption_tests = data.frame(term = "treatment")
      ),
      class = "ordinal_reg"
    )
    multi <- structure(
      list(
        outcome = "recovery",
        model_stats = data.frame(model = "multivariable"),
        assumption_tests = data.frame(term = "overall")
      ),
      class = "ordinal_reg"
    )
    rv$special_exposure <- exposure
    rv$special_multi <- multi
    session$flushReact()

    expect_equal(
      special_diagnostic_choices(),
      c("Exposure Models" = "exposure", "Multivariable Model" = "multi")
    )
    session$setInputs(special_stats_result = "multi")
    session$flushReact()
    expect_equal(
      special_diagnostic_result("special_stats_result")$model_stats$model,
      "multivariable"
    )
    expect_equal(
      special_combined_choices(),
      c("Special exposure + multivariable" = "special_combined")
    )

    rv$special_multi <- structure(
      list(outcome = "case"),
      class = "conditional_logit_reg"
    )
    session$flushReact()
    expect_length(special_combined_choices(), 0L)

    multinomial_exposure <- structure(
      list(
        outcome = "destination",
        outcome_reference = "Home",
        per_outcome = list(Clinic = list(id = "exposure-clinic"),
                           Hospital = list(id = "exposure-hospital"))
      ),
      class = "multinomial_reg"
    )
    multinomial_multi <- structure(
      list(
        outcome = "destination",
        outcome_reference = "Home",
        per_outcome = list(Hospital = list(id = "multi-hospital"),
                           Other = list(id = "multi-other"))
      ),
      class = "multinomial_reg"
    )
    rv$special_exposure <- multinomial_exposure
    rv$special_multi <- multinomial_multi
    session$flushReact()

    expect_equal(
      special_combined_choices(),
      c(
        "Multinomial exposure + multivariable: Hospital vs Home" =
          "special_combined::Hospital"
      )
    )
    expect_equal(
      vapply(
        special_combined_pair("special_combined::Hospital"),
        `[[`, character(1), "id"
      ),
      c("exposure-hospital", "multi-hospital")
    )
    expect_equal(
      special_combined_code_names("special_combined::Hospital"),
      c(
        "special_exposure_result$per_outcome[['Hospital']]",
        "special_multi_result$per_outcome[['Hospital']]"
      )
    )
  })
})
