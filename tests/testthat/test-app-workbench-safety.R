# Permanent safety net for the Stage D0 workbench presentation work.
#
# These tests pin the behaviour of the shared Shiny server for ordinary
# regression so that UI-shell changes cannot silently alter statistical
# results or generated R code:
#   1. For fixed inputs, the app's univariable, multivariable and stratified
#      results must equal the results obtained by running the app's own
#      generated R code outside the app.
#   2. The generated R code for those fixed inputs must match exactly.
#   3. The input/output IDs used by the server must remain present in the UI
#      for both the Addin and web presentations.

gtx_safety_data <- function() {
  data <- gtregression::data_birthwt
  data$race <- factor(data$race, levels = 1:3, labels = c("White", "Black", "Other"))
  data$smoke <- factor(data$smoke, levels = 0:1, labels = c("No", "Yes"))
  data
}

gtx_safety_app_env <- function(mode = "addin", data_env = NULL) {
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  if (!is.null(mode)) {
    app_env$gtx_launch_context <- list(mode = mode, data_env = data_env)
  }
  sys.source(app_file, envir = app_env)
  app_env
}

# Comparable content of a regression result: the displayed table, the
# publication table body, and the fitted coefficients of every model.
gtx_safety_fingerprint <- function(x) {
  coefs <- function(models) {
    if (is.null(models)) return(NULL)
    lapply(models, function(m) {
      if (is.list(m) && !inherits(m, c("glm", "lm", "negbin")) && !is.null(names(m))) {
        return(lapply(m, function(mm) tryCatch(stats::coef(mm), error = function(e) NULL)))
      }
      tryCatch(stats::coef(m), error = function(e) NULL)
    })
  }
  body <- tryCatch(x$table$body$dataset, error = function(e) NULL)
  list(
    class = class(x),
    table_display = x$table_display,
    table_body = body,
    coefficients = coefs(x$models)
  )
}

gtx_safety_eval_code <- function(code, source_data) {
  env <- new.env(parent = globalenv())
  env$study <- source_data
  suppressMessages(suppressWarnings(capture.output(
    eval(parse(text = paste0("library(gtregression)\n", code)), envir = env)
  )))
  env
}

gtx_safety_prefix <- paste(
  "# The app was set to use the original dataset.",
  "analysis_data <- study",
  "analysis_data[['race']] <- stats::relevel(factor(as.character(analysis_data[['race']]), levels = c('White', 'Black', 'Other')), ref = 'Black')",
  "analysis_data[['smoke']] <- stats::relevel(factor(as.character(analysis_data[['smoke']]), levels = c('No', 'Yes')), ref = 'No')",
  sep = "\n"
)

gtx_safety_expected_code <- list(
  uni = paste0(gtx_safety_prefix, "\n\n", paste(
    "uni_result <- uni_reg(",
    "  data = analysis_data,",
    "  outcome = \"low\",",
    "  exposures = c(\"age\", \"race\", \"smoke\"),",
    "  approach = 'logit',",
    "  show_ref = TRUE,",
    "  model_stats = TRUE,",
    "  format = 'flextable'",
    ")",
    sep = "\n"
  )),
  multi = paste0(gtx_safety_prefix, "\n\n", paste(
    "multi_result <- multi_reg(",
    "  data = analysis_data,",
    "  outcome = \"low\",",
    "  exposures = c(\"age\", \"race\", \"smoke\"),",
    "  adjust_for = c(\"lwt\"),",
    "  approach = 'logit',",
    "  show_ref = TRUE,",
    "  model_stats = TRUE,",
    "  format = 'flextable'",
    ")",
    sep = "\n"
  )),
  strat_uni = paste0(gtx_safety_prefix, "\n\n", paste(
    "stratified_result <- stratified_uni_reg(",
    "  data = analysis_data,",
    "  outcome = \"low\",",
    "  exposures = c(\"age\", \"race\", \"smoke\"),",
    "  stratifier = \"ht\",",
    "  approach = 'logit',",
    "  show_ref = TRUE,",
    "  format = 'flextable'",
    ")",
    sep = "\n"
  )),
  strat_multi = paste0(gtx_safety_prefix, "\n\n", paste(
    "stratified_result <- stratified_multi_reg(",
    "  data = analysis_data,",
    "  outcome = \"low\",",
    "  exposures = c(\"age\", \"race\", \"smoke\"),",
    "  adjust_for = c(\"lwt\"),",
    "  stratifier = \"ht\",",
    "  approach = 'logit',",
    "  show_ref = TRUE,",
    "  format = 'flextable'",
    ")",
    sep = "\n"
  ))
)

test_that("regression results and generated code are stable for fixed inputs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")

  source_data <- gtx_safety_data()
  data_env <- new.env(parent = emptyenv())
  data_env$study <- source_data
  app_env <- gtx_safety_app_env("addin", data_env)

  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(
      environment_dataset_name = "study",
      load_environment_data = 1
    ))
    session$flushReact()
    expect_identical(rv$data, source_data)

    session$setInputs(
      reg_outcome = "low",
      reg_exposures = c("age", "race", "smoke"),
      reg_adjust = "lwt",
      reg_approach = "logit",
      reg_show_ref = TRUE,
      reg_model_stats = TRUE,
      reg_interaction = "",
      reg_reference_1 = "Black",
      reg_stratifier = "ht",
      reg_strata_multi = FALSE
    )
    expect_identical(names(reference_candidates()), c("race", "smoke"))

    suppressMessages(suppressWarnings(capture.output({
      session$setInputs(run_uni = 1)
      session$setInputs(run_multi = 1)
      session$setInputs(run_strat = 1)
    })))
    expect_s3_class(rv$uni, "uni_reg")
    expect_s3_class(rv$multi, "multi_reg")
    expect_false(is.null(rv$strat))

    # Generated code is unchanged.
    expect_identical(output$uni_code, gtx_safety_expected_code$uni)
    expect_identical(output$multi_code, gtx_safety_expected_code$multi)
    expect_identical(output$strat_code, gtx_safety_expected_code$strat_uni)

    # Results equal what the generated code reproduces outside the app.
    replay <- gtx_safety_eval_code(
      paste(output$uni_code, output$multi_code, output$strat_code, sep = "\n\n"),
      source_data
    )
    expect_identical(levels(replay$analysis_data$race)[1], "Black")
    expect_equal(gtx_safety_fingerprint(rv$uni), gtx_safety_fingerprint(replay$uni_result))
    expect_equal(gtx_safety_fingerprint(rv$multi), gtx_safety_fingerprint(replay$multi_result))
    expect_equal(gtx_safety_fingerprint(rv$strat), gtx_safety_fingerprint(replay$stratified_result))

    # Stratified multivariable path.
    session$setInputs(reg_strata_multi = TRUE)
    suppressMessages(suppressWarnings(capture.output(session$setInputs(run_strat = 2))))
    expect_identical(output$strat_code, gtx_safety_expected_code$strat_multi)
    replay_multi <- gtx_safety_eval_code(output$strat_code, source_data)
    expect_equal(gtx_safety_fingerprint(rv$strat), gtx_safety_fingerprint(replay_multi$stratified_result))
  })

  # The source object in the user's environment is never modified.
  expect_identical(data_env$study, source_data)
})

test_that("failed regression runs leave no result behind", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  data_env <- new.env(parent = emptyenv())
  data_env$study <- gtx_safety_data()
  app_env <- gtx_safety_app_env("addin", data_env)

  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    # Outcome also listed as an exposure fails role validation.
    session$setInputs(
      reg_outcome = "low", reg_exposures = c("low", "age"), reg_adjust = character(0),
      reg_approach = "logit", reg_show_ref = TRUE, reg_model_stats = TRUE, reg_interaction = ""
    )
    suppressMessages(suppressWarnings(try(session$setInputs(run_uni = 1), silent = TRUE)))
    expect_null(rv$uni)
  })
})

gtx_safety_shared_ids <- c(
  "main_nav", "close_app", "analysis_status", "workflow_guide",
  "dataset_name", "load_dataset", "data_file", "csv_header", "csv_sep", "xlsx_sheet",
  "load_upload", "template_birthwt", "template_lung", "template_diabetes", "template_endometrial",
  "data_preview", "data_summary", "data_code",
  "reg_inputs", "reg_reference_inputs", "reg_approach", "reg_show_ref", "reg_model_stats",
  "reg_preflight", "run_uni", "run_multi", "reg_strata_inputs", "reg_strata_multi", "run_strat",
  "uni_table", "multi_table", "strat_table", "reg_stats", "uni_code", "multi_code", "strat_code",
  "uni_docx", "multi_docx", "strat_docx",
  "full_workflow_code", "download_session_script"
)

gtx_safety_tab_titles <- c(
  "Data", "Data Prep", "Descriptive", "Regression", "Special Models", "Survival",
  "Visualise &amp; Export", "Advanced", "Mediation", "Session Code", "Help"
)

gtx_safety_has_id <- function(html, id) {
  grepl(paste0("id=\"", id, "\""), html, fixed = TRUE)
}

test_that("Addin and web UIs keep the server's input and output IDs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  addin_html <- htmltools::renderTags(
    gtx_safety_app_env("addin", new.env(parent = emptyenv()))$ui
  )$html
  web_html <- htmltools::renderTags(gtx_safety_app_env(NULL)$ui)$html

  for (id in gtx_safety_shared_ids) {
    # In the shared-shell package GUI, Data > Structure (structure_table)
    # replaces the D2 str() printout (data_summary).
    present <- gtx_safety_has_id(addin_html, id) ||
      (identical(id, "data_summary") && gtx_safety_has_id(addin_html, "structure_table"))
    expect_true(present, info = paste("Addin UI is missing", id))
    expect_true(gtx_safety_has_id(web_html, id), info = paste("Web UI is missing", id))
  }
  for (id in c("data_source", "environment_dataset_input", "load_environment_data")) {
    expect_true(gtx_safety_has_id(addin_html, id), info = paste("Addin UI is missing", id))
    expect_false(gtx_safety_has_id(web_html, id))
  }
  for (title in gtx_safety_tab_titles) {
    expect_match(addin_html, paste0(">", title, "<"), fixed = TRUE)
    expect_match(web_html, paste0(">", title, "<"), fixed = TRUE)
  }
})

test_that("dynamic regression controls keep their IDs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  data_env <- new.env(parent = emptyenv())
  data_env$study <- gtx_safety_data()
  app_env <- gtx_safety_app_env("addin", data_env)

  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    session$setInputs(reg_outcome = "low", reg_exposures = c("race", "smoke"))
    reg_html <- paste(unlist(output$reg_inputs), collapse = "")
    for (id in c("reg_outcome", "reg_exposures", "reg_adjust", "reg_interaction")) {
      expect_match(reg_html, paste0("id=\"", id, "\""), fixed = TRUE)
    }
    ref_html <- paste(unlist(output$reg_reference_inputs), collapse = "")
    expect_match(ref_html, "id=\"reg_reference_1\"", fixed = TRUE)
    expect_match(ref_html, "id=\"reg_reference_2\"", fixed = TRUE)
    expect_match(paste(unlist(output$reg_strata_inputs), collapse = ""), "id=\"reg_stratifier\"", fixed = TRUE)
    expect_match(paste(unlist(output$environment_dataset_input), collapse = ""), "id=\"environment_dataset_name\"", fixed = TRUE)
  })
})
