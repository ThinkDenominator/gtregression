# Every table produced in an app session stays available to Merge, Modify and
# plots, within a count and memory limit.

history_app_env <- function() {
  data_env <- new.env(parent = emptyenv())
  b <- gtregression::data_birthwt
  b$race <- factor(b$race, levels = 1:3, labels = c("White", "Black", "Other"))
  b$smoke <- factor(b$smoke, levels = 0:1, labels = c("No", "Yes"))
  data_env$study <- b
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  app_env
}

history_quiet <- function(session, ...) {
  suppressMessages(suppressWarnings(utils::capture.output(session$setInputs(...))))
  invisible(NULL)
}

history_regression_inputs <- function(session) {
  session$setInputs(
    reg_outcome = "low", reg_exposures = c("age", "smoke"), reg_adjust = character(0),
    reg_approach = "logit", reg_show_ref = TRUE, reg_model_stats = TRUE, reg_interaction = "",
    reg_stratifier = "race", reg_strata_multi = FALSE
  )
}

test_that("stratified, repeated and risk-difference tables reach Merge and Modify", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  app_env <- history_app_env()
  shiny::testServer(app_env$server, {
    session$flushReact()
    history_quiet(session, environment_dataset_name = "study", load_environment_data = 1)
    session$flushReact()
    history_regression_inputs(session)
    history_quiet(session, run_uni = 1)
    history_quiet(session, run_multi = 1)
    history_quiet(session, run_strat = 1)
    session$setInputs(reg_exposures = c("age", "smoke", "ht"))
    history_quiet(session, run_multi = 2)
    session$setInputs(trial_outcome = "low", trial_exposure = "smoke", trial_adjust = character(0))
    history_quiet(session, run_trial = 1)

    tables <- names(available_merge_tables())
    expect_identical(tables[1:3], c("Crude", "Adjusted", "Stratified"))
    earlier <- grep("^Adjusted \\(", tables, value = TRUE)
    expect_length(earlier, 1)
    expect_identical(unname(available_merge_code_names()[c("Adjusted", earlier)]), c("multi_result", "multi_result_1"))
    # The earlier adjusted table is the one without ht.
    expect_false("ht" %in% available_merge_tables()[[earlier]]$table_body$variable)
    expect_true("Risk difference" %in% names(modify_tables()))
    expect_false("Risk difference" %in% tables)
    expect_true(any(grepl("^Earlier: Adjusted", names(single_plot_choices()))))

    # Stratified tables can be modified and merged.
    session$setInputs(
      modify_source = "Stratified", modify_caption = "By race", modify_variable_labels = "",
      modify_level_labels = "", modify_header_labels = "", modify_bold_labels = TRUE,
      modify_bold_levels = FALSE, modify_italic_labels = FALSE, modify_italic_levels = FALSE,
      modify_remove_n = FALSE, modify_remove_n_obs = FALSE, modify_remove_abbreviations = FALSE,
      modify_remove_adjustment_note = FALSE, modify_caveat = ""
    )
    history_quiet(session, run_modify_table = 1)
    expect_false(is.null(rv$modified))
    session$setInputs(merge_selection = c("Adjusted", earlier, "Stratified"))
    history_quiet(session, run_merge = 1)
    expect_false(is.null(rv$merged))

    # Session Code recreates every kept result and the merge from them.
    script <- complete_workflow_code()
    expect_match(script, "multi_result_1 <- multi_reg(", fixed = TRUE)
    expect_match(script, "multi_result <- multi_reg(", fixed = TRUE)
    expect_match(script, "stratified_result <- stratified_uni_reg(", fixed = TRUE)
    expect_match(script, "rd_result <- risk_difference(", fixed = TRUE)
    replay <- new.env(parent = globalenv())
    replay$study <- get("study", envir = app_env$gtx_launch_context$data_env)
    old <- setwd(tempdir())
    on.exit(setwd(old), add = TRUE)
    expect_no_error(suppressMessages(suppressWarnings(utils::capture.output(
      eval(parse(text = script), envir = replay)
    ))))
    expect_identical(replay$multi_result_1$table_body, available_merge_tables()[[earlier]]$table_body)
    expect_identical(replay$merged_table$table_body, rv$merged$table_body)
  })
})

test_that("a failed run keeps the earlier result available", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_env <- history_app_env()
  shiny::testServer(app_env$server, {
    session$flushReact()
    history_quiet(session, environment_dataset_name = "study", load_environment_data = 1)
    session$flushReact()
    history_regression_inputs(session)
    history_quiet(session, run_multi = 1)
    session$setInputs(reg_approach = "linear")
    history_quiet(session, run_multi = 2)
    expect_null(rv$multi)
    expect_length(available_merge_tables(), 1)
    expect_match(names(available_merge_tables()), "^Adjusted \\(")
  })
})

test_that("the session keeps a limited number of results and compacts old models", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  old <- options(gtregression.app_history_max = 3L, gtregression.app_history_mb = 1e-6)
  on.exit(options(old), add = TRUE)
  app_env <- history_app_env()
  shiny::testServer(app_env$server, {
    session$flushReact()
    history_quiet(session, environment_dataset_name = "study", load_environment_data = 1)
    session$flushReact()
    history_regression_inputs(session)
    for (i in 1:5) {
      session$setInputs(reg_exposures = c("age", "smoke", "ht", "ui", "lwt")[seq_len(i)])
      history_quiet(session, run_multi = i)
    }
    entries <- history_store()
    expect_length(entries, 3)
    # Latest result keeps its models; older ones keep their table only.
    expect_false(isTRUE(entries[[3]]$compact))
    expect_false(is.null(rv$multi$models))
    expect_true(all(vapply(entries[1:2], function(e) isTRUE(e$compact), logical(1))))
    expect_null(entries[[1]]$result$models)
    expect_s3_class(gtregression::merge_tables(entries[[1]]$result, entries[[3]]$result, spanners = c("a", "b")), "gtregression")
    # Table-only results are offered for plots but not for model checks.
    earlier <- grep("^Earlier:", names(single_plot_choices()), value = TRUE)
    expect_length(earlier, 2)
    expect_false(any(grepl("^Earlier:", names(fit_plot_choices()))))
    # Earlier results can be cleared.
    history_quiet(session, history_clear_older = 1)
    expect_length(history_store(), 1)
  })
})

test_that("result size and compaction helpers", {
  b <- gtregression::data_birthwt
  fit <- suppressMessages(gtregression::uni_reg(b, outcome = "low", exposures = c("age", "lwt"), approach = "logit"))
  small <- gtregression:::gtx_compact_result(fit)
  expect_null(small$models)
  expect_true(isTRUE(attr(small, "gtx_compact")))
  expect_lt(gtregression:::gtx_result_bytes(small), gtregression:::gtx_result_bytes(fit))
  expect_error(
    gtregression::modify_table(structure(list(table_display = data.frame()), class = c("gtregression", "mediation_analysis"))),
    "does not support"
  )
})
