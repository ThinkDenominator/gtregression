# Tests for the shared GT Analysis shell (Stage D3: D3.0, M0, M1, M2).
# The shell is presentation only: it must keep every authoritative input and
# output, keep input definitions identical to the D2 workbench, and produce
# identical descriptive results and generated code.

gtx_shell_app_env <- function(mode = "app", data_env = new.env(parent = emptyenv()), layout = NULL) {
  old <- options(gtregression.gui_layout = layout, gtregression.addin_layout = NULL)
  on.exit(options(old), add = TRUE)
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  if (!is.null(mode)) app_env$gtx_launch_context <- list(mode = mode, data_env = data_env)
  sys.source(app_file, envir = app_env)
  app_env
}

gtx_shell_html <- function(app_env) {
  rendered <- htmltools::renderTags(app_env$ui)
  paste(as.character(rendered$head), rendered$html)
}

gtx_shell_ids <- function(html) {
  ids <- regmatches(html, gregexpr(" id=\"[^\"]+\"", html))[[1]]
  ids <- sub(" id=\"([^\"]+)\"", "\\1", ids)
  ids[!grepl("^tab-[0-9]", ids)]
}

gtx_shell_input_markup <- function(html, id) {
  select <- regmatches(html, regexpr(paste0("(?s)<select id=\"", id, "\"[^>]*>.*?</select>"), html, perl = TRUE))
  if (length(select)) return(select)
  input <- regmatches(html, regexpr(paste0("<input id=\"", id, "\"[^>]*/?>"), html, perl = TRUE))
  if (length(input)) return(input)
  NA_character_
}

test_that("the Addin uses the shared shell by default and keeps D2 behind a switch", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  shell_env <- gtx_shell_app_env("app")
  d2_env <- gtx_shell_app_env("app", layout = "d2")
  web_env <- gtx_shell_app_env(NULL)
  expect_true(shell_env$gta_shell_enabled)
  expect_false(d2_env$gta_shell_enabled)
  expect_false(web_env$gta_shell_enabled)
  expect_identical(shell_env$gta_host$id, "local")
  expect_identical(web_env$gta_host$id, "web")

  shell_html <- gtx_shell_html(shell_env)
  d2_html <- gtx_shell_html(d2_env)
  web_html <- gtx_shell_html(web_env)
  markers <- c("id=\"gta-config\"", "id=\"gta-data\"", "gta-describe", "gta-regression", "id=\"gta-data-chip\"", "gtaBoot")
  for (marker in markers) {
    expect_match(shell_html, marker, fixed = TRUE)
    expect_false(grepl(marker, d2_html, fixed = TRUE), info = marker)
    expect_false(grepl(marker, web_html, fixed = TRUE), info = marker)
  }
  # Data and Data Prep live in the Data workspace, not in the task navigation.
  expect_false(grepl("data-value=\"Data\"", shell_html, fixed = TRUE))
  expect_false(grepl("data-value=\"data_prep\"", shell_html, fixed = TRUE))
  expect_match(d2_html, "data-value=\"Data\"", fixed = TRUE)
  expect_match(shell_html, "data-value=\"Descriptive\"", fixed = TRUE)
  # M3: the permanent left-hand Regression settings panel is not used.
  expect_false(grepl("class=\"gtx-card gtx-side gtw-reg-settings\"", shell_html, fixed = TRUE))
  expect_match(d2_html, "class=\"gtx-card gtx-side gtw-reg-settings\"", fixed = TRUE)
})

test_that("the shared shell keeps every D2 input and output ID exactly once", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  shell_ids <- gtx_shell_ids(gtx_shell_html(gtx_shell_app_env("app")))
  d2_ids <- gtx_shell_ids(gtx_shell_html(gtx_shell_app_env("app", layout = "d2")))
  # The only D2 outputs not placed by the shell are the explanatory Survival
  # and More models headings (surv_mode_intro, special_intro); their text
  # lives in contextual help instead.
  # The D2 str() printout (data_summary) is replaced by the Structure outputs.
  # The D2 Data Prep status, preview, history and code outputs are replaced
  # by the variable-first Prepare workspace (state line, working-data table,
  # Changes and Code panels); every module input is still placed.
  expect_setequal(
    setdiff(d2_ids, shell_ids),
    c(
      "surv_mode_intro", "special_intro", "data_summary",
      "data_prep-status", "data_prep-preview", "data_prep-analysis_dataset", "data_prep-history", "data_prep-code"
    )
  )
  expect_false(any(duplicated(shell_ids)))
  expect_setequal(
    setdiff(shell_ids, d2_ids),
    c(
      "gta-config", "gta-data-chip", "gta-data", "gta-data-title", "refresh_environment",
      "prep_state", "gta-prep-search", "prep_variables", "prep_preview", "gta-prep-sheet", "gta-prep-sheet-title",
      "gta-prep-hint", "gta-prep-changes", "prep_changes", "gta-prep-code", "prep_code", "gta-prep-export",
      "gta-panel-desc-options", "gta-panel-desc-code",
      "gta-reg-run-label", "gta-reg-preflight",
      "gta-panel-reg-options", "gta-panel-reg-refs", "gta-panel-reg-strata", "gta-panel-reg-code",
      "gta-surv-methods", "gta-surv-method-km", "gta-surv-method-cox", "gta-surv-method-survreg",
      "gta-surv-method-competing", "gta-surv-run-label", "gta-surv-preflight",
      "gta-panel-surv-options", "gta-panel-km-plot", "gta-panel-cuminc-plot", "gta-panel-surv-code",
      "gta-more-methods", "gta-more-method-ordinal", "gta-more-method-multinomial", "gta-more-method-multilevel",
      "gta-more-method-matched", "gta-more-method-mediation", "gta-more-run-label", "gta-more-preflight",
      "gta-med-causal", "gta-med-causal-title",
      "gta-panel-more-options", "gta-panel-more-refs", "gta-panel-med-options", "gta-panel-more-code",
      "structure_overview", "structure_table", "structure_detail", "gta-structure-code",
      # Trials (risk difference) exists in the shared shell only.
      "trial_inputs", "trial_event_input", "trial_reference_input", "trial_event_type", "trial_event_type-label",
      "gta-trial-run-label", "run_trial", "gta-trial-preflight", "trial_preflight",
      "risk_difference_docx", "risk_difference_rtf", "risk_difference_html", "trial_table",
      "gta-panel-trial-options", "trial_conf_level", "trial_conf_level-label", "gta-panel-trial-code", "trial_code"
    )
  )
})

test_that("shared-shell inputs keep the D2 definitions, choices and defaults", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  shell_html <- gtx_shell_html(gtx_shell_app_env("app"))
  d2_html <- gtx_shell_html(gtx_shell_app_env("app", layout = "d2"))
  ids <- c(
    "desc_percent", "desc_overall", "desc_missing", "desc_dich", "desc_statistic",
    "csv_header", "csv_sep", "xlsx_sheet", "dataset_name", "data_file",
    "reg_approach", "reg_show_ref", "reg_model_stats", "reg_strata_multi"
  )
  for (id in ids) {
    markup <- gtx_shell_input_markup(shell_html, id)
    expect_false(is.na(markup), info = id)
    expect_identical(markup, gtx_shell_input_markup(d2_html, id), info = id)
  }
  radio <- function(html) {
    block <- regmatches(html, regexpr("(?s)<div id=\"data_source\".*?</div>\\s*</div>\\s*</div>", html, perl = TRUE))
    regmatches(block, gregexpr("<input type=\"radio\"[^>]*>", block))[[1]]
  }
  expect_identical(radio(shell_html), radio(d2_html))
})

test_that("descriptive results and generated code are identical in D2 and the shared shell", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  run_desc <- function(layout) {
    data_env <- new.env(parent = emptyenv())
    data_env$study <- gtregression::data_birthwt
    app_env <- gtx_shell_app_env("app", data_env, layout = layout)
    captured <- NULL
    shiny::testServer(app_env$server, {
      session$flushReact()
      suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
      session$flushReact()
      session$setInputs(
        desc_by = "low", desc_exposures = c("age", "lwt", "race", "smoke"),
        desc_percent = "row", desc_overall = "last", desc_missing = "no",
        desc_dich = "single_row", desc_statistic = "age = mean"
      )
      suppressMessages(suppressWarnings(session$setInputs(run_desc = 1)))
      body <- if (inherits(rv$desc, "flextable")) rv$desc$body$dataset else rv$desc$table$body$dataset
      captured <<- list(body = body, class = class(rv$desc), code = output$desc_code, data_code = output$data_code)
    })
    captured
  }
  d2 <- run_desc("d2")
  shell <- run_desc(NULL)
  expect_false(is.null(d2$body))
  expect_identical(shell, d2)
})

test_that("Refresh list (D-4) updates only the available data frames", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  data_env <- new.env(parent = emptyenv())
  data_env$study <- gtregression::data_birthwt
  app_env <- gtx_shell_app_env("app", data_env)
  shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    session$setInputs(desc_by = "low", desc_exposures = c("age", "race"))
    suppressMessages(suppressWarnings(session$setInputs(run_desc = 1)))
    before <- list(data = rv$data, name = rv$data_name, desc = rv$desc, code = output$desc_code)
    expect_identical(environment_datasets()$name, "study")

    assign("second", mtcars, envir = data_env)
    session$setInputs(refresh_environment = 1)
    session$flushReact()
    expect_setequal(environment_datasets()$name, c("study", "second"))
    expect_match(paste(unlist(output$environment_dataset_input), collapse = ""), "second", fixed = TRUE)
    # Analytical state is untouched.
    expect_identical(rv$data, before$data)
    expect_identical(rv$data_name, before$name)
    expect_identical(rv$desc, before$desc)
    expect_identical(output$desc_code, before$code)
    expect_identical(input$environment_dataset_name, "study")
  })
})

test_that("regression results and generated code are identical in D2 and the shared shell", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  run_reg <- function(layout) {
    data_env <- new.env(parent = emptyenv())
    study <- gtregression::data_birthwt
    study$race <- factor(study$race, 1:3, c("White", "Black", "Other"))
    study$smoke <- factor(study$smoke, 0:1, c("No", "Yes"))
    data_env$study <- study
    app_env <- gtx_shell_app_env("app", data_env, layout = layout)
    captured <- NULL
    shiny::testServer(app_env$server, {
      session$flushReact()
      suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
      session$flushReact()
      session$setInputs(
        reg_outcome = "low", reg_exposures = c("race", "smoke", "ptl"), reg_adjust = c("age", "lwt"),
        reg_approach = "logit", reg_show_ref = FALSE, reg_model_stats = TRUE, reg_interaction = "",
        reg_stratifier = "ht", reg_strata_multi = TRUE
      )
      session$flushReact()
      # Non-default reference levels (the controls live in the References panel in the shell).
      session$setInputs(reg_reference_1 = "Other", reg_reference_2 = "Yes")
      suppressMessages(suppressWarnings(capture.output({
        session$setInputs(run_uni = 1)
        session$setInputs(run_multi = 1)
        session$setInputs(run_strat = 1)
      })))
      body <- function(x) if (inherits(x, "flextable")) x$body$dataset else x$table$body$dataset
      captured <<- list(
        uni = body(rv$uni), multi = body(rv$multi), strat = class(rv$strat), stats = rv$uni$model_stats,
        code = list(output$uni_code, output$multi_code, output$strat_code),
        session = complete_workflow_code()
      )
    })
    captured
  }
  d2 <- run_reg("d2")
  shell <- run_reg(NULL)
  expect_false(is.null(d2$uni))
  expect_match(d2$code[[2]], "ref = 'Other'", fixed = TRUE)
  expect_identical(shell, d2)
})

test_that("shared-shell wording changes are presentation-only and absent from D2", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  labels <- function(layout) {
    data_env <- new.env(parent = emptyenv())
    data_env$study <- gtregression::data_birthwt
    app_env <- gtx_shell_app_env("app", data_env, layout = layout)
    out <- NULL
    shiny::testServer(app_env$server, {
      session$flushReact()
      suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
      session$flushReact()
      out <<- list(
        desc = paste(unlist(output$desc_inputs), collapse = ""),
        reg = paste(unlist(output$reg_inputs), collapse = "")
      )
    })
    out
  }
  shell <- labels(NULL)
  d2 <- labels("d2")
  expect_match(shell$desc, ">Variables</label>", fixed = TRUE)
  expect_match(d2$desc, ">Variables to summarise</label>", fixed = TRUE)
  expect_match(shell$reg, ">Interaction</label>", fixed = TRUE)
  expect_match(d2$reg, ">Interaction term</label>", fixed = TRUE)
  # The input itself (ID, type, default value) is unchanged.
  for (html in list(shell$reg, d2$reg)) {
    expect_match(html, "<input id=\"reg_interaction\" type=\"text\" class=\"shiny-input-text form-control\" value=\"\"", fixed = TRUE)
  }
})

# ---- Launcher unification: gtregression_app() is the canonical GUI --------

gtx_capture_launch <- function(launcher) {
  captured <- NULL
  testthat::with_mocked_bindings(
    launcher(),
    runApp = function(appDir, launch.browser, ...) {
      captured <<- list(app = appDir, launch.browser = launch.browser)
      invisible(NULL)
    },
    .package = "shiny"
  )
  captured
}

gtx_app_page <- function(app) {
  req <- new.env()
  req$PATH_INFO <- "/"
  req$REQUEST_METHOD <- "GET"
  req$HTTP_HOST <- "localhost"
  req$QUERY_STRING <- ""
  req$SCRIPT_NAME <- ""
  html <- paste(app$httpHandler(req)$content, collapse = "")
  html <- gsub("tab-[0-9]+-[0-9]+", "tab-X", html)
  gsub("data-tabsetid=\"[0-9]+\"", "", html)
}

test_that("gtregression_app() opens the canonical D3 app with the local R session", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  launched <- gtx_capture_launch(gtregression_app)
  expect_s3_class(launched$app, "shiny.appobj")
  # Non-interactive sessions never open a browser; interactive ones do.
  expect_identical(launched$launch.browser, interactive())
  context <- environment(launched$app$serverFuncSource())
  expect_identical(context$gtx_app_mode, "app")
  expect_true(context$gta_shell_enabled)
  expect_identical(context$gtx_data_env, globalenv())
  expect_identical(context$gta_host$id, "local")
  page <- gtx_app_page(launched$app)
  for (marker in c("gta-describe", "gta-regression", "id=\"gta-data\"", "refresh_environment", "environment_dataset_input")) {
    expect_match(page, marker, fixed = TRUE)
  }
})

test_that("the RStudio Addin opens the same application DOM in a 1200 x 800 dialog", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("rstudioapi")
  from_app <- gtx_capture_launch(gtregression_app)
  from_addin <- NULL
  # RStudio registers this hook; emulate it so dialogViewer() builds its dialog.
  old <- options(shinygadgets.showdialog = function(...) invisible(NULL))
  on.exit(options(old), add = TRUE)
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      gtregression_addin(),
      runGadget = function(app, server = NULL, port = NULL, viewer, stopOnCancel = TRUE) {
        from_addin <<- list(app = app, viewer = viewer, stopOnCancel = stopOnCancel)
        invisible(NULL)
      },
      .package = "shiny"
    ),
    isAvailable = function(...) TRUE,
    .package = "rstudioapi"
  )
  expect_s3_class(from_addin$app, "shiny.appobj")
  expect_true(is.function(from_addin$viewer))
  viewer_env <- environment(from_addin$viewer)
  expect_identical(viewer_env$dialogName, "GT Regression")
  expect_equal(c(viewer_env$width, viewer_env$height), c(1200, 800))
  expect_true(from_addin$stopOnCancel)
  addin_context <- environment(from_addin$app$serverFuncSource())
  expect_identical(addin_context$gtx_app_mode, "app")
  expect_identical(addin_context$gtx_data_env, globalenv())
  expect_identical(gtx_app_page(from_addin$app), gtx_app_page(from_app$app))
})

test_that("legacy launch context and layout option names still resolve", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  legacy <- gtx_shell_app_env("addin")
  expect_true(legacy$gta_shell_enabled)
  expect_identical(legacy$gta_host$id, "local")
  old <- options(gtregression.gui_layout = NULL, gtregression.addin_layout = "d2")
  on.exit(options(old), add = TRUE)
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = new.env())
  sys.source(app_file, envir = app_env)
  expect_false(app_env$gta_shell_enabled)
})

# ---- Survival (D3) ---------------------------------------------------------

test_that("Survival: the shell keeps every D2 survival input and output", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  shell_html <- gtx_shell_html(gtx_shell_app_env("app"))
  d2_html <- gtx_shell_html(gtx_shell_app_env("app", layout = "d2"))
  expect_match(shell_html, "gta-survival", fixed = TRUE)
  expect_false(grepl("gta-survival", d2_html, fixed = TRUE))
  ids <- c(
    "surv_mode", "surv_inputs", "surv_show_ref", "surv_multivariable", "surv_preflight",
    "run_cox", "run_survreg", "run_competing", "run_km", "run_cuminc",
    "km_group_input", "km_theme", "km_y_percent", "km_grid", "km_conf", "km_p", "km_risk",
    "km_ylim", "km_xlim", "km_break", "cuminc_by", "cuminc_theme", "cuminc_y_percent",
    "cuminc_grid", "cuminc_conf", "cuminc_p", "cuminc_ylim", "cuminc_xlim", "cuminc_break",
    "surv_output_tab", "cox_result_tab", "survreg_result_tab",
    "cox_exposure_table", "cox_multi_table", "survreg_exposure_table", "survreg_multi_table",
    "competing_table", "km_plot", "cuminc_plot", "logrank_table", "surv_summary_table", "surv_code"
  )
  for (id in ids) {
    expect_equal(lengths(regmatches(shell_html, gregexpr(paste0(" id=\"", id, "\""), shell_html))), 1, info = id)
  }
  static <- c(
    "km_theme", "km_y_percent", "km_grid", "km_conf", "km_p", "km_risk", "km_ylim", "km_xlim", "km_break",
    "cuminc_by", "cuminc_theme", "cuminc_y_percent", "cuminc_grid", "cuminc_conf", "cuminc_p",
    "cuminc_ylim", "cuminc_xlim", "cuminc_break", "surv_show_ref", "surv_multivariable"
  )
  for (id in static) {
    expect_identical(gtx_shell_input_markup(shell_html, id), gtx_shell_input_markup(d2_html, id), info = id)
  }
  radio <- function(html) regmatches(html, gregexpr("<input type=\"radio\" name=\"surv_mode\"[^>]*>", html))[[1]]
  expect_identical(radio(shell_html), radio(d2_html))
  # Result tab values used by the server are unchanged.
  for (value in c("Model Result", "KM Plot", "Cumulative Incidence", "Survival Tables")) {
    expect_match(shell_html, paste0("data-value=\"", value, "\""), fixed = TRUE)
  }
})

test_that("Survival: the four methods are a visible radio group over surv_mode", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  html <- gtx_shell_html(gtx_shell_app_env("app"))
  expect_match(html, "<div class=\"gta-seg\" role=\"radiogroup\" id=\"gta-surv-methods\" aria-label=\"Survival method\">", fixed = TRUE)
  # No method is hidden behind a menu.
  expect_false(grepl("gta-select-menu", html, fixed = TRUE))
  expect_false(grepl("menuitemradio", html, fixed = TRUE))
  method <- function(id) {
    regmatches(html, regexpr(paste0("<button[^>]*id=\"gta-surv-method-", id, "\"[^>]*>[^<]*</button>"), html))
  }
  expected <- list(
    km = c("true", "0", "Kaplan-Meier", "", "Kaplan&ndash;Meier"),
    cox = c("false", "-1", "Cox PH", "cox", "Cox PH"),
    survreg = c("false", "-1", "Parametric survival", "survreg", "Parametric"),
    competing = c("false", "-1", "Competing risks", "competing", "Competing risks")
  )
  for (id in names(expected)) {
    b <- method(id)
    e <- expected[[id]]
    expect_length(b, 1)
    expect_match(b, "role=\"radio\"", fixed = TRUE)
    expect_match(b, paste0("aria-checked=\"", e[1], "\""), fixed = TRUE)
    expect_match(b, paste0("tabindex=\"", e[2], "\""), fixed = TRUE)
    expect_match(b, paste0("aria-label=\"", e[3], "\""), fixed = TRUE)
    expect_match(b, paste0("data-gta-mode=\"", e[4], "\""), fixed = TRUE)
    expect_match(b, paste0(">", e[5], "</button>"), fixed = TRUE)
  }
  # The model methods map onto the unchanged surv_mode choices.
  radio_values <- regmatches(html, gregexpr("name=\"surv_mode\" value=\"[a-z]+\"", html))[[1]]
  expect_identical(sub(".*value=\"([a-z]+)\"", "\\1", radio_values), c("cox", "survreg", "competing"))
  # Keyboard model: arrows, Home and End move and select.
  for (key in c("ArrowRight", "ArrowLeft", "Home", "End")) expect_match(html, paste0("ev.key === '", key, "'"), fixed = TRUE)
})

test_that("Survival: Single multivariable model stays a visible bar control", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  html <- gtx_shell_html(gtx_shell_app_env("app"))
  bar <- regmatches(html, regexpr("<div class=\"gta-bar\" role=\"region\" aria-label=\"Survival analysis definition\">.*?<div class=\"gta-ready\"", html))
  expect_length(bar, 1)
  expect_match(bar, "<div class=\"gta-surv-multi\" data-gta-on=\"false\">", fixed = TRUE)
  expect_match(bar, "id=\"surv_multivariable\"", fixed = TRUE)
  options_panel <- regmatches(html, regexpr("id=\"gta-panel-surv-options\".*?id=\"gta-panel-km-plot\"", html))
  expect_length(options_panel, 1)
  expect_match(options_panel, "id=\"surv_show_ref\"", fixed = TRUE)
  expect_false(grepl("surv_multivariable", options_panel, fixed = TRUE))
})

test_that("Survival: the ready line asks for the next missing input", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  html <- gtx_shell_html(gtx_shell_app_env("app"))
  for (msg in c("Choose time.", "Choose event.", "Choose exposures.", "Choose the censoring code.",
                "Choose the event of interest.", "choose Group by to compare",
                "Single multivariable model uses Exposures only: clear Adjust for, or turn it off.")) {
    expect_match(html, msg, fixed = TRUE, info = msg)
  }
  expect_false(grepl("Choose time and event.", html, fixed = TRUE))
  for (state in c("need-time", "need-event", "need-exposure")) {
    expect_match(html, paste0("class=\"gta-empty-state\" data-gta-for=\"", state, "\""), fixed = TRUE)
  }
})

test_that("Survival: results, plots and generated code are identical in D2 and the shell", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("survival")
  run_surv <- function(layout) {
    data_env <- new.env(parent = emptyenv())
    vet <- gtregression::data_lungcancer
    vet$trt <- factor(vet$trt, 1:2, c("Standard", "Test"))
    data_env$veteran <- vet
    data_env$comp <- gtregression::data_competing_risk
    app_env <- gtx_shell_app_env("app", data_env, layout = layout)
    out <- NULL
    shiny::testServer(app_env$server, {
      table_body <- function(x) if (is.null(x)) NULL else if (inherits(x, "flextable")) x$body$dataset else x$table$body$dataset
      plot_data <- function(p) {
        if (is.null(p)) return(NULL)
        g <- if (inherits(p, "ggplot")) p else if (!is.null(p$plot)) p$plot else p
        tryCatch(ggplot2::layer_data(g), error = function(e) class(p))
      }
      res <- list()
      session$flushReact()
      suppressWarnings(session$setInputs(environment_dataset_name = "veteran", load_environment_data = 1))
      session$flushReact()
      session$setInputs(
        surv_mode = "cox", surv_time = "time", surv_event = "status",
        surv_exposures = c("trt", "celltype"), surv_adjust = c("age", "karno"),
        surv_stratifier = "", surv_show_ref = FALSE, surv_multivariable = FALSE,
        reg_model_stats = TRUE, km_by = "trt", km_theme = "minimal", km_y_percent = FALSE,
        km_grid = TRUE, km_conf = FALSE, km_p = TRUE, km_risk = FALSE, km_ylim = "", km_xlim = "0, 600",
        km_break = 100
      )
      suppressMessages(suppressWarnings(capture.output({
        session$setInputs(run_km = 1)
        session$setInputs(run_cox = 1)
        session$setInputs(surv_multivariable = TRUE, surv_adjust = character(0))
        session$setInputs(run_cox = 2)
        session$setInputs(surv_multivariable = FALSE, surv_adjust = c("age", "karno"), surv_stratifier = "prior")
        session$setInputs(run_cox = 3)
        session$setInputs(surv_mode = "survreg", surv_dist = "lognormal", surv_multivariable = FALSE, surv_stratifier = "")
        session$setInputs(run_survreg = 1)
      })))
      res$km <- plot_data(rv$km)
      res$cox_exposure <- table_body(rv$cox_exposure)
      res$cox_multi <- table_body(rv$cox_multi)
      res$survreg <- table_body(rv$survreg_exposure)
      res$code_survreg <- output$surv_code
      # flextable HTML carries random CSS class ids; compare the content.
      res$logrank <- gsub("cl-[0-9a-f]+", "cl", paste(unlist(output$logrank_table), collapse = ""))
      suppressWarnings(session$setInputs(environment_dataset_name = "comp", load_environment_data = 2))
      session$flushReact()
      session$setInputs(
        surv_mode = "competing", surv_time = "follow_up_months", surv_event = "outcome",
        competing_censor_code = "Censored", competing_event_of_interest = "Relapse",
        surv_exposures = c("treatment", "stage"), surv_adjust = "age", surv_multivariable = FALSE,
        cuminc_by = "treatment", cuminc_theme = "bw", cuminc_conf = FALSE, cuminc_p = TRUE,
        cuminc_y_percent = TRUE, cuminc_grid = FALSE, cuminc_xlim = "", cuminc_ylim = "", cuminc_break = NA
      )
      suppressMessages(suppressWarnings(capture.output({
        session$setInputs(run_competing = 1)
        session$setInputs(run_cuminc = 1)
      })))
      res$competing <- table_body(rv$competing)
      res$cuminc <- plot_data(rv$cuminc)
      res$code_competing <- output$surv_code
      res$session <- complete_workflow_code()
      out <<- res
    })
    out
  }
  d2 <- run_surv("d2")
  shell <- run_surv(NULL)
  expect_false(is.null(d2$cox_exposure))
  expect_false(is.null(d2$cox_multi))
  expect_false(is.null(d2$competing))
  expect_false(is.null(d2$km))
  # cuminc_plot() uses base R's %||% (R >= 4.4); on older R it cannot draw,
  # identically in both layouts, so only require a plot where it can exist.
  if (exists("%||%", envir = baseenv())) expect_false(is.null(d2$cuminc))
  expect_false(is.null(d2$survreg))
  expect_match(d2$code_survreg, "distribution = 'lognormal'", fixed = TRUE)
  expect_identical(shell, d2)
})

test_that("More models: the shell keeps every D2 special-model and mediation input and output", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  shell_html <- gtx_shell_html(gtx_shell_app_env("app"))
  d2_html <- gtx_shell_html(gtx_shell_app_env("app", layout = "d2"))
  expect_match(shell_html, "gta-more", fixed = TRUE)
  expect_false(grepl("gta-more", d2_html, fixed = TRUE))
  ids <- c(
    "special_mode", "special_inputs", "special_reference_inputs", "special_show_ref", "special_model_stats",
    "special_preflight", "run_special", "special_output_tab", "special_exposure_table", "special_multi_table",
    "special_stats_panel", "special_assumptions_panel", "special_code",
    "med_inputs", "med_approach", "med_sims", "med_seed", "run_mediation", "med_table", "med_plot", "med_code"
  )
  for (id in ids) {
    expect_equal(lengths(regmatches(shell_html, gregexpr(paste0(" id=\"", id, "\""), shell_html))), 1, info = id)
  }
  for (id in c("special_show_ref", "special_model_stats", "med_approach", "med_sims", "med_seed")) {
    expect_identical(gtx_shell_input_markup(shell_html, id), gtx_shell_input_markup(d2_html, id), info = id)
  }
  radio <- function(html) regmatches(html, gregexpr("<input type=\"radio\" name=\"special_mode\"[^>]*>", html))[[1]]
  expect_identical(radio(shell_html), radio(d2_html))
  # Tab values used by the server are unchanged.
  for (value in c("Exposure Models", "Multivariable Model", "Model Stats", "Assumption Check")) {
    expect_match(shell_html, paste0("data-value=\"", value, "\""), fixed = TRUE)
  }
  # Mediation is a method of the workspace, not a separate page, in the shell.
  expect_false(grepl("data-value=\"Mediation\"", shell_html, fixed = TRUE))
  expect_match(d2_html, "data-value=\"Mediation\"", fixed = TRUE)
})

test_that("More models: five visible methods and two top-level destinations", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app_env <- gtx_shell_app_env("app")
  html <- gtx_shell_html(app_env)
  expect_match(html, "<div class=\"gta-seg\" role=\"radiogroup\" id=\"gta-more-methods\" aria-label=\"Model method\">", fixed = TRUE)
  expected <- list(
    ordinal = c("true", "0", "ordinal", "Ordinal"),
    multinomial = c("false", "-1", "multinomial", "Multinomial"),
    multilevel = c("false", "-1", "multilevel", "Multilevel"),
    matched = c("false", "-1", "matched", "Conditional logistic"),
    mediation = c("false", "-1", "", "Mediation")
  )
  for (id in names(expected)) {
    b <- regmatches(html, regexpr(paste0("<button[^>]*id=\"gta-more-method-", id, "\"[^>]*>[^<]*</button>"), html))
    e <- expected[[id]]
    expect_length(b, 1)
    expect_match(b, "role=\"radio\"", fixed = TRUE)
    expect_match(b, paste0("aria-checked=\"", e[1], "\""), fixed = TRUE)
    expect_match(b, paste0("tabindex=\"", e[2], "\""), fixed = TRUE)
    expect_match(b, paste0("data-gta-mode=\"", e[3], "\""), fixed = TRUE)
    expect_match(b, paste0(">", e[4], "</button>"), fixed = TRUE)
  }
  # Design methods map onto the unchanged special_mode choices.
  values <- regmatches(html, gregexpr("name=\"special_mode\" value=\"[a-z]+\"", html))[[1]]
  expect_identical(sub(".*value=\"([a-z]+)\"", "\\1", values), c("ordinal", "multinomial", "multilevel", "matched"))
  nav <- app_env$gta_nav_config
  expect_length(nav$menus, 0)
  labels <- vapply(nav$primary, `[[`, character(1), "label")
  values <- vapply(nav$primary, `[[`, character(1), "value")
  expect_identical(labels, c("Describe", "Regression", "Survival", "Trials", "More models", "Model tools"))
  expect_identical(values[5:6], c("Special Models", "Advanced"))
})

test_that("More models: guidance, causal assumptions and multivariable placement", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  html <- gtx_shell_html(gtx_shell_app_env("app"))
  for (msg in c("Choose outcome.", "Choose exposures.", "Choose grouping variable.", "Choose matched set.",
                "Choose exposure.", "Choose mediator.")) {
    expect_match(html, msg, fixed = TRUE, info = msg)
  }
  # Causal assumptions are visible in the bar and explained in a popover.
  expect_match(html, "data-gta-pop=\"gta-med-causal\"", fixed = TRUE)
  pop <- regmatches(html, regexpr("<div id=\"gta-med-causal\".*?</ul>", html))
  expect_length(pop, 1)
  # The D2 caution text is kept verbatim, with the other documented assumptions.
  for (txt in c("Screening aid only: ",
                "a causal interpretation assumes no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding.",
                "The result is a screening aid and depends on no unmeasured exposure-mediator, mediator-outcome, or exposure-outcome confounding assumptions.",
                "the exposure comes before the mediator, and the mediator before the outcome")) {
    expect_match(pop, txt, fixed = TRUE, info = txt)
  }
  # The permanent D2 caution card is not used in the shell.
  expect_false(grepl("gtw-caution", regmatches(html, regexpr("<div class=\"gta-workspace gtw-results gta-more\".*", html)), fixed = TRUE))
  # Show reference categories and Store model statistics are in Options;
  # bootstrap replicates and seed are in the mediation Options panel.
  options_panel <- regmatches(html, regexpr("id=\"gta-panel-more-options\".*?id=\"gta-panel-more-refs\"", html))
  expect_match(options_panel, "id=\"special_show_ref\"", fixed = TRUE)
  expect_match(options_panel, "id=\"special_model_stats\"", fixed = TRUE)
  med_panel <- regmatches(html, regexpr("id=\"gta-panel-med-options\".*?id=\"gta-panel-more-code\"", html))
  expect_match(med_panel, "id=\"med_sims\"", fixed = TRUE)
  expect_match(med_panel, "id=\"med_seed\"", fixed = TRUE)
  refs_panel <- regmatches(html, regexpr("id=\"gta-panel-more-refs\".*?id=\"gta-panel-med-options\"", html))
  expect_match(refs_panel, "id=\"special_reference_inputs\"", fixed = TRUE)
})

test_that("More models: results, generated code and Session Code are identical in D2 and the shell", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  run_more <- function(layout) {
    data_env <- new.env(parent = emptyenv())
    bw <- gtregression::data_birthwt
    bw$race <- factor(bw$race, 1:3, c("White", "Black", "Other"))
    bw$smoke <- factor(bw$smoke, 0:1, c("No", "Yes"))
    bw$bwt_cat <- factor(cut(bw$bwt, c(0, 2500, 3000, Inf), labels = c("Low", "Mid", "High")), ordered = TRUE)
    data_env$bw <- bw
    epi <- gtregression::data_epilepsy
    epi$any_seizure <- as.integer(epi$y > 0)
    data_env$epi <- epi
    inf <- gtregression::data_infertility
    inf$induced <- factor(inf$induced)
    data_env$inf <- inf
    data_env$med <- gtregression::data_diabetes_mediation
    app_env <- gtx_shell_app_env("app", data_env, layout = layout)
    out <- NULL
    shiny::testServer(app_env$server, {
      table_body <- function(x) if (is.null(x)) NULL else if (!is.null(x$table_body)) x$table_body else x$table$body$dataset
      res <- list()
      load <- function(name, n) {
        suppressWarnings(session$setInputs(environment_dataset_name = name, load_environment_data = n))
        session$flushReact()
      }
      quiet <- function(expr) suppressMessages(suppressWarnings(capture.output(expr)))
      session$flushReact()
      load("bw", 1)
      session$setInputs(
        special_mode = "ordinal", special_outcome = "bwt_cat", special_exposures = c("smoke", "race"),
        special_adjust = "age", special_multivariable = FALSE, special_assumption_test = TRUE,
        special_show_ref = FALSE, special_model_stats = TRUE
      )
      session$flushReact()
      refs <- special_reference_candidates()
      session$setInputs(special_reference_2 = "Black")
      quiet(session$setInputs(run_special = 1))
      res$ordinal <- table_body(rv$special_exposure)
      res$ordinal_assumptions <- rv$special_exposure$assumption_tests
      res$ordinal_code <- rv$special_exposure_code
      session$setInputs(special_multivariable = TRUE, special_model_stats = FALSE)
      quiet(session$setInputs(run_special = 2))
      res$ordinal_multi <- table_body(rv$special_multi)
      res$ordinal_multi_code <- rv$special_multi_code
      load("epi", 2)
      session$setInputs(
        special_mode = "multilevel", special_outcome = "any_seizure", special_exposures = c("trt", "age"),
        special_adjust = "base", special_group = "subject", special_cluster_approach = "logit"
      )
      quiet(session$setInputs(run_special = 3))
      res$multilevel <- table_body(rv$special_multi)
      res$multilevel_code <- rv$special_multi_code
      load("inf", 3)
      session$setInputs(
        special_mode = "matched", special_outcome = "case", special_exposures = c("induced", "spontaneous"),
        special_adjust = character(0), special_matched_set = "stratum", special_multivariable = FALSE
      )
      quiet(session$setInputs(run_special = 4))
      res$matched <- table_body(rv$special_exposure)
      res$matched_code <- rv$special_exposure_code
      load("med", 4)
      session$setInputs(
        med_exposure = "obesity", med_mediator = "glucose", med_outcome = "blood_pressure",
        med_covariates = "age", med_approach = "linear", med_sims = 60, med_seed = 9
      )
      quiet(session$setInputs(run_mediation = 1))
      res$mediation <- rv$mediation$table_body
      res$mediation_code <- output$med_code
      res$session <- complete_workflow_code()
      res$refs <- refs
      out <<- res
    })
    out
  }
  d2 <- run_more("d2")
  shell <- run_more(NULL)
  expect_false(is.null(d2$ordinal))
  expect_false(is.null(d2$ordinal_multi))
  expect_false(is.null(d2$multilevel))
  expect_false(is.null(d2$matched))
  expect_false(is.null(d2$mediation))
  expect_match(d2$ordinal_code, "ref = 'Black'", fixed = TRUE)
  expect_match(d2$session, "# 9. Causal mediation", fixed = TRUE)
  expect_identical(shell, d2)
})
