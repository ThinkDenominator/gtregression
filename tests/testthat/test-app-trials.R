# Trials workspace: the GUI for risk_difference(). The workspace offers the
# choices, shows the function's own table and writes code that reproduces it.
# risk_difference() stays the only place where anything is calculated.

tr_data <- function() {
  set.seed(4)
  n <- 240
  arm <- factor(rep(c("Control", "Active"), each = n / 2), levels = c("Control", "Active"))
  age <- round(stats::rnorm(n, 60, 9))
  sex <- factor(sample(c("F", "M"), n, replace = TRUE))
  p <- stats::plogis(-1 + 0.03 * (age - 60) - 0.7 * (arm == "Active"))
  died <- stats::rbinom(n, 1, p)
  out <- data.frame(
    arm = arm, age = age, sex = sex, died = died,
    cured = ifelse(died == 1, "No", "Yes"),
    stage = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  out$age[c(3, 50)] <- NA
  out
}

tr_app_env <- function(data_env = new.env(parent = emptyenv()), layout = NULL) {
  old <- options(gtregression.gui_layout = layout, gtregression.addin_layout = NULL)
  on.exit(options(old), add = TRUE)
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  app_env
}

tr_selected <- function(html, id) {
  select <- regmatches(html, regexpr(paste0("(?s)<select id=\"", id, "\"[^>]*>.*?</select>"), html, perl = TRUE))
  if (!length(select)) return(character(0))
  options <- regmatches(select, gregexpr("<option value=\"[^\"]*\" selected>", select))[[1]]
  sub("<option value=\"([^\"]*)\" selected>", "\\1", options)
}

tr_with_app <- function(body) {
  code <- substitute(body)
  data_env <- new.env(parent = emptyenv())
  data_env$trial <- tr_data()
  app_env <- tr_app_env(data_env)
  suppressWarnings(shiny::testServer(app_env$server, {
    html <- function(id) paste(as.character(output[[id]]$html), collapse = "")
    set <- function(...) {
      session$setInputs(...)
      session$flushReact()
    }
    # Browser behaviour: Event and Reference group take the rendered choice.
    sync_levels <- function() {
      event <- tr_selected(html("trial_event_input"), "trial_event")
      reference <- tr_selected(html("trial_reference_input"), "trial_reference")
      set(
        trial_event = if (length(event)) event else NULL,
        trial_reference = if (length(reference)) reference else NULL
      )
    }
    choose <- function(...) {
      set(...)
      sync_levels()
    }
    runs <- 0L
    run <- function() {
      runs <<- runs + 1L
      set(run_trial = runs)
    }
    session$flushReact()
    set(environment_dataset_name = "trial", load_environment_data = 1L)
    set(trial_event_type = "", trial_conf_level = 0.95)
    eval(code, envir = environment())
  }))
}

# Evaluate generated code against `data`, returning the object it creates.
tr_eval <- function(code, data) {
  env <- new.env(parent = globalenv())
  env$df <- data
  env$trial <- data
  eval(parse(text = c("library(gtregression)", code)), envir = env)
  env$rd_result
}

test_that("Trials is a top-level task in the shared shell only", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  shell <- tr_app_env()
  rendered <- htmltools::renderTags(shell$ui)
  html <- paste(as.character(rendered$head), rendered$html)
  expect_match(html, "data-value=\"Trials\"", fixed = TRUE)
  expect_match(html, "gta-trials", fixed = TRUE)
  values <- vapply(shell$gta_nav_config$primary, function(x) x$value, character(1))
  expect_identical(values, c("Descriptive", "Regression", "Survival", "Trials", "Special Models", "Advanced"))
  for (id in c("trial_inputs", "trial_event_input", "trial_reference_input", "trial_event_type",
               "trial_conf_level", "run_trial", "trial_table", "trial_code", "trial_preflight",
               "risk_difference_docx", "risk_difference_rtf", "risk_difference_html")) {
    expect_match(html, paste0("id=\"", id, "\""), fixed = TRUE, info = id)
  }
  # Risk difference is not offered as a regression approach.
  approach <- regmatches(html, regexpr("(?s)<select id=\"reg_approach\".*?</select>", html, perl = TRUE))
  expect_false(grepl("risk", approach, ignore.case = TRUE))
  # The D2 scaffolding is unchanged.
  d2 <- htmltools::renderTags(tr_app_env(layout = "d2")$ui)$html
  expect_false(grepl("data-value=\"Trials\"", d2, fixed = TRUE))
  expect_false(grepl("id=\"run_trial\"", d2, fixed = TRUE))
})

test_that("Event and Reference group offer the two observed values with the function defaults", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  tr_with_app({
    expect_identical(html("trial_event_input"), "")
    set(trial_outcome = "died", trial_exposure = "arm")
    expect_identical(tr_selected(html("trial_event_input"), "trial_event"), "1")
    expect_match(html("trial_event_input"), "<option value=\"0\">0</option>", fixed = TRUE)
    expect_identical(tr_selected(html("trial_reference_input"), "trial_reference"), "Control")
    expect_match(html("trial_reference_input"), "<option value=\"Active\">Active</option>", fixed = TRUE)

    # A different variable starts from its own default, not the last choice.
    set(trial_event = "0", trial_reference = "Active")
    set(trial_outcome = "cured")
    expect_identical(tr_selected(html("trial_event_input"), "trial_event"), "Yes")

    # A variable that cannot be used says why instead of offering a choice.
    set(trial_outcome = "stage")
    expect_match(html("trial_event_input"), "stage has 3 values; the outcome must be binary.", fixed = TRUE)
    expect_false(grepl("<select", html("trial_event_input"), fixed = TRUE))
    set(trial_exposure = "age")
    expect_match(html("trial_reference_input"), "groups; exactly two are compared.", fixed = TRUE)
  })
})

test_that("the unadjusted table and its code match risk_difference()", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  tr_with_app({
    choose(trial_outcome = "died", trial_exposure = "arm")
    run()
    expected <- risk_difference(tr_data(), outcome = "died", exposure = "arm", reference = "Control", event = 1, format = "flextable")
    expect_s3_class(rv$trial, "risk_difference")
    expect_equal(rv$trial$table_body, expected$table_body)
    expect_identical(rv$trial$table_display, expected$table_display)
    expect_s3_class(rv$trial$table, "flextable")
    expect_true(nzchar(html("trial_table")))

    code <- output$trial_code
    expect_match(code, "rd_result <- risk_difference(", fixed = TRUE)
    expect_match(code, "  reference = \"Control\"", fixed = TRUE)
    expect_match(code, "  event = 1,", fixed = TRUE)
    expect_false(grepl("adjust_for|event_type|conf_level", code))
    reproduced <- tr_eval(code, tr_data())
    expect_equal(reproduced$table_body, rv$trial$table_body)
    expect_identical(reproduced$table_display, rv$trial$table_display)
  })
})

test_that("adjustment, reference, event, NNT and confidence level reach the function and the code", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  tr_with_app({
    choose(trial_outcome = "cured", trial_exposure = "arm")
    set(
      trial_reference = "Active", trial_event = "No", trial_adjust = c("age", "sex"),
      trial_event_type = "harmful", trial_conf_level = 0.9
    )
    run()
    expected <- risk_difference(
      tr_data(), outcome = "cured", exposure = "arm", adjust_for = c("age", "sex"),
      reference = "Active", event = "No", event_type = "harmful", conf_level = 0.9, format = "flextable"
    )
    expect_equal(rv$trial$table_body, expected$table_body)
    expect_identical(rv$trial$table_display, expected$table_display)
    expect_identical(rv$trial$nnt, expected$nnt)

    code <- output$trial_code
    for (piece in c("adjust_for = c(\"age\", \"sex\")", "reference = \"Active\"", "event = \"No\"",
                    "event_type = \"harmful\"", "conf_level = 0.9")) {
      expect_match(code, piece, fixed = TRUE, info = piece)
    }
    reproduced <- tr_eval(code, tr_data())
    expect_equal(reproduced$table_body, rv$trial$table_body)
    expect_identical(reproduced$table_display, rv$trial$table_display)

    # Details: what will be analysed, in words.
    details <- html("trial_preflight")
    expect_match(details, "risk of cured = No; Control minus Active (arm)", fixed = TRUE)
    expect_match(details, "influence-function standard error", fixed = TRUE)
    expect_match(details, "238 of 240 rows", fixed = TRUE)
    expect_match(details, "event treated as harmful", fixed = TRUE)
  })
})

test_that("an analysis that cannot be run reports the function's message and leaves no result", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  tr_with_app({
    choose(trial_outcome = "died", trial_exposure = "arm")
    run()
    expect_false(is.null(rv$trial))
    # The outcome as its own adjustment variable is rejected by the function.
    set(trial_adjust = "died")
    run()
    expect_null(rv$trial)
    # Session Code carries the earlier successful analysis only, never the
    # failed one.
    script <- complete_workflow_code()
    expect_identical(lengths(regmatches(script, gregexpr("risk_difference(", script, fixed = TRUE))), 1L)
    expect_false(grepl("adjust_for = \"died\"", script, fixed = TRUE))
  })
})

test_that("Session Code records the analysis as it was run and the table exports", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tr_with_app({
    choose(trial_outcome = "died", trial_exposure = "arm")
    set(trial_event_type = "harmful")
    run()
    script <- complete_workflow_code()
    expect_match(script, "# 6b. Risk difference (Trials)", fixed = TRUE)
    expect_match(script, "event_type = \"harmful\"", fixed = TRUE)
    # Settings changed after the run are not written into the session script.
    set(trial_event_type = "")
    expect_match(complete_workflow_code(), "event_type = \"harmful\"", fixed = TRUE)
    expect_false(grepl("event_type", output$trial_code, fixed = TRUE))

    file <- output$risk_difference_docx
    expect_true(file.exists(file))
    expect_gt(file.size(file), 1000)
  })
})

test_that("Trials selections survive preparation changes and follow a rename", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  tr_with_app({
    choose(trial_outcome = "died", trial_exposure = "arm")
    set(trial_adjust = c("age", "sex"), trial_reference = "Active", trial_event = "0")
    set(`data_prep-menu_rename` = 1L)
    set(`data_prep-variable` = "arm")
    set(`data_prep-new_name` = "treatment")
    set(`data_prep-apply` = 1L)
    inputs <- html("trial_inputs")
    expect_identical(tr_selected(inputs, "trial_outcome"), "died")
    expect_identical(tr_selected(inputs, "trial_exposure"), "treatment")
    expect_match(inputs, "name=\"trial_adjust\" value=\"age\" checked=\"checked\"", fixed = TRUE)
    set(trial_exposure = "treatment")
    # The chosen reference group follows the renamed variable.
    expect_identical(tr_selected(html("trial_reference_input"), "trial_reference"), "Active")
    # A level chosen for a variable that did not change is kept as well.
    set(`data_prep-menu_label` = 1L)
    set(`data_prep-variable` = "died")
    set(`data_prep-variable_label` = "Died within 30 days")
    set(`data_prep-apply` = 2L)
    expect_identical(tr_selected(html("trial_event_input"), "trial_event"), "0")
  })
})

test_that("candidate values use the keys risk_difference() compares", {
  skip_on_cran()
  env <- tr_app_env()
  values <- env$gtx_trial_values
  expect_identical(values(c(0, 1, NA, 1), role = "outcome")$values, c("0", "1"))
  expect_identical(values(c(TRUE, TRUE), role = "outcome")$values, c("FALSE", "TRUE"))
  expect_identical(values(c(TRUE, TRUE), role = "exposure")$values, "TRUE")
  expect_identical(values(factor(c("b", "a"), levels = c("b", "a", "z")))$values, c("b", "a"))
  expect_identical(values(c("y", "n", NA))$values, c("n", "y"))
  expect_identical(values(c(1.5, 2.5))$values, c("1.5", "2.5"))
  expect_identical(values(Sys.Date())$kind, "unsupported")
  expect_identical(env$gtx_trial_literal("1", "numeric"), "1")
  expect_identical(env$gtx_trial_literal("TRUE", "logical"), "TRUE")
  expect_identical(env$gtx_trial_literal("a \"b\"", "text"), "\"a \\\"b\\\"\"")
  expect_null(env$gtx_trial_literal("", "text"))
})
