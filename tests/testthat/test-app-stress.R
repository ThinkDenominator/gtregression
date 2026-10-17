# Stress test of the app: every Run button is pressed with ordinary, unusual
# and deliberately wrong selections. The app must never stop, and every error
# the user sees must be written in plain language.

stress_technical <- paste(
  c(
    "subscript out of bounds", "argument is of length zero", "missing value where TRUE/FALSE",
    "non-numeric argument", "unused argument", "\\$ operator is invalid", "attempt to apply non-function",
    "replacement has", "object of type 'closure'", "cannot coerce", "arguments imply differing number",
    "undefined columns selected", "Error in ", "invalid 'type'", "incorrect number of dimensions",
    "no applicable method", "could not find function", "is not a function", "NAs introduced by coercion",
    "number of items to replace", "contrasts can be applied only", "EXPR must be",
    "'arg' should be one of", "could not be completed. Check the selected variables"
  ),
  collapse = "|"
)

stress_data <- function() {
  b <- gtregression::data_birthwt
  b$race <- factor(b$race, levels = 1:3, labels = c("White", "Black", "Other"))
  b$smoke <- factor(b$smoke, levels = 0:1, labels = c("No", "Yes"))
  b$low_f <- factor(b$low, levels = 0:1, labels = c("Normal", "Low"))
  b$ht <- factor(b$ht, levels = 0:1, labels = c("No", "Yes"))
  b$all_same <- 1
  b$mostly_missing <- ifelse(seq_len(nrow(b)) %% 5 == 0, b$age, NA)
  b
}

stress_app_env <- function(notes) {
  data_env <- new.env(parent = emptyenv())
  data_env$study <- stress_data()
  lung <- gtregression::data_lungcancer
  lung$trt <- factor(lung$trt)
  data_env$lung <- lung
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  app_env$showNotification <- function(ui, ..., type = "default") {
    notes$log <- rbind(notes$log, data.frame(type = type, text = paste(as.character(ui), collapse = " ")))
    invisible(NULL)
  }
  app_env
}

stress_press <- function(session, ...) {
  suppressMessages(suppressWarnings(utils::capture.output(session$setInputs(...))))
  invisible(NULL)
}

test_that("the app survives random and wrong selections with plain-language errors", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("flextable")
  notes <- new.env()
  notes$log <- data.frame(type = character(0), text = character(0))
  old <- options(gtregression.app_notify = function(ui, ..., type = "default") {
    notes$log <- rbind(notes$log, data.frame(type = type, text = paste(as.character(ui), collapse = " ")))
    invisible(NULL)
  })
  on.exit(options(old), add = TRUE)
  app_env <- stress_app_env(notes)
  set.seed(20261013)

  shiny::testServer(app_env$server, {
    session$flushReact()
    stress_press(session, environment_dataset_name = "study", load_environment_data = 1)
    session$flushReact()
    vars <- names(stress_data())
    pick <- function(n) sample(vars, n)
    press <- 0

    # Regression: every approach with random roles.
    approaches <- c("logit", "firth", "logbinomial", "poisson", "robpoisson", "negbin", "linear")
    for (i in seq_len(21)) {
      press <- press + 1
      expect_no_error(stress_press(
        session,
        reg_outcome = sample(c("low", "low_f", "bwt", "ptl", "race", "age", "all_same"), 1),
        reg_exposures = pick(sample(1:4, 1)),
        reg_adjust = if (runif(1) < 0.5) pick(1) else character(0),
        reg_approach = approaches[(i - 1) %% length(approaches) + 1],
        reg_show_ref = TRUE, reg_model_stats = TRUE,
        reg_interaction = if (runif(1) < 0.2) "smoke*race" else "",
        reg_stratifier = if (runif(1) < 0.7) sample(vars, 1) else "",
        reg_strata_multi = runif(1) < 0.5
      ))
      expect_no_error(stress_press(session, run_uni = press))
      expect_no_error(stress_press(session, run_multi = press))
      expect_no_error(stress_press(session, run_strat = press))
      expect_no_error(stress_press(session, gtx_role_fix = press))
    }

    # Descriptive tables.
    for (i in seq_len(6)) {
      expect_no_error(stress_press(
        session,
        desc_exposures = pick(sample(1:5, 1)), desc_by = sample(c("", vars), 1),
        desc_percent = sample(c("column", "row"), 1), desc_overall = sample(c("no", "first", "last"), 1), desc_missing = "ifany",
        desc_dich = sample(c("all_levels", "single_row"), 1), desc_statistic = sample(c("", "age = mean", "nonsense"), 1)
      ))
      expect_no_error(stress_press(session, run_desc = i))
    }

    # More models with wrong outcome types and design variables.
    for (i in seq_len(8)) {
      expect_no_error(stress_press(
        session,
        special_mode = sample(c("ordinal", "multinomial", "multilevel", "matched"), 1),
        special_outcome = sample(c("race", "low", "ptl", "ftv"), 1),
        special_exposures = pick(2), special_adjust = character(0),
        special_group = sample(vars, 1), special_matched_set = sample(vars, 1),
        special_multivariable = runif(1) < 0.5
      ))
      expect_no_error(stress_press(session, run_special = i))
    }

    # Trials: any outcome and treatment group.
    for (i in seq_len(6)) {
      expect_no_error(stress_press(
        session, trial_outcome = sample(c("low", "low_f", "bwt", "race"), 1),
        trial_exposure = sample(c("smoke", "race", "ht", "age"), 1),
        trial_adjust = if (runif(1) < 0.5) pick(2) else character(0)
      ))
      expect_no_error(stress_press(session, run_trial = i))
    }

    # Mediation.
    for (i in seq_len(4)) {
      expect_no_error(stress_press(
        session, med_exposure = sample(vars, 1), med_mediator = sample(vars, 1),
        med_outcome = sample(vars, 1), med_covariates = pick(1),
        med_approach = sample(c("linear", "logit"), 1), med_sims = 20, med_seed = 1
      ))
      expect_no_error(stress_press(session, run_mediation = i))
    }

    # Model tools.
    for (i in seq_len(6)) {
      expect_no_error(stress_press(
        session, adv_approach = sample(c("logit", "linear", "poisson"), 1),
        adv_outcome = sample(c("low", "bwt", "race"), 1), adv_exposures = pick(3),
        adv_covariates = pick(1), conf_exposures = pick(1), conf_exposure = sample(vars, 1),
        conf_candidate = sample(vars, 1), select_direction = "both", adv_distribution = "weibull",
        adv_collinearity_model = sample(c("multi", "uni", ""), 1)
      ))
      for (button in c("run_select_models", "run_confounder", "run_interaction", "run_convergence", "run_collinearity")) {
        args <- stats::setNames(list(i), button)
        expect_no_error(do.call(stress_press, c(list(session), args)))
      }
    }

    # Visualise and export, with and without results.
    for (i in seq_len(4)) {
      expect_no_error(stress_press(session, plot_layout = "single", plot_log_x = TRUE, run_plot_reg = i))
      expect_no_error(stress_press(session, forest_content = sample(c("single", "combined", "all"), 1),
                                   forest_source = sample(c("multi", "uni", "nothing"), 1), run_forest = i))
      expect_no_error(stress_press(session, merge_selection = sample(c("Descriptive", "Univariable", "Multivariable"), 2), run_merge = i))
      expect_no_error(stress_press(session, modify_source = "Multivariable", modify_caption = "x", run_modify_table = i))
      expect_no_error(stress_press(session, fit_source = "multi", fit_type = "auto", run_fit_plot = i))
    }

    # Survival on the lung data, including wrong time and event choices.
    stress_press(session, environment_dataset_name = "lung", load_environment_data = 2)
    session$flushReact()
    lung_vars <- names(gtregression::data_lungcancer)
    for (i in seq_len(10)) {
      expect_no_error(stress_press(
        session,
        surv_mode = sample(c("cox", "survreg", "competing"), 1),
        surv_time = sample(c("time", "time", "age", "trt"), 1),
        surv_event = sample(c("status", "status", "celltype", "karno"), 1),
        surv_exposures = sample(lung_vars, 2), surv_adjust = if (runif(1) < 0.5) sample(lung_vars, 1) else character(0),
        surv_stratifier = sample(c("", lung_vars), 1), surv_multivariable = runif(1) < 0.5,
        surv_show_ref = TRUE, surv_dist = "weibull", km_by = sample(c("", lung_vars), 1),
        competing_event_of_interest = "1", competing_censor_code = "0", cuminc_by = "",
        km_conf = TRUE, km_risk = TRUE, km_p = TRUE, km_y_percent = FALSE, km_theme = "classic", km_grid = FALSE,
        km_break = NA, km_xlim = "", km_ylim = "",
        cuminc_conf = TRUE, cuminc_p = TRUE, cuminc_y_percent = FALSE, cuminc_theme = "classic", cuminc_grid = FALSE,
        cuminc_break = NA, cuminc_xlim = "", cuminc_ylim = ""
      ))
      for (button in c("run_cox", "run_survreg", "run_competing", "run_km", "run_cuminc")) {
        args <- stats::setNames(list(i), button)
        expect_no_error(do.call(stress_press, c(list(session), args)))
      }
    }
  })

  errors <- notes$log[notes$log$type == "error", , drop = FALSE]
  technical <- errors[grepl(stress_technical, errors$text), , drop = FALSE]
  expect_true(
    nrow(technical) == 0,
    info = paste(unique(technical$text), collapse = "\n")
  )
})

test_that("model warnings and messages are kept with the result", {
  collect <- gtregression:::gtx_collect
  res <- collect({
    message("Outcome `low`: modelling the event 'Low'.")
    warning("glm.fit: fitted probabilities numerically 0 or 1 occurred")
    list(a = 1)
  })
  notes <- attr(res, "gtx_notes")
  expect_identical(notes$messages, "Outcome `low`: modelling the event 'Low'.")
  expect_match(notes$warnings, "separation")
  expect_null(attr(collect(list(a = 1)), "gtx_notes"))
})

test_that("pre-run checks describe the chosen data in plain language", {
  checks <- gtregression:::gtx_model_checks
  b <- stress_data()
  expect_length(checks(b, outcome = "low", exposures = c("smoke", "age"), approach = "logit"), 0)
  expect_match(checks(b, outcome = "bwt", exposures = "smoke", approach = "logit"), "two-category outcome", all = FALSE)
  expect_match(checks(b, outcome = "low", exposures = "all_same", approach = "logit"), "same value in every analysed row", all = FALSE)
  expect_match(checks(b, outcome = "low", exposures = "mostly_missing", approach = "logit"), "will be left out", all = FALSE)
  b$race_code <- as.numeric(b$race)
  expect_match(checks(b, outcome = "low", exposures = "race_code", approach = "logit"), "stored as numbers", all = FALSE)
  b$rare <- factor(ifelse(seq_len(nrow(b)) %in% which(b$low == 0)[1:5], "rare", "common"))
  expect_match(checks(b, outcome = "low", exposures = "rare", approach = "logit"), "all or no events", all = FALSE)
  expect_match(checks(b[1:30, ], outcome = "low", exposures = c("race", "smoke", "ht", "age"), approach = "logit",
                      multivariable = TRUE), "fewer than 10 per term", all = FALSE)
  expect_false(any(grepl("outcome", checks(b, outcome = "bwt", exposures = "smoke", approach = "linear"))))
})

test_that("import notes and session inputs", {
  notes <- gtregression:::gtx_import_notes
  d <- data.frame(`a;b;c` = c("1;2;3", "4;5;6"), check.names = FALSE)
  expect_match(notes(d, ext = "csv"), "separator", all = FALSE)
  d <- data.frame(Age..yrs. = 1:2, empty = NA)
  out <- notes(d, original_names = c("Age (yrs)", "empty"), ext = "csv")
  expect_match(out, "completely empty", all = FALSE)
  expect_match(out, "'Age \\(yrs\\)' is now 'Age..yrs.'", all = FALSE)

  keep <- gtregression:::gtx_session_inputs(list(
    reg_outcome = "low", run_uni = structure(1L, class = "shinyActionButtonValue"),
    data_file = data.frame(x = 1), `data_prep-op` = "x", reg_exposures = c("a", "b")
  ))
  expect_identical(names(keep), c("reg_outcome", "reg_exposures"))
})
