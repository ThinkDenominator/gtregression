# Reproducibility contract for Data Prep: running the generated R code on the
# same starting data must give exactly the working dataset the app uses
# (values, classes, factor levels and their order, attributes).

dp_start_data <- function() {
  data.frame(
    age = c(9, 25, 40, NA, 18, 65),
    smoking = factor(c("Y", "N", "Y", NA, "N", "Y"), levels = c("N", "Y")),
    status = c("no data", "yes", "yes", NA, "no data", "unknown"),
    score = c(1, 99, 3, 999, 2, 5),
    weight = c(60.5, 72, 80.25, 55, 90, 68),
    height = c(1.6, 1.75, 1.8, 1.55, 1.9, 1.7),
    grade = c("2", "1", "3", "1", "2", "3"),
    stringsAsFactors = FALSE
  )
}

# Run one or more changes through the real module, then return the working
# data, the data analyses receive, and the code rendered for analysis_data.
dp_run <- function(start, actions) {
  out <- NULL
  source_data <- shiny::reactiveVal(start)
  suppressWarnings(shiny::testServer(mod_data_prep_server, args = list(source_data = source_data), {
    session$flushReact()
    for (a in actions) {
      do.call(session$setInputs, a)
      session$flushReact()
    }
    out <<- list(
      working = session$returned$working_data(),
      result = session$returned$result(),
      prepared = session$returned$using_prepared(),
      code = session$returned$code_for("analysis_data"),
      module_code = session$returned$code(),
      history = length(state$history)
    )
  }))
  out
}

dp_replay <- function(start, code) {
  env <- new.env(parent = globalenv())
  env$analysis_data <- start
  eval(parse(text = code), envir = env)
  env$analysis_data
}

expect_reproduced <- function(run, start, n = 1L) {
  expect_identical(run$history, n)
  expect_identical(dp_replay(start, run$code), run$working)
  expect_identical(run$result, run$working)
  expect_true(run$prepared)
}

apply_n <- function(n) list(apply = n)

test_that("label (and category relabelling) is reproduced and keeps the display label", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(
    list(menu_label = 1), list(variable = "age"), list(variable_label = "Age (years)"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(attr(run$working$age, "label"), "Age (years)")

  run <- dp_run(start, list(
    list(menu_label = 1), list(variable = "smoking"),
    list(variable_label = "Smoking status", label_level_to_1 = "No", label_level_to_2 = "Yes"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("No", "Yes"))
  expect_identical(attr(run$working$smoking, "label"), "Smoking status")
})

test_that("rename is reproduced", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(list(menu_rename = 1), list(variable = "score", new_name = "symptom_score"), apply_n(1)))
  expect_reproduced(run, start)
  expect_true("symptom_score" %in% names(run$working))
})

test_that("P1: values, labels and names containing 'data' are never rewritten", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  # status has observed values in order: "no data", "yes", "unknown"
  run <- dp_run(start, list(
    list(menu_recode = 1), list(variable = "status"),
    list(recode_to_1 = "No data recorded", recode_to_2 = "Yes", recode_to_3 = "Unknown data"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_match(run$code, '"no data" = "No data recorded"', fixed = TRUE)
  expect_false(grepl("analysis_data recorded", run$code, fixed = TRUE))
  expect_false(grepl("no analysis_data", run$code, fixed = TRUE))
  run <- dp_run(start, list(
    list(menu_label = 1), list(variable = "status"),
    list(variable_label = "Has data", label_level_to_1 = "no data", label_level_to_2 = "yes", label_level_to_3 = "unknown"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_match(run$code, '"Has data"', fixed = TRUE)
  # A variable called data is addressed as a column, not the data object.
  start_d <- data.frame(data = c(1, 2, 3), x = c(4, 5, 6))
  run <- dp_run(start_d, list(list(menu_rename = 1), list(variable = "data", new_name = "data_value"), apply_n(1)))
  expect_reproduced(run, start_d)
})

test_that("recode (categorical and ordered) is reproduced", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(
    list(menu_recode = 1), list(variable = "grade"),
    list(recode_to_1 = "Moderate", recode_to_2 = "Mild", recode_to_3 = "Severe", recode_ordered = TRUE), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_true(is.ordered(run$working$grade))
})

test_that("type and level order are reproduced", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  for (type in c("factor", "numeric", "text")) {
    run <- dp_run(start, list(list(menu_type = 1), list(variable = "grade", variable_type = type), apply_n(1)))
    expect_reproduced(run, start)
  }
  run <- dp_run(start, list(
    list(menu_type = 1), list(variable = "grade", variable_type = "ordered"),
    list(type_rank_1 = "2", type_rank_2 = "1", type_rank_3 = "3"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$grade), c("1", "2", "3"))
  run <- dp_run(start, list(list(menu_type = 1), list(variable = "age", variable_type = "factor"), apply_n(1)))
  expect_reproduced(run, start)
})

test_that("missing values: codes to NA, and P3 missing-as-category level order", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(
    list(menu_missing = 1), list(variable = "score", missing_action = "to_na"), list(codes = "99, 999"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(sum(is.na(run$working$score)), 2L)
  # smoking is a factor with levels N, Y (first row is Y): its own level
  # order is kept and the missing category is appended last.
  run <- dp_run(start, list(
    list(menu_missing = 1), list(variable = "smoking", missing_action = "show_category"),
    list(missing_label = "Missing"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("N", "Y", "Missing"))
  # A text variable keeps the order in which values appear.
  run <- dp_run(start, list(
    list(menu_missing = 1), list(variable = "status", missing_action = "show_category"),
    list(missing_label = "Not recorded"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$status), c("no data", "yes", "unknown", "Not recorded"))
})

test_that("calculations are reproduced exactly", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(
    list(menu_calculation = 1), list(calculation_mode = "single"),
    list(transform_source = "weight", transform_operator = "*", transform_number = 2.2046226218, transform_new = "weight_lb"), apply_n(1)
  ))
  expect_reproduced(run, start)
  run <- dp_run(start, list(
    list(menu_calculation = 1), list(calculation_mode = "two_variables"),
    list(transform_source = "weight", transform_operator = "-", transform_second = "score", transform_new = "difference"), apply_n(1)
  ))
  expect_reproduced(run, start)
  run <- dp_run(start, list(
    list(menu_calculation = 1), list(calculation_mode = "ratio_power"),
    list(transform_source = "weight", transform_second = "height", transform_number = 2, transform_new = "bmi"), apply_n(1)
  ))
  expect_reproduced(run, start)
})

test_that("groups are reproduced, including missing source values", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  # Two groups with a missing age: the app puts it in the everyone-else group.
  run <- dp_run(start, list(
    list(menu_groups = 1), list(group_variable = "age", group_count = "2"),
    list(rule_op_1 = ">=", rule_value_1 = "18", rule_result_1 = "Adult", rule_default = "Child or unknown", transform_new = "age_group"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(run$working$age_group[4], "Child or unknown")
  run <- dp_run(start, list(
    list(menu_groups = 1), list(group_variable = "age", group_count = "3"),
    list(rule_op_1 = "between", rule_value_1 = "18", rule_value2_1 = "39.5", rule_result_1 = "18-39",
         rule_op_2 = ">=", rule_value_2 = "40", rule_result_2 = "40+", rule_default = "Other", transform_new = "age_band"), apply_n(1)
  ))
  expect_reproduced(run, start)
  run <- dp_run(start, list(
    list(menu_groups = 1), list(group_variable = "status", group_count = "2"),
    list(rule_op_1 = "==", rule_value_1 = "yes", rule_result_1 = "Yes", rule_default = "Not yes", transform_new = "status_yes"), apply_n(1)
  ))
  expect_reproduced(run, start)
})

test_that("P2: numeric filters stay numeric in the generated code", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(
    list(menu_filter = 1), list(variable1 = "age", operator1 = ">=", value1 = "18", filter_second = FALSE), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_false(9 %in% run$working$age)
  expect_match(run$code, "age >= 18", fixed = TRUE)
  run <- dp_run(start, list(
    list(menu_filter = 1), list(variable1 = "smoking", operator1 = "==", value1 = "Y", filter_second = TRUE),
    list(connector = "OR", variable2 = "age", operator2 = "<", value2 = "10"), apply_n(1)
  ))
  expect_reproduced(run, start)
  run <- dp_run(start, list(
    list(menu_filter = 1), list(variable1 = "status", operator1 = "!=", value1 = "no data", filter_second = TRUE),
    list(connector = "AND", variable2 = "weight", operator2 = ">", value2 = "60.5"), apply_n(1)
  ))
  expect_reproduced(run, start)
})

test_that("keep variables is reproduced, including the chosen order", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(list(menu_keep = 1), list(keep_vars = c("weight", "age", "smoking")), apply_n(1)))
  expect_reproduced(run, start)
  expect_identical(names(run$working), c("weight", "age", "smoking"))
})

test_that("a chain of changes is reproduced by the combined code", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  run <- dp_run(start, list(
    list(menu_missing = 1), list(variable = "score", missing_action = "to_na"), list(codes = "99, 999"), apply_n(1),
    list(menu_filter = 1), list(variable1 = "age", operator1 = ">=", value1 = "18", filter_second = FALSE), apply_n(2),
    list(menu_recode = 1), list(variable = "status"), list(recode_to_1 = "Yes", recode_to_2 = "No data"), apply_n(3),
    list(menu_rename = 1), list(variable = "weight", new_name = "weight_kg"), apply_n(4),
    list(menu_keep = 1), list(keep_vars = c("age", "status", "weight_kg", "score")), apply_n(5)
  ))
  expect_reproduced(run, start, 5L)
})

test_that("P4: analyses always receive the current working data (Apply, Undo, Redo, Reset)", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_start_data()
  source_data <- shiny::reactiveVal(start)
  suppressWarnings(shiny::testServer(mod_data_prep_server, args = list(source_data = source_data), {
    session$flushReact()
    expect_identical(session$returned$result(), start)
    expect_false(session$returned$using_prepared())
    session$setInputs(menu_keep = 1); session$flushReact()
    session$setInputs(keep_vars = c("age", "smoking", "status")); session$setInputs(apply = 1); session$flushReact()
    after_keep <- session$returned$working_data()
    expect_identical(session$returned$result(), after_keep)
    expect_true(session$returned$using_prepared())
    session$setInputs(menu_rename = 1); session$flushReact()
    session$setInputs(variable = "status", new_name = "state"); session$setInputs(apply = 2); session$flushReact()
    after_rename <- session$returned$working_data()
    expect_identical(names(session$returned$result()), c("age", "smoking", "state"))
    session$setInputs(undo = 1); session$flushReact()
    expect_identical(session$returned$result(), after_keep)
    session$setInputs(undo = 2); session$flushReact()
    expect_identical(session$returned$result(), start)
    expect_false(session$returned$using_prepared())
    session$setInputs(redo = 1); session$flushReact()
    expect_identical(session$returned$result(), after_keep)
    session$setInputs(redo = 2); session$flushReact()
    expect_identical(session$returned$result(), after_rename)
    expect_identical(dp_replay(start, session$returned$code_for("analysis_data")), after_rename)
    session$setInputs(reset = 1); session$setInputs(confirm_reset = 1); session$flushReact()
    expect_identical(session$returned$result(), start)
    expect_false(session$returned$using_prepared())
    expect_identical(session$returned$code_for("analysis_data"), "")
  }))
})

test_that("the app's analysis data and Session Code follow the working data", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  data_env <- new.env(parent = emptyenv())
  data_env$study <- dp_start_data()
  app_env <- new.env(parent = globalenv())
  app_env$gtx_launch_context <- list(mode = "app", data_env = data_env)
  sys.source(system.file("shiny", "app.R", package = "gtregression"), envir = app_env)
  suppressWarnings(shiny::testServer(app_env$server, {
    session$flushReact()
    suppressWarnings(session$setInputs(environment_dataset_name = "study", load_environment_data = 1))
    session$flushReact()
    expect_identical(analysis_data(), dp_start_data())
    session$setInputs(`data_prep-menu_recode` = 1); session$flushReact()
    session$setInputs(`data_prep-variable` = "status"); session$flushReact()
    session$setInputs(`data_prep-recode_to_1` = "No data recorded", `data_prep-recode_to_2` = "Yes", `data_prep-recode_to_3` = "Unknown")
    session$setInputs(`data_prep-apply` = 1); session$flushReact()
    expect_identical(levels(analysis_data()$status), c("No data recorded", "Yes", "Unknown"))
    prefix <- analysis_code_prefix()
    expect_match(prefix, "# The app was set to use the prepared dataset.", fixed = TRUE)
    expect_match(prefix, '"no data" = "No data recorded"', fixed = TRUE)
    expect_false(grepl("analysis_data recorded", prefix, fixed = TRUE))
    env <- new.env(parent = globalenv())
    env$study <- dp_start_data()
    eval(parse(text = prefix), envir = env)
    expect_identical(env$analysis_data, analysis_data())
  }))
})

# P3b: an existing factor keeps its level order -- and so its reference
# category -- unless the user explicitly asks for a different order.
dp_factor_data <- function() {
  data.frame(
    # Reference level "Never" is neither alphabetically first nor first in the rows.
    smoking = factor(c("Current", "Former", NA, "Never", "Current", "Never"),
      levels = c("Never", "Former", "Current", "Unused")),
    grade = factor(c("Moderate", "Mild", "Severe", NA, "Mild", "Moderate"),
      levels = c("Mild", "Moderate", "Severe"), ordered = TRUE),
    id = 1:6,
    stringsAsFactors = FALSE
  )
}

test_that("P3b: Recode keeps an existing factor's level order and reference", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_factor_data()
  # Values are listed in level order: Never, Former, Current, Unused.
  run <- dp_run(start, list(
    list(menu_recode = 1), list(variable = "smoking"),
    list(recode_to_1 = "Never smoked", recode_to_2 = "Ex-smoker", recode_to_3 = "Smoker", recode_to_4 = "Unused"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("Never smoked", "Ex-smoker", "Smoker", "Unused"))
  expect_identical(levels(run$working$smoking)[1], "Never smoked")
  expect_identical(as.character(run$working$smoking), c("Smoker", "Ex-smoker", NA, "Never smoked", "Smoker", "Never smoked"))
  # Combining categories: the combined label takes the first position of its members.
  run <- dp_run(start, list(
    list(menu_recode = 1), list(variable = "smoking"),
    list(recode_to_1 = "Never", recode_to_2 = "Ever", recode_to_3 = "Ever", recode_to_4 = "Unused"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("Never", "Ever", "Unused"))
  # An ordered factor keeps its order when recoded as ordered.
  run <- dp_run(start, list(
    list(menu_recode = 1), list(variable = "grade"),
    list(recode_to_1 = "Grade 1", recode_to_2 = "Grade 2", recode_to_3 = "Grade 3", recode_ordered = TRUE), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_true(is.ordered(run$working$grade))
  expect_identical(levels(run$working$grade), c("Grade 1", "Grade 2", "Grade 3"))
})

test_that("P3b: Missing as category keeps an existing factor's levels and appends the category", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_factor_data()
  run <- dp_run(start, list(
    list(menu_missing = 1), list(variable = "smoking", missing_action = "show_category"),
    list(missing_label = "Missing"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("Never", "Former", "Current", "Unused", "Missing"))
  expect_identical(as.character(run$working$smoking)[3], "Missing")
  expect_error(gt_dp_show_missing(start, "smoking", "Unused"), "already a recorded value")
})

test_that("P3b: Change type keeps an existing factor's order unless an order is chosen", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_factor_data()
  run <- dp_run(start, list(list(menu_type = 1), list(variable = "smoking", variable_type = "factor"), apply_n(1)))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), levels(start$smoking))
  expect_false(is.ordered(run$working$smoking))
  # Ordered with the default positions: the factor's own order.
  run <- dp_run(start, list(
    list(menu_type = 1), list(variable = "smoking", variable_type = "ordered"),
    list(type_rank_1 = "1", type_rank_2 = "2", type_rank_3 = "3", type_rank_4 = "4"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("Never", "Former", "Current", "Unused"))
  # An explicit order change is honoured.
  run <- dp_run(start, list(
    list(menu_type = 1), list(variable = "smoking", variable_type = "ordered"),
    list(type_rank_1 = "3", type_rank_2 = "2", type_rank_3 = "1", type_rank_4 = "4"), apply_n(1)
  ))
  expect_reproduced(run, start)
  expect_identical(levels(run$working$smoking), c("Current", "Former", "Never", "Unused"))
})

test_that("P3b: the reference category reaching a model is preserved", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  start <- dp_factor_data()
  run <- dp_run(start, list(
    list(menu_recode = 1), list(variable = "smoking"),
    list(recode_to_1 = "Never smoked", recode_to_2 = "Ex-smoker", recode_to_3 = "Smoker", recode_to_4 = "Unused"), apply_n(1),
    list(menu_missing = 1), list(variable = "smoking", missing_action = "show_category"), list(missing_label = "Missing"), apply_n(2)
  ))
  expect_reproduced(run, start, 2L)
  fit <- stats::lm(id ~ smoking, data = droplevels(run$working))
  expect_false("smokingNever smoked" %in% names(stats::coef(fit)))
  expect_true("smokingSmoker" %in% names(stats::coef(fit)))
})
