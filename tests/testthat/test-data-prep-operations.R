test_that("internal data-preparation helpers apply safe, traceable changes", {
  data <- data.frame(
    age = c(20, 30, 999),
    sex = c("M", "F", "Unknown"),
    stringsAsFactors = FALSE
  )

  renamed <- gt_dp_rename(data, "age", "age_years")
  expect_identical(names(renamed), c("age_years", "sex"))
  expect_error(gt_dp_rename(data, "age", "sex"), "already exists")

  labelled <- gt_dp_set_label(data, "age", "Maternal age")
  expect_identical(attr(labelled$age, "label"), "Maternal age")
  expect_identical(attr(gt_dp_set_label(labelled, "age", "")$age, "label"), NULL)

  recoded <- gt_dp_recode(data, "sex", c("M", "F"), c("Male", "Female"))
  expect_equal(recoded$sex[1:2], c("Male", "Female"))
  expect_equal(attr(recoded, "gt_dp_affected"), 2)

  missing <- gt_dp_define_missing(data, "age", "999")
  expect_true(is.na(missing$age[[3L]]))
  expect_equal(attr(missing, "gt_dp_affected"), 1)

  ordered <- gt_dp_set_type(data, "sex", "ordered", c("F", "M", "Unknown"))
  expect_true(is.ordered(ordered$sex))
  expect_identical(levels(ordered$sex), c("F", "M", "Unknown"))

  filtered <- gt_dp_filter(data, "age", ">=", "25")
  expect_equal(nrow(filtered), 2L)
  expect_equal(unname(attr(filtered, "gt_dp_filter_counts")[["before"]]), 3L)

  transformed <- gt_dp_transform_arithmetic(data, "age", "age_plus_one", "+", 1)
  expect_equal(transformed$age_plus_one[[1L]], 21)

  grouped <- gt_dp_transform_case_when(
    data, "age_group", "age", ">=", "25", "Older", "Younger"
  )
  expect_equal(grouped$age_group[1:2], c("Younger", "Older"))
})

test_that("group creation respects first-match order and numeric ranges", {
  data <- data.frame(age = c(20, 34, 35, 64, 65, 80))

  grouped <- gt_dp_transform_case_when(
    data,
    "age_group",
    variables = c("age", "age"),
    operators = c("<", "<"),
    values = c("35", "65"),
    results = c("Young", "Older"),
    default = "Elder"
  )
  expect_identical(grouped$age_group, c("Young", "Young", "Older", "Older", "Elder", "Elder"))

  ranged <- gt_dp_transform_case_when(
    data,
    "age_band",
    variables = c("age", "age"),
    operators = c("between", ">="),
    values = c("35", "65"),
    values2 = c("64", ""),
    results = c("35 to 64", "65 or older"),
    default = "Under 35"
  )
  expect_identical(ranged$age_band, c("Under 35", "Under 35", "35 to 64", "35 to 64", "65 or older", "65 or older"))
  expect_error(gt_dp_condition(data, "age", "between", "65", "35"), "lower bound")

  birth_age <- data.frame(age = 14:45)
  preview <- gt_dp_group_values(
    birth_age,
    variables = c("age", "age"),
    operators = c("<=", ">="),
    values = c("45", "66"),
    results = c("<=45", ">65"),
    default = "46-65"
  )
  expect_identical(
    attr(preview, "gt_dp_group_counts"),
    c("<=45" = 32L, ">65" = 0L, "46-65" = 0L)
  )
  expect_match(
    gt_dp_empty_group_message(
      birth_age, c("age", "age"), attr(preview, "gt_dp_group_counts")
    ),
    "Observed range of `age`: 14 to 45"
  )
  expect_error(
    gt_dp_transform_case_when(
      birth_age, "age_group", c("age", "age"), c("<=", ">="),
      c("45", "66"), c("<=45", ">65"), "46-65"
    ),
    "empty group\\(s\\): >65, 46-65"
  )
  expect_error(
    gt_dp_transform_case_when(
      data, "duplicate_group", "age", "<", "35", "Age band", "Age band"
    ),
    "unique label"
  )
})

test_that("Data Prep activates original data by default and supports prepared-data choice", {
  skip_if_not_installed("shiny")
  source_data <- shiny::reactiveVal(data.frame(
    age = c(20, 30),
    group = c("A", "B"),
    stringsAsFactors = FALSE
  ))

  suppressWarnings(shiny::testServer(
    mod_data_prep_server,
    args = list(source_data = source_data),
    {
    session$flushReact()
    expect_identical(session$returned$result(), source_data())
    expect_false(session$returned$using_prepared())

    session$setInputs(continue_original = 1)
    session$flushReact()
    expect_identical(session$returned$result(), source_data())
    expect_false(session$returned$using_prepared())

    session$setInputs(use_prepared = 1)
    session$flushReact()
    expect_identical(session$returned$result(), session$returned$working_data())
    expect_true(session$returned$using_prepared())
    }
  ))
})

test_that("Set display label can also polish categorical levels", {
  skip_if_not_installed("shiny")
  source_data <- shiny::reactiveVal(data.frame(
    smoking = factor(c("N", "Y"), levels = c("N", "Y")),
    stringsAsFactors = FALSE
  ))

  suppressWarnings(shiny::testServer(
    mod_data_prep_server,
    args = list(source_data = source_data),
    {
      session$flushReact()
      session$setInputs(variable = "smoking")
      session$flushReact()
      session$setInputs(
        variable_label = "Smoking during pregnancy",
        label_level_to_1 = "No",
        label_level_to_2 = "Yes"
      )
      session$flushReact()
      session$setInputs(apply = 0)
      session$flushReact()
      session$setInputs(apply = 1)
      session$flushReact()

      prepared <- session$returned$working_data()
      expect_identical(attr(prepared$smoking, "label"), "Smoking during pregnancy")
      expect_identical(levels(prepared$smoking), c("No", "Yes"))
      expect_false(session$returned$using_prepared())
    }
  ))
})

test_that("Data Prep module retains its core recovery and reuse controls", {
  ui_text <- paste(deparse(body(mod_data_prep_ui)), collapse = " ")
  server_text <- paste(deparse(body(mod_data_prep_server)), collapse = " ")

  expect_match(ui_text, "Quick starts", fixed = TRUE)
  expect_match(ui_text, "Set display label", fixed = TRUE)
  expect_match(ui_text, "Undo", fixed = TRUE)
  expect_match(ui_text, "Redo", fixed = TRUE)
  expect_match(ui_text, "Reset", fixed = TRUE)
  expect_match(server_text, "confirm_reset", fixed = TRUE)
  expect_match(server_text, "download_prepared", fixed = TRUE)
  expect_match(server_text, "download_code", fixed = TRUE)
  expect_match(server_text, "gt_dp_set_label", fixed = TRUE)
  expect_match(server_text, "Category display labels", fixed = TRUE)
  expect_match(server_text, "label_level_mapping", fixed = TRUE)
  expect_match(server_text, 'reactiveVal("label")', fixed = TRUE)
})
