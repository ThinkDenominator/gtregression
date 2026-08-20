test_that("reference controls include only observed categorical predictors", {
  data <- data.frame(
    outcome = factor(c("No", "Yes", "No")),
    group = factor(c("A", "B", "A"), levels = c("A", "B", "Unused")),
    character_group = c("Control", "Treatment", "Control"),
    age = c(20, 30, 40),
    one_level = factor(c("Only", "Only", "Only"))
  )

  candidates <- gtx_reference_candidates(
    data,
    predictors = c("outcome", "group", "character_group", "age", "one_level"),
    outcome = "outcome"
  )

  expect_identical(names(candidates), c("group", "character_group"))
  expect_identical(candidates$group, c("A", "B"))
  expect_identical(candidates$character_group, c("Control", "Treatment"))
})

test_that("selected reference categories are applied and reproducible", {
  data <- data.frame(
    group = factor(c("A", "B", "A"), levels = c("A", "B")),
    age = c(20, 30, 40)
  )
  candidates <- gtx_reference_candidates(data, "group")
  changed <- gtx_relevel_predictors(data, list(group = "B"))
  code <- gtx_reference_code(list(group = "B"), candidates)

  expect_identical(levels(changed$group), c("B", "A"))
  expect_match(code, "stats::relevel", fixed = TRUE)
  expect_match(code, "ref = [\"']B[\"']")
  expect_error(gtx_relevel_predictors(data, list(group = "Missing")), "not observed")
  expect_error(gtx_relevel_predictors(data, list(age = "20")), "categorical predictors")
})

test_that("the app exposes Data Prep before modelling and reference guidance", {
  app_file <- system.file("shiny", "app.R", package = "gtregression")
  app_text <- paste(readLines(app_file, warn = FALSE), collapse = "\n")

  expect_lt(regexpr('"Data Prep"', app_text, fixed = TRUE)[[1]],
            regexpr('"Regression"', app_text, fixed = TRUE)[[1]])
  module_text <- paste(deparse(body(mod_data_prep_server)), collapse = " ")
  expect_match(module_text, "Original data is active for analysis", fixed = TRUE)
  expect_match(module_text, "use the prepared data", ignore.case = TRUE)
  expect_match(app_text, "The selected category is the baseline", fixed = TRUE)
  expect_match(app_text, "gtx_reference_code", fixed = TRUE)
})
