test_that("import_data reads delimited files with format-aware defaults", {
  data <- data.frame(outcome = c(0, 1), age = c(45, 61))
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  utils::write.table(data, csv, sep = ",", row.names = FALSE)
  utils::write.table(data, tsv, sep = "\t", row.names = FALSE)

  expect_equal(import_data(csv), data)
  expect_equal(import_data(tsv), data)

  semicolon <- tempfile(fileext = ".txt")
  utils::write.table(data, semicolon, sep = ";", row.names = FALSE)
  expect_equal(import_data(semicolon, sep = ";"), data)
})

test_that("import_data accepts an explicit format for extensionless paths", {
  path <- tempfile()
  utils::write.csv(data.frame(x = 1:2), path, row.names = FALSE)

  expect_equal(import_data(path, format = "csv")$x, 1:2)
  expect_equal(import_data(path, format = ".CSV")$x, 1:2)
})

test_that("import_data validates RDS contents and file paths", {
  data_path <- tempfile(fileext = ".rds")
  object_path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1:3), data_path)
  saveRDS(letters, object_path)

  expect_equal(import_data(data_path), data.frame(x = 1:3))
  expect_error(import_data(object_path), "must be a data frame")
  expect_error(import_data(tempfile()), "does not exist")
  expect_error(import_data(tempdir()), "not a directory")
  expect_error(import_data(data_path, format = "json"), "Unsupported data format")
})

test_that("import_data handles column-name repair explicitly", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("x,x", "1,2"), path)

  expect_error(import_data(path), "non-empty and unique")
  repaired <- import_data(path, name_repair = "unique")
  expect_equal(names(repaired), c("x", "x.1"))
  minimal <- import_data(path, name_repair = "minimal")
  expect_equal(names(minimal), c("x", "x"))
})

test_that("import_data preserves haven labelled columns", {
  skip_if_not_installed("haven")
  path <- tempfile(fileext = ".dta")
  labelled <- haven::labelled(c(0, 1), labels = c(No = 0, Yes = 1))
  attr(labelled, "label") <- "Outcome status"
  haven::write_dta(data.frame(outcome = labelled), path)

  imported <- import_data(path)
  expect_s3_class(imported$outcome, "haven_labelled")
  expect_equal(attr(imported$outcome, "label"), "Outcome status")
  expect_equal(attr(imported$outcome, "labels"), c(No = 0, Yes = 1))
})

test_that("import_data reads a selected Excel sheet", {
  skip_if_not_installed("readxl")
  path <- readxl::readxl_example("datasets.xlsx")

  imported <- import_data(path, sheet = "mtcars")
  expect_s3_class(imported, "data.frame")
  expect_equal(nrow(imported), 32L)
  expect_true(all(c("mpg", "cyl", "disp") %in% names(imported)))
})

test_that("import_data validates common arguments", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1), path, row.names = FALSE)

  expect_error(import_data(character()), "one non-empty file path")
  expect_error(import_data(path, header = NA), "TRUE or FALSE")
  expect_error(import_data(path, na = NA_character_), "without missing")
  expect_error(import_data(path, encoding = ""), "non-empty")
  expect_error(import_data(path, format = character()), "must be NULL")
})

test_that("import_data preserves apostrophes in delimited fields", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("id,name", "1,O'Brien", "2,D'Costa"), path)

  imported <- import_data(path)

  expect_equal(nrow(imported), 2L)
  expect_equal(imported$name, c("O'Brien", "D'Costa"))
})
