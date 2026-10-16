# Regression tests for the round-2 audit findings in descriptive_table() and
# import_data().

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

desc_data <- function() {
  set.seed(2)
  n <- 41
  d <- data.frame(
    by = rep(c("a", "b"), c(21, 20)),
    age = round(rnorm(n, 50, 8)),
    sex = sample(c("F", "M"), n, TRUE),
    code = sample(c(1, 2, 10), n, TRUE),
    stringsAsFactors = FALSE
  )
  d$allT <- TRUE
  d$one <- 5
  d$dt <- as.Date("2020-01-01") + seq_len(n)
  d$px <- as.POSIXct(d$dt)
  d$age[1:5] <- NA
  d
}

test_that("Date and POSIXct columns are excluded with a message", {
  d <- desc_data()
  expect_message(
    res <- descriptive_table(d, c("age", "dt", "px")),
    "date/time variable\\(s\\) are not summarised and were excluded: dt, px"
  )
  expect_identical(unique(res$table_body$var), "age")
  expect_false(any(grepl("1,5", res$table_display$Overall)))
  expect_error(
    suppressMessages(descriptive_table(d, "dt")),
    "all `exposures` are date/time variables"
  )
  expect_error(
    suppressMessages(descriptive_table(d, c("dt", "px"), by = "by")),
    "date/time"
  )
})

test_that("constant logical and numeric columns are shown as categories", {
  d <- desc_data()
  # odd group size used to crash with "formatC ... unsupported type"
  res <- quiet(descriptive_table(d, c("allT", "one"), by = "by",
                                 show_overall = "last"))
  td <- res$table_display
  expect_identical(trimws(td$Characteristic), c("allT", "TRUE", "one", "5"))
  expect_identical(td$a[2], "21 (100.0%)")
  expect_identical(td$b[2], "20 (100.0%)")
  expect_identical(td$Overall[4], "41 (100.0%)")
  expect_false(any(grepl("1.0 (1.0-1.0)", unlist(td), fixed = TRUE)))
  expect_false(any(grepl("Continuous", res$footnotes)))

  odd <- quiet(descriptive_table(d[1:21, , drop = FALSE], "allT"))
  expect_identical(odd$table_display$Overall[2], "21 (100.0%)")

  d$allF <- FALSE
  f <- quiet(descriptive_table(d, "allF"))
  expect_identical(trimws(f$table_display$Characteristic), c("allF", "FALSE"))

  # an explicit numeric statistic is still honoured for a constant column
  m <- quiet(descriptive_table(d, "one", statistic = c(one = "mean")))
  expect_identical(m$table_display$Overall[1], "5.0 (0.0)")
})

test_that("continuous variables get a (Missing) row under show_missing = ifany", {
  d <- desc_data()
  res <- quiet(descriptive_table(d, "age", by = "by", show_overall = "last"))
  td <- res$table_display
  expect_identical(trimws(td$Characteristic), c("age", "(Missing)"))
  expect_identical(td$is_header, c(TRUE, FALSE))
  expect_identical(td$a[2], "5 (23.8%)")          # 5 / 21
  expect_identical(td$b[2], "0 (0.0%)")
  expect_identical(td$Overall[2], "5 (12.2%)")    # 5 / 41

  x <- d$age[d$by == "a"]
  q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  f <- function(z) formatC(z, digits = 1, format = "f")
  expect_identical(td$a[1], paste0(f(stats::median(x, na.rm = TRUE)), " (",
                                   f(q[1]), "-", f(q[2]), ")"))

  none <- quiet(descriptive_table(d, "age", show_missing = "no"))
  expect_identical(nrow(none$table_display), 1L)
  # no missing values: still one row
  d$age2 <- seq_len(nrow(d))
  complete <- quiet(descriptive_table(d, "age2"))
  expect_identical(nrow(complete$table_display), 1L)
  # every statistic mode
  # (`statistic` held in a variable is accepted as well)
  for (st in c("mean", "median", "mode", "count")) {
    r <- quiet(descriptive_table(d, "age", statistic = st))
    expect_identical(trimws(r$table_display$Characteristic),
                     c("age", "(Missing)"), info = st)
    expect_identical(r$table_display$Overall[2], "5 (12.2%)", info = st)
  }
  g <- quiet(descriptive_table(d, "age", by = "by", format = "gt"))
  expect_identical(g$table_display, quiet(
    descriptive_table(d, "age", by = "by")
  )$table_display)
})

test_that("forced-categorical numerics are sorted by value", {
  d <- desc_data()
  res <- quiet(descriptive_table(d, "code",
                                 statistic = c(code = "categorical")))
  expect_identical(trimws(res$table_display$Characteristic),
                   c("code", "1", "2", "10"))
  expect_identical(
    res$table_display$Overall[-1],
    paste0(as.integer(table(d$code)), " (",
           formatC(100 * as.integer(table(d$code)) / 41, digits = 1,
                   format = "f"), "%)")
  )
})

test_that("a non-existent `value` level is an error", {
  d <- desc_data()
  expect_error(
    suppressMessages(descriptive_table(
      d, "sex", show_dichotomous = "single_row", value = list(sex = "Female")
    )),
    "`value` for `sex` \\('Female'\\) is not one of its observed levels: F, M"
  )
  ok <- quiet(descriptive_table(d, "sex", show_dichotomous = "single_row",
                                value = list(sex = "F")))
  expect_identical(ok$table_display$Overall,
                   paste0(sum(d$sex == "F"), " (",
                          formatC(100 * mean(d$sex == "F"), digits = 1,
                                  format = "f"), "%)"))
  ok2 <- quiet(descriptive_table(d, "sex", show_dichotomous = "single_row",
                                 value = list(sex ~ "M")))
  expect_identical(ok2$table_body$level, "M")
})

test_that("a `by` factor with an explicit NA level does not crash", {
  d <- desc_data()
  d$byna <- addNA(factor(ifelse(d$age > 50, "hi", "lo")))
  res <- quiet(descriptive_table(d, "sex", by = "byna"))
  expect_identical(names(res$header_labels), c("hi", "lo"))
})

test_that("import_data reads a Latin-1 file completely", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(c(charToRaw("name,x\nJos"), as.raw(0xe9), charToRaw(",1\nZo"),
             as.raw(0xeb), charToRaw(",2\n")), con)
  close(con)

  expect_message(
    d <- import_data(path),
    "not valid UTF-8; it was read as Latin-1"
  )
  expect_identical(nrow(d), 2L)
  expect_identical(d$x, 1:2)
  # UTF-8 bytes of "Jos\u00e9" and "Zo\u00eb"
  expect_identical(charToRaw(d$name[1]),
                   as.raw(c(0x4a, 0x6f, 0x73, 0xc3, 0xa9)))
  expect_identical(charToRaw(d$name[2]), as.raw(c(0x5a, 0x6f, 0xc3, 0xab)))
  expect_identical(Encoding(d$name), c("UTF-8", "UTF-8"))

  # same content as an explicit latin1 read of the raw bytes
  ref <- utils::read.csv(path, encoding = "latin1", stringsAsFactors = FALSE)
  expect_identical(nrow(ref), nrow(d))
  expect_false(.import_is_valid_utf8(path))
})

test_that("import_data leaves valid UTF-8 and ASCII files alone", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, "wb")
  writeBin(c(charToRaw("name,x\nJos"), as.raw(c(0xc3, 0xa9)),
             charToRaw(",1\nAnn,2\n")), con)
  close(con)
  expect_true(.import_is_valid_utf8(path))
  # re-encoding UTF-8 input needs a UTF-8 session locale
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    expect_no_message(d <- import_data(path))
    expect_identical(nrow(d), 2L)
  }

  ascii <- tempfile(fileext = ".csv")
  on.exit(unlink(ascii), add = TRUE)
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), ascii,
                   row.names = FALSE)
  expect_no_message(a <- import_data(ascii))
  expect_identical(a, utils::read.csv(ascii, stringsAsFactors = FALSE))
})
