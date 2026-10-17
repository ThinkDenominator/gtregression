# Structure engine (Data > Structure): base R, structured values.

ds_example <- function() {
  data.frame(
    id = 1:30,
    age = rep(seq(20, 76, by = 4), length.out = 30),
    smoking = factor(rep(c("Current", "Never", "Former"), 10), levels = c("Never", "Former", "Current", "Unused")),
    grade = factor(rep(c("Mild", "Severe", "Moderate"), 10), levels = c("Mild", "Moderate", "Severe"), ordered = TRUE),
    flag = rep(c(0, 1, 1), 10),
    constant = rep("same", 30),
    sparse = c(rep(NA, 10), rep(1:5, 4)),
    visit = as.Date("2024-01-01") + seq(0, 29 * 20, by = 20),
    ok = rep(c(TRUE, FALSE, TRUE, FALSE, NA), 6),
    note = rep(c("a<b", "c & d"), 15),
    stringsAsFactors = FALSE
  )
}

test_that("types, counts and dataset overview are reported", {
  skip_on_cran()
  s <- gt_data_structure(ds_example())
  v <- s$variables
  expect_identical(v$type, c("Numeric", "Numeric", "Factor", "Ordered", "Numeric", "Text", "Numeric", "Date", "Logical", "Text"))
  expect_identical(s$dataset$rows, 30L)
  expect_identical(s$dataset$variables, 10L)
  expect_identical(s$dataset$missing_cells, sum(is.na(ds_example())))
  expect_identical(v$missing[v$variable == "sparse"], 10L)
  expect_identical(v$distinct[v$variable == "smoking"], 3L)
  expect_identical(s$dataset$types[["Numeric"]], 4L)
})

test_that("worth-a-look features are objective and correctly identified", {
  skip_on_cran()
  s <- gt_data_structure(ds_example(), id_min_rows = 20L)
  expect_identical(s$notes$possible_id, "id")
  expect_identical(s$notes$constant, "constant")
  expect_identical(s$notes$high_missing, "sparse")
  expect_identical(s$notes$zero_one, "flag")
  # Continuous all-unique values are not called identifiers.
  expect_false("age" %in% s$notes$possible_id)
  # Too few rows to call anything an identifier.
  expect_length(gt_data_structure(ds_example())$notes$possible_id, 0)
})

test_that("duplicate rows match base R exactly, including NA, factors and dates", {
  skip_on_cran()
  sets <- list(
    gtregression::data_birthwt,
    rbind(ds_example(), ds_example()[c(1, 5, 5), ]),
    data.frame(a = c(1, NA, NA, 1, 2), b = c("x", NA, NA, "x", "y")),
    data.frame(a = numeric(0)),
    data.frame(a = 1)
  )
  for (d in sets) expect_identical(gt_ds_duplicate_rows(d), sum(duplicated(d)))
  expect_identical(gt_data_structure(gtregression::data_birthwt)$dataset$duplicate_rows, 5L)
})

test_that("factor summaries and details keep the factor's own level order", {
  skip_on_cran()
  d <- ds_example()
  s <- gt_data_structure(d)
  expect_identical(
    gt_ds_text(s$variables$summary[s$variables$variable == "smoking"]),
    "Never 33% - Former 33% - Current 33% - +1 more"
  )
  det <- gt_variable_detail(d$smoking)
  expect_identical(det$levels$level, c("Never", "Former", "Current", "Unused"))
  expect_identical(det$levels$n, c(10L, 10L, 10L, 0L))
  expect_identical(gt_variable_detail(d$grade)$levels$level, c("Mild", "Moderate", "Severe"))
})

test_that("type-specific summaries and details", {
  skip_on_cran()
  d <- ds_example()
  s <- gt_data_structure(d)
  sm <- stats::setNames(gt_ds_text(s$variables$summary), s$variables$variable)
  expect_identical(sm[["flag"]], "0 33% - 1 67%")
  expect_identical(sm[["age"]], "48 +/- 17.6")
  expect_identical(sm[["visit"]], "Jan 2024 - Aug 2025")
  expect_identical(sm[["ok"]], "FALSE 50% - TRUE 50%")
  expect_identical(gt_ds_text(gt_ds_date_range(as.Date(c("2024-01-01", "2024-02-10")), "Date")), "01 Jan 2024 - 10 Feb 2024")
  det <- gt_variable_detail(d$age)
  expect_identical(unname(det$counts), c(30L, 0L, 15L))
  expect_identical(det$stats[["Median"]], gt_ds_num(stats::median(d$age)))
  expect_identical(sum(det$histogram$counts), 30L)
  expect_identical(names(gt_variable_detail(d$visit)$stats), c("Earliest", "Median", "Latest"))
  expect_identical(gt_variable_detail(d$sparse)$missing_pct, "33%")
})

test_that("display text is HTML-safe and locale independent", {
  skip_on_cran()
  s <- gt_data_structure(ds_example())
  html <- gt_ds_html(s$variables$summary[s$variables$variable == "note"])
  expect_identical(html, "a&lt;b 50% &middot; c &amp; d 50%")
  expect_false(any(grepl("[^\\x01-\\x7F]", s$variables$summary, perl = TRUE)))
})

test_that("the engine is fast on a large dataset (about 100,000 x 50)", {
  skip_on_cran()
  set.seed(1)
  n <- 100000
  big <- data.frame(
    replicate(30, round(stats::rnorm(n), 3)),
    replicate(15, factor(sample(letters[1:5], n, TRUE))),
    replicate(5, sample(c(NA, 1:50), n, TRUE))
  )
  elapsed <- system.time(s <- gt_data_structure(big))[["elapsed"]]
  expect_identical(s$dataset$rows, as.integer(n))
  expect_lt(elapsed, 5)
})
