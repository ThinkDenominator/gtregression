# Bare-name resolution rule (audit B1 / R4):
# a bare symbol that is a column name resolves to the column, unless a calling
# FUNCTION forwards the name of another column through an argument or local
# variable with that name.

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

bare_data <- function() {
  set.seed(1)
  n <- 400
  data.frame(
    rank = rnorm(n), sample = rnorm(n), df = rnorm(n),
    class = factor(sample(c("a", "b"), n, TRUE)),
    by = factor(sample(c("g1", "g2"), n, TRUE)),
    time = rexp(n), time2 = rexp(n),
    status = rbinom(n, 1, 0.6), o = rbinom(n, 1, 0.4)
  )
}

with_globals <- function(values, code) {
  env <- globalenv()
  had <- vapply(names(values), exists, logical(1), envir = env,
                inherits = FALSE)
  old <- lapply(names(values)[had], get, envir = env)
  names(old) <- names(values)[had]
  for (nm in names(values)) assign(nm, values[[nm]], envir = env)
  on.exit({
    rm(list = names(values)[!had], envir = env)
    for (nm in names(old)) assign(nm, old[[nm]], envir = env)
  }, add = TRUE)
  force(code)
}

test_that("columns named like base functions resolve to the columns", {
  D <- bare_data()
  res <- quiet(uni_reg(D, o, c(rank, class, by, time, df, sample)))
  expect_identical(unique(res$table_body$exposure),
                   c("rank", "class", "by", "time", "df", "sample"))
  direct <- stats::glm(o ~ rank, stats::binomial(), D)
  expect_equal(res$table_body$estimate[1],
               unname(exp(stats::coef(direct)["rank"])), tolerance = 1e-8)

  dt <- quiet(descriptive_table(D, c(rank, df, sample), by = by))
  expect_identical(names(dt$header_labels), c("g1", "g2"))
  cx <- quiet(select_models(D, exposures = c(rank, df), approach = cox,
                            time = time, event = status, format = "tibble"))
  expect_match(attr(cx$all_models[[1]], "formula_str"), "time", fixed = TRUE)
})

test_that("direct call with a masking global object: the column wins", {
  D <- bare_data()
  res <- with_globals(
    list(rank = "sample", class = "by", by = "class", time = "time2",
         df = "rank"),
    list(
      uni = quiet(uni_reg(D, o, rank)),
      uni_many = quiet(uni_reg(D, o, c(rank, class, by, time, df, sample))),
      desc = quiet(descriptive_table(D, rank, by = by)),
      multi = quiet(multi_reg(D, o, c(rank, class))),
      strat = quiet(stratified_uni_reg(D, o, rank, by))
    )
  )
  expect_identical(unique(res$uni$table_body$exposure), "rank")
  expect_identical(unique(res$uni_many$table_body$exposure),
                   c("rank", "class", "by", "time", "df", "sample"))
  expect_identical(names(res$desc$header_labels), c("g1", "g2"))
  expect_identical(unique(res$multi$table_body$exposure), c("rank", "class"))
  expect_identical(res$strat$by, "by")
})

test_that("a masking variable in a non-function scope does not redirect", {
  D <- bare_data()
  # test_that() and local() bodies are evaluated with eval(), they are not
  # function frames: the column still wins
  rank <- "sample"
  res <- quiet(uni_reg(D, o, rank))
  expect_identical(unique(res$table_body$exposure), "rank")
  res_local <- local({
    class <- "by"
    quiet(uni_reg(D, o, class))
  })
  expect_identical(unique(res_local$table_body$exposure), "class")
})

test_that("a wrapper with a same-named parameter passes its value through", {
  D <- bare_data()
  wrap <- function(dat, by) quiet(descriptive_table(dat, "rank", by = by))
  expect_identical(names(wrap(D, "class")$header_labels), c("a", "b"))
  expect_identical(
    unname(wrap(D, "class")$header_labels),
    paste0(c("a", "b"), ", N=", as.integer(table(D$class)))
  )
  # the wrapper asks for the column that has the parameter's own name
  expect_identical(names(wrap(D, "by")$header_labels), c("g1", "g2"))

  wrap_uni <- function(dat, rank) quiet(uni_reg(dat, "o", rank))
  expect_identical(unique(wrap_uni(D, "sample")$table_body$exposure), "sample")
  expect_identical(unique(wrap_uni(D, "rank")$table_body$exposure), "rank")
  expect_identical(unique(wrap_uni(D, c("sample", "df"))$table_body$exposure),
                   c("sample", "df"))

  # a wrapper called while a global object masks the name
  out <- with_globals(list(by = "rank"), wrap(D, "class"))
  expect_identical(names(out$header_labels), c("a", "b"))

  # anonymous functions in lapply()
  each <- lapply(c("sample", "df", "rank"), function(rank) {
    unique(quiet(uni_reg(D, o, rank))$table_body$exposure)
  })
  expect_identical(unlist(each), c("sample", "df", "rank"))

  # nested wrappers
  outer <- function(dat, by) wrap(dat, by)
  expect_identical(names(outer(D, "class")$header_labels), c("a", "b"))

  # stratifier and survival time through wrappers
  wrap_strat <- function(dat, by) quiet(stratified_uni_reg(dat, "o", "rank", by))
  expect_identical(wrap_strat(D, "class")$by, "class")
  wrap_time <- function(dat, time) {
    quiet(select_models(dat, exposures = c("rank", "df"), approach = "cox",
                        time = time, event = "status", format = "tibble"))
  }
  expect_match(attr(wrap_time(D, "time2")$all_models[[1]], "formula_str"),
               "time2", fixed = TRUE)
})

test_that("a wrapper value that is not another column leaves the column", {
  D <- bare_data()
  # non-character value: column `rank` is used
  wrap_num <- function(dat, rank) quiet(uni_reg(dat, "o", rank))
  expect_identical(unique(wrap_num(D, c(1, 2, 3))$table_body$exposure), "rank")
  # parameters not named like a column behave as before
  wrap_ok <- function(dat, vars, g) {
    quiet(descriptive_table(dat, vars, by = g))
  }
  res <- wrap_ok(D, c("rank", "df"), "class")
  expect_identical(names(res$header_labels), c("a", "b"))
  expect_null(.wrapper_passed_name("rank", globalenv(), names(D)))
})

test_that("compare_models(primary_exposure = ) prefers the model variable", {
  D <- bare_data()
  m1 <- quiet(multi_reg(D, "o", c("rank", "by")))
  m2 <- quiet(multi_reg(D, "o", c("rank", "by", "sample")))
  or_by <- exp(stats::coef(m1$models[[1]])["byg2"])
  or_rank <- exp(stats::coef(m1$models[[1]])["rank"])

  # no user object: `by` and `rank` are base functions (was an error)
  cm <- quiet(compare_models(m1, m2, primary_exposure = by, format = "tibble"))
  expect_identical(cm$table_body$primary_term, c("byg2", "byg2"))
  expect_equal(cm$table_body$primary_estimate[1], unname(or_by),
               tolerance = 1e-10)

  # masking global objects (was: silently tracked `rank`)
  res <- with_globals(
    list(by = "rank", rank = c(1, 2, 3)),
    list(
      by = quiet(compare_models(m1, m2, primary_exposure = by,
                                format = "tibble")),
      rank = quiet(compare_models(m1, m2, primary_exposure = rank,
                                  format = "tibble"))
    )
  )
  expect_equal(res$by$table_body$primary_estimate[1], unname(or_by),
               tolerance = 1e-10)
  expect_equal(res$rank$table_body$primary_estimate[1], unname(or_rank),
               tolerance = 1e-10)
  expect_identical(res$rank$primary_exposure, "rank")

  # quoted names and wrappers still work
  quoted <- quiet(compare_models(m1, m2, primary_exposure = "rank",
                                 format = "tibble"))
  expect_equal(quoted$table_body$primary_estimate[1], unname(or_rank),
               tolerance = 1e-10)
  wrap <- function(a, b, by) {
    quiet(compare_models(a, b, primary_exposure = by, format = "tibble"))
  }
  expect_equal(wrap(m1, m2, "rank")$table_body$primary_estimate[1],
               unname(or_rank), tolerance = 1e-10)
})
