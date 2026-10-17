test_that("data-frame discovery returns metadata for eligible objects only", {
  skip_on_cran()
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(x = 1:3)
  env$tbl <- tibble::tibble(x = 1:3)
  env$vec <- 1:3
  env$mat <- matrix(1:4, 2)
  env$fun <- function() NULL
  env$model <- stats::lm(mpg ~ wt, data = mtcars)

  discovered <- gtx_discover_data_frames(env)

  expect_identical(discovered$name, c("df", "tbl"))
  expect_identical(discovered$rows, c(3L, 3L))
  expect_identical(discovered$columns, c(1L, 1L))
  expect_match(discovered$class, "data.frame")
})

test_that("data-frame discovery handles empty and unusual environments", {
  skip_on_cran()
  empty <- new.env(parent = emptyenv())
  expect_identical(
    names(gtx_discover_data_frames(empty)),
    c("name", "rows", "columns", "class")
  )
  expect_equal(nrow(gtx_discover_data_frames(empty)), 0L)

  env <- new.env(parent = emptyenv())
  assign("study data", data.frame(x = 1:2), envir = env)
  assign("another-dataset", data.frame(x = 1:4, y = 5:8), envir = env)
  discovered <- gtx_discover_data_frames(env)

  expect_identical(discovered$name, c("another-dataset", "study data"))
  expect_identical(discovered$rows, c(4L, 2L))
  expect_identical(discovered$columns, c(2L, 1L))
})

test_that("discovery skips active bindings and retrieval failures", {
  skip_on_cran()
  env <- new.env(parent = emptyenv())
  active_reads <- 0L
  makeActiveBinding("active_data", function(value) {
    if (!missing(value)) stop("read only")
    active_reads <<- active_reads + 1L
    data.frame(x = 1)
  }, env)
  delayedAssign("broken_data", stop("cannot retrieve"), assign.env = env)
  env$good_data <- data.frame(x = 1:2)

  discovered <- gtx_discover_data_frames(env)

  expect_identical(discovered$name, "good_data")
  expect_identical(active_reads, 0L)
})

test_that("loading an environment dataset is read-only and validated", {
  skip_on_cran()
  env <- new.env(parent = emptyenv())
  original <- data.frame(x = 1:3, group = c("a", "b", "a"))
  env$analysis_data <- original
  env$vec <- 1:3

  loaded <- gtx_get_data_frame(env, "analysis_data")
  loaded$x[1] <- 99L

  expect_identical(env$analysis_data, original)
  expect_error(gtx_get_data_frame(env, "missing"), "no longer available")
  expect_error(gtx_get_data_frame(env, "vec"), "not a data frame")
  expect_identical(gtx_object_reference("analysis_data"), "analysis_data")
  expect_identical(gtx_object_reference("study data"), "`study data`")
})
