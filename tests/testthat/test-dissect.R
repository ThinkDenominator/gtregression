test_that("dissect() covers all branches and returns a tibble", {
  # Build a 6-row data frame to exercise all code paths
  df <- data.frame(
    num3   = c(1, 2, 3, NA, 4, 5),                         # numeric >2 uniques  -> compatible
    num2   = c(0, 1, 1, 0, 1, 0),                          # numeric 2 uniques   -> maybe
    fac3   = factor(c("a","b","a", NA, "b","a")),          # factor ≤5 levels    -> compatible
    fac10  = factor(c("L1","L2","L3","L4","L5","L6"),
                    levels = paste0("L", 1:10)),           # factor >5 levels    -> maybe + "..."
    chr3   = c("x","y","x", NA, "y","x"),                  # character ≤5 uniq   -> compatible
    chr10  = c("a","b","c","d","e","f"),                   # character >5 uniq   -> maybe + "..."
    logi   = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE),        # logical            -> compatible
    allNA  = rep(NA, 6),                                    # all NA             -> incompatible
    const  = rep(5, 6),                                     # 1 unique           -> incompatible
    date   = as.Date(c("2020-01-01","2020-01-02", NA,
                       "2020-01-04","2020-01-05","2020-01-06"))  # Date -> maybe
  )

  res <- dissect(df)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("Variable","Type","Missing (%)","Unique","Levels","Compatibility") %in% names(res)))
  expect_equal(nrow(res), ncol(df))

  get_row <- function(v) res[res$Variable == v, , drop = FALSE]

  expect_equal(get_row("num3")$Compatibility,  "compatible")
  expect_equal(get_row("num2")$Compatibility,  "maybe")
  expect_equal(get_row("fac3")$Compatibility,  "compatible")
  expect_equal(get_row("fac10")$Compatibility, "maybe")
  expect_equal(get_row("chr3")$Compatibility,  "compatible")
  expect_equal(get_row("chr10")$Compatibility, "maybe")
  expect_equal(get_row("logi")$Compatibility,  "compatible")
  expect_equal(get_row("allNA")$Compatibility, "incompatible")
  expect_equal(get_row("const")$Compatibility, "incompatible")
  expect_equal(get_row("date")$Compatibility,  "maybe")

  # Ellipsis for long level lists
  expect_true(grepl(", \\.\\.\\.$", get_row("fac10")$Levels))
  expect_true(grepl(", \\.\\.\\.$", get_row("chr10")$Levels))

  # Missing (%) formatting like "0%", "16.7%", "100%"
  expect_true(all(grepl("^\\d+(\\.\\d)?%$", res$`Missing (%)`)))

  expect_equal(get_row("const")$Unique, 1)
  expect_equal(get_row("num2")$Unique, 2)
})

test_that("dissect() errors on non-data.frame", {
  expect_error(dissect(1:3), "must be a data frame", fixed = TRUE)
})
