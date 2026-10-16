# Regression tests for forest_df() and save_table() audit findings
# (CI separator, M7, M8).

fixf_bw <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, 1:3, c("White", "Black", "Other")),
      smoke = factor(smoke, 0:1, c("No", "Yes")),
      ht = factor(ht, 0:1, c("No", "Yes")),
      low = factor(low, 0:1, c("Normal BW", "Low BW")),
      ftv3 = factor(ifelse(ftv >= 2, "2+", ftv), levels = c("0", "1", "2+"), ordered = TRUE)
    )
}
fixf_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
fixf_ex <- c("age", "lwt", "smoke", "race", "ht")
# (exposure, level) of every forest row, rebuilt from the printed labels
fixf_keys <- function(out) {
  label <- out$Characteristic
  is_level <- grepl("^  ", label)
  block <- character(nrow(out))
  current <- NA_character_
  for (i in seq_len(nrow(out))) {
    if (!is_level[i]) current <- label[i]
    block[i] <- current
  }
  data.frame(exposure = block, level = ifelse(is_level, trimws(label), label),
             is_level = is_level, stringsAsFactors = FALSE)
}
fixf_ci <- function(tbl, exposure, level, digits = 2) {
  body <- tbl$table_body
  row <- body[body$exposure == exposure & body$level == level, , drop = FALSE]
  if (!nrow(row)) return("")
  if (isTRUE(row$ref[1])) return("Ref.")
  sprintf(paste0("%.", digits, "f (%.", digits, "f\u2013%.", digits, "f)"),
          row$estimate[1], row$conf.low[1], row$conf.high[1])
}

test_that("forest_df() writes the en dash used by the regression tables", {
  bw <- fixf_bw()
  uni <- fixf_quiet(uni_reg(bw, "low", fixf_ex))
  multi <- fixf_quiet(multi_reg(bw, "low", fixf_ex))
  out <- forest_df(uni, multi)
  keys <- fixf_keys(out)

  for (i in seq_len(nrow(out))) {
    level <- if (keys$is_level[i] || !any(uni$table_body$ref[uni$table_body$exposure == keys$exposure[i]])) {
      keys$level[i]
    } else {
      NA_character_
    }
    expected_crude <- if (is.na(level)) "" else fixf_ci(uni, keys$exposure[i], level)
    expected_adjusted <- if (is.na(level)) "" else fixf_ci(multi, keys$exposure[i], level)
    expect_identical(out[["OR (95% CI)"]][i], expected_crude, info = out$Characteristic[i])
    expect_identical(out[["Adjusted OR (95% CI)"]][i], expected_adjusted, info = out$Characteristic[i])
  }
  effects <- unlist(out[c("OR (95% CI)", "Adjusted OR (95% CI)")])
  effects <- effects[grepl("\\(", effects)]
  expect_true(all(grepl("\u2013", effects)))
  expect_false(any(grepl("[0-9]-[0-9]", effects)))
  # same separator as the multivariable table text
  shown <- multi$table_display[["Adjusted OR (95% CI)"]]
  expect_true(all(out[["Adjusted OR (95% CI)"]][nzchar(out[["Adjusted OR (95% CI)"]])] %in% shown))

  linear <- forest_df(fixf_quiet(multi_reg(bw, "bwt", c("smoke", "ht"), approach = "linear")))
  negative <- linear[[grep("95% CI", names(linear))]]
  expect_true(any(grepl("\\(-[0-9.]+\u2013-[0-9.]+\\)", negative)))
})

test_that("M7: forest_df() keeps rows that exist only in the adjusted table", {
  bw <- fixf_bw()
  multi <- fixf_quiet(multi_reg(bw, "low", fixf_ex))

  crude <- fixf_quiet(uni_reg(droplevels(bw[bw$race != "Other", ]), "low", fixf_ex))
  out <- forest_df(crude, multi)
  keys <- fixf_keys(out)
  other <- which(keys$exposure == "race" & keys$level == "Other")
  expect_length(other, 1L)
  expect_identical(keys$level[keys$exposure == "race"], c("race", "White", "Black", "Other"))
  expect_identical(out[["OR (95% CI)"]][other], "")
  expect_identical(out[["Adjusted OR (95% CI)"]][other], fixf_ci(multi, "race", "Other"))
  expect_true(is.na(attr(out, "est")[other]))
  expect_equal(attr(out, "est2")[other],
               multi$table_body$estimate[multi$table_body$level == "Other"])
  expect_identical(length(attr(out, "est")), nrow(out))
  expect_identical(length(attr(out, "est2")), nrow(out))

  small <- fixf_quiet(uni_reg(bw, "low", c("smoke", "age")))
  out <- forest_df(small, multi)
  keys <- fixf_keys(out)
  expect_identical(unique(keys$exposure), c("smoke", "age", "lwt", "race", "ht"))
  for (i in which(keys$is_level | keys$level %in% c("age", "lwt"))) {
    expect_identical(out[["Adjusted OR (95% CI)"]][i],
                     fixf_ci(multi, keys$exposure[i], keys$level[i]))
    expect_identical(out[["OR (95% CI)"]][i],
                     fixf_ci(small, keys$exposure[i], keys$level[i]))
  }

  inter <- fixf_quiet(multi_reg(bw, "low", c("age", "smoke"), interaction = "age*smoke"))
  out <- forest_df(inter)
  age_rows <- which(out$Characteristic == "age")
  expect_length(age_rows, 1L)
  expect_true(nzchar(out[["Adjusted OR (95% CI)"]][age_rows]))
  expect_equal(attr(out, "est")[age_rows],
               inter$table_body$estimate[inter$table_body$exposure == "age" &
                                           inter$table_body$level == "age"])
})

test_that("M7: forest_df() uses relabelled levels", {
  bw <- fixf_bw()
  uni <- fixf_quiet(uni_reg(bw, "low", fixf_ex))
  multi <- fixf_quiet(multi_reg(bw, "low", fixf_ex))
  relabelled <- modify_table(uni, level_labels = list(race = c(Black = "Blk")))
  out <- forest_df(relabelled, multi)
  plain <- forest_df(uni, multi)
  expect_identical(out$Characteristic[plain$Characteristic == "  Black"], "  Blk")
  expect_identical(out[["OR (95% CI)"]], plain[["OR (95% CI)"]])
  expect_identical(out[["Adjusted OR (95% CI)"]], plain[["Adjusted OR (95% CI)"]])
  expect_identical(attr(out, "est2"), attr(plain, "est2"))
})

test_that("M8: save_table() explains that flextables cannot be saved as PDF", {
  skip_if_not_installed("flextable")
  bw <- fixf_bw()
  uni <- fixf_quiet(uni_reg(bw, "low", c("age", "smoke")))
  target <- file.path(tempdir(), "fix-output-flextable")
  error <- tryCatch(
    save_table(uni, filename = target, format = "pdf"),
    error = function(e) conditionMessage(e)
  )
  expect_match(error, "PDF is not directly supported", fixed = TRUE)
  expect_match(error, "docx", fixed = TRUE)
  expect_match(error, "format = \"gt\"", fixed = TRUE)
  expect_false(file.exists(paste0(target, ".pdf")))

  merged <- fixf_quiet(merge_tables(uni, uni))
  expect_error(save_table(merged, filename = target, format = "pdf"),
               "PDF is not directly supported")
})

test_that("M8: an existing output extension is replaced, not doubled", {
  dir <- tempdir()
  expect_identical(basename(.normalize_save_path(file.path(dir, "x.html"), "docx")), "x.docx")
  expect_identical(basename(.normalize_save_path(file.path(dir, "x.DOCX"), "docx")), "x.DOCX")
  expect_identical(basename(.normalize_save_path(file.path(dir, "report.v2"), "html")), "report.v2.html")
  expect_identical(basename(.normalize_save_path(file.path(dir, "plot.png"), "pdf")), "plot.pdf")

  skip_if_not_installed("flextable")
  uni <- fixf_quiet(uni_reg(fixf_bw(), "low", c("age", "smoke")))
  path <- fixf_quiet(save_table(uni, filename = file.path(dir, "fix-output-ext.html"), format = "docx"))
  expect_match(path, "fix-output-ext\\.docx$")
  expect_true(file.exists(path))
  unlink(path)
})
