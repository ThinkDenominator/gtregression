# Regression tests for modify_table() audit findings (H5, N5, M3, M5 and
# remove_abbreviations for prefix-less abbreviation notes).

fixm_bw <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, 1:3, c("White", "Black", "Other")),
      smoke = factor(smoke, 0:1, c("No", "Yes")),
      ht = factor(ht, 0:1, c("No", "Yes")),
      ui = factor(ui, 0:1, c("No", "Yes")),
      low = factor(low, 0:1, c("Normal BW", "Low BW")),
      ftv3 = factor(ifelse(ftv >= 2, "2+", ftv), levels = c("0", "1", "2+"), ordered = TRUE)
    )
}
fixm_lung <- function() {
  data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, 1:2, c("Standard", "Test")),
      prior = factor(prior, c(0, 10), c("No", "Yes"))
    )
}
fixm_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
fixm_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}
# what the reader sees: column labels, spanners, body text, notes
fixm_view <- function(obj) {
  tbl <- obj$table
  spec <- .rendered_header(tbl)
  body <- if (inherits(tbl, "flextable")) {
    text <- .flex_part_text(tbl, "body")
    unname(text[, tbl$col_keys, drop = FALSE])
  } else {
    data <- as.data.frame(tbl[["_data"]], stringsAsFactors = FALSE)
    unname(as.matrix(data[, c("Characteristic", spec$cols), drop = FALSE]))
  }
  body[] <- fixm_txt(body)
  list(
    labels = unname(spec$labels),
    spanners = lapply(spec$levels, function(level) {
      list(label = level$label, width = lengths(level$cols))
    }),
    body = body,
    notes = .rendered_footnotes(tbl)
  )
}

test_that("H5: modify_table() without arguments leaves the content unchanged", {
  bw <- fixm_bw()
  lung <- fixm_lung()
  ex <- c("age", "smoke", "race")
  for (fmt in c("flextable", "gt")) {
    objects <- list(
      descriptive = descriptive_table(bw, ex, by = "low", format = fmt),
      uni = uni_reg(bw, "low", ex, format = fmt),
      multi_adjusted = multi_reg(bw, "low", c("smoke", "race"),
                                 adjust_for = c("age", "lwt"), format = fmt),
      cox_mv = cox_reg(lung, "time", "status", c("age", "karno", "trt"),
                       multivariable = TRUE, format = fmt),
      strat_uni = stratified_uni_reg(bw, "low", ex, stratifier = "ht", format = fmt),
      strat_multi = stratified_multi_reg(bw, "low", ex, stratifier = "ui", format = fmt),
      strat_cox = cox_reg(lung, "time", "status", c("age", "karno", "trt"),
                          stratifier = "prior", multivariable = TRUE, format = fmt)
    ) |> fixm_quiet()
    objects$merged <- fixm_quiet(merge_tables(objects$descriptive, objects$uni, format = fmt))
    if (requireNamespace("MASS", quietly = TRUE)) {
      objects$ordinal <- fixm_quiet(ordinal_reg(bw, "ftv3", ex, format = fmt))
    }
    if (requireNamespace("nnet", quietly = TRUE)) {
      objects$multinomial <- fixm_quiet(
        multinomial_reg(bw, "race", c("age", "smoke", "ht"), format = fmt)
      )
    }

    for (name in names(objects)) {
      before <- objects[[name]]
      after <- fixm_quiet(modify_table(before))
      info <- paste(fmt, name)
      view_before <- fixm_view(before)
      view_after <- fixm_view(after)
      expect_identical(view_after$labels, view_before$labels, info = info)
      expect_identical(view_after$spanners, view_before$spanners, info = info)
      expect_identical(view_after$body, view_before$body, info = info)
      expect_identical(view_after$notes, view_before$notes, info = info)
      expect_identical(after$footnotes, view_before$notes, info = info)
      expect_identical(
        as.data.frame(lapply(after$table_display, fixm_txt)),
        as.data.frame(lapply(before$table_display, fixm_txt)),
        info = info
      )
    }
  }
})

test_that("H5: conditional-logit and multilevel notes keep their order", {
  inf <- data_infertility |>
    dplyr::mutate(spont_f = factor(spontaneous > 0, c(FALSE, TRUE), c("No", "Yes")))
  cl <- fixm_quiet(conditional_logit_reg(
    inf, "case", c("spont_f", "age"), matched_set = "stratum", multivariable = TRUE
  ))
  expect_identical(modify_table(cl)$footnotes, cl$footnotes)
  relabelled <- modify_table(cl, variable_labels = c(stratum = "Matched set"))
  expect_identical(
    grep("^Matched by set identifier:", relabelled$footnotes),
    grep("^Matched by set identifier:", cl$footnotes)
  )
  expect_true("Matched by set identifier: Matched set." %in% relabelled$footnotes)
  expect_length(relabelled$footnotes, length(cl$footnotes))

  skip_if_not_installed("lme4")
  ml <- fixm_quiet(multilevel_reg(
    data_epilepsy, "y", c("trt", "base", "age"), group = "subject", approach = "linear"
  ))
  expect_identical(modify_table(ml)$footnotes, ml$footnotes)
})

test_that("N5: remove_N_obs and remove_N remove the matching N notes", {
  bw <- fixm_bw()
  lung <- fixm_lung()
  n_column_note <- "N is the number of complete observations used in each model."
  cox_mv <- fixm_quiet(cox_reg(lung, "time", "status", c("age", "karno", "trt"),
                               multivariable = TRUE))
  multi <- fixm_quiet(multi_reg(bw, "low", c("age", "smoke")))
  skip_if_not(n_column_note %in% cox_mv$footnotes)

  for (obj in list(cox_mv, multi)) {
    no_obs <- modify_table(obj, remove_N_obs = TRUE)
    expect_false(any(.is_n_note(no_obs$footnotes)))
    expect_false(any(.is_n_note(.rendered_footnotes(no_obs$table))))
    expect_identical(no_obs$footnotes, obj$footnotes[!.is_n_note(obj$footnotes)])
    # the N column itself is controlled by remove_N
    expect_true("N" %in% names(no_obs$table_display))
  }

  no_n <- modify_table(cox_mv, remove_N = TRUE)
  expect_false("N" %in% names(no_n$table_display))
  expect_false(n_column_note %in% no_n$footnotes)
  expect_false(n_column_note %in% .rendered_footnotes(no_n$table))
  expect_false("N" %in% .rendered_header(no_n$table)$labels)

  both <- modify_table(multi, remove_N = TRUE, remove_N_obs = TRUE)
  expect_false("N" %in% names(both$table_display))
  expect_false(any(.is_n_note(both$footnotes)))

  expect_true(all(.is_n_note(c(
    n_column_note,
    "N is the number of complete analysis intervals used in each model.",
    "N = 189 complete observations included in the model.",
    "N = 189 complete observations included in each adjusted model.",
    "Complete observations included by ui stratum: No: N = 161; Yes: N = 28",
    "ui = No: N = 161 complete observations included in the multivariable model"
  ))))
  expect_false(any(.is_n_note(c(
    "Ref. = reference category.", "Adjusted for age and lwt",
    "Abbreviations: OR = Odds Ratio; CI = Confidence Interval."
  ))))

  strat <- fixm_quiet(stratified_multi_reg(bw, "low", c("age", "smoke"), stratifier = "ui"))
  strat_no_obs <- fixm_quiet(modify_table(strat, remove_N_obs = TRUE))
  expect_false(any(.is_n_note(.rendered_footnotes(strat_no_obs$table))))
  expect_true(any(grepl("^Stratified by: ui", .rendered_footnotes(strat_no_obs$table))))
})

test_that("remove_abbreviations works for prefix-less abbreviation notes", {
  bw <- fixm_bw()
  tables <- list()
  if (requireNamespace("MASS", quietly = TRUE)) {
    tables$ordinal <- fixm_quiet(ordinal_reg(bw, "ftv3", c("age", "smoke")))
  }
  if (requireNamespace("nnet", quietly = TRUE)) {
    tables$multinomial <- fixm_quiet(multinomial_reg(bw, "race", c("age", "smoke")))
  }
  inf <- data_infertility |>
    dplyr::mutate(spont_f = factor(spontaneous > 0, c(FALSE, TRUE), c("No", "Yes")))
  tables$conditional <- fixm_quiet(conditional_logit_reg(
    inf, "case", c("spont_f", "age"), matched_set = "stratum"
  ))
  tables$uni <- fixm_quiet(uni_reg(bw, "low", c("age", "smoke")))

  for (name in names(tables)) {
    obj <- tables[[name]]
    expect_true(any(.is_abbreviation_note(obj$footnotes)), info = name)
    kept <- fixm_quiet(modify_table(obj))
    # no second abbreviation line is appended
    expect_identical(sum(.is_abbreviation_note(kept$footnotes)),
                     sum(.is_abbreviation_note(obj$footnotes)), info = name)
    dropped <- fixm_quiet(modify_table(obj, remove_abbreviations = TRUE))
    expect_false(any(.is_abbreviation_note(dropped$footnotes)), info = name)
    expect_false(any(grepl("Odds Ratio|Confidence Interval", dropped$footnotes)), info = name)
    expect_identical(dropped$footnotes, obj$footnotes[!.is_abbreviation_note(obj$footnotes)],
                     info = name)
    expect_identical(.rendered_footnotes(dropped$table), dropped$footnotes, info = name)
  }
  expect_false(any(.is_abbreviation_note(c(
    "Ref. = reference category.", "N = 189 complete observations included in the model.",
    "Outcome comparison: Black vs White.", "Adjusted for age and lwt"
  ))))
})

test_that("M5: gt footnotes keep '*' in interaction terms", {
  skip_if_not_installed("gt")
  bw <- fixm_bw()
  multi <- fixm_quiet(multi_reg(bw, "low", c("age", "smoke", "race"),
                                interaction = "smoke*race", format = "gt"))
  modified <- modify_table(multi, caveat = "Sensitivity model: smoke*race and age*lwt.")
  notes <- .rendered_footnotes(modified$table)
  expect_true("Sensitivity model: smoke*race and age*lwt." %in% notes)
  expect_true(any(grepl("smoke*race", notes[notes != "Sensitivity model: smoke*race and age*lwt."], fixed = TRUE)))
  html <- as.character(gt::as_raw_html(modified$table))
  expect_match(html, "Sensitivity model: smoke*race and age*lwt.", fixed = TRUE)
  expect_false(grepl("<em>", html, fixed = TRUE))
  # one source note per footnote, as in the single tables
  expect_identical(notes, modified$footnotes)
})

test_that("M3: modify_table() handles merged tables", {
  bw <- fixm_bw()
  ex <- c("age", "smoke", "race")
  for (fmt in c("flextable", "gt")) {
    uni <- fixm_quiet(uni_reg(bw, "low", ex, format = fmt))
    multi <- fixm_quiet(multi_reg(bw, "low", ex, format = fmt))
    merged <- fixm_quiet(merge_tables(uni, multi, spanners = c("Crude", "Adjusted"), format = fmt))

    no_n <- modify_table(merged, remove_N = TRUE)
    expect_false(any(grepl("^N_p", names(no_n$table_display))))
    spec <- .rendered_header(no_n$table)
    expect_identical(unname(spec$labels),
                     c("OR (95% CI)", "p-value", "Adjusted OR (95% CI)", "p-value"))
    expect_identical(spec$levels[[1]]$label, c("Crude", "Adjusted"))
    expect_identical(lengths(spec$levels[[1]]$cols), c(2L, 2L))
    kept <- setdiff(names(no_n$table_display), c("Characteristic", "is_header"))
    for (column in kept) {
      expect_identical(fixm_txt(no_n$table_display[[column]]),
                       fixm_txt(merged$table_display[[column]]))
    }

    renamed <- modify_table(merged, header_labels = c("OR (95% CI)" = "Crude OR", p.value = "P"))
    expect_identical(unname(.rendered_header(renamed$table)$labels),
                     c("N", "Crude OR", "P", "N", "Adjusted OR (95% CI)", "P"))
    expect_error(modify_table(merged, header_labels = c(nonsense = "x")), "not found")
  }

  # labels set with modify_table() are carried into a later merge
  relabelled <- modify_table(fixm_quiet(uni_reg(bw, "low", ex)),
                             header_labels = c(estimate = "Crude OR"))
  merged <- fixm_quiet(merge_tables(relabelled, fixm_quiet(multi_reg(bw, "low", ex))))
  expect_identical(unname(merged$column_labels[[1]]), c("N", "Crude OR", "p-value"))
})

test_that("H4: modify_table() accepts a stored format of length two", {
  bw <- fixm_bw()
  uni <- fixm_quiet(uni_reg(bw, "low", c("age", "smoke")))
  uni$format <- c("flextable", "gt")
  modified <- expect_no_error(modify_table(uni, caveat = "Note."))
  expect_s3_class(modified$table, "flextable")
  expect_identical(fixm_view(modified)$body, fixm_view(uni)$body)
})

test_that("M4: modify_table() keeps the single-table flextable styling", {
  skip_if_not_installed("flextable")
  bw <- fixm_bw()
  uni <- fixm_quiet(uni_reg(bw, "low", c("age", "smoke", "race")))
  modified <- modify_table(uni, caveat = "Note.")
  cells <- function(obj, part, what) obj$table[[part]]$styles$cells[[what]]$data
  expect_equal(cells(modified, "body", "border.width.bottom"),
               cells(uni, "body", "border.width.bottom"))
  expect_equal(cells(modified, "header", "border.width.top"),
               cells(uni, "header", "border.width.top"))
  expect_equal(unique(c(modified$table$footer$styles$text$font.size$data)), 8)
  expect_identical(
    unname(modified$table$body$styles$pars$padding.left$data[, "Characteristic"]),
    unname(uni$table$body$styles$pars$padding.left$data[, "Characteristic"])
  )
})
