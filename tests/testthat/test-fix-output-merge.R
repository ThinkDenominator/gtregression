# Regression tests for the merge / output-layer audit findings
# (N1, N2, C3/H1 leftovers, H3, H4, H6, N6, CI separator, footnotes).

fix_bw <- function() {
  data_birthwt |>
    dplyr::mutate(
      race = factor(race, 1:3, c("White", "Black", "Other")),
      smoke = factor(smoke, 0:1, c("No", "Yes")),
      ht = factor(ht, 0:1, c("No", "Yes")),
      ui = factor(ui, 0:1, c("No", "Yes")),
      low = factor(low, 0:1, c("Normal BW", "Low BW"))
    )
}
fix_quiet <- function(expr) suppressWarnings(suppressMessages(expr))
fix_ex <- c("age", "lwt", "smoke", "race", "ht")

# text helpers ---------------------------------------------------------------
fix_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # the uni_reg hyphen and the en dash of the other tables are equivalent
  .tidy_effect_cells(trimws(x))
}
fix_rendered_body <- function(obj) {
  tbl <- obj$table
  if (inherits(tbl, "flextable")) {
    out <- as.data.frame(.flex_part_text(tbl, "body"), stringsAsFactors = FALSE)
    out[, tbl$col_keys, drop = FALSE]
  } else {
    as.data.frame(tbl[["_data"]], stringsAsFactors = FALSE)
  }
}
fix_header_text <- function(obj) {
  spec <- .rendered_header(obj$table)
  c(unname(spec$labels), unlist(lapply(spec$levels, `[[`, "label")))
}
# rendered body must show exactly table_display, in both engines
expect_rendered_matches_display <- function(obj) {
  display <- obj$table_display
  body <- fix_rendered_body(obj)
  cols <- setdiff(names(display), "is_header")
  expect_true(all(cols %in% names(body)))
  for (column in cols) {
    expect_identical(
      trimws(fix_txt(body[[column]])), trimws(fix_txt(display[[column]])),
      info = column
    )
  }
}
# source cells of table `i`, addressed by the merged column position
fix_part <- function(merged, i) {
  merged$table_display[, names(merged$column_labels[[i]]), drop = FALSE]
}
fix_row <- function(display, label, header = NULL) {
  hit <- which(trimws(display$Characteristic) == label)
  if (!is.null(header)) hit <- hit[display$is_header[hit] %in% header]
  hit
}
# variable block (nearest header above) of every merged row
fix_block <- function(display) {
  block <- character(nrow(display))
  current <- NA_character_
  for (i in seq_len(nrow(display))) {
    if (isTRUE(display$is_header[i])) current <- trimws(display$Characteristic[i])
    block[i] <- current
  }
  block
}

test_that("same-structure merges still match their sources cell for cell", {
  bw <- fix_bw()
  for (fmt in c("flextable", "gt")) {
    desc <- fix_quiet(descriptive_table(bw, fix_ex, by = "low", format = fmt))
    uni <- fix_quiet(uni_reg(bw, "low", fix_ex, format = fmt))
    multi <- fix_quiet(multi_reg(bw, "low", fix_ex, format = fmt))
    merged <- fix_quiet(merge_tables(desc, uni, multi, format = fmt))
    sources <- list(desc, uni, multi)

    expect_identical(trimws(merged$table_display$Characteristic),
                     trimws(uni$table_display$Characteristic))
    expect_identical(merged$table_display$is_header, uni$table_display$is_header)
    for (i in seq_along(sources)) {
      part <- fix_part(merged, i)
      spec <- .header_spec(sources[[i]])
      expect_identical(unname(merged$column_labels[[i]]), unname(spec$labels[spec$cols]))
      for (j in seq_along(spec$cols)) {
        expect_identical(
          fix_txt(part[[j]]),
          fix_txt(sources[[i]]$table_display[[spec$cols[j]]]),
          info = paste(fmt, i, spec$cols[j])
        )
      }
    }
    expect_rendered_matches_display(merged)
  }

  ft <- fix_quiet(merge_tables(
    fix_quiet(uni_reg(bw, "low", fix_ex)), fix_quiet(multi_reg(bw, "low", fix_ex))
  ))
  gt <- fix_quiet(merge_tables(
    fix_quiet(uni_reg(bw, "low", fix_ex, format = "gt")),
    fix_quiet(multi_reg(bw, "low", fix_ex, format = "gt")),
    format = "gt"
  ))
  cols <- setdiff(names(ft$table_display), "is_header")
  for (column in cols) {
    expect_identical(fix_txt(fix_rendered_body(ft)[[column]]),
                     fix_txt(fix_rendered_body(gt)[[column]]))
  }
})

test_that("N1: all-compact merges keep the variable name on the compact row", {
  bw <- fix_bw()
  for (fmt in c("flextable", "gt")) {
    uni <- fix_quiet(uni_reg(bw, "low", fix_ex, show_ref = FALSE, format = fmt))
    multi <- fix_quiet(multi_reg(bw, "low", fix_ex, show_ref = FALSE, format = fmt))
    merged <- fix_quiet(merge_tables(uni, multi, format = fmt))
    display <- merged$table_display

    expect_identical(trimws(display$Characteristic), trimws(uni$table_display$Characteristic))
    expect_identical(display$is_header, uni$table_display$is_header)
    expect_false("Yes" %in% trimws(display$Characteristic))
    expect_length(fix_row(display, "smoke", header = TRUE), 1L)
    expect_length(fix_row(display, "ht", header = TRUE), 1L)

    for (i in 1:2) {
      src <- list(uni, multi)[[i]]
      spec <- .header_spec(src)
      part <- fix_part(merged, i)
      for (j in seq_along(spec$cols)) {
        expect_identical(fix_txt(part[[j]]), fix_txt(src$table_display[[spec$cols[j]]]))
      }
    }
    expect_rendered_matches_display(merged)
  }
})

test_that("N1: labelled all-compact merges start with the variable label", {
  cr <- data_competing_risk
  rx <- c("treatment", "age", "sex", "stage")
  crude <- fix_quiet(competing_risk_reg(
    cr, "follow_up_months", "outcome", "Relapse", rx,
    censor_code = "Censored", show_ref = FALSE
  ))
  adjusted <- fix_quiet(competing_risk_reg(
    cr, "follow_up_months", "outcome", "Relapse", rx,
    censor_code = "Censored", multivariable = TRUE, show_ref = FALSE
  ))
  merged <- fix_quiet(merge_tables(crude, adjusted))
  expect_identical(trimws(merged$table_display$Characteristic),
                   trimws(crude$table_display$Characteristic))
  expect_true(merged$table_display$is_header[1])
  expect_identical(fix_txt(fix_part(merged, 2)[[2]]),
                   fix_txt(adjusted$table_display[[.header_spec(adjusted)$cols[2]]]))
})

test_that("N2: compact tables with different references get a header and Ref. rows", {
  bw <- fix_bw()
  bw2 <- bw
  bw2$smoke <- stats::relevel(bw2$smoke, "Yes")
  first <- fix_quiet(uni_reg(bw, "low", fix_ex, show_ref = FALSE))
  second <- fix_quiet(uni_reg(bw2, "low", fix_ex, show_ref = FALSE))

  for (fmt in c("flextable", "gt")) {
    merged <- fix_quiet(merge_tables(first, second, format = fmt))
    display <- merged$table_display
    block <- fix_block(display)

    header <- fix_row(display, "smoke", header = TRUE)
    expect_length(header, 1L)
    yes <- which(block == "smoke" & trimws(display$Characteristic) == "Yes")
    no <- which(block == "smoke" & trimws(display$Characteristic) == "No")
    expect_length(yes, 1L)
    expect_length(no, 1L)
    expect_false(display$is_header[yes])
    expect_false(display$is_header[no])

    p1 <- fix_part(merged, 1)
    p2 <- fix_part(merged, 2)
    src1 <- first$table_display[first$table_display$Characteristic == "smoke", ]
    src2 <- second$table_display[second$table_display$Characteristic == "smoke", ]
    # N on the header row, estimates on the level they belong to
    expect_identical(fix_txt(p1[[1]][header]), fix_txt(src1$N))
    expect_identical(fix_txt(p2[[1]][header]), fix_txt(src2$N))
    expect_identical(fix_txt(p1[[2]][yes]), fix_txt(src1[["OR (95% CI)"]]))
    expect_identical(fix_txt(p1[[3]][yes]), fix_txt(src1[["p-value"]]))
    expect_identical(fix_txt(p2[[2]][no]), fix_txt(src2[["OR (95% CI)"]]))
    expect_identical(fix_txt(p2[[3]][no]), fix_txt(src2[["p-value"]]))
    # each table's reference level is marked
    expect_identical(fix_txt(p1[[2]][no]), "Ref.")
    expect_identical(fix_txt(p2[[2]][yes]), "Ref.")
    expect_identical(fix_txt(p1[[1]][c(yes, no)]), c("", ""))
    expect_true("Ref. = reference category." %in% merged$footnotes)
    expect_rendered_matches_display(merged)
  }
})

test_that("H1/C3: single-row descriptive + show_ref = TRUE shows the label once", {
  bw <- fix_bw()
  desc <- fix_quiet(descriptive_table(bw, fix_ex, show_dichotomous = "single_row"))
  uni <- fix_quiet(uni_reg(bw, "low", fix_ex))
  desc_value <- desc$table_display$Overall[desc$table_display$Characteristic == "smoke"]

  for (order in c("desc_first", "uni_first")) {
    merged <- if (order == "desc_first") {
      fix_quiet(merge_tables(desc, uni))
    } else {
      fix_quiet(merge_tables(uni, desc))
    }
    display <- merged$table_display
    block <- fix_block(display)
    expect_identical(sum(trimws(display$Characteristic) == "smoke"), 1L, info = order)
    expect_identical(anyDuplicated(trimws(display$Characteristic[display$is_header])), 0L)

    yes <- which(block == "smoke" & trimws(display$Characteristic) == "Yes")
    expect_length(yes, 1L)
    expect_false(display$is_header[yes])
    desc_part <- fix_part(merged, if (order == "desc_first") 1 else 2)
    uni_part <- fix_part(merged, if (order == "desc_first") 2 else 1)
    expect_identical(fix_txt(desc_part[[1]][yes]), fix_txt(desc_value))
    expect_identical(
      fix_txt(uni_part[[2]][yes]),
      fix_txt(uni$table_display[["OR (95% CI)"]][trimws(uni$table_display$Characteristic) == "Yes"][1])
    )
    expect_identical(fix_txt(desc_part[[1]][fix_row(display, "smoke")]), "")
    expect_rendered_matches_display(merged)
  }
})

test_that("M9: N of a compact table is printed on the variable row", {
  bw <- fix_bw()
  uni <- fix_quiet(uni_reg(bw, "low", fix_ex))
  multi <- fix_quiet(multi_reg(bw, "low", fix_ex, show_ref = FALSE))
  merged <- fix_quiet(merge_tables(uni, multi))
  display <- merged$table_display
  block <- fix_block(display)
  part <- fix_part(merged, 2)
  src <- multi$table_display[multi$table_display$Characteristic == "smoke", ]

  header <- fix_row(display, "smoke", header = TRUE)
  yes <- which(block == "smoke" & trimws(display$Characteristic) == "Yes")
  expect_identical(fix_txt(part[[1]][header]), fix_txt(src$N))
  expect_identical(fix_txt(part[[1]][yes]), "")
  expect_identical(fix_txt(part[[2]][header]), "")
  expect_identical(fix_txt(part[[2]][yes]), fix_txt(src[["Adjusted OR (95% CI)"]]))
  expect_identical(fix_txt(part[[3]][yes]), fix_txt(src[["p-value"]]))
  expect_rendered_matches_display(merged)
})

test_that("H3: stratified inputs keep labels, stratum spanners, order and notes", {
  bw <- fix_bw()
  ex <- c("age", "smoke", "race")
  for (fmt in c("flextable", "gt")) {
    desc <- fix_quiet(descriptive_table(bw, ex, by = "low", format = fmt))
    strat <- fix_quiet(stratified_uni_reg(bw, "low", ex, stratifier = "ht", format = fmt))
    merged <- fix_quiet(merge_tables(desc, strat, spanners = c("Desc", "By HT"), format = fmt))

    spec <- .rendered_header(merged$table)
    expect_false(any(grepl("^\\.\\.|_p[0-9]+$|__", spec$labels)), info = fmt)
    expect_identical(unname(spec$labels[names(merged$column_labels[[2]])]),
                     rep(c("N", "OR (95% CI)", "p-value"), 2))
    expect_true(all(grepl(", N=[0-9]+$", spec$labels[names(merged$column_labels[[1]])])))
    spanner_text <- unlist(lapply(spec$levels, `[[`, "label"))
    expect_true(all(c("No", "Yes", "Desc", "By HT") %in% spanner_text))
    inner <- spec$levels[[1]]
    expect_identical(inner$label, c("No", "Yes"))
    expect_identical(lengths(inner$cols), c(3L, 3L))

    notes <- .rendered_footnotes(merged$table)
    expect_true("Stratified by: ht." %in% notes)
    expect_true("Ref. = reference category." %in% notes)
    expect_identical(notes, merged$footnotes)

    # cell values: stratum columns of the source, in the source's column order
    source_spec <- .header_spec(strat)
    part <- fix_part(merged, 2)
    for (j in seq_along(source_spec$cols)) {
      expect_identical(fix_txt(part[[j]]),
                       fix_txt(strat$table_display[[source_spec$cols[j]]]))
    }
    expect_rendered_matches_display(merged)
  }
})

test_that("H3: stratified Cox merges keep N, Events, effect, p per stratum", {
  lung <- data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, 1:2, c("Standard", "Test")),
      prior = factor(prior, c(0, 10), c("No", "Yes"))
    )
  crude <- fix_quiet(cox_reg(lung, "time", "status", c("age", "karno", "trt"),
                             stratifier = "prior"))
  adjusted <- fix_quiet(cox_reg(lung, "time", "status", c("age", "karno", "trt"),
                                stratifier = "prior", multivariable = TRUE))
  merged <- fix_quiet(merge_tables(crude, adjusted))
  expect_identical(
    unname(merged$column_labels[[2]]),
    rep(c("N", "Events", "Adjusted HR (95% CI)", "p-value"), 2)
  )
  expect_true(any(grepl("^Stratified by: prior", merged$footnotes)))
  expect_true(any(grepl("^Complete observations included by prior stratum", merged$footnotes)))
  expect_true(any(grepl("^Events are event counts", merged$footnotes)))
  expect_identical(sum(.is_abbreviation_note(merged$footnotes)), 1L)
})

test_that("H3/H4: multinomial and nested merges keep headers and spanners", {
  skip_if_not_installed("nnet")
  bw <- fix_bw()
  mn <- fix_quiet(multinomial_reg(bw, "race", c("age", "smoke", "ht")))
  # H4: a stored format of length two must not break merging or modifying
  mn$format <- c("flextable", "gt")
  merged <- expect_no_error(fix_quiet(merge_tables(mn, mn, spanners = c("A", "B"))))
  expect_identical(merged$part_formats, c("flextable", "flextable"))

  spec <- .rendered_header(merged$table)
  expect_identical(unname(spec$labels), rep(c("N", "OR (95% CI)", "p-value"), 4))
  expect_identical(spec$levels[[1]]$label,
                   rep(c("Black vs White", "Other vs White"), 2))
  expect_identical(spec$levels[[2]]$label, c("A", "B"))
  source_spec <- .header_spec(mn)
  for (i in 1:2) {
    part <- fix_part(merged, i)
    for (j in seq_along(source_spec$cols)) {
      expect_identical(fix_txt(part[[j]]),
                       fix_txt(mn$table_display[[source_spec$cols[j]]]))
    }
  }
  expect_rendered_matches_display(merged)
  expect_no_error(modify_table(mn))

  uni <- fix_quiet(uni_reg(bw, "low", fix_ex))
  multi <- fix_quiet(multi_reg(bw, "low", fix_ex))
  inner <- fix_quiet(merge_tables(uni, multi, spanners = c("Crude", "Adjusted")))
  desc <- fix_quiet(descriptive_table(bw, fix_ex, by = "low"))
  nested <- fix_quiet(merge_tables(desc, inner, spanners = c("Descriptive", "Models")))
  nested_spec <- .rendered_header(nested$table)
  expect_false(any(grepl("_p[0-9]+$", nested_spec$labels)))
  expect_identical(
    unname(nested_spec$labels[names(nested$column_labels[[2]])]),
    c("N", "OR (95% CI)", "p-value", "N", "Adjusted OR (95% CI)", "p-value")
  )
  expect_identical(nested_spec$levels[[1]]$label, c("Crude", "Adjusted"))
  expect_identical(nested_spec$levels[[2]]$label, c("Descriptive", "Models"))
  part <- fix_part(nested, 2)
  for (j in seq_along(part)) {
    expect_identical(fix_txt(part[[j]]), fix_txt(inner$table_display[[names(part)[j] |> sub(pattern = "_p2$", replacement = "")]]))
  }
})

test_that("M6: repeated spanner labels do not break gt merges", {
  bw <- fix_bw()
  uni <- fix_quiet(uni_reg(bw, "low", c("age", "smoke"), format = "gt"))
  merged <- expect_no_error(
    merge_tables(uni, uni, spanners = c("Model", "Model"), format = "gt")
  )
  spec <- .rendered_header(merged$table)
  expect_identical(spec$levels[[1]]$label, c("Model", "Model"))
  expect_identical(lengths(spec$levels[[1]]$cols), c(3L, 3L))
})

test_that("H6: compact themes keep the level indentation", {
  skip_if_not_installed("flextable")
  bw <- fix_bw()
  ex <- c("age", "smoke", "race")
  indent <- function(obj) {
    left <- obj$table$body$styles$pars$padding.left$data[, "Characteristic"]
    header <- obj$table_display$is_header %in% TRUE
    list(header = unique(unname(left[header])), level = unique(unname(left[!header])))
  }
  for (theme in c("clinical", "striped", "jama", "minimal")) {
    uni <- fix_quiet(uni_reg(bw, "low", ex, theme = theme))
    multi <- fix_quiet(multi_reg(bw, "low", ex, theme = theme))
    strat <- fix_quiet(stratified_uni_reg(bw, "low", ex, stratifier = "ht", theme = theme))
    merged <- fix_quiet(merge_tables(uni, multi, theme = theme))
    for (obj in list(uni, multi, strat, merged)) {
      pad <- indent(obj)
      expect_identical(pad$level, 14, info = theme)
      expect_true(all(pad$header < 14), info = theme)
    }
  }
  compact <- fix_quiet(uni_reg(bw, "low", ex, theme = "clinical"))
  expect_true(all(compact$table$body$styles$pars$padding.top$data == 2))
})

test_that("N6: non-estimable coefficients are blank, not 'NA (NA-NA)'", {
  td <- data.frame(
    exposure = c("age", "parity"), level = c("age", "parity"),
    estimate = c(1.2, NA), conf.low = c(1.1, NA), conf.high = c(1.3, NA),
    p.value = c(0.01, NA), ref = FALSE, stringsAsFactors = FALSE
  )
  display <- .make_display_multi(
    td, data = data.frame(age = 1, parity = 1), outcome = "y",
    effect_label = "OR (95% CI)",
    variable_labels = c(age = "age", parity = "parity")
  )
  expect_identical(display[["OR (95% CI)"]], c("1.20 (1.10\u20131.30)", ""))
  expect_identical(display[["p-value"]][2], "")
  expect_identical(.blank_missing_estimate(function(e, l, h) paste(e, l, h))(NA, 1, 2), "")

  expect_identical(
    .tidy_effect_cells(c("NA ( NA\u2013 NA)", "NA (NA-NA)", "1.00 (0.50\u20132.00)", "Ref.", "")),
    c("", "", "1.00 (0.50\u20132.00)", "Ref.", "")
  )

  # a source table that still carries the literal text is cleaned on merging
  bw <- fix_bw()
  multi <- fix_quiet(multi_reg(bw, "low", c("age", "lwt")))
  broken <- multi
  broken$table_display[["Adjusted OR (95% CI)"]][2] <- " NA ( NA\u2013 NA)"
  broken$table <- NULL
  for (fmt in c("flextable", "gt")) {
    merged <- fix_quiet(merge_tables(multi, broken, format = fmt))
    expect_identical(fix_txt(fix_part(merged, 2)[[2]]),
                     c(fix_txt(multi$table_display[["Adjusted OR (95% CI)"]][1]), ""))
    expect_false(any(grepl("NA", unlist(fix_rendered_body(merged)))))
  }
})

test_that("M1: merged effect columns use one confidence-interval separator", {
  bw <- fix_bw()
  expect_identical(
    .tidy_effect_cells(c("-283.78 (-493.43--74.12)", "0.95 (0.89-1.01)",
                         "1,200.50 (-3.10-4,000.00)", "23.0 (19.0\u201326.0)", "0.105")),
    c("-283.78 (-493.43\u2013-74.12)", "0.95 (0.89\u20131.01)",
      "1,200.50 (-3.10\u20134,000.00)", "23.0 (19.0\u201326.0)", "0.105")
  )

  uni <- fix_quiet(uni_reg(bw, "bwt", fix_ex, approach = "linear"))
  multi <- fix_quiet(multi_reg(bw, "bwt", fix_ex, approach = "linear"))
  desc <- fix_quiet(descriptive_table(bw, fix_ex))
  merged <- fix_quiet(merge_tables(desc, uni, multi))
  effects <- c(fix_part(merged, 2)[[2]], fix_part(merged, 3)[[2]])
  effects <- effects[grepl("\\(", effects)]
  expect_true(length(effects) > 0L)
  expect_true(all(grepl("\u2013", effects)))
  expect_false(any(grepl("[0-9]--?[0-9].*\\)$", gsub("\u2013-?[0-9.,]+\\)$", ")", effects))))
  # the numbers themselves are untouched
  strip <- function(x) gsub("[^0-9.,()]", "", x)
  expect_identical(strip(fix_part(merged, 2)[[2]]), strip(uni$table_display[["Beta (95% CI)"]]))
  # descriptive Median (IQR) cells are not rewritten
  expect_identical(as.character(fix_part(merged, 1)[[1]]), as.character(desc$table_display$Overall))
})

test_that("M2: merged footnotes have one abbreviation line and grouped notes", {
  lung <- data_lungcancer |>
    dplyr::mutate(trt = factor(trt, 1:2, c("Standard", "Test")))
  cx <- c("age", "karno", "trt")
  logit <- fix_quiet(uni_reg(lung, "status", cx))
  cox <- fix_quiet(cox_reg(lung, "time", "status", cx, multivariable = TRUE))
  aft <- fix_quiet(surv_reg(lung, "time", "status", cx, multivariable = TRUE))
  merged <- fix_quiet(merge_tables(logit, cox, aft))

  abbreviation <- merged$footnotes[.is_abbreviation_note(merged$footnotes)]
  expect_length(abbreviation, 1L)
  expect_match(abbreviation, "^Abbreviations: ")
  for (term in c("OR = Odds Ratio", "HR = Hazard Ratio", "Time Ratio = ")) {
    expect_match(abbreviation, term, fixed = TRUE)
  }
  expect_identical(lengths(regmatches(abbreviation, gregexpr("CI = Confidence Interval", abbreviation))), 1L)
  expect_match(abbreviation, "CI = Confidence Interval\\.$")
  expect_identical(anyDuplicated(merged$footnotes), 0L)
  expect_identical(.rendered_footnotes(merged$table), merged$footnotes)

  expect_identical(
    .combine_abbreviation_notes(c(
      "OR = common Odds Ratio; CI = Confidence Interval.", "Ref. = reference category.",
      "Abbreviations: HR = Hazard Ratio; CI = Confidence Interval."
    )),
    c("Abbreviations: OR = common Odds Ratio; HR = Hazard Ratio; CI = Confidence Interval.",
      "Ref. = reference category.")
  )
  single <- c("Abbreviations: OR = Odds Ratio; CI = Confidence Interval.", "Ref. = reference category.")
  expect_identical(.combine_abbreviation_notes(single), single)
  expect_identical(
    .merge_footnotes(list(
      c("A = Odds Ratio.", "Outcome comparison: Black vs White.", "N is the number of complete observations used in each model."),
      c("A = Odds Ratio.", "Outcome comparison: Other vs White.", "N is the number of complete observations used in each model.")
    )),
    c("A = Odds Ratio.", "Outcome comparison: Black vs White.",
      "Outcome comparison: Other vs White.",
      "N is the number of complete observations used in each model.")
  )
})

test_that("M4: merged and single flextables share footer size, rules and alignment", {
  skip_if_not_installed("flextable")
  bw <- fix_bw()
  uni <- fix_quiet(uni_reg(bw, "low", fix_ex))
  multi <- fix_quiet(multi_reg(bw, "low", fix_ex))
  merged <- fix_quiet(merge_tables(uni, multi))
  footer_size <- function(obj) unique(c(obj$table$footer$styles$text$font.size$data))
  expect_identical(footer_size(merged), footer_size(uni))
  expect_identical(footer_size(merged), footer_size(multi))
  expect_equal(footer_size(merged), 8)

  top <- function(obj) obj$table$header$styles$cells$border.width.top$data[1, 2]
  bottom <- function(obj) {
    data <- obj$table$header$styles$cells$border.width.bottom$data
    data[nrow(data), 2]
  }
  expect_equal(unname(top(merged)), unname(top(uni)))
  expect_equal(unname(bottom(merged)), unname(bottom(uni)))

  align <- function(obj) unname(obj$table$body$styles$pars$text.align$data[1, ])
  expect_identical(align(uni), c("left", "center", "center", "center"))
  expect_identical(align(multi), align(uni))
  expect_identical(unique(align(merged)[-1]), "center")
})

test_that("merges across model families place every source cell on its own row", {
  bw <- fix_bw()
  lung <- data_lungcancer |>
    dplyr::mutate(
      trt = factor(trt, 1:2, c("Standard", "Test")),
      prior = factor(prior, c(0, 10), c("No", "Yes"))
    )
  cx <- c("age", "karno", "trt", "celltype", "prior")
  bw_missing <- bw
  bw_missing$lwt[1:25] <- NA
  bw_missing$smoke[30:45] <- NA

  # every non-empty source row must be found at (variable block, row label)
  # in the merged table with identical cells
  expect_merge_aligned <- function(sources, info, ...) {
    for (fmt in c("flextable", "gt")) {
      merged <- fix_quiet(do.call(merge_tables, c(sources, list(format = fmt, ...))))
      display <- merged$table_display
      merged_key <- paste(fix_block(display), trimws(display$Characteristic),
                          display$is_header, sep = "||")
      expect_identical(anyDuplicated(merged_key), 0L, info = info)
      for (i in seq_along(sources)) {
        src <- sources[[i]]$table_display
        spec <- .header_spec(sources[[i]])
        part <- fix_part(merged, i)
        src_key <- paste(fix_block(src), trimws(src$Characteristic), src$is_header, sep = "||")
        rows <- match(src_key, merged_key)
        expect_false(anyNA(rows), info = paste(info, fmt, i))
        for (j in seq_along(spec$cols)) {
          expect_identical(fix_txt(part[[j]][rows]), fix_txt(src[[spec$cols[j]]]),
                           info = paste(info, fmt, i, spec$cols[j]))
        }
        # rows the source does not have stay empty in its columns
        other <- setdiff(seq_len(nrow(display)), rows)
        if (length(other)) {
          expect_true(all(unlist(lapply(part, function(x) fix_txt(x[other]))) == ""),
                      info = paste(info, fmt, i))
        }
      }
      expect_rendered_matches_display(merged)
    }
  }

  uni <- fix_quiet(uni_reg(bw, "low", fix_ex))
  multi <- fix_quiet(multi_reg(bw, "low", fix_ex))
  desc <- fix_quiet(descriptive_table(bw, fix_ex, by = "low"))
  expect_merge_aligned(list(uni, multi), "logit crude + adjusted")
  expect_merge_aligned(list(multi, uni), "reversed order")
  expect_merge_aligned(list(desc, uni, multi, multi), "four tables")
  expect_merge_aligned(
    list(uni, fix_quiet(multi_reg(bw, "low", c("smoke", "race"), adjust_for = c("age", "lwt")))),
    "adjust_for subset"
  )
  expect_merge_aligned(
    list(uni, fix_quiet(multi_reg(bw, "low", c("ht", "race", "age", "smoke", "lwt")))),
    "shuffled exposures"
  )
  expect_merge_aligned(
    list(fix_quiet(uni_reg(droplevels(bw[bw$race != "Black", ]), "low", fix_ex)), multi),
    "level missing from the first table"
  )
  expect_merge_aligned(
    list(fix_quiet(uni_reg(bw_missing, "low", fix_ex)),
         fix_quiet(multi_reg(bw_missing, "low", fix_ex))),
    "missing data"
  )
  expect_merge_aligned(
    list(fix_quiet(uni_reg(bw, "bwt", fix_ex, approach = "linear")),
         fix_quiet(multi_reg(bw, "bwt", fix_ex, approach = "linear"))),
    "linear"
  )
  expect_merge_aligned(
    list(fix_quiet(cox_reg(lung, "time", "status", cx)),
         fix_quiet(cox_reg(lung, "time", "status", cx, multivariable = TRUE))),
    "cox"
  )
  expect_merge_aligned(
    list(fix_quiet(surv_reg(lung, "time", "status", cx)),
         fix_quiet(surv_reg(lung, "time", "status", cx, multivariable = TRUE))),
    "survreg"
  )
  expect_merge_aligned(
    list(fix_quiet(stratified_uni_reg(bw, "low", c("age", "smoke", "race"), stratifier = "ht")),
         fix_quiet(stratified_multi_reg(bw, "low", c("age", "smoke", "race"), stratifier = "ui"))),
    "stratified + stratified"
  )
  expect_merge_aligned(
    list(uni, fix_quiet(multi_reg(bw, "low", c("age", "smoke", "race"), interaction = "age*smoke"))),
    "interaction model"
  )
})
