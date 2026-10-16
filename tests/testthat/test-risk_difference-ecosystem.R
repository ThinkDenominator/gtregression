# risk_difference() and the rest of the package: modify_table() support,
# friendly rejections, and unchanged behaviour for regression objects
# (qualification matrix cases 26 and 27).

rd_gt_notes <- function(tb) unlist(tb[["_source_notes"]])
rd_flex_notes <- function(ft) unname(.flex_part_text(ft, "footer")[, 1])
rd_flex_header <- function(ft) unname(.flex_part_text(ft, "header")[1, ])

rd_example <- function(format = "gt", ...) {
  d <- data_birthwt
  d$smoking <- factor(d$smoke, levels = c(0, 1), labels = c("No", "Yes"))
  suppressMessages(
    risk_difference(d, "low", "smoking", format = format, ...)
  )
}

test_that("case 26: unsupported functions reject risk_difference tables", {
  rd <- rd_example()
  uni <- suppressMessages(
    uni_reg(data_birthwt, "low", "smoke", approach = "logit", format = "gt")
  )
  msg <- function(fn) {
    paste0("risk_difference() tables are not supported by ", fn, "().")
  }
  expect_error(merge_tables(rd, uni), msg("merge_tables"), fixed = TRUE)
  expect_error(merge_tables(uni, rd), msg("merge_tables"), fixed = TRUE)
  expect_error(merge_tables(rd, rd), msg("merge_tables"), fixed = TRUE)
  expect_error(plot_reg(rd), msg("plot_reg"), fixed = TRUE)
  expect_error(plot_reg_combine(rd, uni), msg("plot_reg_combine"),
               fixed = TRUE)
  expect_error(plot_reg_combine(uni, rd), msg("plot_reg_combine"),
               fixed = TRUE)
  expect_error(forest_df(rd), msg("forest_df"), fixed = TRUE)
  expect_error(forest_df(uni, multi = rd), msg("forest_df"), fixed = TRUE)
  expect_error(forest_df(uni, desc = rd), msg("forest_df"), fixed = TRUE)
  expect_error(forest_reg(uni = rd), msg("forest_reg"), fixed = TRUE)
  expect_error(forest_reg(df = rd), msg("forest_reg"), fixed = TRUE)
  expect_error(forest_reg(uni = uni, multi = rd), msg("forest_reg"),
               fixed = TRUE)
})

test_that("case 26: the object does not look like a regression table", {
  rd <- rd_example(adjust_for = c("age", "lwt"), event_type = "harmful")
  expect_identical(class(rd), c("risk_difference", "gtregression"))
  expect_null(rd$approach)
  expect_false(any(c("exposure", "level", "estimate", "conf.low",
                     "conf.high", "p.value", "ref") %in%
                     names(rd$table_body)))
  expect_false(any(c("Characteristic", "is_header") %in%
                     names(rd$table_display)))
  expect_true(all(
    c("table", "table_display", "table_body", "footnotes", "analysis",
      "outcome", "exposure", "adjust_for", "event", "reference",
      "comparison", "event_type", "conf_level", "method", "n", "nnt",
      "model", "source", "format") %in% names(rd)
  ))
})

test_that("modify_table() applies the safe generic operations", {
  for (fmt in c("gt", "flextable")) {
    rd <- rd_example(fmt, adjust_for = c("age", "lwt"))
    notes <- if (fmt == "gt") rd_gt_notes else rd_flex_notes

    same <- modify_table(rd)
    expect_identical(same$table_display, rd$table_display)
    expect_identical(same$footnotes, rd$footnotes)
    expect_identical(class(same), class(rd))
    expect_equal(same$table_body, rd$table_body)

    out <- modify_table(
      rd,
      caption = "Table 2. Low birth weight by smoking",
      caveat = "Exploratory analysis.",
      variable_labels = c(smoking = "Maternal smoking", low = "Low birth weight",
                          lwt = "Maternal weight"),
      level_labels = list(smoking = c(Yes = "Smoker", No = "Non-smoker"),
                          low = c(`1` = "yes")),
      header_labels = c("p-value" = "P", "Events / N" = "n / N")
    )
    expect_s3_class(out, "risk_difference")
    expect_s3_class(out$table, if (fmt == "gt") "gt_tbl" else "flextable")
    expect_identical(
      names(out$table_display),
      c("Maternal smoking", "n / N", "Adjusted risk",
        "Adjusted risk difference (95% CI)", "P")
    )
    expect_identical(out$table_display[[1]], c("Non-smoker", "Smoker"))
    expect_identical(out$table_display[-1] |> unname(),
                     rd$table_display[-1] |> unname())
    expect_match(out$footnotes[1], "(Low birth weight = yes)", fixed = TRUE)
    expect_match(out$footnotes[1],
                 "Smoker \u2212 Non-smoker (groups of Maternal smoking)",
                 fixed = TRUE)
    expect_match(out$footnotes[2], "adjusted for age, Maternal weight.",
                 fixed = TRUE)
    expect_identical(out$footnotes[length(out$footnotes)],
                     "Exploratory analysis.")
    expect_identical(notes(out$table), out$footnotes)
    expect_identical(out$caption, "Table 2. Low birth weight by smoking")
    # numbers and resolved levels are untouched
    expect_equal(out$table_body, rd$table_body)
    expect_identical(c(out$reference, out$comparison, out$event),
                     c("No", "Yes", "1"))

    # repeated modification keeps earlier changes
    again <- modify_table(out, header_labels = c(P = "p"),
                          caveat = "Second note.",
                          remove_abbreviations = TRUE)
    expect_identical(names(again$table_display)[c(2, 5)], c("n / N", "p"))
    expect_identical(again$table_display[[1]], c("Non-smoker", "Smoker"))
    expect_identical(tail(again$footnotes, 2),
                     c("Exploratory analysis.", "Second note."))
    expect_false(any(grepl("^CI = confidence interval", again$footnotes)))
    expect_identical(again$caption, out$caption)
  }
  f <- modify_table(rd_example("flextable"), header_labels = c(Risk = "Risk (%)"))
  expect_identical(rd_flex_header(f$table)[3], "Risk (%)")
})

test_that("modify_table() refuses regression-specific options", {
  rd <- rd_example()
  for (args in list(
    list(remove_N = TRUE), list(remove_N_obs = TRUE),
    list(remove_adjustment_note = TRUE), list(bold_levels = TRUE),
    list(bold_labels = FALSE), list(italic_labels = TRUE),
    list(italic_levels = TRUE)
  )) {
    expect_error(
      do.call(modify_table, c(list(rd, caption = "x"), args)),
      paste0("`", names(args), "` is not supported for risk_difference\\(\\) ",
             "tables; nothing was changed")
    )
  }
  expect_error(modify_table(rd, remove_N = TRUE, remove_N_obs = TRUE),
               "`remove_N`, `remove_N_obs` are not supported")
  expect_error(modify_table(rd, variable_labels = c(age = "Age")),
               "can relabel only the variables of this table")
  expect_error(modify_table(rd, level_labels = list(smoking = c(Maybe = "x"))),
               "must be named by the displayed level")
  expect_error(modify_table(rd, level_labels = list(race = c(a = "x"))),
               "can relabel only the groups")
  expect_error(modify_table(rd, header_labels = c(Nope = "x")),
               "columns not found in the table")
  expect_error(modify_table(rd, header_labels = c(Risk = "Characteristic")),
               "non-empty text other than")
  expect_error(modify_table(rd, header_labels = c(Risk = "p-value")),
               "same header")
})

test_that("case 27: regression objects behave as before", {
  # Values produced by the unmodified package (base copy of 1.2.0).
  uni <- suppressMessages(
    uni_reg(data_birthwt, "low", c("smoke", "age"), approach = "logit",
            format = "gt")
  )
  expect_identical(class(uni), c("gtregression", "uni_reg", "gt_uni", "list"))
  expect_equal(uni$table_body$estimate, c(2.0219435737, 0.9501333439),
               tolerance = 1e-9)
  expect_identical(
    names(uni$table_display),
    c("Characteristic", "N", "OR (95% CI)", "p-value", "is_header")
  )
  expect_identical(uni$table_display[["OR (95% CI)"]],
                   c("2.02 (1.08\u20133.78)", "0.95 (0.89\u20131.01)"))
  expect_identical(uni$footnotes,
                   "Abbreviations: OR = Odds Ratio; CI = Confidence Interval.")

  mod <- modify_table(uni, caption = "C", caveat = "x")
  expect_identical(
    mod$footnotes,
    c("Abbreviations: OR = Odds Ratio; CI = Confidence Interval.", "x")
  )
  expect_identical(names(mod$table_display), names(uni$table_display))
  expect_s3_class(modify_table(uni, remove_N = TRUE), "uni_reg")

  multi <- suppressMessages(
    multi_reg(data_birthwt, "low", c("smoke", "age"), approach = "logit",
              format = "gt")
  )
  merged <- merge_tables(uni, multi)
  expect_s3_class(merged, "merged_table")
  expect_identical(
    merged$table_display[["Adjusted.OR..95..CI._p2"]],
    c("2.00 (1.06\u20133.75)", "0.95 (0.89\u20131.01)")
  )
  expect_s3_class(forest_df(uni), "data.frame")
  # suppressWarnings(): ggplot2 deprecation notice raised inside plot_reg()
  expect_s3_class(suppressWarnings(plot_reg(uni)), "ggplot")
})
