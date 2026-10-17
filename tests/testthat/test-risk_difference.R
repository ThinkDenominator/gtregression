# Qualification tests for risk_difference() (matrix cases 1-25 of the
# pre-implementation audit, section 14; cases 26-27 are in
# test-risk_difference-ecosystem.R).
#
# Provenance of hard-coded reference values (generated once, in scratch
# scripts outside the package; none of these packages is a dependency):
#  * Miettinen-Nurminen limits and p-values: ratesci 1.1.0,
#    scoreci(x1, n1, x2, n2, contrast = "RD", level = , skew = FALSE,
#            bcf = TRUE, cc = FALSE, precis = 10)
#  * Adjusted point estimates: risks 0.4.3 riskdiff(approach =
#    "margstd_delta") and beeca 0.2.0 get_marginal_effect(contrast = "diff").
#  * Influence-function standard errors: an independent stacked
#    estimating-equation sandwich with numerical (five-point) derivatives;
#    beeca 0.2.0 method "Ge" (type "HC0") and method "Ye" for comparison.
#  * Simulation-based cases (16-19) are checked here on deterministic
#    constructed data; the coverage simulations (4,000 replicates each) were
#    run outside the test suite and are recorded in the qualification report.

minus <- "\u2212"

rd_2x2 <- function(x1, n1, x0, n0) {
  data.frame(
    y = c(rep(1, x1), rep(0, n1 - x1), rep(1, x0), rep(0, n0 - x0)),
    g = rep(c(1, 0), c(n1, n0))
  )
}
rd_cells <- function(a, z, n, events) {
  data.frame(y = rep(c(1, 0), c(events, n - events)), a = a, z = z)
}
quiet_rd <- function(...) suppressMessages(risk_difference(...))
gt_cells <- function(tb) {
  out <- as.data.frame(tb[["_data"]], stringsAsFactors = FALSE)
  names(out) <- unlist(tb[["_boxhead"]]$column_label)
  out
}
flex_cells <- function(ft) {
  body <- as.data.frame(.flex_part_text(ft, "body"), stringsAsFactors = FALSE)
  names(body) <- unname(.flex_part_text(ft, "header")[1, ])
  body
}
flex_notes <- function(ft) unname(.flex_part_text(ft, "footer")[, 1])
gt_notes <- function(tb) unlist(tb[["_source_notes"]])

# x1, n1, x0, n0, level, lower, upper, p  (ratesci 1.1.0, see header)
mn_reference <- rbind(
  c(27, 152, 45, 150, 0.95, -0.21762941, -0.02636942, 1.273509e-02), # case 1
  c(27, 152, 45, 150, 0.90, -0.20232785, -0.04192442, 1.273509e-02),
  c(56, 70, 48, 80, 0.95, 0.05282971, 0.33817294, 8.258288e-03),
  c(40, 200, 30, 100, 0.95, -0.20904067, 0.00163153, 5.394870e-02), # case 2
  c(55, 500, 9, 50, 0.95, -0.20052129, 0.01826059, 1.414437e-01),
  c(3, 1000, 9, 1000, 0.95, -0.01436460, 0.00090930, 8.241611e-02), # case 3
  c(5, 56, 0, 29, 0.95, -0.03259656, 0.19333098, 9.917565e-02),     # case 4
  c(0, 10, 0, 20, 0.95, -0.16576023, 0.28438134, 1),                # case 5
  c(10, 10, 10, 10, 0.95, -0.28793394, 0.28793394, 1),
  c(10, 10, 0, 20, 0.95, 0.71561866, 1, 7.237830e-08),              # case 6
  c(0, 3, 3, 3, 0.90, -1, -0.29776700, 2.534732e-02),
  c(3, 3, 0, 2, 0.95, 0.02021833, 1, 4.550026e-02),                 # tiny
  c(2, 1000, 20, 50, 0.95, -0.53627767, -0.27399622, 7.019037e-82), # unequal
  c(30, 74, 29, 115, 0.95, 0.01722098, 0.28976494, 2.689321e-02),   # birthwt
  c(30, 74, 29, 115, 0.99, -0.02450913, 0.33135466, 2.689321e-02)
)

test_that("cases 1-6: Miettinen-Nurminen engine matches ratesci", {
  for (i in seq_len(nrow(mn_reference))) {
    k <- mn_reference[i, ]
    out <- .rd_mn(k[1], k[2], k[3], k[4], conf_level = k[5])
    expect_equal(out$rd, k[1] / k[2] - k[3] / k[4], tolerance = 1e-12)
    expect_lt(abs(out$low - k[6]), 1e-7)
    expect_lt(abs(out$high - k[7]), 1e-7)
    expect_equal(out$p_value, k[8], tolerance = 1e-5)
  }
})

test_that("case 1: ordinary balanced trial, crude table and object", {
  d <- rd_2x2(27, 152, 45, 150)
  expect_no_message(res <- risk_difference(d, y, g))
  expect_s3_class(res, c("risk_difference", "gtregression"), exact = TRUE)
  expect_s3_class(res$table, "gt_tbl")
  tb <- res$table_body
  expect_identical(
    names(tb),
    c("group", "events", "n", "risk", "adjusted_risk", "adjusted_risk_se",
      "rd", "rd_se", "rd_low", "rd_high", "p_value")
  )
  expect_identical(tb$group, c("0", "1"))
  expect_equal(tb$events, c(45, 27))
  expect_equal(tb$n, c(150, 152))
  expect_equal(tb$risk, c(45 / 150, 27 / 152))
  expect_equal(tb$rd, c(NA, 27 / 152 - 45 / 150))
  expect_lt(abs(tb$rd_low[2] - -0.21762941), 1e-7)
  expect_lt(abs(tb$rd_high[2] - -0.02636942), 1e-7)
  expect_equal(tb$p_value[2], 1.273509e-02, tolerance = 1e-5)
  expect_true(all(is.na(tb[1, c("rd", "rd_low", "rd_high", "p_value")])))
  expect_true(all(is.na(tb$adjusted_risk)))
  expect_true(all(is.na(tb$rd_se)))

  expect_identical(
    names(res$table_display),
    c("Group", "Events / N", "Risk", "Risk difference (95% CI)", "p-value")
  )
  expect_identical(res$table_display[[2]], c("45 / 150", "27 / 152"))
  expect_identical(res$table_display[[3]], c("30.0%", "17.8%"))
  expect_identical(
    res$table_display[[4]],
    c("Ref.", paste0(minus, "12.2 (", minus, "21.8 to ", minus, "2.6)"))
  )
  expect_identical(res$table_display[[5]], c("", "0.013"))
  expect_false(any(grepl("-", res$table_display[[4]], fixed = TRUE)))

  expect_identical(res$analysis, "crude")
  expect_identical(res$source, "risk_difference")
  expect_identical(res$format, "gt")
  expect_identical(c(res$event, res$reference, res$comparison),
                   c("1", "0", "1"))
  expect_null(res$nnt)
  expect_null(res$model)
  expect_null(res$adjust_for)
  expect_identical(res$n, list(supplied = 302L, analysed = 302L,
                               excluded = 0L))
  expect_match(res$method$interval, "Miettinen-Nurminen")
  expect_match(res$footnotes[1], "Risk = proportion with the event (y = 1)",
               fixed = TRUE)
  expect_match(res$footnotes[1],
               paste0("Risk difference = 1 ", minus, " 0"), fixed = TRUE)
  expect_match(res$footnotes[1], "in percentage points", fixed = TRUE)
  expect_match(res$footnotes[2], "Miettinen\u2013Nurminen score method",
               fixed = TRUE)
  expect_identical(res$footnotes[3], "N = 302; no observations excluded.")
  expect_identical(res$footnotes[4],
                   "CI = confidence interval; Ref. = reference group.")
  expect_false(any(grepl("effect", res$footnotes, ignore.case = TRUE)))
  expect_false(any(grepl("effect", names(res$table_display),
                         ignore.case = TRUE)))
  # regression column names must not be used
  expect_false(any(c("exposure", "level", "estimate", "conf.low", "conf.high",
                     "p.value", "ref") %in% names(tb)))
  expect_false(any(c("Characteristic", "is_header") %in%
                     names(res$table_display)))
})

test_that("case 2: unbalanced allocation and other confidence levels", {
  res <- risk_difference(rd_2x2(55, 500, 9, 50), y, g)
  expect_lt(abs(res$table_body$rd_low[2] - -0.20052129), 1e-7)
  expect_lt(abs(res$table_body$rd_high[2] - 0.01826059), 1e-7)
  res90 <- risk_difference(rd_2x2(27, 152, 45, 150), y, g, conf_level = 0.90)
  expect_lt(abs(res90$table_body$rd_low[2] - -0.20232785), 1e-7)
  expect_lt(abs(res90$table_body$rd_high[2] - -0.04192442), 1e-7)
  expect_identical(names(res90$table_display)[4], "Risk difference (90% CI)")
  expect_identical(res90$conf_level, 0.90)
  expect_identical(
    names(risk_difference(rd_2x2(27, 152, 45, 150), y, g,
                          conf_level = 0.975)$table_display)[4],
    "Risk difference (97.5% CI)"
  )
})

test_that("case 3: rare events are shown with two decimals", {
  res <- risk_difference(rd_2x2(3, 1000, 9, 1000), y, g)
  expect_identical(res$table_display[[3]], c("0.90%", "0.30%"))
  expect_identical(
    res$table_display[[4]],
    c("Ref.", paste0(minus, "0.60 (", minus, "1.44 to 0.09)"))
  )
  expect_identical(res$table_display[[5]], c("", "0.082"))
  # one decimal otherwise
  expect_identical(
    risk_difference(rd_2x2(30, 100, 20, 100), y, g)$table_display[[3]],
    c("20.0%", "30.0%")
  )
  # A risk of exactly 0% does not trigger two decimals.
  zero_one <- risk_difference(rd_2x2(0, 40, 6, 38), y, g)
  expect_identical(zero_one$table_display[[3]], c("15.8%", "0.0%"))
  expect_identical(
    zero_one$table_display[[4]][2],
    paste0(minus, "15.8 (", minus, "30.5 to ", minus, "6.2)")
  )
  zero_both <- risk_difference(rd_2x2(0, 40, 0, 38), y, g)
  expect_identical(zero_both$table_display[[3]], c("0.0%", "0.0%"))
  expect_identical(
    zero_both$table_display[[4]][2],
    paste0("0.0 (", minus, "9.3 to 8.9)")
  )
  # A rare non-zero risk beside a zero risk still gives two decimals.
  expect_identical(
    risk_difference(rd_2x2(0, 500, 3, 500), y, g)$table_display[[3]],
    c("0.60%", "0.00%")
  )
})

test_that("cases 4-6: boundary tables through the public function", {
  one <- risk_difference(rd_2x2(5, 56, 0, 29), y, g)
  expect_equal(one$table_body$risk, c(0, 5 / 56))
  expect_lt(abs(one$table_body$rd_low[2] - -0.03259656), 1e-7)
  expect_lt(abs(one$table_body$rd_high[2] - 0.19333098), 1e-7)

  both <- risk_difference(rd_2x2(0, 10, 0, 20), y, g)
  expect_identical(both$table_body$rd[2], 0)
  expect_identical(both$table_body$p_value[2], 1)
  expect_lt(abs(both$table_body$rd_low[2] - -0.16576023), 1e-7)
  expect_lt(abs(both$table_body$rd_high[2] - 0.28438134), 1e-7)
  expect_identical(both$table_display[[5]][2], "1.000")

  all_events <- risk_difference(rd_2x2(10, 10, 10, 10), y, g)
  expect_identical(all_events$table_body$p_value[2], 1)
  expect_lt(abs(all_events$table_body$rd_high[2] - 0.28793394), 1e-7)

  full <- risk_difference(rd_2x2(10, 10, 0, 20), y, g)
  expect_identical(full$table_body$rd[2], 1)
  expect_identical(full$table_body$rd_high[2], 1)
  expect_lt(abs(full$table_body$rd_low[2] - 0.71561866), 1e-7)
  expect_identical(full$table_display[[5]][2], "<0.001")
  expect_identical(.rd_mn(0, 3, 3, 3)$low, -1)
})

test_that("case 7: score-test p-value equals Pearson chi-square x (N-1)/N", {
  for (k in list(c(27, 152, 45, 150), c(40, 200, 30, 100), c(5, 56, 0, 29),
                 c(3, 1000, 9, 1000), c(3, 3, 0, 2), c(2, 1000, 20, 50))) {
    m <- matrix(c(k[1], k[2] - k[1], k[3], k[4] - k[3]), 2)
    n <- sum(m)
    chi <- suppressWarnings(stats::chisq.test(m, correct = FALSE))$statistic
    expect_equal(
      .rd_mn(k[1], k[2], k[3], k[4])$p_value,
      unname(stats::pchisq(chi * (n - 1) / n, 1, lower.tail = FALSE)),
      tolerance = 1e-10
    )
  }
  # prop.test() is a different method
  expect_gt(
    abs(.rd_mn(27, 152, 45, 150)$p_value -
          stats::prop.test(c(27, 45), c(152, 150), correct = FALSE)$p.value),
    1e-4
  )
})

test_that("case 8: interval excludes 0 exactly when p < alpha", {
  checked <- 0L
  incoherent <- 0L
  out_of_order <- 0L
  for (level in c(0.90, 0.95)) {
    alpha <- 1 - level
    for (n1 in c(4, 12, 40)) for (n0 in c(5, 30, 90)) {
      for (x1 in unique(round(seq(0, n1, length.out = 9)))) {
        for (x0 in unique(round(seq(0, n0, length.out = 9)))) {
          r <- .rd_mn(x1, n1, x0, n0, level)
          checked <- checked + 1L
          if (r$low > r$rd + 1e-9 || r$high < r$rd - 1e-9 ||
              r$low < -1 || r$high > 1) {
            out_of_order <- out_of_order + 1L
          }
          if (abs(r$p_value - alpha) > 1e-7 &&
              !identical(r$low > 0 || r$high < 0, r$p_value < alpha)) {
            incoherent <- incoherent + 1L
          }
        }
      }
    }
  }
  expect_gt(checked, 900L)
  expect_identical(incoherent, 0L)
  expect_identical(out_of_order, 0L)
})

test_that("case 9: one complete-case population with counts", {
  d <- data_birthwt
  d$low[c(1, 2)] <- NA
  d$smoke[c(2, 3, 4)] <- NA
  d$age[c(4, 5, 6, 7)] <- NA
  crude <- risk_difference(d, low, smoke)
  # rows 1-4 lack outcome or exposure
  expect_identical(crude$n, list(supplied = 189L, analysed = 185L,
                                 excluded = 4L))
  expect_equal(sum(crude$table_body$n), 185)
  expect_identical(
    crude$footnotes[3],
    "N = 185; 4 observations excluded because of missing values."
  )
  adjusted <- risk_difference(d, low, smoke, adjust_for = c(age, lwt))
  # rows 1-7 lack outcome, exposure or a covariate
  expect_identical(adjusted$n, list(supplied = 189L, analysed = 182L,
                                    excluded = 7L))
  expect_equal(sum(adjusted$table_body$n), 182)
  expect_equal(stats::nobs(adjusted$model), 182)
  expect_true(any(grepl("N = 182; 7 observations excluded",
                        adjusted$footnotes, fixed = TRUE)))
  d1 <- data_birthwt
  d1$low[1] <- NA
  expect_true(any(grepl("1 observation excluded",
                        risk_difference(d1, low, smoke)$footnotes,
                        fixed = TRUE)))
  d1$low <- NA
  expect_error(risk_difference(d1, low, smoke), "No complete observations")
})

test_that("case 10: reversing the reference flips signs and swaps risks", {
  for (adj in list(NULL, c("age", "lwt"))) {
    a <- risk_difference(data_birthwt, "low", "smoke", adjust_for = adj)
    b <- risk_difference(data_birthwt, "low", "smoke", adjust_for = adj,
                         reference = 1)
    expect_identical(c(b$reference, b$comparison), c("1", "0"))
    expect_identical(b$table_body$group, rev(a$table_body$group))
    expect_equal(b$table_body$risk, rev(a$table_body$risk))
    expect_equal(b$table_body$events, rev(a$table_body$events))
    expect_equal(b$table_body$rd[2], -a$table_body$rd[2], tolerance = 1e-10)
    expect_equal(b$table_body$rd_low[2], -a$table_body$rd_high[2],
                 tolerance = 1e-8)
    expect_equal(b$table_body$rd_high[2], -a$table_body$rd_low[2],
                 tolerance = 1e-8)
    expect_equal(b$table_body$p_value[2], a$table_body$p_value[2],
                 tolerance = 1e-8)
    if (!is.null(adj)) {
      expect_equal(b$table_body$adjusted_risk, rev(a$table_body$adjusted_risk),
                   tolerance = 1e-10)
      expect_equal(b$table_body$adjusted_risk_se,
                   rev(a$table_body$adjusted_risk_se), tolerance = 1e-8)
      expect_equal(b$table_body$rd_se[2], a$table_body$rd_se[2],
                   tolerance = 1e-8)
    }
    expect_match(b$footnotes[1], paste0("= 0 ", minus, " 1"), fixed = TRUE)
  }
})

test_that("case 11: reversing the event gives 1 - risk and flips the sign", {
  for (adj in list(NULL, c("age", "lwt"))) {
    a <- risk_difference(data_birthwt, "low", "smoke", adjust_for = adj)
    b <- risk_difference(data_birthwt, "low", "smoke", adjust_for = adj,
                         event = 0)
    expect_identical(b$event, "0")
    expect_equal(b$table_body$risk, 1 - a$table_body$risk)
    expect_equal(b$table_body$events, a$table_body$n - a$table_body$events)
    expect_equal(b$table_body$rd[2], -a$table_body$rd[2], tolerance = 1e-10)
    expect_equal(b$table_body$rd_low[2], -a$table_body$rd_high[2],
                 tolerance = 1e-8)
    expect_equal(b$table_body$rd_high[2], -a$table_body$rd_low[2],
                 tolerance = 1e-8)
    expect_equal(b$table_body$p_value[2], a$table_body$p_value[2],
                 tolerance = 1e-8)
    if (!is.null(adj)) {
      expect_equal(b$table_body$adjusted_risk,
                   1 - a$table_body$adjusted_risk, tolerance = 1e-10)
    }
    expect_match(b$footnotes[1], "(low = 0)", fixed = TRUE)
  }
})

test_that("case 12: outcome types resolve the event with the right messages", {
  base <- rd_2x2(27, 152, 45, 150)
  ref <- risk_difference(base, y, g)$table_body

  d <- base
  d$y <- d$y == 1
  expect_no_message(r <- risk_difference(d, y, g))
  expect_identical(r$event, "TRUE")
  expect_equal(r$table_body$risk, ref$risk)

  d$y <- as.integer(base$y)
  expect_no_message(r <- risk_difference(d, y, g))
  expect_identical(r$event, "1")

  d$y <- base$y + 1
  expect_message(r <- risk_difference(d, y, g), "the event is '2'")
  expect_identical(r$event, "2")
  expect_equal(r$table_body$risk, ref$risk)

  d$y <- factor(ifelse(base$y == 1, "Dead", "Alive"),
                levels = c("Dead", "Alive"))
  expect_message(r <- risk_difference(d, y, g), "the event is 'Alive'")
  expect_identical(r$event, "Alive")
  expect_equal(r$table_body$risk, 1 - ref$risk)
  expect_no_message(r <- risk_difference(d, y, g, event = "Dead"))
  expect_identical(r$event, "Dead")
  expect_equal(r$table_body$risk, ref$risk)

  d$y <- ifelse(base$y == 1, "yes", "no")
  expect_message(r <- risk_difference(d, y, g), "the event is 'yes'")
  expect_equal(r$table_body$risk, ref$risk)

  d$y <- ifelse(base$y == 1, 10, 5)
  expect_error(risk_difference(d, y, g), "Use 0/1 or 1/2 coding")
  expect_no_message(r <- risk_difference(d, y, g, event = 10))
  expect_equal(r$table_body$risk, ref$risk)
  expect_error(risk_difference(d, y, g, event = 7),
               "not an observed value of `y`. Observed values: '5', '10'")

  # one observed value: unambiguous for logical, 0/1 and two-level factors
  d$y <- factor(rep("no", nrow(d)), levels = c("no", "yes"))
  r <- quiet_rd(d, y, g)
  expect_identical(r$event, "yes")
  expect_equal(r$table_body$events, c(0, 0))
  d$y <- rep("no", nrow(d))
  expect_error(risk_difference(d, y, g), "only one observed value")
})

test_that("case 12: exposure types resolve the reference with messages", {
  base <- rd_2x2(27, 152, 45, 150)
  ref <- risk_difference(base, y, g)$table_body

  d <- base
  d$g <- base$g == 1
  expect_no_message(r <- risk_difference(d, y, g))
  expect_identical(c(r$reference, r$comparison), c("FALSE", "TRUE"))
  expect_equal(r$table_body$rd, ref$rd)

  d$g <- base$g + 1
  expect_message(r <- risk_difference(d, y, g), "'2' minus '1'")
  expect_equal(r$table_body$rd, ref$rd)

  d$g <- factor(ifelse(base$g == 1, "Drug", "Placebo"),
                levels = c("Placebo", "Drug", "Unused"))
  expect_message(r <- risk_difference(d, y, g),
                 "'Drug' minus 'Placebo' \\(reference group: 'Placebo'\\)")
  expect_identical(r$table_display[[1]], c("Placebo", "Drug"))
  expect_equal(r$table_body$rd, ref$rd)
  expect_no_message(r <- risk_difference(d, y, g, reference = "Drug"))
  expect_identical(r$table_display[[1]], c("Drug", "Placebo"))
  expect_equal(r$table_body$rd[2], -ref$rd[2])
  expect_error(
    risk_difference(d, y, g, reference = "Unused"),
    "not an observed group of `g`. Observed groups: 'Placebo', 'Drug'"
  )

  d$g <- ifelse(base$g == 1, "b", "a")
  expect_message(r <- risk_difference(d, y, g), "'b' minus 'a'")
  expect_equal(r$table_body$rd, ref$rd)

  # variable label becomes the group header and appears in the footnote
  attr(d$g, "label") <- "Treatment arm"
  attr(d$y, "label") <- "Death"
  r <- quiet_rd(d, y, g)
  expect_identical(names(r$table_display)[1], "Treatment arm")
  expect_match(r$footnotes[1], "(Death = 1)", fixed = TRUE)
  expect_match(r$footnotes[1], "(groups of Treatment arm)", fixed = TRUE)
})

test_that("case 12: value-labelled variables are converted by their labels", {
  skip_if_not_installed("haven")
  base <- rd_2x2(27, 152, 45, 150)
  d <- base
  d$y <- haven::labelled(base$y + 1, labels = c(Alive = 1, Dead = 2),
                         label = "Vital status")
  d$g <- haven::labelled(base$g, labels = c(Placebo = 0, Drug = 1))
  expect_message(
    expect_message(r <- risk_difference(d, y, g), "the event is 'Dead'"),
    "'Drug' minus 'Placebo'"
  )
  expect_identical(c(r$event, r$reference, r$comparison),
                   c("Dead", "Placebo", "Drug"))
  expect_equal(r$table_body$rd[2], 27 / 152 - 45 / 150)
  expect_match(r$footnotes[1], "(Vital status = Dead)", fixed = TRUE)
})

test_that("bare and quoted names, and names passed through a wrapper", {
  a <- risk_difference(data_birthwt, low, smoke, adjust_for = c(age, lwt))
  b <- risk_difference(data_birthwt, "low", "smoke",
                       adjust_for = c("age", "lwt"))
  expect_equal(a$table_body, b$table_body)
  expect_identical(a$adjust_for, c("age", "lwt"))
  wrapper <- function(dat, out, exp, adj) {
    risk_difference(dat, out, exp, adjust_for = adj, format = "flextable")
  }
  w <- wrapper(data_birthwt, "low", "smoke", c("age", "lwt"))
  expect_equal(w$table_body, b$table_body)
  expect_s3_class(w$table, "flextable")
  expect_error(risk_difference(data_birthwt, nope, smoke), "not found")
  expect_error(risk_difference(data_birthwt, low, low), "different variables")
  expect_error(risk_difference(data_birthwt, low, smoke, adjust_for = smoke),
               "must not contain the outcome or the exposure")
  expect_error(risk_difference(data_birthwt, low, smoke, conf_level = 95),
               "between 0 and 1")
  expect_error(risk_difference(data_birthwt, low, smoke, event_type = "bad"),
               "event_type")
  expect_error(risk_difference(data_birthwt, low, smoke, format = "pdf"),
               "format")
  expect_error(risk_difference(data_birthwt, low, smoke, theme = "bogus"),
               "Invalid `theme`")
  expect_s3_class(
    risk_difference(data_birthwt, low, smoke, theme = "striped",
                    format = "flextable")$table,
    "flextable"
  )
})

test_that("case 13: no-covariate standardisation equals the observed RD", {
  d <- data_birthwt
  out <- .rd_standardise(d$low, d$smoke, NULL)
  p1 <- mean(d$low[d$smoke == 1])
  p0 <- mean(d$low[d$smoke == 0])
  expect_equal(out$rd, p1 - p0, tolerance = 1e-10)
  expect_equal(c(out$risk1, out$risk0), c(p1, p0), tolerance = 1e-10)
  # influence-function SE reduces to the unpooled binomial SE
  expect_equal(out$se, sqrt(p1 * (1 - p1) / 74 + p0 * (1 - p0) / 115),
               tolerance = 1e-8)
})

test_that("case 14: adjusted risks and RD match risks and beeca", {
  res <- risk_difference(data_birthwt, low, smoke,
                         adjust_for = c(age, lwt, ht))
  tb <- res$table_body
  # risks 0.4.3 margstd_delta and beeca 0.2.0 both give 0.1364323025727;
  # beeca counterfactual means 0.3934855203149 / 0.2570532177422
  expect_equal(tb$rd[2], 0.1364323025727, tolerance = 1e-8)
  expect_equal(tb$adjusted_risk, c(0.2570532177422, 0.3934855203149),
               tolerance = 1e-8)
  expect_equal(tb$risk, c(29 / 115, 30 / 74))
  expect_equal(tb$events, c(29, 30))
  expect_identical(res$analysis, "adjusted")
  expect_s3_class(res$model, "glm")
  expect_identical(res$method$variance, "influence-function standard error")
  expect_identical(
    names(res$table_display),
    c("Group", "Events / N", "Adjusted risk",
      "Adjusted risk difference (95% CI)", "p-value")
  )
  expect_identical(res$table_display[[3]], c("25.7%", "39.3%"))
  expect_identical(res$table_display[[4]], c("Ref.", "13.6 (0.3 to 27.0)"))
  expect_identical(res$table_display[[5]], c("", "0.046"))
  expect_match(res$footnotes[1], "Adjusted risk difference = 1 \u2212 0",
               fixed = TRUE)
  expect_match(res$footnotes[1], "(low = 1)", fixed = TRUE)
  expect_identical(
    res$footnotes[2],
    paste0("Adjusted risks are standardised to the covariate distribution of ",
           "the analysis population using logistic regression adjusted for ",
           "age, lwt, ht.")
  )
  expect_identical(
    res$footnotes[3],
    "Confidence interval and p-value: Wald, influence-function standard error."
  )
  expect_false(any(grepl("effect", res$footnotes, ignore.case = TRUE)))

  # factor and character covariates
  d <- data_birthwt
  d$race_f <- factor(d$race)
  d$race_c <- as.character(d$race)
  f <- risk_difference(d, low, smoke, adjust_for = race_f)
  ch <- risk_difference(d, low, smoke, adjust_for = race_c)
  expect_equal(f$table_body$rd, ch$table_body$rd)
  expect_equal(length(stats::coef(f$model)), 4)
})

test_that("case 15: influence-function SE matches the independent sandwich", {
  tb <- risk_difference(data_birthwt, low, smoke,
                        adjust_for = c(age, lwt, ht))$table_body
  # independent numerical stacked-estimating-equation sandwich
  expect_equal(tb$rd_se[2], 0.0683217236907, tolerance = 1e-8)
  expect_equal(tb$adjusted_risk_se, c(0.0406429261793, 0.0559199228692),
               tolerance = 1e-8)
  # beeca 0.2.0: Ge/HC0 0.0685218678743, Ye 0.0684851259995 (close, not
  # identical: different variance estimators)
  expect_lt(abs(tb$rd_se[2] / 0.0685218678743 - 1), 0.01)
  expect_lt(abs(tb$rd_se[2] / 0.0684851259995 - 1), 0.01)
})

test_that("case 16: strong prognostic covariate gives a smaller SE", {
  # logit model holds exactly: odds 1/9, x2.25 for the group, x81 for z
  d <- rbind(rd_cells(0, 0, 100, 10), rd_cells(1, 0, 100, 20),
             rd_cells(0, 1, 100, 90), rd_cells(1, 1, 85, 81))
  adj <- risk_difference(d, y, a, adjust_for = z)
  crude <- risk_difference(d, y, a)
  # closed form (200 * 0.1 + 185 * (81/85 - 0.9)) / 385; risks and beeca agree
  expect_equal(adj$table_body$rd[2], 0.0773873185638, tolerance = 1e-8)
  # independent numerical sandwich
  expect_equal(adj$table_body$rd_se[2], 0.0317128355353, tolerance = 1e-7)
  unadjusted_se <- .rd_standardise(d$y, d$a, NULL)$se
  expect_lt(adj$table_body$rd_se[2], 0.7 * unadjusted_se)
  expect_lt(
    adj$table_body$rd_high[2] - adj$table_body$rd_low[2],
    crude$table_body$rd_high[2] - crude$table_body$rd_low[2]
  )
})

test_that("case 17: baseline imbalance moves the crude RD as expected", {
  # the comparison group has more of the high-risk stratum (z = 1)
  d <- rbind(rd_cells(0, 0, 120, 24), rd_cells(1, 0, 90, 30),
             rd_cells(0, 1, 80, 40), rd_cells(1, 1, 120, 80))
  adj <- risk_difference(d, y, a, adjust_for = z)$table_body
  crude <- risk_difference(d, y, a)$table_body
  expect_equal(crude$rd[2], 110 / 210 - 64 / 200)
  # closed form (210 * (1/3 - 1/5) + 200 * (2/3 - 1/2)) / 410
  expect_equal(adj$rd[2], 0.149593495935, tolerance = 1e-8)
  expect_gt(crude$rd[2], adj$rd[2])
  expect_equal(adj$rd_se[2], 0.0467148089301, tolerance = 1e-7)
})

test_that("case 18: confounding - adjusted recovers the standardised RD", {
  d <- rbind(rd_cells(0, 0, 300, 60), rd_cells(1, 0, 90, 30),
             rd_cells(0, 1, 100, 50), rd_cells(1, 1, 300, 200))
  adj <- risk_difference(d, y, a, adjust_for = z)$table_body
  crude <- risk_difference(d, y, a)$table_body
  truth <- (390 * (1 / 3 - 1 / 5) + 400 * (2 / 3 - 1 / 2)) / 790
  expect_equal(adj$rd[2], truth, tolerance = 1e-8)
  expect_equal(adj$adjusted_risk,
               c((390 / 5 + 400 / 2) / 790, (390 / 3 + 400 * 2 / 3) / 790),
               tolerance = 1e-8)
  expect_equal(crude$rd[2], 230 / 390 - 110 / 400)
  expect_gt(crude$rd[2] - truth, 0.15)
  expect_true(crude$rd_low[2] > truth)
  expect_true(adj$rd_low[2] < truth && adj$rd_high[2] > truth)
  expect_equal(adj$rd_se[2], 0.0395191870588, tolerance = 1e-7)
})

test_that("case 19: misspecified working model with balanced covariates", {
  # Covariate distribution identical in both groups; outcome depends on z^2
  # and on a group-by-z interaction, the working model has main effects only.
  # With exact balance the standardised RD must equal the observed RD.
  g <- expand.grid(j = 1:40, r = 1:3, a = 0:1)
  z <- seq(-2, 2, length.out = 40)[g$j]
  u <- ((g$j * 3 + g$r * 7 + g$a * 5) * 0.6180339887) %% 1
  d <- data.frame(
    y = as.integer(u < stats::plogis(-1.2 + 0.6 * z^2 + g$a - 0.9 * g$a * z)),
    a = g$a, z = z
  )
  adj <- risk_difference(d, y, a, adjust_for = z)$table_body
  expect_equal(adj$rd[2], adj$risk[2] - adj$risk[1], tolerance = 1e-8)
  expect_equal(adj$adjusted_risk, adj$risk, tolerance = 1e-8)
  # independent numerical sandwich 0.0625649251771; beeca Ge/HC0
  # 0.0625492018136, Ye 0.0628234955289
  expect_equal(adj$rd_se[2], 0.0625649251771, tolerance = 1e-7)
})

test_that("case 20: Wald interval and p-value follow from RD and SE", {
  excl <- risk_difference(data_birthwt, low, smoke,
                          adjust_for = c(age, lwt, ht))$table_body
  expect_equal(excl$rd_low[2], excl$rd[2] - stats::qnorm(0.975) * excl$rd_se[2])
  expect_equal(excl$rd_high[2],
               excl$rd[2] + stats::qnorm(0.975) * excl$rd_se[2])
  expect_equal(excl$p_value[2],
               2 * stats::pnorm(-abs(excl$rd[2] / excl$rd_se[2])))
  expect_gt(excl$rd_low[2], 0)
  expect_lt(excl$p_value[2], 0.05)

  cross <- risk_difference(data_birthwt, low, ht, adjust_for = c(age, smoke),
                           conf_level = 0.99)$table_body
  z <- stats::qnorm(0.995)
  expect_equal(cross$rd_low[2], cross$rd[2] - z * cross$rd_se[2])
  expect_equal(cross$rd_high[2], cross$rd[2] + z * cross$rd_se[2])
  expect_true(cross$rd_low[2] < 0 && cross$rd_high[2] > 0)
  expect_gt(cross$p_value[2], 0.01)
  # limits are clipped to [-1, 1]
  wide <- .rd_standardise(c(1, 1, 1, 0, 1, 0, 0, 1), c(1, 1, 1, 1, 0, 0, 0, 0),
                          NULL, conf_level = 0.9999)
  expect_identical(wide$low, -1)
  expect_identical(wide$high, 1)
})

test_that("case 21: NNTB / NNTH assignment follows the sign table", {
  expect_identical(.rd_nnt(-0.1, -0.2, -0.05, "harmful")$type, "NNTB")
  expect_identical(.rd_nnt(0.1, 0.05, 0.2, "harmful")$type, "NNTH")
  expect_identical(.rd_nnt(-0.1, -0.2, -0.05, "beneficial")$type, "NNTH")
  expect_identical(.rd_nnt(0.1, 0.05, 0.2, "beneficial")$type, "NNTB")

  d <- rd_2x2(27, 152, 45, 150)
  harm <- risk_difference(d, y, g, event_type = "harmful")
  benefit <- risk_difference(d, y, g, event_type = beneficial)
  expect_identical(harm$nnt$type, "NNTB")
  expect_identical(benefit$nnt$type, "NNTH")
  expect_identical(harm$event_type, "harmful")
  expect_identical(names(harm$table_display)[6], "NNT (95% CI)")
  expect_identical(harm$table_display[[6]], c("", "NNTB 9 (5 to 38)"))
  expect_identical(benefit$table_display[[6]], c("", "NNTH 9 (5 to 38)"))
  expect_true(any(grepl("follow-up period", harm$footnotes, fixed = TRUE)))
  expect_true(any(grepl("event classified as harmful", harm$footnotes,
                        fixed = TRUE)))
  expect_true(any(grepl("NNTB = number needed to treat", harm$footnotes,
                        fixed = TRUE)))
  # no NNT column or footnote without event_type
  plain <- risk_difference(d, y, g)
  expect_false(any(grepl("NNT", c(names(plain$table_display),
                                  plain$footnotes))))
})

test_that("case 22: NNT estimate, interval segments and display strings", {
  inf <- "\u221e"
  # ordinary: reciprocals by hand, unrounded storage, rounded up for display
  a <- .rd_nnt(-0.12, -0.21, -0.03, "harmful")
  expect_equal(a$estimate, 1 / 0.12)
  expect_false(a$crosses_infinity)
  expect_equal(a$segments,
               data.frame(type = "NNTB", from = 1 / 0.21, to = 1 / 0.03,
                          stringsAsFactors = FALSE))
  expect_identical(.rd_fmt_nnt(a), "NNTB 9 (5 to 34)")
  # exact reciprocals are not pushed up by floating point
  expect_identical(.rd_fmt_nnt(.rd_nnt(0.3 - 0.2, 0.05, 0.25, "harmful")),
                   "NNTH 10 (4 to 20)")

  # Altman (1998, BMJ 317:1309) example: ARR 10%, 95% CI -5% to 25% gives
  # NNTB 10 with an interval from NNTB 4 through infinity to NNTH 20
  b <- .rd_nnt(-0.10, -0.25, 0.05, "harmful")
  expect_true(b$crosses_infinity)
  expect_identical(b$type, "NNTB")
  expect_equal(b$estimate, 10)
  expect_identical(b$segments$type, c("NNTB", "NNTH"))
  expect_equal(b$segments$from, c(4, 20))
  expect_identical(b$segments$to, c(Inf, Inf))
  expect_identical(.rd_fmt_nnt(b),
                   paste0("NNTB 10 (NNTB 4 to ", inf, " to NNTH 20)"))
  # estimate on the harm side: harm segment first
  h <- .rd_nnt(0.02, -0.05, 0.10, "harmful")
  expect_identical(h$segments$type, c("NNTH", "NNTB"))
  expect_identical(.rd_fmt_nnt(h),
                   paste0("NNTH 50 (NNTH 10 to ", inf, " to NNTB 20)"))

  # interval touching 0 at one limit
  t0 <- .rd_nnt(-0.12, -0.21, 0, "harmful")
  expect_false(t0$crosses_infinity)
  expect_equal(t0$segments$from, 1 / 0.21)
  expect_identical(t0$segments$to, Inf)
  expect_identical(.rd_fmt_nnt(t0), paste0("NNTB 9 (5 to ", inf, ")"))

  # RD exactly 0: infinite estimate without a type
  z <- .rd_nnt(0, -0.125, 1 / 15, "harmful")
  expect_identical(z$type, NA_character_)
  expect_identical(z$estimate, Inf)
  expect_true(z$crosses_infinity)
  expect_identical(.rd_fmt_nnt(z),
                   paste0(inf, " (NNTB 8 to ", inf, " to NNTH 15)"))

  # through the public function: crossing interval, stored as segments
  res <- risk_difference(rd_2x2(3, 1000, 9, 1000), y, g,
                         event_type = "harmful")
  expect_true(res$nnt$crosses_infinity)
  expect_equal(res$nnt$estimate, 1 / 0.006, tolerance = 1e-10)
  expect_equal(res$nnt$segments$from,
               c(1 / abs(res$table_body$rd_low[2]),
                 1 / res$table_body$rd_high[2]))
  expect_identical(
    res$table_display[[6]][2],
    paste0("NNTB 167 (NNTB 70 to ", inf, " to NNTH 1,100)")
  )
  zero <- risk_difference(rd_2x2(0, 10, 0, 20), y, g, event_type = "harmful")
  expect_identical(zero$nnt$type, NA_character_)
  expect_identical(
    zero$table_display[[6]][2],
    paste0(inf, " (NNTB 7 to ", inf, " to NNTH 4)")
  )
  # adjusted analysis with NNT
  adj <- risk_difference(data_birthwt, low, smoke,
                         adjust_for = c(age, lwt, ht), event_type = "harmful")
  expect_identical(adj$nnt$type, "NNTH")
  expect_equal(adj$nnt$estimate, 1 / adj$table_body$rd[2])
  expect_identical(adj$table_display[[6]], c("", "NNTH 8 (4 to 397)"))
})

test_that("case 23: non-estimable adjusted models stop with clear errors", {
  d <- data_birthwt
  crude_hint <- "Consider the crude analysis"

  # complete separation by a covariate (low is defined by bwt < 2500)
  expect_error(risk_difference(d, low, smoke, adjust_for = bwt),
               "fitted probabilities of 0 or 1.*Consider the crude analysis")
  # quasi-complete separation
  d$very_low <- as.integer(d$bwt < 2000)
  expect_error(risk_difference(d, low, smoke, adjust_for = very_low),
               "separation")
  # no events in one group
  z <- rd_2x2(5, 56, 0, 29)
  z$age <- rep(c(30, 40, 50, 60, 35), length.out = nrow(z))
  expect_error(risk_difference(z, y, g, adjust_for = age),
               "one group has no events.*Consider the crude analysis")
  expect_s3_class(risk_difference(z, y, g), "risk_difference")
  # constant outcome
  expect_error(
    risk_difference(d[d$low == 0, ], low, smoke, adjust_for = age),
    "outcome is constant \\(no events\\)"
  )
  # aliased adjustment variables are named
  d$constant <- 1
  expect_error(risk_difference(d, low, smoke, adjust_for = c(age, constant)),
               "adjustment variable `constant` is constant")
  d$age_months <- d$age * 12
  expect_error(
    risk_difference(d, low, smoke, adjust_for = c(age, age_months)),
    "aliased in the logistic model.*: age_months\\."
  )
  # aliased exposure
  d$smoker <- d$smoke
  expect_error(risk_difference(d, low, smoke, adjust_for = c(age, smoker)),
               "the exposure is aliased")
  expect_error(risk_difference(d, low, smoke, adjust_for = smoker),
               crude_hint)

  # non-convergence: a genuinely non-converged fit handed to the check used
  # by the engine
  md <- data.frame(.rd_y = d$low, .rd_a = d$smoke, lwt = d$lwt)
  fit <- suppressWarnings(stats::glm(
    .rd_y ~ .rd_a + lwt, family = stats::binomial(), data = md,
    control = stats::glm.control(maxit = 1)
  ))
  expect_false(fit$converged)
  expect_error(.rd_check_logistic(fit, character(0), " Hint."),
               "did not converge\\. Hint\\.")
  expect_error(
    .rd_check_logistic(
      stats::glm(.rd_y ~ .rd_a + lwt, family = stats::binomial(), data = md),
      "glm.fit: algorithm did not converge"
    ),
    "did not converge"
  )
  ok <- stats::glm(.rd_y ~ .rd_a + lwt, family = stats::binomial(), data = md)
  expect_true(.rd_check_logistic(ok))
})

test_that("case 24: version-1 limits are named in the errors", {
  d <- data_birthwt
  expect_error(risk_difference(d, low, race),
               "more than two groups are not supported.*version 1 limit")
  d$race_f <- factor(d$race, labels = c("White", "Black", "Other"))
  expect_error(
    risk_difference(d, low, race_f),
    "has 3 observed groups \\(White, Black, Other\\).*exactly two groups"
  )
  expect_error(risk_difference(d, low, age),
               "Continuous exposures.*version 1 limit")
  expect_error(risk_difference(d, race, smoke),
               "has 3 observed values \\(1, 2, 3\\).*binary outcome only")
  expect_error(risk_difference(d, bwt, smoke),
               "binary outcome only \\(version 1 limit\\)")
  expect_error(risk_difference(d[d$smoke == 1, ], low, smoke),
               "only one observed group")
  # two groups left after exclusions are accepted
  d$race_f[d$race_f == "Other"] <- NA
  r <- quiet_rd(d, low, race_f)
  expect_identical(r$table_body$group, c("White", "Black"))
  expect_identical(r$n$excluded, 67L)
})

test_that("case 25: gt and flextable show identical cells and footnotes", {
  specs <- list(
    list(adjust_for = NULL, event_type = NULL),
    list(adjust_for = c("age", "lwt"), event_type = "harmful")
  )
  for (s in specs) {
    g <- risk_difference(data_birthwt, "low", "smoke",
                         adjust_for = s$adjust_for,
                         event_type = s$event_type, format = "gt")
    f <- risk_difference(data_birthwt, "low", "smoke",
                         adjust_for = s$adjust_for,
                         event_type = s$event_type, format = "flextable")
    expect_s3_class(g$table, "gt_tbl")
    expect_s3_class(f$table, "flextable")
    expect_identical(g$format, "gt")
    expect_identical(f$format, "flextable")
    expect_identical(g$table_display, f$table_display)
    expect_identical(g$footnotes, f$footnotes)
    expect_equal(g$table_body, f$table_body)
    # what is actually rendered
    expect_identical(gt_cells(g$table), g$table_display)
    expect_identical(flex_cells(f$table), f$table_display)
    expect_identical(gt_notes(g$table), g$footnotes)
    expect_identical(flex_notes(f$table), f$footnotes)
  }
})

test_that("printing, accessors and save_table() work", {
  skip_if_not_installed("flextable")
  f <- risk_difference(data_birthwt, low, smoke, format = "flextable")
  g <- risk_difference(data_birthwt, low, smoke, format = "gt")
  expect_identical(f$engine, "flextable")
  expect_s3_class(f[["table"]], "flextable")
  utils::capture.output(printed_f <- print(f))
  utils::capture.output(printed_g <- print(g))
  expect_s3_class(printed_f, "risk_difference")
  expect_s3_class(printed_g, "risk_difference")
  dir <- tempfile("rd_save_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (ext in c("docx", "html", "rtf")) {
    path <- file.path(dir, paste0("rd.", ext))
    suppressMessages(save_table(f, path, format = ext))
    expect_true(file.exists(path))
    expect_gt(file.size(path), 0)
  }
  path <- file.path(dir, "rd_gt.html")
  suppressMessages(save_table(g, path, format = "html"))
  expect_true(file.exists(path))
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  expect_match(html, "Risk difference (95% CI)", fixed = TRUE)
})
