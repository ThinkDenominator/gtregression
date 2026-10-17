# Regression tests for the survival audit findings (round 2).
# Every expectation is checked against a direct survival / cmprsk fit.

fix_surv_data <- function() {
  d <- data_lungcancer
  d$trt <- factor(d$trt, levels = c(1, 2), labels = c("Standard", "Test"))
  d$prior <- factor(d$prior, levels = c(0, 10), labels = c("No", "Yes"))
  d
}

fix_surv_quiet <- function(expr) suppressMessages(expr)

fix_surv_cell <- function(est, lo, hi) {
  f <- function(z) formatC(z, digits = 2, format = "f", big.mark = ",")
  paste0(f(est), " (", f(lo), "\u2013", f(hi), ")")
}

fix_surv_hr_cells <- function(fit) {
  ci <- exp(stats::confint.default(fit))
  out <- fix_surv_cell(exp(stats::coef(fit)), ci[, 1], ci[, 2])
  stats::setNames(out, names(stats::coef(fit)))
}

# N1 ---------------------------------------------------------------------------

test_that("N1: stratified footnote shows the N range without warnings", {
  d <- fix_surv_data()
  d$karno[1:30] <- NA

  for (fun in list(cox_reg, surv_reg)) {
    res <- expect_no_warning(fix_surv_quiet(
      fun(d, time, status, exposures = c(trt, karno), stratifier = prior)
    ))
    note <- grep("^Complete observations", res$footnotes, value = TRUE)
    expect_identical(
      note,
      "Complete observations included by prior stratum: No: N = 79\u201397; Yes: N = 28\u201340"
    )
    expect_false(any(grepl("NA", res$footnotes)))

    gt_res <- expect_no_warning(fix_surv_quiet(
      fun(d, time, status, exposures = c(trt, karno), stratifier = prior,
          format = "gt", model_stats = TRUE)
    ))
    expect_identical(gt_res$footnotes, res$footnotes)
  }

  # direct fits give the two ends of each range
  n_direct <- vapply(split(d, d$prior), function(x) {
    c(survival::coxph(survival::Surv(time, status) ~ trt, x)$n,
      survival::coxph(survival::Surv(time, status) ~ karno, x)$n)
  }, numeric(2))
  expect_equal(unname(n_direct[, "No"]), c(97, 79))
  expect_equal(unname(n_direct[, "Yes"]), c(40, 28))

  # one model per stratum: a single number, no range
  one <- fix_surv_quiet(cox_reg(d, time, status, exposures = c(trt, karno),
                                stratifier = prior, multivariable = TRUE))
  expect_true(any(one$footnotes ==
    "Complete observations included by prior stratum: No: N = 79; Yes: N = 28"))
})

test_that("N4: rows with a missing stratifier are reported in a footnote", {
  d <- fix_surv_data()
  d$prior[c(3, 9, 27)] <- NA

  res <- fix_surv_quiet(cox_reg(d, time, status, exposures = karno, stratifier = prior))
  expect_true(any(res$footnotes == "3 rows with a missing value of prior were excluded."))
  expect_equal(
    sum(vapply(res$models, function(m) m$karno$n, numeric(1))),
    nrow(d) - 3
  )
  sr <- fix_surv_quiet(surv_reg(d, time, status, exposures = karno, stratifier = prior))
  expect_true(any(sr$footnotes == "3 rows with a missing value of prior were excluded."))

  full <- fix_surv_quiet(cox_reg(fix_surv_data(), time, status, exposures = karno,
                                 stratifier = prior))
  expect_false(any(grepl("missing value", full$footnotes)))
})

# C7 ---------------------------------------------------------------------------

test_that("C7: crude stratified N and Events come from the fitted models", {
  d <- fix_surv_data()
  set.seed(2)
  d$entry <- pmax(0, d$time - stats::runif(nrow(d), 1, 60))
  d$entry[1:30] <- NA
  d$w <- 1
  d$w[5:29] <- NA
  d$cl <- rep(1:20, length.out = nrow(d))
  d$cl[100:137] <- NA

  res <- fix_surv_quiet(cox_reg(
    d, time, status, exposures = c(celltype, karno), stratifier = trt,
    cluster = cl, weights = w, time_start = entry
  ))
  td <- res$table_display
  hdr <- td[td$is_header, , drop = FALSE]

  for (lev in levels(d$trt)) {
    sub <- d[d$trt == lev, ]
    sub <- droplevels(sub[stats::complete.cases(sub[, c("entry", "w", "cl")]), ])
    for (i in seq_along(c("celltype", "karno"))) {
      x <- c("celltype", "karno")[i]
      direct <- survival::coxph(
        stats::as.formula(paste("survival::Surv(entry, time, status) ~", x)),
        data = sub, weights = w, cluster = cl
      )
      expect_identical(hdr[[paste0("..N__", lev)]][i], as.character(direct$n))
      expect_identical(hdr[[paste0("..Events__", lev)]][i], as.character(direct$nevent))
      expect_equal(stats::coef(res$models[[lev]][[x]]), stats::coef(direct),
                   tolerance = 1e-8)
    }
  }
  expect_true(any(grepl(
    "^Complete analysis intervals included by trt stratum: Standard: N = 39; Test: N = 30$",
    res$footnotes
  )))
})

# C4 / N2 ----------------------------------------------------------------------

test_that("C4: Ref. row is the reference level the model actually used", {
  d <- fix_surv_data()
  d$ct <- factor(d$celltype,
                 levels = c("none", "squamous", "smallcell", "adeno", "large", "extra"))
  direct <- survival::coxph(survival::Surv(time, status) ~ droplevels(ct), d)
  cells <- unname(fix_surv_hr_cells(direct))

  res <- cox_reg(d, time, status, exposures = ct)
  td <- res$table_display
  expect_identical(trimws(td$Characteristic),
                   c("ct", "squamous", "smallcell", "adeno", "large"))
  expect_identical(td[["HR (95% CI)"]], c("", "Ref.", cells))
  expect_equal(unname(stats::coef(res$models$ct)), unname(stats::coef(direct)),
               tolerance = 1e-8)
  expect_false(any(grepl("NA", td[["HR (95% CI)"]])))

  sr <- surv_reg(d, time, status, exposures = ct)
  direct_sr <- survival::survreg(survival::Surv(time, status) ~ droplevels(ct), d)
  expect_identical(trimws(sr$table_display$Characteristic),
                   c("ct", "squamous", "smallcell", "adeno", "large"))
  expect_equal(sr$table_body$estimate[-1],
               unname(exp(stats::coef(direct_sr)[-1])), tolerance = 1e-8)
  expect_identical(sr$table_body$level[sr$table_body$ref], "squamous")

  mv <- cox_reg(d, time, status, exposures = c(ct, age), multivariable = TRUE)
  direct_mv <- survival::coxph(survival::Surv(time, status) ~ droplevels(ct) + age, d)
  expect_equal(mv$table_body$estimate[!mv$table_body$ref],
               unname(exp(stats::coef(direct_mv))), tolerance = 1e-8)
  expect_identical(mv$table_body$level[mv$table_body$ref], "squamous")
})

test_that("C4: reference lost through missing adjuster values is handled", {
  d <- fix_surv_data()
  d$karno[d$celltype == "squamous"] <- NA
  res <- cox_reg(d, time, status, exposures = celltype, adjust_for = karno)
  sub <- droplevels(d[!is.na(d$karno), ])
  direct <- survival::coxph(survival::Surv(time, status) ~ celltype + karno, sub)
  tb <- res$table_body
  expect_identical(tb$level, c("smallcell", "adeno", "large"))
  expect_identical(tb$ref, c(TRUE, FALSE, FALSE))
  expect_equal(tb$estimate[-1], unname(exp(stats::coef(direct)[1:2])), tolerance = 1e-8)
})

test_that("C4: a stratum lacking the reference level shows its own reference", {
  d <- fix_surv_data()
  d <- d[!(d$trt == "Test" & d$celltype == "squamous"), ]
  test_fit <- survival::coxph(
    survival::Surv(time, status) ~ celltype,
    droplevels(d[d$trt == "Test", ])
  )
  std_fit <- survival::coxph(
    survival::Surv(time, status) ~ celltype, d[d$trt == "Standard", ]
  )

  for (fmt in c("flextable", "gt")) {
    res <- fix_surv_quiet(cox_reg(d, time, status, exposures = celltype,
                                  stratifier = trt, format = fmt))
    td <- res$table_display
    expect_identical(trimws(td$Characteristic),
                     c("celltype", "squamous", "smallcell", "adeno", "large"))
    expect_identical(td[["..eff__Test"]],
                     c("", "", "Ref.", unname(fix_surv_hr_cells(test_fit))))
    expect_identical(td[["..eff__Standard"]],
                     c("", "Ref.", unname(fix_surv_hr_cells(std_fit))))
    expect_true(any(grepl("reference category differs between strata", res$footnotes)))
  }

  sr <- fix_surv_quiet(surv_reg(d, time, status, exposures = celltype, stratifier = trt))
  expect_identical(sr$table_display[["..eff__Test"]][2:3], c("", "Ref."))
  direct_sr <- survival::survreg(
    survival::Surv(time, status) ~ celltype, droplevels(d[d$trt == "Test", ])
  )
  expect_equal(unname(stats::coef(sr$models$Test$celltype)),
               unname(stats::coef(direct_sr)), tolerance = 1e-8)

  same <- fix_surv_quiet(cox_reg(fix_surv_data(), time, status,
                                 exposures = celltype, stratifier = trt))
  expect_false(any(grepl("reference category differs", same$footnotes)))
})

test_that("N2: ordered factors are reported as level-versus-reference effects", {
  d <- fix_surv_data()
  d$oo <- factor(d$celltype, ordered = TRUE)
  direct <- survival::coxph(survival::Surv(time, status) ~ celltype, d)
  direct_sr <- survival::survreg(survival::Surv(time, status) ~ celltype, d)

  res <- cox_reg(d, time, status, exposures = oo)
  expect_identical(res$table_body$level, levels(d$celltype))
  expect_equal(res$table_body$estimate[-1], unname(exp(stats::coef(direct))),
               tolerance = 1e-8)
  expect_identical(res$table_display[["HR (95% CI)"]],
                   c("", "Ref.", unname(fix_surv_hr_cells(direct))))
  expect_false(any(grepl("\\.L|\\.Q|\\.C", res$table_display$Characteristic)))

  sr <- surv_reg(d, time, status, exposures = oo)
  expect_identical(sr$table_body$level, levels(d$celltype))
  expect_equal(sr$table_body$estimate[-1], unname(exp(stats::coef(direct_sr)[-1])),
               tolerance = 1e-8)

  strat <- fix_surv_quiet(cox_reg(d, time, status, exposures = oo, stratifier = trt))
  for (lev in levels(d$trt)) {
    f <- survival::coxph(survival::Surv(time, status) ~ celltype, d[d$trt == lev, ])
    expect_identical(strat$table_display[[paste0("..eff__", lev)]],
                     c("", "Ref.", unname(fix_surv_hr_cells(f))))
  }
  strat_sr <- fix_surv_quiet(surv_reg(d, time, status, exposures = oo, stratifier = trt))
  expect_false(any(strat_sr$table_display[["..eff__Test"]][3:5] == ""))

  cr <- data_competing_risk
  cr$stage_o <- factor(cr$stage, ordered = TRUE)
  cr$stage_u <- factor(cr$stage, ordered = FALSE)
  a <- competing_risk_reg(cr, follow_up_months, outcome, event_of_interest = "Relapse",
                          censor_code = "Censored", exposures = stage_o)
  b <- competing_risk_reg(cr, follow_up_months, outcome, event_of_interest = "Relapse",
                          censor_code = "Censored", exposures = stage_u)
  expect_equal(a$table_body$estimate, b$table_body$estimate, tolerance = 1e-10)
})

# C6 / C8 / H5 -----------------------------------------------------------------

test_that("C6: surv_model_compare counts events in the rows used by the model", {
  d <- fix_surv_data()
  d$karno[1:30] <- NA
  out <- surv_model_compare(d, time, status, exposures = c(trt, karno),
                            format = "tibble")
  direct <- survival::survreg(survival::Surv(time, status) ~ trt + karno, d)
  used <- stats::complete.cases(d[, c("time", "status", "trt", "karno")])
  expect_true(all(out$N == nrow(direct$y)))
  expect_true(all(out$Events == sum(d$status[used] == 1)))
  expect_equal(unique(out$Events), 102)

  sr <- surv_reg(d, time, status, exposures = c(trt, karno), multivariable = TRUE,
                 model_stats = TRUE)
  expect_equal(sr$model_stats$events, 102)
  expect_equal(sr$model_stats$n, 107)
})

test_that("C8: rmst_table headers follow conf.level", {
  d <- fix_surv_data()
  r90 <- rmst_table(d, time, status, by = trt, tau = 365, conf.level = 0.90)
  expect_true(all(c("RMST (90% CI)", "RMST difference (90% CI)") %in%
                    names(r90$table_display)))
  expect_false(any(grepl("95%", names(r90$table_display))))
  r95 <- rmst_table(d, time, status, by = trt, tau = 365, format = "gt")
  expect_true("RMST (95% CI)" %in% names(r95$table_display))
  r975 <- rmst_table(d, time, status, tau = 365, conf.level = 0.975)
  expect_true("RMST (97.5% CI)" %in% names(r975$table_display))

  fit <- survival::survfit(survival::Surv(time, status) ~ trt, d)
  tab <- summary(fit, rmean = 365)$table
  z <- stats::qnorm(0.95)
  expect_equal(r90$table_body$CI.lower[1:2],
               unname(tab[, "rmean"] - z * tab[, "se(rmean)"]), tolerance = 1e-8)
})

test_that("H5: rmst_table warns and notes when tau exceeds follow-up", {
  set.seed(1)
  x <- data.frame(t = stats::rexp(50, 0.1), g = rep(c("a", "b"), 25))
  x$e <- as.integer(x$t < 8)
  x$t <- pmin(x$t, 8)

  expect_warning(
    res <- rmst_table(x, t, e, by = g, tau = 100),
    "exceeds the last follow-up time in: a \\(8\\), b \\(8\\)"
  )
  expect_true(any(grepl("exceeds the last follow-up time", res$footnotes)))
  expect_warning(rmst_table(x, t, e, tau = 100, format = "tibble"), "exceeds")
  expect_warning(rmst_table(x, t, e, by = g, tau = 100, format = "gt"), "exceeds")

  # only the group whose follow-up ends early is named
  d <- fix_surv_data()
  last <- tapply(d$time, d$trt, max)
  tau <- mean(last)
  expect_warning(
    rmst_table(d, time, status, by = trt, tau = tau),
    paste0("in: ", names(last)[last < tau], " \\(")
  )
  ok <- expect_no_warning(rmst_table(d, time, status, by = trt, tau = 365))
  expect_length(ok$footnotes, 1L)
})

# H1 ---------------------------------------------------------------------------

test_that("H1: data and strata without censored observations are accepted", {
  d <- fix_surv_data()
  all_ev <- d[d$status == 1, ]
  res <- cox_reg(all_ev, time, status, exposures = karno)
  direct <- survival::coxph(survival::Surv(time, status) ~ karno, all_ev)
  expect_equal(res$table_body$estimate, unname(exp(stats::coef(direct))),
               tolerance = 1e-8)

  sr <- surv_reg(all_ev, time, status, exposures = karno)
  direct_sr <- survival::survreg(survival::Surv(time, status) ~ karno, all_ev)
  expect_equal(sr$table_body$estimate, unname(exp(stats::coef(direct_sr)[2])),
               tolerance = 1e-8)

  d$status[d$celltype == "adeno"] <- 1
  strat <- expect_no_warning(fix_surv_quiet(
    cox_reg(d, time, status, exposures = age, stratifier = celltype)
  ))
  expect_identical(names(strat$models), levels(d$celltype))
  direct_adeno <- survival::coxph(survival::Surv(time, status) ~ age,
                                  d[d$celltype == "adeno", ])
  expect_equal(unname(stats::coef(strat$models$adeno$age)),
               unname(stats::coef(direct_adeno)), tolerance = 1e-8)

  # 1/2 coding: a stratum that is entirely censored is skipped as "no events",
  # not re-read as "all events"
  d2 <- fix_surv_data()
  d2$status2 <- d2$status + 1
  d2$status2[d2$celltype == "adeno"] <- 1
  expect_warning(
    s2 <- fix_surv_quiet(cox_reg(d2, time, status2, exposures = age,
                                 stratifier = celltype)),
    "Skipping stratum adeno: .*at least one event"
  )
  expect_false("adeno" %in% names(s2$models))
  direct_sq <- survival::coxph(survival::Surv(time, status) ~ age,
                               d2[d2$celltype == "squamous", ])
  expect_equal(unname(stats::coef(s2$models$squamous$age)),
               unname(stats::coef(direct_sq)), tolerance = 1e-8)
})

# H3 ---------------------------------------------------------------------------

test_that("H3: km_plot curve and risk table share the x range", {
  d <- fix_surv_data()
  x_range <- function(p, i) {
    ggplot2::ggplot_build(p[[i]])$layout$panel_params[[1]]$x.range
  }
  for (args in list(list(break_time_by = 200), list(), list(break_time_by = 150),
                    list(break_time_by = 100, xlim = c(0, 500)))) {
    p <- do.call(km_plot, c(list(d, "time", "status", by = "trt"), args))
    expect_equal(x_range(p, 1), x_range(p, 2))
  }
  p <- km_plot(d, time, status, by = trt, break_time_by = 200)
  expect_true(x_range(p, 1)[2] >= max(d$time))
})

# H4 ---------------------------------------------------------------------------

test_that("H4: plot_cox_survival draws one curve per profile and stratum", {
  d <- fix_surv_data()
  res <- cox_reg(d, time, status, exposures = c(trt, karno),
                 multivariable = TRUE, baseline_strata = celltype)
  p <- plot_cox_survival(res, by = trt)
  pd <- attr(p, "plot_data")

  expect_equal(nlevels(pd$curve), 2 * nlevels(d$celltype))
  expect_true(all(tapply(pd$time, pd$curve, function(t) all(diff(t) >= 0))))
  expect_true(all(tapply(pd$survival, pd$curve, function(s) all(diff(s) <= 1e-12))))
  expect_no_error(ggplot2::ggplot_build(p))

  strata <- survival::strata
  direct <- survival::coxph(
    survival::Surv(time, status) ~ trt + karno + strata(celltype), d
  )
  nd <- data.frame(
    trt = factor("Test", levels = levels(d$trt)),
    karno = stats::median(d$karno),
    celltype = factor("adeno", levels = levels(d$celltype))
  )
  sf <- survival::survfit(direct, newdata = nd)
  got <- pd[pd$curve == "Test, adeno", ]
  expect_equal(got$time[-1], sf$time)
  expect_equal(got$survival[-1], as.numeric(sf$surv), tolerance = 1e-8)

  # a direct coxph fit behaves the same way
  direct_m <- survival::coxph(
    survival::Surv(time, status) ~ trt + karno + strata(celltype), d, model = TRUE
  )
  pd2 <- attr(plot_cox_survival(direct_m, by = trt), "plot_data")
  expect_equal(pd2$survival, pd$survival, tolerance = 1e-8)

  # single profile: one curve per stratum
  p1 <- plot_cox_survival(res)
  expect_identical(levels(attr(p1, "plot_data")$curve), levels(d$celltype))
  expect_no_error(ggplot2::ggplot_build(p1))

  # newdata naming the stratum: one curve per row
  nd2 <- data.frame(
    trt = factor(c("Standard", "Test"), levels = levels(d$trt)),
    karno = 60,
    celltype = factor(c("adeno", "large"), levels = levels(d$celltype)),
    row.names = c("A", "B")
  )
  p2 <- plot_cox_survival(res, newdata = nd2)
  pd3 <- attr(p2, "plot_data")
  expect_identical(levels(pd3$curve), c("A", "B"))
  sf2 <- survival::survfit(direct, newdata = nd2)
  expect_equal(nrow(pd3), length(sf2$time) + 2L)
  expect_true(all(tapply(pd3$time, pd3$curve, function(t) all(diff(t) >= 0))))

  expect_error(plot_cox_survival(res, by = celltype), "baseline-hazard stratum")

  # models without strata are unchanged
  plain <- cox_reg(d, time, status, exposures = c(trt, karno), multivariable = TRUE)
  pd4 <- attr(plot_cox_survival(plain, by = trt), "plot_data")
  expect_identical(levels(pd4$curve), levels(d$trt))
  sf4 <- survival::survfit(
    plain$models[[1]],
    newdata = data.frame(trt = factor("Test", levels = levels(d$trt)),
                         karno = stats::median(d$karno))
  )
  expect_equal(pd4$survival[pd4$curve == "Test"][-1], as.numeric(sf4$surv),
               tolerance = 1e-8)
})

# Consistency ------------------------------------------------------------------

test_that("non-stratified tables report events with one shared sentence", {
  d <- fix_surv_data()
  d$karno[1:30] <- NA

  cx <- cox_reg(d, time, status, exposures = c(trt, karno))
  ev <- vapply(cx$models, function(m) m$nevent, numeric(1))
  expect_identical(
    cx$footnotes[length(cx$footnotes)],
    paste0("Number of events: ", min(ev), "\u2013", max(ev), " across models.")
  )
  cx1 <- cox_reg(d, time, status, exposures = c(trt, karno), multivariable = TRUE)
  expect_identical(cx1$footnotes[length(cx1$footnotes)],
                   paste0("Number of events: ", cx1$models[[1]]$nevent, "."))

  sv <- surv_reg(d, time, status, exposures = c(trt, karno), multivariable = TRUE)
  expect_identical(sv$footnotes[length(sv$footnotes)], "Number of events: 102.")

  cr <- data_competing_risk
  fg <- competing_risk_reg(cr, follow_up_months, outcome, event_of_interest = "Relapse",
                           censor_code = "Censored", exposures = c(treatment, age),
                           multivariable = TRUE, model_stats = TRUE)
  expect_identical(
    fg$footnotes[length(fg$footnotes)],
    paste0("Number of events: ", fg$model_stats$events_of_interest, ".")
  )
  # the sentence before it is the shared N sentence in all three functions
  n_note <- "N is the number of complete observations used in each model."
  expect_identical(cx1$footnotes[length(cx1$footnotes) - 1L], n_note)
  expect_identical(sv$footnotes[length(sv$footnotes) - 1L], n_note)
  expect_identical(fg$footnotes[length(fg$footnotes) - 1L], n_note)
})

test_that("show_sample defaults to both and is ignored without stratifier", {
  expect_identical(formals(cox_reg)$show_sample, "both")
  expect_identical(formals(surv_reg)$show_sample, "both")
  d <- fix_surv_data()
  a <- cox_reg(d, time, status, exposures = karno, show_sample = "none")
  b <- cox_reg(d, time, status, exposures = karno)
  expect_identical(a$table_display, b$table_display)
  expect_identical(a$footnotes, b$footnotes)
})

test_that("N6: interaction labels keep factor levels that contain a colon", {
  d <- fix_surv_data()
  d$grp <- factor(ifelse(d$karno >= 60, "a:b", "c:d"))
  res <- cox_reg(d, time, status, exposures = c(grp, trt), multivariable = TRUE,
                 interaction = "grp*trt")
  expect_true("grp: c:d x trt: Test" %in% res$table_body$level)
  expect_false(any(grepl("c x d", res$table_body$level)))
  direct <- survival::coxph(survival::Surv(time, status) ~ grp * trt, d)
  expect_equal(
    res$table_body$estimate[res$table_body$level == "grp: c:d x trt: Test"][1],
    unname(exp(stats::coef(direct)["grpc:d:trtTest"])), tolerance = 1e-8
  )
  sr <- surv_reg(d, time, status, exposures = c(grp, trt), multivariable = TRUE,
                 interaction = "grp*trt")
  expect_true("grp: c:d x trt: Test" %in% sr$table_body$level)

  # ordinary interactions are labelled as before
  plain <- cox_reg(d, time, status, exposures = c(trt, prior), multivariable = TRUE,
                   interaction = "trt*prior")
  expect_true("trt: Test x prior: Yes" %in% plain$table_body$level)
})

test_that("M8/M10: stratified footnotes use labels and interval wording", {
  d <- fix_surv_data()
  attr(d$age, "label") <- "Age (y)"
  attr(d$karno, "label") <- "Karnofsky score"
  res <- fix_surv_quiet(cox_reg(d, time, status, exposures = celltype,
                                adjust_for = c(age, karno), stratifier = trt))
  expect_true("Adjusted for Age (y) and Karnofsky score" %in% res$footnotes)
})

test_that("cuminc_plot zooms with ylim and orders groups by factor level", {
  skip_if_not_installed("cmprsk")
  cr <- data_competing_risk
  cr$treatment <- factor(cr$treatment, levels = c("Standard", "Intervention"))
  full <- cuminc_plot(cr, follow_up_months, outcome, event_of_interest = "Relapse",
                      censor_code = "Censored", by = treatment)
  zoom <- cuminc_plot(cr, follow_up_months, outcome, event_of_interest = "Relapse",
                      censor_code = "Censored", by = treatment, ylim = c(0, 0.3))
  expect_no_warning(built <- ggplot2::ggplot_build(zoom))
  expect_equal(nrow(built$data[[2]]), nrow(ggplot2::ggplot_build(full)$data[[2]]))
  expect_false(anyNA(built$data[[2]]$y))
  expect_lt(built$layout$panel_params[[1]]$y.range[2], 0.35)
  expect_identical(levels(attr(full, "plot_data")$group), c("Standard", "Intervention"))
  expect_identical(
    built$plot$scales$get_scales("colour")$get_labels(),
    c("Standard", "Intervention")
  )
  # estimates are unchanged
  fit <- cmprsk::cuminc(cr$follow_up_months, as.character(cr$outcome),
                        group = cr$treatment, cencode = "Censored")
  pd <- attr(full, "plot_data")
  expect_equal(pd$estimate[pd$group == "Standard"], fit[["Standard Relapse"]]$est)
  expect_error(
    cuminc_plot(cr, follow_up_months, outcome, event_of_interest = "Relapse",
                censor_code = "Censored", xlim = c(24, 0)),
    "lower < upper"
  )
})

test_that("small validation fixes give clear messages", {
  d <- fix_surv_data()
  d$w0 <- 1
  d$w0[1:40] <- 0
  expect_error(cox_reg(d, time, status, exposures = trt, weights = w0),
               "greater than zero")
  d$entry <- 0
  expect_error(
    cox_reg(d, time, status, exposures = trt, time_start = entry, ties = "exact"),
    "cannot be combined with `time_start`"
  )
  d0 <- d
  d0$time[1] <- 0
  expect_error(surv_reg(d0, time, status, exposures = trt), "greater than zero")
  expect_no_error(cox_reg(d0, time, status, exposures = trt))

  dup <- cox_reg(d, time, status, exposures = c(age, age, karno))
  expect_identical(dup$table_display$Characteristic, c("age", "karno"))
  expect_error(logrank_test(d, time, status), "`by` must be a single grouping variable")

  d$schr <- ifelse(d$status == 1, "Deceased", "Living")
  chr <- cox_reg(d, time, schr, exposures = karno)
  expect_true("Event: Living (Deceased treated as censored)." %in% chr$footnotes)
  expect_equal(chr$models[[1]]$nevent, sum(d$schr == "Living"))

  d$wt <- rep(c(0.5, 1.5), length.out = nrow(d))
  wres <- cox_reg(d, time, status, exposures = karno, weights = wt)
  direct <- survival::coxph(survival::Surv(time, status) ~ karno, d, weights = wt)
  expect_identical(!is.null(direct$naive.var),
                   any(grepl("Robust sandwich variance", wres$footnotes)))

  strat <- fix_surv_quiet(surv_reg(d, time, status, exposures = karno, stratifier = trt))
  expect_error(surv_predict(strat, times = 100), "stratified surv_reg")
})
