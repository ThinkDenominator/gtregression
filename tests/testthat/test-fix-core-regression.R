# Regression tests for the round-2 audit findings in the core regression
# functions (uni_reg, multi_reg, stratified_uni_reg, stratified_multi_reg).

core_bw <- function() {
  bw <- gtregression::data_birthwt
  bw$race <- factor(bw$race, labels = c("White", "Black", "Other"))
  bw$smoke <- factor(bw$smoke, labels = c("No", "Yes"))
  bw
}

quiet <- function(expr) suppressWarnings(suppressMessages(expr))
en_dash <- "\u2013"

# ---- C4: integer-coded 1/2 outcomes --------------------------------------

test_that("C4: INTEGER 1/2 outcomes are recoded like 0/1 (uni_reg, multi_reg)", {
  bw <- core_bw()
  d <- bw
  d$low12 <- d$low + 1L
  expect_true(is.integer(d$low12))

  # robust Poisson: RR must equal the Poisson fit on 0/1 (was 1.12)
  direct_rr <- unname(exp(stats::coef(
    stats::glm(low ~ smoke, family = stats::poisson(), data = bw)
  ))["smokeYes"])
  res_rp <- quiet(uni_reg(d, "low12", "smoke", approach = "robpoisson"))
  expect_equal(res_rp$table_body$estimate[2], direct_rr, tolerance = 1e-6)
  expect_equal(round(res_rp$table_body$estimate[2], 2), 1.61)

  # logit: previously "All models failed"
  direct_or <- unname(exp(stats::coef(
    stats::glm(low ~ smoke, family = stats::binomial(), data = bw)
  ))["smokeYes"])
  res_lg <- quiet(uni_reg(d, "low12", "smoke", approach = "logit"))
  expect_equal(res_lg$table_body$estimate[2], direct_or, tolerance = 1e-8)

  ref01 <- quiet(uni_reg(bw, "low", "smoke", approach = "logit"))
  expect_equal(res_lg$table_body$estimate, ref01$table_body$estimate)
  expect_equal(res_lg$table_body$conf.low, ref01$table_body$conf.low)

  direct_multi <- stats::glm(low ~ age + smoke, family = stats::binomial(),
                             data = bw)
  res_multi <- quiet(multi_reg(d, "low12", c("age", "smoke")))
  expect_equal(
    res_multi$table_body$estimate[res_multi$table_body$exposure == "age"],
    unname(exp(stats::coef(direct_multi)["age"])),
    tolerance = 1e-8
  )
})

test_that("C4: the 1/2 recode is decided once, not per stratum", {
  bw <- core_bw()
  d <- bw
  d$low12 <- d$low + 1L
  # one stratum contains only the value 2 (all events)
  d$g <- ifelse(d$low12 == 2L & d$race == "Black", "only2",
                as.character(d$smoke))
  d01 <- d
  d01$low01 <- d01$low

  a <- quiet(stratified_uni_reg(d, "low12", "lwt", "g",
                                approach = "robpoisson"))
  b <- quiet(stratified_uni_reg(d01, "low01", "lwt", "g",
                                approach = "robpoisson"))
  expect_equal(a$table_display[["..eff__only2"]],
               b$table_display[["..eff__only2"]])
  expect_equal(a$table_display[["..p__only2"]],
               b$table_display[["..p__only2"]])
  expect_equal(a$table_display[["..eff__No"]], b$table_display[["..eff__No"]])

  a2 <- quiet(stratified_multi_reg(d, "low12", c("lwt", "age"), "smoke"))
  b2 <- quiet(stratified_multi_reg(d01, "low01", c("lwt", "age"), "smoke"))
  expect_equal(a2$table_display[["..eff__Yes"]],
               b2$table_display[["..eff__Yes"]])
})

# ---- H4: factor / character / logical outcomes ----------------------------

test_that("H4: factor, character and logical outcomes agree across approaches", {
  bw <- core_bw()
  d <- bw
  d$lowf <- factor(d$low, labels = c("Normal", "Low"))
  d$lowc <- ifelse(d$low == 1, "yes", "no")       # second level "yes" = event
  d$lowl <- d$low == 1

  direct <- c(
    logit = unname(exp(stats::coef(
      stats::glm(low ~ smoke, stats::binomial(), bw)))[2]),
    logbinomial = unname(exp(stats::coef(
      stats::glm(low ~ smoke, stats::binomial("log"), bw)))[2]),
    robpoisson = unname(exp(stats::coef(
      stats::glm(low ~ smoke, stats::poisson(), bw)))[2]),
    poisson = unname(exp(stats::coef(
      stats::glm(low ~ smoke, stats::poisson(), bw)))[2])
  )

  for (outcome in c("lowf", "lowc", "lowl")) {
    for (approach in names(direct)) {
      res <- quiet(uni_reg(d, outcome, "smoke", approach = approach))
      expect_equal(res$table_body$estimate[2], direct[[approach]],
                   tolerance = 1e-5,
                   info = paste(outcome, approach))
      res_m <- quiet(multi_reg(d, outcome, c("smoke", "age"),
                               approach = approach))
      expect_true(is.finite(res_m$table_body$estimate[2]),
                  info = paste("multi", outcome, approach))
    }
  }

  strat <- quiet(stratified_uni_reg(d, "lowf", "lwt", "smoke",
                                    approach = "robpoisson"))
  strat01 <- quiet(stratified_uni_reg(d, "low", "lwt", "smoke",
                                      approach = "robpoisson"))
  expect_equal(strat$table_display[["..eff__Yes"]],
               strat01$table_display[["..eff__Yes"]])
})

test_that("H4: the event level of a factor outcome is stated once", {
  bw <- core_bw()
  d <- bw
  d$lowf <- factor(d$low, labels = c("Normal", "Low"))
  msgs <- character()
  withCallingHandlers(
    uni_reg(d, "lowf", c("smoke", "age")),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  hits <- grep("modelling the event 'Low'", msgs)
  expect_length(hits, 1L)

  msgs <- character()
  withCallingHandlers(
    stratified_uni_reg(d, "lowf", "age", "smoke"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(grep("modelling the event 'Low'", msgs), 1L)

  # numeric and logical outcomes: no message about the event level
  expect_no_message(uni_reg(bw, "low", "age"), message = "modelling the event")
})

# ---- H3: linear CIs use the t distribution --------------------------------

test_that("H3: linear-model CIs match confint.lm in all four functions", {
  bw <- core_bw()
  fit <- stats::lm(bwt ~ race, data = bw)
  ci <- stats::confint(fit)

  res <- quiet(uni_reg(bw, "bwt", "race", approach = "linear"))
  tb <- res$table_body
  expect_equal(tb$conf.low[tb$level == "Black"], ci["raceBlack", 1],
               tolerance = 1e-8)
  expect_equal(tb$conf.high[tb$level == "Black"], ci["raceBlack", 2],
               tolerance = 1e-8)
  expect_equal(tb$conf.low[tb$level == "Other"], ci["raceOther", 1],
               tolerance = 1e-8)

  # small sample: the difference between z and t is large
  small <- data.frame(y = c(1.1, 2.3, 2.9, 4.2, 4.8, 6.5, 6.9, 8.4),
                      x = 1:8)
  ci_small <- stats::confint(stats::lm(y ~ x, small))
  res_small <- quiet(uni_reg(small, "y", "x", approach = "linear"))
  expect_equal(res_small$table_body$conf.low, ci_small["x", 1],
               tolerance = 1e-8)
  expect_equal(res_small$table_body$conf.high, ci_small["x", 2],
               tolerance = 1e-8)

  fit_m <- stats::lm(bwt ~ age + smoke, data = bw)
  ci_m <- stats::confint(fit_m)
  res_m <- quiet(multi_reg(bw, "bwt", c("age", "smoke"), approach = "linear"))
  tm <- res_m$table_body
  expect_equal(tm$conf.low[tm$exposure == "age"], ci_m["age", 1],
               tolerance = 1e-8)
  expect_equal(tm$conf.high[tm$level == "Yes"], ci_m["smokeYes", 2],
               tolerance = 1e-8)
  # p-values unchanged (t based)
  expect_equal(tm$p.value[tm$exposure == "age"],
               summary(fit_m)$coefficients["age", 4], tolerance = 1e-10)

  sub <- bw[bw$smoke == "Yes", ]
  ci_s <- stats::confint(stats::lm(bwt ~ age, data = sub))
  f <- function(z) formatC(z, digits = 2, format = "f", big.mark = ",")
  expected <- paste0(
    f(stats::coef(stats::lm(bwt ~ age, data = sub))["age"]), " (",
    f(ci_s["age", 1]), en_dash, f(ci_s["age", 2]), ")"
  )
  su <- quiet(stratified_uni_reg(bw, "bwt", "age", "smoke",
                                 approach = "linear"))
  expect_equal(su$table_display[["..eff__Yes"]][1], expected)

  ci_sm <- stats::confint(stats::lm(bwt ~ age + lwt, data = sub))
  fit_sm <- stats::lm(bwt ~ age + lwt, data = sub)
  expected_m <- paste0(
    f(stats::coef(fit_sm)["lwt"]), " (", f(ci_sm["lwt", 1]), en_dash,
    f(ci_sm["lwt", 2]), ")"
  )
  sm <- quiet(stratified_multi_reg(bw, "bwt", c("age", "lwt"), "smoke",
                                   approach = "linear"))
  expect_equal(sm$table_display[["..eff__Yes"]][2], expected_m)
})

test_that("non-linear approaches keep Wald (normal) intervals unchanged", {
  bw <- core_bw()
  fit <- stats::glm(low ~ age, stats::binomial(), bw)
  se <- summary(fit)$coefficients["age", 2]
  res <- quiet(uni_reg(bw, "low", "age"))
  expect_equal(res$table_body$conf.low,
               unname(exp(stats::coef(fit)["age"] - stats::qnorm(0.975) * se)),
               tolerance = 1e-10)
})

# ---- H6: log-binomial ------------------------------------------------------

test_that("H6: log-binomial multi_reg fits ordinary models with start values", {
  bw <- core_bw()
  res <- quiet(multi_reg(bw, "low", c("age", "race", "smoke"),
                         approach = "logbinomial"))
  mm <- stats::model.matrix(~ age + race + smoke, bw)
  direct <- suppressWarnings(stats::glm(
    low ~ age + race + smoke, family = stats::binomial("log"), data = bw,
    start = c(log(mean(bw$low)), rep(0, ncol(mm) - 1))
  ))
  tb <- res$table_body
  expect_equal(tb$estimate[tb$exposure == "age"],
               unname(exp(stats::coef(direct)["age"])), tolerance = 1e-5)
  expect_equal(tb$estimate[tb$level == "Yes"],
               unname(exp(stats::coef(direct)["smokeYes"])), tolerance = 1e-5)
  expect_true(all(stats::fitted(res$models[[1]]) < 1))
})

test_that("H6: models that converged before are fitted exactly as before", {
  bw <- core_bw()
  direct <- stats::glm(low ~ smoke, family = stats::binomial("log"), data = bw)
  res <- quiet(uni_reg(bw, "low", "smoke", approach = "logbinomial"))
  expect_identical(unname(stats::coef(res$models$smoke)),
                   unname(stats::coef(direct)))
})

test_that("H6: a failed multivariable fit names the approach and suggests robpoisson", {
  bad <- data.frame(y = c(0, 1, 0, 1, 1, 0), x = factor(rep("a", 6)),
                    z = 1:6)
  expect_error(
    .fit_multi_model(bad, "y", c("x", "z"), approach = "logbinomial"),
    "approach 'logbinomial'.*robpoisson"
  )
  expect_match(.fit_failure_hint("logbinomial"), "robpoisson")
  expect_identical(.fit_failure_hint("logit"), "")
})

test_that("H6: uni_reg keeps a failed exposure visible in the table", {
  bw <- core_bw()
  fit_one <- .fit_uni_model
  testthat::local_mocked_bindings(
    .fit_uni_model = function(data, outcome, exposures, approach) {
      if (identical(exposures, "bwt")) {
        warning("Model failed for exposure(s) 'bwt' using 'logbinomial': boom",
                call. = FALSE)
        return(NULL)
      }
      fit_one(data, outcome, exposures, approach)
    }
  )
  res <- quiet(uni_reg(bw, "low", c("lwt", "bwt"), approach = "logbinomial"))
  td <- res$table_display
  expect_identical(td$Characteristic, c("lwt", "bwt"))
  expect_identical(td[["RR (95% CI)"]][2], "\u2014")
  expect_identical(td$N[2], 189L)
  expect_identical(res$failed_exposures, "bwt")
  expect_true(any(grepl("model could not be fitted for: bwt", res$footnotes)))
  # numeric results are not padded with empty rows
  expect_identical(unique(res$table_body$exposure), "lwt")
})

test_that("H6: a log-binomial fit that cannot converge is reported, not dropped", {
  bw <- core_bw()
  # low is defined from bwt: the log-binomial model has no valid default
  # start and the refit ends on the boundary
  expect_warning(
    res <- suppressMessages(
      uni_reg(bw, "low", c("lwt", "bwt"), approach = "logbinomial")
    ),
    "Model failed for exposure\\(s\\) 'bwt' using 'logbinomial'.*robpoisson"
  )
  expect_identical(res$table_display$Characteristic, c("lwt", "bwt"))
  expect_identical(res$table_display[["RR (95% CI)"]][2], "\u2014")
  expect_identical(res$failed_exposures, "bwt")
  direct <- stats::glm(low ~ lwt, stats::binomial("log"), bw)
  expect_equal(res$table_body$estimate,
               unname(exp(stats::coef(direct)["lwt"])), tolerance = 1e-8)
  gt_res <- quiet(uni_reg(bw, "low", c("lwt", "bwt"),
                          approach = "logbinomial", format = "gt"))
  expect_s3_class(gt_res$table, "gt_tbl")

  expect_error(
    quiet(multi_reg(bw, "low", "bwt", approach = "logbinomial")),
    "approach 'logbinomial'.*did not converge.*robpoisson"
  )
  expect_error(
    quiet(uni_reg(bw, "low", "bwt", approach = "logbinomial")),
    "All models failed.*logbinomial.*robpoisson"
  )
})

# ---- C6 / N1 / N6: labels and coefficient matching -------------------------

test_that("C6: interaction labels keep ':' inside factor levels", {
  bw <- core_bw()
  d <- bw
  d$ratio <- factor(d$race, labels = c("1:1", "1:2", "2:1"))
  res <- quiet(multi_reg(d, "low", c("ratio", "smoke"),
                         interaction = "ratio*smoke"))
  labels <- trimws(res$table_display$Characteristic)
  expect_true("ratio: 1:2 x smoke: Yes" %in% labels)
  expect_true("ratio: 2:1 x smoke: Yes" %in% labels)
  expect_false(any(grepl("1 x 2", labels, fixed = TRUE)))

  direct <- stats::glm(low ~ ratio * smoke, stats::binomial(), d)
  tb <- res$table_body
  expect_equal(
    tb$estimate[tb$level == "ratio: 1:2 x smoke: Yes"][1],
    unname(exp(stats::coef(direct)["ratio1:2:smokeYes"])),
    tolerance = 1e-8
  )

  # ordinary labels are unchanged
  plain <- quiet(multi_reg(bw, "low", c("race", "smoke"),
                           interaction = "race*smoke"))
  expect_true("race: Black x smoke: Yes" %in%
                trimws(plain$table_display$Characteristic))
  num <- quiet(multi_reg(bw, "low", c("age", "smoke"),
                         interaction = "age*smoke"))
  expect_true("age x smoke: Yes" %in% trimws(num$table_display$Characteristic))
})

test_that("C6: a level containing ' x ' keeps its position", {
  bw <- core_bw()
  d <- bw
  d$sp <- factor(d$race, labels = c("a b", "c x d", "race"))
  res <- quiet(multi_reg(d, "low", c("sp", "age")))
  expect_identical(trimws(res$table_display$Characteristic)[1:4],
                   c("sp", "a b", "c x d", "race"))
  direct <- stats::glm(low ~ sp + age, stats::binomial(), d)
  expect_equal(res$table_body$estimate[res$table_body$level == "c x d"],
               unname(exp(stats::coef(direct)["spc x d"])), tolerance = 1e-8)
})

test_that("N1: duplicated coefficient names do not drop a row", {
  bw <- core_bw()
  set.seed(1)
  d <- bw
  d$x <- factor(sample(rep(c("a", "b", "c"), 63)))
  d$xb <- d$lwt / 100
  direct <- stats::coef(stats::glm(low ~ x + xb, stats::binomial(), d))
  expect_identical(names(direct), c("(Intercept)", "xb", "xc", "xb"))

  check <- function(res) {
    tb <- res$table_body
    expect_equal(tb$estimate[tb$exposure == "x" & tb$level == "b"],
                 unname(exp(direct[2])), tolerance = 1e-8)
    expect_equal(tb$estimate[tb$exposure == "x" & tb$level == "c"],
                 unname(exp(direct[3])), tolerance = 1e-8)
    expect_equal(tb$estimate[tb$exposure == "xb"],
                 unname(exp(direct[4])), tolerance = 1e-8)
  }
  check(quiet(multi_reg(d, "low", c("x", "xb"))))
  check(quiet(multi_reg(d, "low", c("xb", "x"))))

  expect_identical(
    .match_coefficient_columns(c("(Intercept)", "xb", "xc", "xb"),
                               c("(Intercept)", "xb", "xc", "xb")),
    1:4
  )
  # an aliased (dropped) column in between
  expect_identical(
    .match_coefficient_columns(c("a", "xb", "xb"), c("a", "xb", "zz", "xb")),
    c(1L, 2L, 4L)
  )
})

test_that("N6: an empty-string stratum level does not crash", {
  bw <- core_bw()
  d <- bw
  d$e <- factor(d$race, labels = c("A", "", "C"))
  res <- quiet(stratified_uni_reg(d, "low", "lwt", "e"))
  expect_true("..eff__(blank)" %in% names(res$table_display))
  direct <- stats::glm(low ~ lwt, stats::binomial(), d[d$e == "", ])
  expect_match(res$table_display[["..eff__(blank)"]][1],
               formatC(exp(stats::coef(direct)["lwt"]), digits = 2,
                       format = "f"),
               fixed = TRUE)
  res_m <- quiet(stratified_multi_reg(d, "low", c("lwt", "age"), "e"))
  expect_true("..eff__(blank)" %in% names(res_m$table_display))
})

# ---- N2: non-treatment contrasts -------------------------------------------

test_that("N2: factor rows are level-vs-reference whatever the contrasts", {
  bw <- core_bw()
  direct <- exp(stats::coef(stats::glm(
    low ~ race, stats::binomial(), bw,
    contrasts = list(race = "contr.treatment")
  )))

  old <- options(contrasts = c("contr.sum", "contr.poly"))
  res <- tryCatch(quiet(uni_reg(bw, "low", "race")),
                  finally = options(old))
  expect_identical(res$table_body$level, c("White", "Black", "Other"))
  expect_equal(res$table_body$estimate[2:3],
               unname(direct[c("raceBlack", "raceOther")]), tolerance = 1e-8)
  expect_identical(getOption("contrasts"), old$contrasts)

  d <- bw
  stats::contrasts(d$race) <- stats::contr.sum(3)
  res2 <- quiet(uni_reg(d, "low", "race"))
  expect_identical(res2$table_body$level, c("White", "Black", "Other"))
  expect_equal(res2$table_body$estimate[2:3],
               unname(direct[c("raceBlack", "raceOther")]), tolerance = 1e-8)

  direct_m <- exp(stats::coef(stats::glm(low ~ race + age, stats::binomial(),
                                         bw)))
  res3 <- quiet(multi_reg(d, "low", c("race", "age")))
  expect_equal(res3$table_body$estimate[res3$table_body$level == "Black"],
               unname(direct_m["raceBlack"]), tolerance = 1e-8)
})

# ---- M / L items ------------------------------------------------------------

test_that("M1: uni_reg uses the same en dash CI separator as multi_reg", {
  bw <- core_bw()
  uni <- quiet(uni_reg(bw, "low", "age"))
  multi <- quiet(multi_reg(bw, "low", c("age", "lwt")))
  expect_match(uni$table_display[["OR (95% CI)"]][1], en_dash, fixed = TRUE)
  expect_match(multi$table_display[["Adjusted OR (95% CI)"]][1], en_dash,
               fixed = TRUE)
  expect_false(grepl("[0-9]-[0-9]", uni$table_display[["OR (95% CI)"]][1]))

  fit <- stats::glm(low ~ age, stats::binomial(), bw)
  f <- function(z) formatC(z, digits = 2, format = "f")
  ci <- stats::coef(fit)["age"] + c(-1, 1) * stats::qnorm(0.975) *
    summary(fit)$coefficients["age", 2]
  expect_identical(
    uni$table_display[["OR (95% CI)"]][1],
    paste0(f(exp(stats::coef(fit)["age"])), " (", f(exp(ci[1])), en_dash,
           f(exp(ci[2])), ")")
  )
})

test_that("N3: N is the second table_display column in all four functions", {
  bw <- core_bw()
  expect_identical(names(quiet(uni_reg(bw, "low", "age"))$table_display)[2],
                   "N")
  expect_identical(
    names(quiet(multi_reg(bw, "low", c("age", "lwt")))$table_display)[2], "N"
  )
  su <- quiet(stratified_uni_reg(bw, "low", "age", "smoke"))$table_display
  sm <- quiet(stratified_multi_reg(bw, "low", c("age", "lwt"),
                                   "smoke"))$table_display
  expect_identical(names(su)[3:5], c("..N__No", "..eff__No", "..p__No"))
  expect_identical(names(sm)[3:5], c("..N__No", "..eff__No", "..p__No"))
  expect_identical(names(sm)[6:8], c("..N__Yes", "..eff__Yes", "..p__Yes"))
})

test_that("N4: a numeric stratifier is ordered by value", {
  bw <- core_bw()
  expect_identical(bw$ui[1], 1L)   # first appearance is 1
  res <- quiet(stratified_uni_reg(bw, "low", "age", "ui"))
  expect_equal(res$levels, c(0, 1))
  expect_identical(names(res$per_stratum), c("0", "1"))
  res_m <- quiet(stratified_multi_reg(bw, "low", c("age", "lwt"), "ui"))
  expect_equal(res_m$levels, c(0, 1))
})

test_that("M8: exposure validation messages are well formed", {
  bw <- core_bw()
  d <- bw
  d$one <- "a"
  d$flag <- d$low == 1
  err <- tryCatch(uni_reg(d, "low", c("one", "flag")),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Exposure with <2 levels: one\\.")
  expect_match(err, "unsupported types.*: flag\\.")
  expect_match(err, "Please fix them to proceed\\.$")
  expect_false(grepl("FALSE", err, fixed = TRUE))
  expect_false(grepl("onePlease", err, fixed = TRUE))
})

test_that("M9: non-syntactic column names are backticked in formulas", {
  bw <- core_bw()
  d <- bw
  d[["my var"]] <- d$lwt
  d[["a-b"]] <- d$smoke
  d[["out come"]] <- d$low
  direct <- stats::glm(low ~ lwt, stats::binomial(), bw)
  uni <- quiet(uni_reg(d, "out come", c("my var", "a-b")))
  expect_identical(trimws(uni$table_display$Characteristic),
                   c("my var", "a-b", "No", "Yes"))
  expect_equal(uni$table_body$estimate[1],
               unname(exp(stats::coef(direct)["lwt"])), tolerance = 1e-8)

  direct_m <- stats::glm(low ~ lwt + smoke, stats::binomial(), bw)
  multi <- quiet(multi_reg(d, "out come", c("my var", "a-b")))
  expect_equal(multi$table_body$estimate[multi$table_body$level == "Yes"],
               unname(exp(stats::coef(direct_m)["smokeYes"])),
               tolerance = 1e-8)
  rp <- quiet(uni_reg(d, "low", "my var", approach = "robpoisson"))
  expect_equal(rp$table_body$estimate,
               unname(exp(stats::coef(stats::glm(low ~ lwt, stats::poisson(),
                                                 bw))["lwt"])),
               tolerance = 1e-6)
  expect_identical(.formula_name(c("age", "my var", "`x y`")),
                   c("age", "`my var`", "`x y`"))
})

test_that("M7: variables used in two roles give clear errors", {
  bw <- core_bw()
  expect_error(multi_reg(bw, "low", c("age", "low")),
               "outcome `low` cannot also be listed as an exposure")
  expect_error(uni_reg(bw, "low", "low"),
               "outcome `low` cannot also be listed as an exposure")
  expect_error(multi_reg(bw, "low", c("age", "age")),
               "Duplicate exposure\\(s\\): age")
  expect_error(uni_reg(bw, "low", c("age", "age")),
               "Duplicate exposure\\(s\\): age")
  expect_error(quiet(stratified_uni_reg(bw, "low", "lwt", "low")),
               "stratifier cannot be the outcome")
  expect_error(quiet(stratified_multi_reg(bw, "low", c("lwt", "age"), "low")),
               "stratifier cannot be the outcome")
  expect_error(quiet(stratified_uni_reg(bw, "low", c("lwt", "smoke"),
                                        "smoke")),
               "stratifier `smoke` cannot also be an exposure")
})

test_that("L1: an invalid theme is rejected with the valid choices", {
  bw <- core_bw()
  expect_error(uni_reg(bw, "low", "age", theme = "bogus"),
               "Invalid `theme`.*minimal, clinical, striped, shaded, jama")
  expect_error(multi_reg(bw, "low", c("age", "lwt"), theme = "bogus"),
               "Invalid `theme`")
  expect_error(stratified_uni_reg(bw, "low", "age", "smoke", theme = "bogus"),
               "Invalid `theme`")
  expect_error(stratified_multi_reg(bw, "low", c("age", "lwt"), "smoke",
                                    theme = "bogus"),
               "Invalid `theme`")
  # presets and primitives are still accepted
  expect_s3_class(quiet(uni_reg(bw, "low", "age", theme = "jama")), "uni_reg")
  expect_s3_class(
    quiet(uni_reg(bw, "low", "age", theme = c("plain", "zebra"))), "uni_reg"
  )
})

test_that("estimates that were correct are unchanged (direct fits)", {
  bw <- core_bw()
  fit <- stats::glm(low ~ age + lwt + race + smoke, stats::binomial(), bw)
  res <- quiet(multi_reg(bw, "low", c("age", "lwt", "race", "smoke")))
  tb <- res$table_body
  co <- summary(fit)$coefficients
  expect_equal(tb$estimate[!tb$ref], unname(exp(co[-1, 1])), tolerance = 1e-10)
  expect_equal(tb$p.value[!tb$ref], unname(co[-1, 4]), tolerance = 1e-10)
  expect_equal(tb$conf.high[!tb$ref],
               unname(exp(co[-1, 1] + stats::qnorm(0.975) * co[-1, 2])),
               tolerance = 1e-10)
  nb <- suppressWarnings(MASS::glm.nb(ftv ~ smoke, data = bw))
  res_nb <- quiet(uni_reg(bw, "ftv", "smoke", approach = "negbin"))
  expect_equal(res_nb$table_body$estimate[2],
               unname(exp(stats::coef(nb)["smokeYes"])), tolerance = 1e-6)
})
