# Cross-area regression tests added when the three fix branches were merged.

bw_int <- function() {
  d <- MASS::birthwt
  d$smoke <- factor(d$smoke, labels = c("No", "Yes"))
  d$race <- factor(d$race, labels = c("White", "Black", "Other"))
  d$ui <- factor(d$ui, labels = c("No", "Yes"))
  d
}

vet_int <- function() {
  v <- survival::veteran
  v$trt <- factor(v$trt, labels = c("Standard", "Test"))
  v$prior <- factor(v$prior, labels = c("No", "Yes"))
  v
}

test_that("merge keeps a continuous variable with a (Missing) row on one row", {
  d <- bw_int()
  d$age[c(3, 7, 20)] <- NA
  d$lwt[5] <- NA
  vars <- c("age", "lwt", "smoke", "race")
  desc <- suppressMessages(descriptive_table(d, exposures = vars, by = "low"))
  uni <- suppressMessages(uni_reg(d, outcome = "low", exposures = vars,
                                  approach = "logit"))

  check <- function(merged, desc_suffix, uni_suffix) {
    md <- merged$table_display
    lab <- trimws(md$Characteristic)
    # one row per variable / level, no duplicated variable row
    expect_identical(
      lab,
      c("age", "(Missing)", "lwt", "(Missing)", "smoke", "No", "Yes",
        "race", "White", "Black", "Other")
    )
    # descriptive cells: identical to the source table, row by row
    dd <- desc$table_display
    expect_identical(trimws(dd$Characteristic), lab)
    for (col in setdiff(names(dd), c("Characteristic", "is_header"))) {
      mcol <- grep(paste0("^X?", col, desc_suffix, "$"), names(md), value = TRUE)
      expect_length(mcol, 1L)
      expect_identical(as.character(md[[mcol]]), as.character(dd[[col]]))
    }
    # regression cells: each variable / level row carries its own estimate
    ud <- uni$table_display
    eff_u <- grep("95", names(ud), value = TRUE)
    eff_m <- grep(paste0("95.*", uni_suffix, "$"), names(md), value = TRUE)
    n_m <- grep(paste0("^N", uni_suffix, "$"), names(md), value = TRUE)
    p_m <- grep(paste0("^p.*", uni_suffix, "$"), names(md), value = TRUE)
    norm <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      gsub("-", "\u2013", x, fixed = TRUE)
    }
    not_missing <- lab != "(Missing)"
    expect_identical(lab[not_missing], trimws(ud$Characteristic))
    expect_identical(norm(md[[eff_m]][not_missing]), norm(ud[[eff_u]]))
    expect_identical(norm(md[[n_m]][not_missing]), norm(ud$N))
    expect_identical(norm(md[[p_m]][not_missing]),
                     norm(ud[[grep("^p", names(ud), value = TRUE)]]))
    expect_true(all(norm(md[[eff_m]][!not_missing]) == ""))
    expect_true(all(norm(md[[n_m]][!not_missing]) == ""))
    # the age estimate is on the row that shows the age summary
    age_row <- which(lab == "age")
    expect_match(md[[eff_m]][age_row], "^0\\.95 \\(")
    expect_identical(norm(md[[n_m]][age_row]), "186")
  }

  check(merge_tables(desc, uni), "_p1", "_p2")
  check(merge_tables(uni, desc), "_p2", "_p1")
  check(merge_tables(desc, uni, format = "flextable"), "_p1", "_p2")
})

test_that("forest_df recovers the model standard error for linear models", {
  d <- bw_int()[1:25, ]
  uni <- suppressMessages(uni_reg(d, outcome = "bwt",
                                  exposures = c("age", "race"),
                                  approach = "linear"))
  multi <- suppressMessages(multi_reg(d, outcome = "bwt",
                                      exposures = c("age", "race"),
                                      approach = "linear"))
  fd <- forest_df(uni, multi)
  se_age <- summary(stats::lm(bwt ~ age, d))$coefficients[-1, 2]
  se_race <- summary(stats::lm(bwt ~ race, d))$coefficients[-1, 2]
  se_adj <- summary(stats::lm(bwt ~ age + race, d))$coefficients[-1, 2]
  expect_equal(stats::na.omit(fd$se_uni), c(se_age, se_race),
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(stats::na.omit(fd$se_adj), se_adj,
               tolerance = 1e-8, ignore_attr = TRUE)

  # ratio models keep the Wald (normal) back-calculation
  lg <- suppressMessages(uni_reg(bw_int(), outcome = "low", exposures = "age",
                                 approach = "logit"))
  fit <- stats::glm(low ~ age, data = bw_int(), family = stats::binomial())
  expect_equal(stats::na.omit(forest_df(lg)$se_uni),
               summary(fit)$coefficients[2, 2],
               tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("survival single-variable arguments follow the wrapper rule", {
  v <- vet_int()
  v$event <- v$status
  v$by <- v$celltype
  v$weights <- 1
  v$w2 <- 2
  set.seed(11)
  v$ev3 <- sample(0:2, nrow(v), replace = TRUE)

  w_cox <- function(d, time, event) {
    cox_reg(d, time = time, event = event, exposures = "age")
  }
  res <- w_cox(v, "diagtime", "status")
  expect_identical(res$time, "diagtime")
  expect_identical(res$event, "status")
  direct <- cox_reg(v, time = "diagtime", event = "status", exposures = "age")
  expect_equal(res$table_body$estimate, direct$table_body$estimate)

  w_weights <- function(d, weights) {
    cox_reg(d, time = "time", event = "status", exposures = "age",
            weights = weights)
  }
  expect_true(any(grepl("w2", w_weights(v, "w2")$footnotes, fixed = TRUE)))

  w_surv <- function(d, time, event) {
    surv_reg(d, time = time, event = event, exposures = "age")
  }
  expect_identical(w_surv(v, "diagtime", "status")$time, "diagtime")

  w_lr <- function(d, time, event, by) {
    logrank_test(d, time = time, event = event, by = by)
  }
  expect_identical(w_lr(v, "time", "status", "trt")$by, "trt")

  w_crr <- function(d, time, event) {
    competing_risk_reg(d, time = time, event = event, event_of_interest = 1,
                       exposures = "age")
  }
  res_crr <- w_crr(v, "diagtime", "ev3")
  expect_identical(c(res_crr$time, res_crr$event), c("diagtime", "ev3"))

  w_ci <- function(d, time, event, by) {
    cuminc_plot(d, time = time, event = event, event_of_interest = 1, by = by)
  }
  expect_identical(
    levels(attr(w_ci(v, "time", "ev3", "trt"), "plot_data")$group),
    levels(v$trt)
  )

  # a column called `time` still wins over a workspace object of that name
  time <- "diagtime"
  expect_identical(
    cox_reg(v, time = time, event = status, exposures = "age")$time,
    "time"
  )
  expect_error(
    competing_risk_reg(v, time = "time", event = "ev3", exposures = "age"),
    "event_of_interest"
  )
})

test_that("an invalid table theme errors everywhere; plot themes still work", {
  v <- vet_int()
  d <- bw_int()
  one <- cox_reg(v, "time", "status", "age")
  expect_error(cox_reg(v, "time", "status", "age", theme = "foo"), "Invalid `theme`")
  expect_error(surv_reg(v, "time", "status", "age", theme = "foo"), "Invalid `theme`")
  expect_error(survival_summary(v, "time", "status", theme = "foo"), "Invalid `theme`")
  expect_error(logrank_test(v, "time", "status", by = "trt", theme = "foo"),
               "Invalid `theme`")
  expect_error(rmst_table(v, "time", "status", by = "trt", tau = 100, theme = "foo"),
               "Invalid `theme`")
  expect_error(merge_tables(one, one, theme = "foo"), "Invalid `theme`")
  expect_error(
    suppressMessages(identify_confounder(d, "low", "smoke", "age", theme = "foo")),
    "Invalid `theme`"
  )
  # presets, vectors of primitives and the stored (resolved) theme are valid
  expect_s3_class(cox_reg(v, "time", "status", "age", theme = c("zebra", "compact")),
                  "gtregression")
  expect_s3_class(modify_table(cox_reg(v, "time", "status", "age", theme = "jama"),
                               variable_labels = c(age = "Age")), "gtregression")
  # km_plot() / cuminc_plot() use ggplot theme names
  expect_s3_class(km_plot(v, "time", "status", by = "trt", theme = "minimal"),
                  "patchwork")
  set.seed(11)
  v$ev3 <- sample(0:2, nrow(v), replace = TRUE)
  expect_s3_class(
    cuminc_plot(v, "time", "ev3", event_of_interest = 1, theme = "bw"), "ggplot"
  )
})

test_that("survival interaction labels keep levels that contain a colon", {
  v <- vet_int()
  v$grp <- factor(ifelse(v$karno > 60, "c:d", "a:b"))
  for (fun in list(cox_reg, surv_reg)) {
    res <- fun(v, "time", "status", exposures = c("grp", "trt"),
               multivariable = TRUE, interaction = "grp*trt")
    expect_true("grp: c:d x trt: Test" %in% res$table_body$level)
    expect_true(all(c("a:b", "c:d") %in% res$table_body$level))
    strat <- suppressMessages(fun(
      v, "time", "status", exposures = c("grp", "trt"), multivariable = TRUE,
      interaction = "grp*trt", stratifier = "prior"
    ))
    expect_true("grp: c:d x trt: Test" %in% trimws(strat$table_display$Characteristic))
  }
})

test_that("compact descriptive flextables keep the level indentation", {
  d <- bw_int()
  for (theme in c("clinical", "jama", "striped")) {
    desc <- suppressMessages(descriptive_table(
      d, exposures = c("age", "race"), by = "low", theme = theme
    ))
    pad_left <- desc$table$body$styles$pars$padding.left$data[, "Characteristic"]
    pad_top <- desc$table$body$styles$pars$padding.top$data[, "Characteristic"]
    is_level <- !desc$table_display$is_header
    expect_true(all(pad_left[is_level] == 14))
    expect_true(all(pad_left[!is_level] < 14))
    expect_true(all(pad_top == 2))
  }
})

test_that("stratified tables print a blank cell for non-estimable coefficients", {
  d <- bw_int()
  d$x2 <- d$age * 2
  multi <- suppressWarnings(suppressMessages(stratified_multi_reg(
    d, "low", c("age", "x2", "race"), stratifier = "ui", approach = "logit"
  )))
  cells <- unlist(multi$table_display[, grep("^\\.\\.(eff|p)__", names(multi$table_display))])
  expect_false(any(grepl("NA", cells, fixed = TRUE)))
  x2 <- multi$table_display[trimws(multi$table_display$Characteristic) == "x2", ]
  expect_identical(unname(unlist(x2[, grep("^\\.\\.eff__", names(x2))])), c("", ""))
  age <- multi$table_display[trimws(multi$table_display$Characteristic) == "age", ]
  expect_true(all(nzchar(unlist(age[, grep("^\\.\\.eff__", names(age))]))))

  v <- vet_int()
  v$age2 <- v$age * 2
  surv <- suppressWarnings(suppressMessages(cox_reg(
    v, "time", "status", exposures = c("age", "age2"), multivariable = TRUE,
    stratifier = "prior"
  )))
  cells <- unlist(surv$table_display[, grep("^\\.\\.(eff|p)__", names(surv$table_display))])
  expect_false(any(grepl("NA", cells, fixed = TRUE)))
})

test_that("stratified footnotes: robust N note, labelled stratifier, stored notes", {
  expect_silent(
    note <- .n_note_multi_strata(
      "g", list(a = "79-97", b = c(28L, 40L), c = 12, d = NA, e = "1,200")
    )
  )
  expect_identical(
    note,
    paste0(
      "Complete observations included by g stratum: ",
      "a: N = 79\u201397 across models; b: N = 28\u201340 across models; ",
      "c: N = 12; d: N unavailable; e: N = 1200"
    )
  )

  d <- bw_int()
  plain_uni <- suppressMessages(stratified_uni_reg(
    d, "low", c("age", "race"), stratifier = "ui", approach = "logit"
  ))
  expect_identical(plain_uni$footnotes[1], "Stratified by: ui.")
  expect_identical(plain_uni$footnotes, .rendered_footnotes(plain_uni$table))

  attr(d$ui, "label") <- "Uterine irritability"
  uni <- suppressMessages(stratified_uni_reg(
    d, "low", c("age", "race"), stratifier = "ui", approach = "logit"
  ))
  multi <- suppressMessages(stratified_multi_reg(
    d, "low", c("age", "race"), stratifier = "ui", approach = "logit"
  ))
  expect_identical(uni$footnotes[1], "Stratified by: Uterine irritability.")
  expect_identical(multi$footnotes[1], "Stratified by: Uterine irritability.")
  expect_identical(multi$footnotes, .rendered_footnotes(multi$table))
  expect_true(any(grepl("No: N = 161; Yes: N = 28", multi$footnotes, fixed = TRUE)))

  v <- vet_int()
  attr(v$prior, "label") <- "Prior therapy"
  surv <- suppressMessages(cox_reg(v, "time", "status", exposures = "age",
                                   stratifier = "prior"))
  expect_identical(surv$footnotes[1], "Stratified by: Prior therapy.")
})

test_that("multinomial_reg stores the header structure of its table", {
  d <- bw_int()
  res <- suppressMessages(multinomial_reg(d, "race", c("age", "lwt"),
                                          theme = "striped"))
  expect_identical(res$spanners, c("Black vs White", "Other vs White"))
  expect_length(res$column_labels, 2L)
  expect_identical(unname(res$column_labels[[1]]), c("N", "OR (95% CI)", "p-value"))
  expect_identical(res$theme, c("zebra", "labels_bold", "compact"))
  expect_true(all(unlist(lapply(res$column_labels, names)) %in% names(res$table_display)))
  # the stored structure agrees with what the rendered table shows
  spec <- .header_spec(res)
  expect_identical(unname(spec$labels[names(res$column_labels[[1]])]),
                   unname(res$column_labels[[1]]))
  expect_identical(spec$levels[[1]]$label, res$spanners)
  # and survives modify_table() and a later merge
  modified <- modify_table(res, remove_N = TRUE)
  expect_false(any(grepl("^N_", unlist(lapply(modified$column_labels, names)))))
  merged <- merge_tables(res, res, spanners = c("A", "B"))
  expect_s3_class(merged, "merged_table")
})

test_that("remove_N keeps the model N note; remove_N_obs removes it", {
  d <- bw_int()
  multi <- suppressMessages(multi_reg(d, outcome = "low",
                                      exposures = c("age", "smoke"),
                                      approach = "logit"))
  n_note <- "complete observations included in the model"
  no_col <- modify_table(multi, remove_N = TRUE)
  expect_false("N" %in% names(no_col$table_display))
  expect_true(any(grepl(n_note, no_col$footnotes, fixed = TRUE)))
  expect_false(any(grepl("^N is the number of complete", no_col$footnotes)))
  no_note <- modify_table(multi, remove_N_obs = TRUE)
  expect_true("N" %in% names(no_note$table_display))
  expect_false(any(grepl(n_note, no_note$footnotes, fixed = TRUE)))
  both <- modify_table(multi, remove_N = TRUE, remove_N_obs = TRUE)
  expect_false(any(.is_n_note(both$footnotes)))
})
