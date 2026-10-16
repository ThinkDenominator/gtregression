# Regression tests for the round-2 audit findings in the special-model tools
# (select_models, identify_confounder, interaction_models, compare_models,
# mediation_analysis, multinomial_reg).

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

special_bw <- function() {
  bw <- gtregression::data_birthwt
  bw$race <- factor(bw$race, levels = 1:3, labels = c("White", "Black", "Other"))
  bw$smoke <- factor(bw$smoke, levels = c(0, 1), labels = c("No", "Yes"))
  bw$lowf <- factor(bw$low, levels = c(0, 1), labels = c("Normal", "Low"))
  bw
}

# ---- select_models ----------------------------------------------------------

test_that("select_models compares candidates on one complete-case sample", {
  set.seed(1)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 1 + 0.5 * d$x1 + 0.15 * d$x2 + rnorm(n)
  d$x3[sample(n, 150)] <- NA

  expect_message(
    s <- select_models(d, y, c(x1, x2, x3), approach = linear,
                       format = "tibble"),
    "same N = 150 complete cases \\(150 of 300 rows excluded"
  )
  expect_identical(s$n_used, 150L)
  expect_identical(s$n_excluded, 150L)
  expect_true(all(vapply(s$all_models, stats::nobs, numeric(1)) == 150))

  cc <- stats::na.omit(d)
  ref <- stats::step(stats::lm(y ~ 1, cc), scope = ~ x1 + x2 + x3,
                     direction = "forward", trace = 0)
  expect_equal(min(s$results_table$AIC), stats::AIC(ref), tolerance = 1e-8)
  # the noise variable with 50% missing is no longer picked first
  expect_identical(s$results_table$model_terms[2], "x1")
  expect_equal(s$results_table$AIC[1], stats::AIC(stats::lm(y ~ 1, cc)),
               tolerance = 1e-8)

  tab <- quiet(select_models(d, y, c(x1, x2, x3), approach = linear))
  expect_true(any(grepl("same N = 150 complete cases \\(150 rows",
                        tab$table$footer$dataset[[1]])))
})

test_that("select_models on Cox models uses one sample", {
  lung <- gtregression::data_lungcancer
  lung$age[1:40] <- NA
  s <- quiet(select_models(lung, exposures = c("age", "karno"),
                           approach = "cox", time = "time", event = "status",
                           format = "tibble"))
  ns <- vapply(s$all_models, function(m) m$n, numeric(1))
  expect_true(length(unique(ns)) == 1L)
  expect_identical(s$n_used, as.integer(ns[[1]]))
  expect_identical(s$n_excluded, 40L)
})

test_that("backward selection can reach the intercept-only model", {
  set.seed(3)
  d <- data.frame(y = rnorm(200), a1 = rnorm(200), a2 = rnorm(200))
  s <- quiet(select_models(d, y, c(a1, a2), approach = linear,
                           direction = "backward", format = "tibble"))
  null_aic <- stats::AIC(stats::lm(y ~ 1, d))
  ref <- stats::step(stats::lm(y ~ a1 + a2, d), direction = "backward",
                     trace = 0)
  expect_identical(length(attr(stats::terms(ref), "term.labels")), 0L)
  expect_equal(min(s$results_table$AIC), null_aic, tolerance = 1e-8)
  expect_identical(utils::tail(s$results_table$n_predictors, 1), 0L)
  expect_length(attr(s$best_model, "selected_vars"), 0L)
})

# ---- identify_confounder ------------------------------------------------------

test_that("identify_confounder interaction p is an LRT for cox, survreg, negbin", {
  lung <- gtregression::data_lungcancer
  lung$trt <- factor(lung$trt)
  lung$celltype <- factor(lung$celltype)

  c0 <- survival::coxph(survival::Surv(time, status) ~ trt + celltype, lung)
  c1 <- survival::coxph(survival::Surv(time, status) ~ trt * celltype, lung)
  p_cox <- stats::pchisq(2 * (c1$loglik[2] - c0$loglik[2]), 3,
                         lower.tail = FALSE)
  res <- quiet(identify_confounder(lung, time = time, event = status,
                                   exposure = trt,
                                   potential_confounder = celltype,
                                   approach = cox))
  expect_equal(res$interaction_p, p_cox, tolerance = 1e-8)
  expect_equal(round(res$interaction_p, 4), 0.0417)

  s0 <- survival::survreg(survival::Surv(time, status) ~ trt + celltype, lung)
  s1 <- survival::survreg(survival::Surv(time, status) ~ trt * celltype, lung)
  p_sr <- stats::pchisq(2 * (s1$loglik[2] - s0$loglik[2]), 3,
                        lower.tail = FALSE)
  res_sr <- quiet(identify_confounder(lung, time = time, event = status,
                                      exposure = trt,
                                      potential_confounder = celltype,
                                      approach = survreg))
  expect_equal(res_sr$interaction_p, p_sr, tolerance = 1e-8)
  expect_equal(round(res_sr$interaction_p, 4), 0.0426)

  b <- special_bw()
  n0 <- suppressWarnings(MASS::glm.nb(ftv ~ smoke + race, b))
  n1 <- suppressWarnings(MASS::glm.nb(ftv ~ smoke * race, b))
  p_nb <- stats::pchisq(
    2 * (as.numeric(stats::logLik(n1)) - as.numeric(stats::logLik(n0))), 2,
    lower.tail = FALSE
  )
  res_nb <- quiet(identify_confounder(b, ftv, smoke, race, approach = negbin))
  expect_equal(res_nb$interaction_p, p_nb, tolerance = 1e-6)
  expect_equal(round(res_nb$interaction_p, 3), 0.684)

  # glm approaches unchanged
  g0 <- stats::glm(low ~ smoke + race, stats::binomial(), b)
  g1 <- stats::glm(low ~ smoke * race, stats::binomial(), b)
  res_lg <- quiet(identify_confounder(b, low, smoke, race, approach = logit))
  expect_equal(res_lg$interaction_p,
               stats::anova(g0, g1, test = "LRT")[2, "Pr(>Chi)"],
               tolerance = 1e-10)
})

test_that("identify_confounder treats numeric-coded candidates consistently", {
  b <- special_bw()
  res <- quiet(identify_confounder(b, lowf, smoke, ftv, method = both,
                                   emm_test = both))
  # adjusted as categories, as in the stratified / MH screens
  direct <- stats::glm(low ~ smoke + factor(ftv), stats::binomial(), b)
  expect_equal(res$adjusted, unname(exp(stats::coef(direct)["smokeYes"])),
               tolerance = 1e-8)
  expect_identical(res$mh_status, "calculated")

  # sparse strata are flagged and kept out of the estimate-spread screen
  se <- res$stratum_estimates
  expect_true("stable" %in% names(se))
  expect_false(any(se$stable[se$n < 20]))
  used <- se$estimate[se$stable]
  expect_equal(res$emm_statistic,
               (max(used) - min(used)) / min(abs(used)) * 100,
               tolerance = 1e-8)
  expect_true(res$emm_statistic < 1000)

  b$agef <- factor(b$age)
  many <- quiet(identify_confounder(b, lowf, smoke, agef, method = both,
                                    emm_test = both))
  expect_true(is.na(many$emm_statistic) || many$emm_statistic < 1e6)
  expect_false(any(many$stratum_estimates$stable &
                     many$stratum_estimates$n < 20))
})

test_that("identify_confounder text matches the decision", {
  b <- special_bw()
  mh <- quiet(identify_confounder(b, lowf, smoke, lwt, method = mh))
  expect_identical(mh$decision, "not_estimable")
  expect_match(mh$recommendation, "could not be assessed")
  expect_false(grepl("No clear statistical evidence", mh$recommendation))

  both <- quiet(identify_confounder(b, lowf, smoke, lwt, method = both))
  expect_false(is.na(both$is_confounder))
  expect_identical(both$is_confounder, both$is_confounder_model)
  expect_false(grepl("..", both$reason, fixed = TRUE))
})

# ---- interaction_models -------------------------------------------------------

test_that("interaction_models(robpoisson) returns the robust Wald test", {
  b <- special_bw()
  p0 <- stats::glm(low ~ smoke + race, stats::poisson(), b)
  p1 <- stats::glm(low ~ smoke * race, stats::poisson(), b)
  added <- setdiff(names(stats::coef(p1)), names(stats::coef(p0)))
  v <- sandwich::vcovHC(p1, type = "HC0")[added, added]
  beta <- stats::coef(p1)[added]
  p_robust <- stats::pchisq(as.numeric(t(beta) %*% solve(v) %*% beta),
                            df = length(added), lower.tail = FALSE)
  p_lmtest <- lmtest::waldtest(
    p0, p1, vcov = function(m) sandwich::vcovHC(m, type = "HC0"),
    test = "Chisq"
  )[2, 4]
  expect_equal(p_robust, p_lmtest, tolerance = 1e-8)

  expect_message(
    lrt <- interaction_models(b, low, smoke, effect_modifier = race,
                              approach = robpoisson, test = "LRT",
                              format = "tibble"),
    "likelihood-ratio test is not valid for robust Poisson"
  )
  wald <- quiet(interaction_models(b, low, smoke, effect_modifier = race,
                                   approach = robpoisson, test = "Wald",
                                   format = "tibble"))
  expect_equal(lrt$p_value, p_robust, tolerance = 1e-8)
  expect_equal(wald$p_value, p_robust, tolerance = 1e-8)
  expect_equal(round(wald$p_value, 4), 0.1450)
  expect_match(wald$test, "Robust Wald")
  expect_match(lrt$summary$test, "Robust Wald")

  # identify_confounder uses the same test for robust Poisson
  conf <- quiet(identify_confounder(b, low, smoke, race,
                                    approach = robpoisson))
  expect_equal(conf$interaction_p, p_robust, tolerance = 1e-6)

  # other approaches unchanged
  g0 <- stats::glm(low ~ smoke + race, stats::binomial(), b)
  g1 <- stats::glm(low ~ smoke * race, stats::binomial(), b)
  logit <- quiet(interaction_models(b, low, smoke, effect_modifier = race,
                                    format = "tibble"))
  expect_equal(logit$p_value,
               stats::anova(g0, g1, test = "LRT")[2, "Pr(>Chi)"],
               tolerance = 1e-10)
  expect_identical(logit$test, "Likelihood Ratio Test")
})

# ---- compare_models -------------------------------------------------------------

test_that("compare_models change-from-first is on the ratio scale", {
  b <- special_bw()
  m1 <- quiet(multi_reg(b, "low", c("smoke", "age")))
  m2 <- quiet(multi_reg(b, "low", c("smoke", "age", "race")))
  cm <- quiet(compare_models(m1, m2, primary_exposure = "smoke",
                             format = "tibble"))
  or1 <- exp(stats::coef(m1$models[[1]])["smokeYes"])
  or2 <- exp(stats::coef(m2$models[[1]])["smokeYes"])
  expect_equal(cm$table_body$primary_estimate, unname(c(or1, or2)),
               tolerance = 1e-10)
  expect_equal(cm$table_body$primary_pct_change,
               unname(c(0, (or2 - or1) / or1 * 100)), tolerance = 1e-8)

  # same scale as identify_confounder()
  conf <- quiet(identify_confounder(b, low, smoke, age, approach = logit))
  crude <- exp(stats::coef(stats::glm(low ~ smoke, stats::binomial(), b))[2])
  expect_equal(conf$percent_change,
               unname(abs(or1 - crude) / crude * 100), tolerance = 1e-6)
})

# ---- mediation_analysis ---------------------------------------------------------

test_that("mediation_analysis validates format and labels the interval", {
  d <- gtregression::data_diabetes_mediation
  m <- quiet(mediation_analysis(d, obesity, glucose, diabetes,
                                outcome_approach = logit, sims = 30, seed = 1,
                                conf_level = 0.9))
  expect_identical(m$format, "flextable")
  expect_true("90% CI" %in% names(m$table_display))
  expect_false("95% CI" %in% names(m$table_display))
  expect_identical(m$conf_level, 0.9)
  q <- stats::quantile(m$boot$total, c(0.05, 0.95), names = FALSE)
  expect_equal(m$table_body$conf.low[1], q[1], tolerance = 1e-10)

  m95 <- quiet(mediation_analysis(d, obesity, glucose, diabetes,
                                  outcome_approach = logit, sims = 30,
                                  seed = 1))
  expect_true("95% CI" %in% names(m95$table_display))
  # the estimator itself is unchanged
  expect_equal(m95$table_body$estimate, m$table_body$estimate)

  mg <- quiet(mediation_analysis(d, obesity, glucose, diabetes,
                                 outcome_approach = logit, sims = 30, seed = 1,
                                 format = gt, conf_level = 0.9))
  expect_s3_class(mg$table, "gt_tbl")
  expect_identical(mg$format, "gt")

  expect_error(
    mediation_analysis(d, obesity, glucose, diabetes,
                       outcome_approach = logit, sims = 30, format = "bogus"),
    "`format` must be one of: flextable, gt"
  )
  expect_error(
    mediation_analysis(d, obesity, glucose, diabetes,
                       outcome_approach = logit, sims = 30, theme = "nonsense"),
    "Invalid `theme`"
  )
  expect_match(m$table$footer$dataset[[1]][1],
               "mediator set to its predicted mean")
  expect_match(m$table$footer$dataset[[1]][1], "approximation")
})

test_that("mediation_analysis rejects invalid exposure values", {
  d <- gtregression::data_diabetes_mediation
  expect_error(
    mediation_analysis(d, obesity, glucose, diabetes, outcome_approach = logit,
                       sims = 30, exposure_value = "zzz"),
    "`exposure_value` = zzz is not a value of the exposure.*No, Yes"
  )
  expect_error(
    mediation_analysis(d, obesity, glucose, diabetes, outcome_approach = logit,
                       sims = 30, reference_value = "zzz"),
    "`reference_value` = zzz is not a value of the exposure"
  )
  expect_error(
    mediation_analysis(d, obesity, glucose, diabetes, outcome_approach = logit,
                       sims = 30, exposure_value = "No"),
    "must differ"
  )
  expect_error(
    mediation_analysis(d, obesity, glucose, diabetes, outcome_approach = logit,
                       sims = 25.7),
    "`sims` must be a whole number"
  )
})

test_that("mediation_analysis warns about failed bootstrap replicates", {
  d <- gtregression::data_diabetes_mediation
  d6 <- d[1:60, ]
  d6$rare <- factor(c(rep("a", 58), "b", "b"))
  expect_warning(
    m <- suppressMessages(mediation_analysis(
      d6, obesity, glucose, diabetes, covariates = rare,
      outcome_approach = logit, sims = 100, seed = 1
    )),
    "of 100 bootstrap replicates failed"
  )
  failed <- sum(!stats::complete.cases(m$boot[, c("total", "direct",
                                                  "indirect")]))
  expect_true(failed > 0)
  expect_identical(m$boot_failed, failed)
  expect_match(m$table$footer$dataset[[1]][2],
               paste0("Bootstrap replicates = 100 \\(", failed, " failed"))

  ok <- quiet(mediation_analysis(d, obesity, glucose, diabetes,
                                 outcome_approach = logit, sims = 30,
                                 seed = 1))
  expect_identical(ok$boot_failed, 0L)
  expect_no_warning(suppressMessages(mediation_analysis(
    d, obesity, glucose, diabetes, outcome_approach = logit, sims = 30,
    seed = 1
  )))
})

# ---- multinomial_reg --------------------------------------------------------------

test_that("multinomial_reg stores one validated format and validates theme", {
  set.seed(1)
  d <- data.frame(
    ym = factor(sample(c("A", "B", "C", "Exc"), 300, TRUE)),
    trt = factor(sample(c("a", "b"), 300, TRUE)),
    age = rnorm(300)
  )
  res <- quiet(multinomial_reg(d, ym, c(trt, age)))
  expect_identical(res$format, "flextable")
  res_gt <- quiet(multinomial_reg(d, ym, c(trt, age), format = gt))
  expect_identical(res_gt$format, "gt")
  expect_error(multinomial_reg(d, ym, trt, format = "bogus"),
               "`format` must be one of: flextable, gt")
  expect_error(multinomial_reg(d, ym, trt, theme = "x"), "Invalid `theme`")

  fit <- nnet::multinom(ym ~ trt, data = d, trace = FALSE)
  body <- res$per_outcome[["B"]]$table_body
  expect_equal(body$estimate[body$exposure == "trt" & !body$ref],
               unname(exp(stats::coef(fit)["B", "trtb"])), tolerance = 1e-5)
})

test_that("multinomial_reg names the exposure and the lost outcome level", {
  set.seed(1)
  d <- data.frame(
    ym = factor(sample(c("A", "B", "C", "Exc"), 300, TRUE)),
    trt = factor(sample(c("a", "b"), 300, TRUE)),
    age = rnorm(300)
  )
  d$age[d$ym == "Exc"] <- NA
  expect_error(
    quiet(multinomial_reg(d, ym, c(trt, age))),
    "exposure `age`, the outcome level\\(s\\) 'Exc' have no complete cases"
  )
  d3 <- droplevels(d[d$ym != "A", ])
  expect_error(
    quiet(multinomial_reg(d3, ym, c(trt, age))),
    "at least three outcome levels.*predictor\\(s\\) age.*'Exc'"
  )
})

test_that("special-model table functions reject an invalid theme", {
  b <- special_bw()
  b$bwt_cat <- cut(b$bwt, 3, labels = c("low", "mid", "high"),
                   ordered_result = TRUE)
  expect_error(ordinal_reg(b, bwt_cat, smoke, theme = "bogus"),
               "Invalid `theme`")
  m1 <- quiet(multi_reg(b, "low", c("smoke", "age")))
  m2 <- quiet(multi_reg(b, "low", c("smoke", "age", "race")))
  expect_error(compare_models(m1, m2, theme = "bogus"), "Invalid `theme`")
  expect_error(descriptive_table(b, "age", theme = "nonsense"),
               "Invalid `theme`")
})
