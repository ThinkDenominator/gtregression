## Manual API test: risk_difference()
## Package: gtregression 1.2.0
##
## Run this file one numbered section at a time from the package root and
## compare what you see with the "Expected" notes. The expected numbers were
## produced with the package source of 7 October 2026 on the birth-weight data
## shipped with the package; they should match to the digits shown.
##
## Story:
## A trial-style question on the birth-weight data: how much higher is the
## absolute risk of low birth weight when the mother smokes? The answer is a
## risk difference in percentage points, with the number needed to harm.


## 0. Setup -------------------------------------------------------------------

## During package development, run this first from the package root:
## devtools::load_all(".")
##
## For an installed package, use:
## library(gtregression)

library(gtregression)

data("data_birthwt", package = "gtregression")

bw <- data_birthwt
bw$smoke <- factor(bw$smoke, levels = c(0, 1), labels = c("Non-smoker", "Smoker"))
bw$low <- factor(bw$low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
bw$race <- factor(bw$race, levels = c(1, 2, 3), labels = c("White", "Black", "Other"))
attr(bw$smoke, "label") <- "Smoking in pregnancy"
attr(bw$low, "label") <- "Low birth weight"

## The observed counts every table below must agree with.
table(Smoking = bw$smoke, Outcome = bw$low)

## Expected:
##              Outcome
## Smoking      Normal BW Low BW
##   Non-smoker        86     29
##   Smoker            44     30


## 1. Crude risk difference ---------------------------------------------------

rd_crude <- risk_difference(bw, outcome = low, exposure = smoke)
rd_crude

## Expected (message first):
## - A message names the event ('Low BW', the second level) and the
##   reference group ('Non-smoker', the first level).
## - Non-smoker: 29 / 115, 25.2%, Ref.
## - Smoker:     30 / 74,  40.5%, 15.3 (1.7 to 29.0), p = 0.027
## - Header "Risk difference (95% CI)"; minus signs are true minus signs and
##   the limits are joined by "to".
## - Footnotes state the event, "Smoker - Non-smoker", percentage points,
##   and "Miettinen-Nurminen score".

rd_crude$table_body
rd_crude$method

## Expected: rd = 0.1532315, rd_low = 0.01722098, rd_high = 0.2897649,
## p_value = 0.02689321.


## 2. Hand check of the crude numbers ----------------------------------------

## Risks and their difference from the counts alone.
risk_smoker <- 30 / 74
risk_non <- 29 / 115
c(smoker = risk_smoker, non_smoker = risk_non, difference = risk_smoker - risk_non)

## Expected: 0.4054054, 0.2521739, 0.1532315 (identical to section 1).

## prop.test() is a different method (Wald/Wilson with continuity
## correction), so its interval must NOT be identical. It should be close.
prop.test(c(30, 29), c(74, 115))$conf.int

## Optional independent check of the Miettinen-Nurminen interval. ratesci is
## not a gtregression dependency; skip this if it is not installed.
if (requireNamespace("ratesci", quietly = TRUE)) {
  ratesci::scoreci(x1 = 30, n1 = 74, x2 = 29, n2 = 115, skew = FALSE,
                   bcf = TRUE, contrast = "RD", level = 0.95)$estimates
}

## Expected: Lower 0.0172, Upper 0.2898 (agrees with section 1 to at least
## 6 decimals).


## 3. Event and reference group ----------------------------------------------

## Naming them explicitly removes the message and gives the same table.
rd_named <- risk_difference(bw, outcome = low, exposure = smoke,
                            event = "Low BW", reference = "Non-smoker")
identical(rd_named$table_body, rd_crude$table_body)

## Expected: TRUE.

## Swapping the reference reverses the sign; the p-value is unchanged.
risk_difference(bw, outcome = low, exposure = smoke, reference = "Smoker")

## Expected: Smoker is Ref.; Non-smoker -15.3 (-29.0 to -1.7), p = 0.027.

## Counting "Normal BW" as the event gives the complementary risks.
risk_difference(bw, outcome = low, exposure = smoke, event = "Normal BW")

## Expected: Non-smoker 86 / 115 74.8%; Smoker 44 / 74 59.5%;
## -15.3 (-29.0 to -1.7).


## 4. Number needed to treat -------------------------------------------------

rd_nnt <- risk_difference(bw, outcome = low, exposure = smoke,
                          event_type = "harmful")
rd_nnt
rd_nnt$nnt

## Expected:
## - New column "NNT (95% CI)": NNTH 7 (4 to 59).
## - $nnt: type "NNTH", estimate 6.526 (unrounded 1 / 0.1532),
##   crosses_infinity FALSE, one segment from 3.45 to 58.07.
## - The footnote defines NNTB/NNTH and carries the caution that it applies to
##   the study follow-up and assumes the difference reflects the intervention.

## Beneficial event: the same difference is now a benefit.
risk_difference(bw, outcome = low, exposure = smoke, event = "Normal BW",
                event_type = "beneficial")

## Expected: NNTH again, because the smokers have fewer normal-weight births:
## the label follows the direction of the effect, not the word chosen.

## No event_type: no NNT column at all.
"NNT (95% CI)" %in% names(rd_crude$table_display)

## Expected: FALSE.


## 5. An interval that crosses zero (NNT through infinity) -------------------

## Any physician visit in the first trimester (ftv > 0) versus none.
bw$visit <- factor(bw$ftv > 0, levels = c(FALSE, TRUE), labels = c("No visit", "Visit"))
rd_visit <- risk_difference(bw, outcome = low, exposure = visit,
                            event_type = "harmful")
rd_visit
rd_visit$nnt$crosses_infinity
rd_visit$nnt$segments

## Expected:
## - Risk difference -10.2 (-23.0 to 3.1): the interval includes 0.
## - NNT column: NNTB 10 (NNTB 5 to oo to NNTH 32), with the infinity sign.
## - crosses_infinity TRUE; two segments, one NNTB and one NNTH, each running
##   to infinity.


## 6. Adjusted risk difference -----------------------------------------------

rd_adj <- risk_difference(bw, outcome = low, exposure = smoke,
                          adjust_for = c(age, lwt, race),
                          event_type = "harmful")
rd_adj
rd_adj$table_body

## Expected:
## - Headers "Adjusted risk" and "Adjusted risk difference (95% CI)".
## - Non-smoker 29 / 115, adjusted risk 23.4%, Ref.
## - Smoker     30 / 74,  adjusted risk 44.3%, 21.0 (6.9 to 35.1),
##   p = 0.004, NNTH 5 (3 to 15).
## - Events / N stay the observed counts; only the risks are adjusted.
## - Footnote: standardised to the covariate distribution of the analysis
##   population, logistic regression adjusted for age, lwt, race,
##   "Wald, influence-function standard error".
## - table_body: adjusted_risk 0.2336070 and 0.4433505, rd 0.2097435,
##   rd_se 0.07201588, p_value 0.003585924.


## 7. Hand check of the adjusted point estimate ------------------------------

## Standardisation by hand: fit the same main-effects logistic model, predict
## everyone as non-smoker and as smoker, and average.
fit <- glm(I(low == "Low BW") ~ smoke + age + lwt + race,
           family = binomial, data = bw)
as_non <- transform(bw, smoke = factor("Non-smoker", levels = levels(bw$smoke)))
as_smk <- transform(bw, smoke = factor("Smoker", levels = levels(bw$smoke)))
p0 <- mean(predict(fit, as_non, type = "response"))
p1 <- mean(predict(fit, as_smk, type = "response"))
c(non_smoker = p0, smoker = p1, difference = p1 - p0)

## Expected: 0.2336070, 0.4433505, 0.2097435 (identical to section 6).
## The standard error is not reproduced here; it is checked against an
## independent calculation in the automated tests.


## 8. Confidence level -------------------------------------------------------

risk_difference(bw, outcome = low, exposure = smoke, conf_level = 0.90)

## Expected: header "Risk difference (90% CI)", 15.3 (3.9 to 26.8).
## The p-value stays 0.027: conf_level changes the interval only.


## 9. Missing values and the analysis population -----------------------------

bw_missing <- bw
bw_missing$lwt[c(5, 40, 101)] <- NA
risk_difference(bw_missing, outcome = low, exposure = smoke,
                adjust_for = c(age, lwt))

## Expected: "N = 186; 3 observations excluded because of missing values."
## Events / N count only the 186 analysed (29 / 114 and 30 / 72);
## 15.0 (1.0 to 28.9), p = 0.035.


## 10. Display precision ------------------------------------------------------

## Exactly 0% does not trigger two decimals.
zero <- data.frame(
  arm = factor(rep(c("Control", "Active"), c(40, 38)), levels = c("Control", "Active")),
  event = c(rep(0, 40), rep(1, 6), rep(0, 32))
)
risk_difference(zero, outcome = event, exposure = arm)$table_display

## Expected: Control 0.0%, Active 15.8%; RD 15.8 (6.2 to 30.5), p = 0.009.

## Genuinely rare, non-zero risks get two decimals.
rare <- data.frame(
  arm = factor(rep(c("Control", "Active"), each = 1000), levels = c("Control", "Active")),
  event = c(rep(1, 3), rep(0, 997), rep(1, 9), rep(0, 991))
)
risk_difference(rare, outcome = event, exposure = arm)$table_display

## Expected: 0.30% and 0.90%; RD 0.60 (-0.09 to 1.44), p = 0.082.


## 11. gt, flextable and export ----------------------------------------------

rd_ft <- risk_difference(bw, outcome = low, exposure = smoke,
                         event_type = "harmful", format = "flextable")
class(rd_ft$table)
rd_ft$table

## Expected: "flextable"; same rows, headers and footnotes as section 4.

out_dir <- file.path(tempdir(), "rd-manual")
dir.create(out_dir, showWarnings = FALSE)
save_table(rd_nnt, filename = file.path(out_dir, "rd.docx"))
save_table(rd_ft, filename = file.path(out_dir, "rd_flex.docx"))
save_table(rd_nnt, filename = file.path(out_dir, "rd.html"), format = "html")
list.files(out_dir)

## Expected: rd.docx, rd.html and rd_flex.docx. (save_table() takes the file
## type from `format`, not from the file name: without format = "html" the
## third call would write rd.docx again.)
##
## Open the files (for example with utils::browseURL(out_dir) or Finder).
## Expected: the table and every footnote are present; the minus sign and
## the infinity sign display correctly in Word.


## 12. modify_table() --------------------------------------------------------

rd_mod <- modify_table(
  rd_nnt,
  variable_labels = c(smoke = "Smoking status"),
  level_labels = list(smoke = c(Smoker = "Current smoker")),
  header_labels = c("Risk difference (95% CI)" = "Difference in risk (95% CI)"),
  caption = "Table 2. Absolute risk of low birth weight by smoking",
  caveat = "Observational data; not a randomised comparison."
)
rd_mod

## Expected: caption above the table, "Current smoker" in place of Smoker,
## the new RD header, and the caveat as an extra footnote. Numbers unchanged.

## Regression-only options are refused with a clear message.
try(modify_table(rd_nnt, remove_N_obs = TRUE))

## Expected: "`remove_N_obs` is not supported for risk_difference() tables;
## nothing was changed", followed by the list of supported arguments.


## 13. Functions that do not accept risk-difference tables -------------------

uni <- uni_reg(bw, outcome = low, exposures = smoke, approach = "logit")

try(merge_tables(uni, rd_crude))
try(plot_reg(rd_crude))
try(forest_df(rd_crude))

## Expected: each fails with "risk_difference() tables are not supported by
## <function>()." and nothing else.


## 14. Inputs outside the version 1 limits -----------------------------------

## More than two exposure groups.
try(risk_difference(bw, outcome = low, exposure = race))

## A continuous outcome.
try(risk_difference(bw, outcome = bwt, exposure = smoke))

## The outcome used as an adjustment variable.
try(risk_difference(bw, outcome = low, exposure = smoke, adjust_for = low))

## A constant outcome: accepted unadjusted (coding is unambiguous) ...
const <- bw
const$low <- factor(rep("Normal BW", nrow(bw)), levels = levels(bw$low))
risk_difference(const, outcome = low, exposure = smoke)

## ... and refused when adjusted.
try(risk_difference(const, outcome = low, exposure = smoke, adjust_for = age))

## Expected:
## - race: "has 3 observed groups (White, Black, Other) ... compares exactly
##   two groups (version 1 limit)".
## - bwt: "has 131 observed values ... supports a binary outcome only".
## - adjust_for = low: "`adjust_for` must not contain the outcome or the
##   exposure."
## - Constant outcome, crude: 0 / 115 and 0 / 74, 0.0% in both groups,
##   0.0 (-3.2 to 5.0), p = 1.000.
## - Constant outcome, adjusted: stops with "the outcome is constant (no
##   events) in the analysis population. Consider the crude analysis".


## 15. Done -------------------------------------------------------------------

## Success means: every section ran, every "Expected" note matched, and no
## warning appeared other than those described above.
