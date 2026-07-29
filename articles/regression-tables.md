# Regression Tables

Go from fitted models to publication-ready tables without
hand-formatting effect estimates. `gtregression` supports logistic,
log-binomial, Poisson, robust Poisson, negative binomial, Cox survival,
parametric survival, and linear regression.

``` r

library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")

birthwt_data <- data_birthwt |>
  mutate(
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    ptl_cat = factor(ifelse(ptl > 0, "Yes", "No"), levels = c("No", "Yes")),
    ftv_cat = factor(case_when(
      ftv == 0 ~ "None",
      ftv == 1 ~ "One",
      ftv >= 2 ~ "Two or more"
    ), levels = c("None", "One", "Two or more"))
  )

birthwt_exposures <- c(
  "age", "lwt", "race", "smoke", "ht", "ui", "ptl_cat", "ftv_cat"
)

attr(birthwt_data$age, "label") <- "Maternal age"
attr(birthwt_data$lwt, "label") <- "Maternal weight"
attr(birthwt_data$race, "label") <- "Maternal race"
attr(birthwt_data$smoke, "label") <- "Smoking during pregnancy"
attr(birthwt_data$ht, "label") <- "Hypertension"
attr(birthwt_data$ui, "label") <- "Uterine irritability"
attr(birthwt_data$ptl_cat, "label") <- "Previous preterm labour"
attr(birthwt_data$ftv_cat, "label") <- "First trimester visits"
```

## Univariable Models

[`uni_reg()`](https://thinkdenominator.github.io/gtregression/reference/uni_reg.md)
fits one model per exposure and returns a table ready for reports.
Variable labels set with `attr(x, "label")` or
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
are used automatically in the displayed table, while raw column names
remain in `$table_body`.

``` r

birthwt_uni <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = birthwt_exposures,
  approach = "logit",
  theme = clinical
)

birthwt_uni$table
```

| Characteristic | N | OR (95% CI) | p-value |
|----|----|----|----|
| Maternal age | 189 | 0.95 (0.89-1.01) | 0.105 |
| Maternal weight | 189 | 0.99 (0.97-1.00) | 0.023 |
| Maternal race | 189 |  |  |
| White |  | Ref. |  |
|  Black |  | 2.33 (0.94-5.77) | 0.068 |
|  Other |  | 1.89 (0.96-3.74) | 0.067 |
| Smoking during pregnancy | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 2.02 (1.08-3.78) | 0.028 |
| Hypertension | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 3.37 (1.02-11.09) | 0.046 |
| Uterine irritability | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 2.58 (1.14-5.83) | 0.023 |
| Previous preterm labour | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 4.32 (1.92-9.73) | \<0.001 |
| First trimester visits | 189 |  |  |
| None |  | Ref. |  |
|  One |  | 0.54 (0.25-1.20) | 0.130 |
|  Two or more |  | 0.71 (0.32-1.56) | 0.394 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |
| Ref. = reference category. |  |  |  |

## Multivariable Models

With `adjust_for = NULL`,
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
fits all supplied exposures in one multivariable model. This is the
usual fully adjusted model when every exposure listed should appear in
the same formula.

``` r

birthwt_full <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c("age", "lwt", "race", "smoke", "ht", "ui"),
  approach = logit,
  theme = clinical
)

birthwt_full$table
```

| Characteristic | Adjusted OR (95% CI) | p-value |
|----|----|----|
| Maternal age | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight | 0.98 (0.97–1.00) | 0.018 |
| Maternal race |  |  |
| White | Ref. |  |
|  Black | 3.60 (1.28–10.10) | 0.015 |
|  Other | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy |  |  |
| No | Ref. |  |
|  Yes | 2.79 (1.29–6.05) | 0.009 |
| Hypertension |  |  |
| No | Ref. |  |
|  Yes | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability |  |  |
| No | Ref. |  |
|  Yes | 2.45 (1.02–5.90) | 0.046 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |
| Ref. = reference category. |  |  |
| N = 189 complete observations included in the multivariable model |  |  |

## Exposure-Specific Adjusted Models

Use `adjust_for` when you want one adjusted model per exposure, each
adjusted for the same core covariate set. This is useful for screening
several clinically important exposures while keeping the adjustment
strategy explicit.

``` r

birthwt_multi <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c("smoke", "ht", "ui", "ptl_cat", "ftv_cat"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit,
  theme = striped
)

birthwt_multi$table
```

| Characteristic | Adjusted OR (95% CI) | p-value |
|----|----|----|
| Smoking during pregnancy |  |  |
| No | Ref. |  |
|  Yes | 2.87 (1.36–6.04) | 0.006 |
| Hypertension |  |  |
| No | Ref. |  |
|  Yes | 5.99 (1.51–23.79) | 0.011 |
| Uterine irritability |  |  |
| No | Ref. |  |
|  Yes | 2.27 (0.98–5.24) | 0.055 |
| Previous preterm labour |  |  |
| No | Ref. |  |
|  Yes | 4.49 (1.90–10.58) | \<0.001 |
| First trimester visits |  |  |
| None | Ref. |  |
|  One | 0.60 (0.26–1.38) | 0.230 |
|  Two or more | 0.86 (0.38–1.96) | 0.717 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |
| Ref. = reference category. |  |  |
| Adjusted for age, lwt, and race |  |  |
| N = 189 complete observations included across outcome, exposure, and adjustment variables |  |  |

The adjustment variables are recorded in a compact table footnote, so
the result is ready for manuscript-style reporting without making the
table unnecessarily tall.

## Optional Model Statistics

Publication tables should stay readable. When you need model-fit
information, set `model_stats = TRUE` and inspect the returned object’s
`$model_stats` element. This keeps AIC, BIC, log-likelihood, deviance,
pseudo R-squared, and linear-model R-squared values available without
adding clutter to the main table.

``` r

birthwt_uni_stats <- uni_reg(
  data = birthwt_data,
  outcome = low,
  exposures = birthwt_exposures,
  approach = logit,
  model_stats = TRUE
)

birthwt_uni_stats$model_stats
```

    ##     model      AIC      BIC    logLik deviance null_deviance  pseudo_r2
    ## 1     age 235.9120 242.3955 -115.9560 231.9120       234.672 0.01176126
    ## 2     lwt 232.6907 239.1742 -114.3453 228.6907       234.672 0.02548803
    ## 3    race 235.6616 245.3869 -114.8308 229.6616       234.672 0.02135051
    ## 4   smoke 233.8046 240.2881 -114.9023 229.8046       234.672 0.02074128
    ## 5      ht 234.6499 241.1334 -115.3249 230.6499       234.672 0.01713938
    ## 6      ui 233.5959 240.0794 -114.7979 229.5959       234.672 0.02163060
    ## 7 ptl_cat 225.8978 232.3812 -110.9489 221.8978       234.672 0.05443445
    ## 8 ftv_cat 238.0851 247.8103 -116.0425 232.0851       234.672 0.01102346
    ##   r_squared adj_r_squared   n
    ## 1        NA            NA 189
    ## 2        NA            NA 189
    ## 3        NA            NA 189
    ## 4        NA            NA 189
    ## 5        NA            NA 189
    ## 6        NA            NA 189
    ## 7        NA            NA 189
    ## 8        NA            NA 189

For adjusted mode,
[`multi_reg()`](https://thinkdenominator.github.io/gtregression/reference/multi_reg.md)
returns one row of statistics per adjusted exposure-specific model.

``` r

birthwt_multi_stats <- multi_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c("smoke", "ht", "ui", "ptl_cat"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit,
  model_stats = TRUE
)

birthwt_multi_stats$model_stats
```

    ##     model      AIC      BIC    logLik deviance null_deviance  pseudo_r2
    ## 1   smoke 226.5772 246.0277 -107.2886 214.5772       234.672 0.08562914
    ## 2      ht 227.7487 247.1991 -107.8743 215.7487       234.672 0.08063739
    ## 3      ui 231.0194 250.4699 -109.5097 219.0194       234.672 0.06669985
    ## 4 ptl_cat 222.4421 241.8926 -105.2211 210.4421       234.672 0.10324998
    ##   r_squared adj_r_squared   n
    ## 1        NA            NA 189
    ## 2        NA            NA 189
    ## 3        NA            NA 189
    ## 4        NA            NA 189

## Other Effect Measures

Switch the `approach` to change the estimand.

``` r

uni_reg(
  data = birthwt_data,
  outcome = low,
  exposures = c("smoke", "ht", "ui", "ptl_cat"),
  approach = logbinomial
)$table
```

| Characteristic | N | RR (95% CI) | p-value |
|----|----|----|----|
| Smoking during pregnancy | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 1.61 (1.06-2.44) | 0.026 |
| Hypertension | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 1.99 (1.17-3.37) | 0.011 |
| Uterine irritability | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 1.79 (1.15-2.79) | 0.011 |
| Previous preterm labour | 189 |  |  |
| No |  | Ref. |  |
|  Yes |  | 2.33 (1.57-3.45) | \<0.001 |
| Abbreviations: RR = Risk Ratio; CI = Confidence Interval. |  |  |  |
| Ref. = reference category. |  |  |  |

Use `approach = firth` when a binary-outcome logistic model has sparse
cells, very wide intervals, or separation concerns. The output remains
an odds-ratio table, but the model is fitted with Firth penalized
logistic regression.

The built-in `data_endometrial` dataset is a useful teaching example
because neovascularization is completely absent among low-grade cases, a
pattern that can make ordinary logistic regression unstable.

``` r

data("data_endometrial", package = "gtregression")

endometrial_data <- data_endometrial |>
  mutate(
    HG = factor(HG, levels = c(0, 1),
                labels = c("Low grade", "High grade")),
    NV = factor(NV, levels = c(0, 1), labels = c("Absent", "Present"))
  )

multi_reg(
  data = endometrial_data,
  outcome = HG,
  exposures = c(NV, PI, EH),
  approach = firth
)$table
```

| Characteristic | Adjusted OR (95% CI) | p-value |
|----|----|----|
| NV |  |  |
| Absent | Ref. |  |
|  Present | 18.71 (1.84–2,577.65) | 0.009 |
| PI | 0.97 (0.88–1.04) | 0.387 |
| EH | 0.07 (0.01–0.29) | \<0.001 |
| Abbreviations: OR = Odds Ratio from Firth penalized logistic regression; CI = Confidence Interval. |  |  |
| Ref. = reference category. |  |  |
| N = 79 complete observations included in the multivariable model |  |  |

## Cox Survival Models

[`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md)
uses direct `time` and `event` arguments and returns hazard ratios.
Without `adjust_for`, the table shows crude HRs. With `adjust_for`, the
table shows adjusted HRs.

``` r

data("data_lungcancer", package = "gtregression")

lung_data <- data_lungcancer |>
  mutate(
    trt = factor(trt, levels = c(1, 2),
                 labels = c("Standard treatment", "Test treatment")),
    prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
  )

attr(lung_data$trt, "label") <- "Treatment group"
attr(lung_data$celltype, "label") <- "Cancer cell type"
attr(lung_data$karno, "label") <- "Karnofsky performance score"
attr(lung_data$age, "label") <- "Age"
attr(lung_data$prior, "label") <- "Prior therapy"
```

``` r

lung_hr <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "karno", "age"),
  theme = clinical
)

lung_hr$table
```

| Characteristic | HR (95% CI) | p-value |
|----|----|----|
| Treatment group |  |  |
| Standard treatment | Ref. |  |
|  Test treatment | 1.02 (0.71–1.45) | 0.922 |
| Cancer cell type |  |  |
| squamous | Ref. |  |
|  smallcell | 2.72 (1.66–4.47) | \<0.001 |
|  adeno | 3.15 (1.77–5.59) | \<0.001 |
|  large | 1.26 (0.73–2.17) | 0.407 |
| Karnofsky performance score | 0.97 (0.96–0.98) | \<0.001 |
| Age | 1.01 (0.99–1.03) | 0.433 |
| Abbreviations: HR = Hazard Ratio; CI = Confidence Interval. |  |  |
| Ref. = reference category. |  |  |
| Event variable: status (1 = event, 0 = censored after internal coding). |  |  |

``` r

lung_adj_hr <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior),
  adjust_for = c(age, karno),
  model_stats = TRUE,
  theme = striped
)

lung_adj_hr$table
```

| Characteristic | Adjusted HR (95% CI) | p-value |
|----|----|----|
| Treatment group |  |  |
| Standard treatment | Ref. |  |
|  Test treatment | 1.21 (0.84–1.74) | 0.307 |
| Cancer cell type |  |  |
| squamous | Ref. |  |
|  smallcell | 2.06 (1.26–3.39) | 0.004 |
|  adeno | 3.23 (1.82–5.74) | \<0.001 |
|  large | 1.38 (0.80–2.37) | 0.244 |
| Prior therapy |  |  |
| No | Ref. |  |
|  Yes | 0.96 (0.64–1.42) | 0.820 |
| Abbreviations: HR = Hazard Ratio; CI = Confidence Interval. |  |  |
| Ref. = reference category. |  |  |
| Adjusted for age and karno |  |  |
| Event variable: status (1 = event, 0 = censored after internal coding). |  |  |

``` r

lung_adj_hr$model_stats
```

    ##      model      AIC      BIC    logLik concordance events   n
    ## 1      trt 973.7560 982.3121 -483.8780   0.7119491    128 128
    ## 2 celltype 961.0882 975.3484 -475.5441   0.7349500    128 128
    ## 3    prior 974.7459 983.3019 -484.3729   0.7134257    128 128

## Parametric Survival Models

[`surv_reg()`](https://thinkdenominator.github.io/gtregression/reference/surv_reg.md)
uses the same `time`, `event`, `exposures`, and `adjust_for` grammar as
[`cox_reg()`](https://thinkdenominator.github.io/gtregression/reference/cox_reg.md),
but fits parametric survival models with
[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html).
The table reports time ratios rather than hazard ratios. A time ratio
above 1 suggests longer survival time; below 1 suggests shorter survival
time, conditional on the chosen distribution.

``` r

lung_time_ratio <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c("trt", "celltype", "karno", "age"),
  distribution = weibull,
  theme = clinical
)

lung_time_ratio$table
```

| Characteristic | Time Ratio (95% CI) | p-value |
|----|----|----|
| Treatment group |  |  |
| Standard treatment | Ref. |  |
|  Test treatment | 1.05 (0.70–1.58) | 0.818 |
| Cancer cell type |  |  |
| squamous | Ref. |  |
|  smallcell | 0.34 (0.21–0.54) | \<0.001 |
|  adeno | 0.30 (0.17–0.51) | \<0.001 |
|  large | 0.77 (0.45–1.32) | 0.339 |
| Karnofsky performance score | 1.04 (1.03–1.05) | \<0.001 |
| Age | 0.99 (0.97–1.01) | 0.284 |
| Abbreviations: Time Ratio = exponentiated accelerated failure time coefficient; CI = Confidence Interval. |  |  |
| Distribution: weibull. |  |  |
| Ref. = reference category. |  |  |
| Event variable: status (1 = event, 0 = censored after internal coding). |  |  |

``` r

lung_adj_time_ratio <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = c(trt, celltype, prior),
  adjust_for = c(age, karno),
  distribution = lognormal,
  model_stats = TRUE,
  theme = striped
)

lung_adj_time_ratio$table
```

| Characteristic | Adjusted Time Ratio (95% CI) | p-value |
|----|----|----|
| Treatment group |  |  |
| Standard treatment | Ref. |  |
|  Test treatment | 0.87 (0.60–1.27) | 0.468 |
| Cancer cell type |  |  |
| squamous | Ref. |  |
|  smallcell | 0.58 (0.36–0.93) | 0.025 |
|  adeno | 0.53 (0.31–0.91) | 0.022 |
|  large | 1.15 (0.67–1.99) | 0.609 |
| Prior therapy |  |  |
| No | Ref. |  |
|  Yes | 1.03 (0.68–1.57) | 0.885 |
| Abbreviations: Time Ratio = exponentiated accelerated failure time coefficient; CI = Confidence Interval. |  |  |
| Distribution: lognormal. |  |  |
| Ref. = reference category. |  |  |
| Adjusted for age and karno |  |  |
| Event variable: status (1 = event, 0 = censored after internal coding). |  |  |

``` r

lung_adj_time_ratio$model_stats
```

    ##      model distribution      AIC      BIC    logLik    scale events   n
    ## 1      trt    lognormal 1451.264 1465.864 -720.6318 1.110644    128 137
    ## 2 celltype    lognormal 1444.379 1464.819 -715.1894 1.064351    128 137
    ## 3    prior    lognormal 1451.768 1466.368 -720.8840 1.112741    128 137

## Continuous Outcomes

Linear regression outputs beta coefficients and keeps diagnostics under
`$reg_check`.

``` r

birthwt_linear <- multi_reg(
  data = birthwt_data,
  outcome = bwt,
  exposures = c("age", "lwt", "race", "smoke", "ht", "ui"),
  approach = linear
)

birthwt_linear$table
```

| Characteristic | Adjusted Beta (95% CI) | p-value |
|----|----|----|
| Maternal age | -4.67 (-22.97–13.63) | 0.617 |
| Maternal weight | 4.40 (1.05–7.74) | 0.011 |
| Maternal race |  |  |
| White | Ref. |  |
|  Black | -490.64 (-783.04–-198.23) | 0.001 |
|  Other | -356.61 (-579.77–-133.46) | 0.002 |
| Smoking during pregnancy |  |  |
| No | Ref. |  |
|  Yes | -360.71 (-564.60–-156.82) | \<0.001 |
| Hypertension |  |  |
| No | Ref. |  |
|  Yes | -590.03 (-982.59–-197.47) | 0.004 |
| Uterine irritability |  |  |
| No | Ref. |  |
|  Yes | -528.53 (-793.30–-263.77) | \<0.001 |
| Abbreviations: Beta = Linear regression coefficient; CI = Confidence Interval. |  |  |
| Ref. = reference category. |  |  |
| N = 189 complete observations included in the multivariable model |  |  |

## What To Inspect

- `$table`: publication-ready table.
- `$table_body`: numeric estimates behind the display.
- `$models`: fitted model objects.
- `$model_summaries`: model-level summaries.
- `$model_stats`: optional model-fit statistics when
  `model_stats = TRUE`.
- `$variable_labels`: labels used in publication output.
- `$reg_check`: diagnostics for linear models.
