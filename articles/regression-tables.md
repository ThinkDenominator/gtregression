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

[`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md)
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

birthwt_uni
```

## Multivariable Models

With `adjust_for = NULL`,
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
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

birthwt_full
```

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

birthwt_multi
```

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
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md)
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
)
```

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
)
```

## Cox Survival Models

[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
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

lung_hr
```

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

lung_adj_hr
lung_adj_hr$model_stats
```

    ##      model      AIC      BIC    logLik concordance events   n
    ## 1      trt 973.7560 982.3121 -483.8780   0.7119491    128 137
    ## 2 celltype 961.0882 975.3484 -475.5441   0.7349500    128 137
    ## 3    prior 974.7459 983.3019 -484.3729   0.7134257    128 137

Use `interaction = exposure*modifier` for planned effect modification.
In the default exposure-by-exposure workflow, provide one exposure.

``` r

cox_interaction <- cox_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno),
  interaction = trt*prior
)

cox_interaction
```

## Parametric Survival Models

[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
uses the same `time`, `event`, `exposures`, and `adjust_for` grammar as
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md),
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

lung_time_ratio
```

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

lung_adj_time_ratio
lung_adj_time_ratio$model_stats
```

    ##      model distribution      AIC      BIC    logLik    scale events   n
    ## 1      trt    lognormal 1451.264 1465.864 -720.6318 1.110644    128 137
    ## 2 celltype    lognormal 1444.379 1464.819 -715.1894 1.064351    128 137
    ## 3    prior    lognormal 1451.768 1466.368 -720.8840 1.112741    128 137

``` r

surv_interaction <- surv_reg(
  data = lung_data,
  time = time,
  event = status,
  exposures = trt,
  adjust_for = c(age, karno),
  interaction = trt*prior,
  distribution = weibull
)

surv_interaction
```

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

birthwt_linear
```

## What To Inspect

- `$table`: publication-ready table.
- `$table_body`: numeric estimates behind the display.
- `$models`: fitted model objects.
- `$model_summaries`: model-level summaries.
- `$model_stats`: optional model-fit statistics when
  `model_stats = TRUE`.
- `$variable_labels`: labels used in publication output.
- `$reg_check`: diagnostics for linear models.
