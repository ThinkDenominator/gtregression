# Merge and Customise Tables

A manuscript table often brings several analyses together: the baseline
characteristics, the crude estimates and the adjusted estimates, or a
primary analysis next to its sensitivity analyses.
[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
places any number of `gtregression` tables side by side, aligns the rows
by variable and category, and keeps every footnote.

This article covers:

- merging two, three or more tables;
- labelling each block with `spanners`;
- keeping binary rows consistent;
- merging sensitivity analyses and survival models;
- relabelling the merged table with
  [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md);
- further styling with `flextable` functions;
- saving the result.

## Example data

``` r

library(gtregression)
library(dplyr)

data("data_birthwt", package = "gtregression")

birthwt <- data_birthwt |>
  mutate(
    race = factor(race, levels = c(1, 2, 3), labels = c("White", "Black", "Other")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

attr(birthwt$age, "label") <- "Maternal age (years)"
attr(birthwt$lwt, "label") <- "Maternal weight (lb)"
attr(birthwt$race, "label") <- "Race"
attr(birthwt$smoke, "label") <- "Smoking during pregnancy"
attr(birthwt$ht, "label") <- "History of hypertension"
attr(birthwt$ui, "label") <- "Uterine irritability"

exposures <- c("age", "lwt", "race", "smoke", "ht", "ui")
```

## Build the tables you want to combine

Use the same binary layout in every table you plan to merge. Showing
both levels in the descriptive table and the reference row in each
regression table gives one row per category across the whole merged
table.

``` r

desc <- descriptive_table(
  birthwt,
  exposures = exposures,
  by = "low",
  show_dichotomous = "all_levels"
)

crude <- uni_reg(
  birthwt,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  show_ref = TRUE
)

adjusted <- multi_reg(
  birthwt,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  show_ref = TRUE
)
```

## Merge two tables

Supply the tables in the order they should appear, and one spanner label
per table.

``` r

crude_adjusted <- merge_tables(
  crude,
  adjusted,
  spanners = c("Crude", "Adjusted")
)

crude_adjusted$table
```

|  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|
| Characteristic | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race | 189 |  |  | 189 |  |  |
|  White |  | Ref. |  |  | Ref. |  |
|  Black |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |

## Merge three or more tables

There is no limit of two: pass as many tables as you need. The classic
“Table 2” combines the descriptive summary, the crude estimates and the
adjusted estimates.

``` r

table_2 <- merge_tables(
  desc,
  crude,
  adjusted,
  spanners = c("Low birth weight", "Crude", "Adjusted")
)

table_2$table
```

|  | Low birth weight |  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|----|----|
| Characteristic | Normal BW, N=130 | Low BW, N=59 | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race |  |  | 189 |  |  | 189 |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | Ref. |  |  | Ref. |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy |  |  | 189 |  |  | 189 |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | Ref. |  |  | Ref. |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension |  |  | 189 |  |  | 189 |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | Ref. |  |  | Ref. |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability |  |  | 189 |  |  | 189 |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | Ref. |  |  | Ref. |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |  |  |

Rows are matched on the underlying variable and category, not on the
visible label, so the tables stay aligned even when labels differ. A
variable that appears in some tables but not in others is kept and left
blank where it was not analysed.

If one table shows binary variables as a single row and another shows
both levels,
[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
warns and names the settings that make them compatible
(`show_dichotomous = "all_levels"` with `show_ref = TRUE`, or
`show_dichotomous = "single_row"` with `show_ref = FALSE`, used
consistently).

## Sensitivity analyses side by side

Tables of the same type can be merged too. Here a primary model is shown
next to a model restricted to mothers without hypertension and a model
with fewer covariates.

``` r

primary <- multi_reg(
  birthwt,
  outcome = "low",
  exposures = c("smoke", "race", "ui"),
  adjust_for = c("age", "lwt"),
  approach = "logit",
  show_ref = TRUE
)

no_hypertension <- multi_reg(
  filter(birthwt, ht == "No"),
  outcome = "low",
  exposures = c("smoke", "race", "ui"),
  adjust_for = c("age", "lwt"),
  approach = "logit",
  show_ref = TRUE
)

fewer_covariates <- multi_reg(
  birthwt,
  outcome = "low",
  exposures = c("smoke", "race", "ui"),
  adjust_for = "age",
  approach = "logit",
  show_ref = TRUE
)

sensitivity <- merge_tables(
  primary,
  no_hypertension,
  fewer_covariates,
  spanners = c("Primary analysis", "Without hypertension", "Age-adjusted only")
)

sensitivity$table
```

|  | Primary analysis |  |  | Without hypertension |  |  | Age-adjusted only |  |  |
|----|----|----|----|----|----|----|----|----|----|
| Characteristic | N | Adjusted OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Smoking during pregnancy | 189 |  |  | 177 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |  | Ref. |  |
|  Yes |  | 1.96 (1.03–3.70) | 0.040 |  | 1.96 (0.99–3.86) | 0.052 |  | 2.00 (1.06–3.75) | 0.032 |
| Race | 189 |  |  | 177 |  |  | 189 |  |  |
|  White |  | Ref. |  |  | Ref. |  |  | Ref. |  |
|  Black |  | 2.73 (1.03–7.24) | 0.044 |  | 2.41 (0.85–6.85) | 0.099 |  | 2.11 (0.84–5.31) | 0.114 |
|  Other |  | 1.56 (0.77–3.16) | 0.218 |  | 1.42 (0.67–2.98) | 0.358 |  | 1.77 (0.89–3.53) | 0.106 |
| Uterine irritability | 189 |  |  | 177 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.22 (0.96–5.12) | 0.061 |  | 2.50 (1.06–5.85) | 0.035 |  | 2.48 (1.09–5.64) | 0.030 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |  |  |  |
| Adjusted for Maternal age (years) and Maternal weight (lb) |  |  |  |  |  |  |  |  |  |
| N = 189 complete observations included in each adjusted model. |  |  |  |  |  |  |  |  |  |
| N = 177 complete observations included in each adjusted model. |  |  |  |  |  |  |  |  |  |
| Adjusted for Maternal age (years) |  |  |  |  |  |  |  |  |  |

Each adjustment note is carried over from its own table, so readers can
see exactly what each block was adjusted for.

## Survival tables

Cox and parametric survival tables merge in the same way. Unadjusted and
adjusted hazard ratios are a common pair.

``` r

data("data_lungcancer", package = "gtregression")

lung <- data_lungcancer |>
  mutate(
    trt = factor(trt, levels = c(1, 2), labels = c("Standard", "Test")),
    celltype = factor(celltype)
  )

cox_crude <- cox_reg(
  lung,
  time = "time",
  event = "status",
  exposures = c("trt", "celltype", "karno"),
  show_ref = TRUE
)

cox_adjusted <- cox_reg(
  lung,
  time = "time",
  event = "status",
  exposures = c("trt", "celltype", "karno"),
  multivariable = TRUE,
  show_ref = TRUE
)

survival_table <- merge_tables(
  cox_crude,
  cox_adjusted,
  spanners = c("Unadjusted HR", "Adjusted HR")
)

survival_table$table
```

|  | Unadjusted HR |  |  | Adjusted HR |  |  |
|----|----|----|----|----|----|----|
| Characteristic | N | HR (95% CI) | p-value | N | Adjusted HR (95% CI) | p-value |
| trt | 137 |  |  | 137 |  |  |
|  Standard |  | Ref. |  |  | Ref. |  |
|  Test |  | 1.02 (0.71–1.45) | 0.922 |  | 1.30 (0.88–1.93) | 0.193 |
| celltype | 137 |  |  | 137 |  |  |
|  squamous |  | Ref. |  |  | Ref. |  |
|  smallcell |  | 2.72 (1.66–4.47) | \<0.001 |  | 2.28 (1.35–3.87) | 0.002 |
|  adeno |  | 3.15 (1.77–5.59) | \<0.001 |  | 3.17 (1.78–5.65) | \<0.001 |
|  large |  | 1.26 (0.73–2.17) | 0.407 |  | 1.48 (0.85–2.58) | 0.162 |
| karno | 137 | 0.97 (0.96–0.98) | \<0.001 | 137 | 0.97 (0.96–0.98) | \<0.001 |
| Abbreviations: HR = Hazard Ratio; CI = Confidence Interval. |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |
| N is the number of complete observations used in each model. |  |  |  |  |  |  |
| Number of events: 128. |  |  |  |  |  |  |
| Adjusted for the other variables in the model. |  |  |  |  |  |  |

## Relabel with modify_table()

[`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
works on merged tables. Use the original variable names on the left-hand
side; the visible labels can be anything.

``` r

table_2_final <- modify_table(
  table_2,
  variable_labels = c(lwt = "Weight at last menstrual period (lb)"),
  level_labels = list(smoke = c(Yes = "Smoker", No = "Non-smoker")),
  caption = "Table 2. Factors associated with low birth weight",
  caveat = "Odds ratios from logistic regression; adjusted estimates include all listed characteristics."
)

table_2_final$table
```

|  | Low birth weight |  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|----|----|
| Characteristic | Normal BW, N=130 | Low BW, N=59 | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race |  |  | 189 |  |  | 189 |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | Ref. |  |  | Ref. |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy |  |  | 189 |  |  | 189 |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | Ref. |  |  | Ref. |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension |  |  | 189 |  |  | 189 |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | Ref. |  |  | Ref. |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability |  |  | 189 |  |  | 189 |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | Ref. |  |  | Ref. |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |  |  |
| Odds ratios from logistic regression; adjusted estimates include all listed characteristics. |  |  |  |  |  |  |  |  |

Table 2. Factors associated with low birth weight {.table .cl-d8bad469
quarto-disable-processing="true"}

## Choose a table theme

Every table function and
[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
accept a `theme`. Presets are `"minimal"` (the default), `"clinical"`,
`"striped"`, `"shaded"` and `"jama"`. You can also combine primitives:
`"plain"`, `"zebra"`, `"lines"`, `"labels_bold"`, `"compact"` and
`"header_shaded"`.

``` r

jama_table <- merge_tables(
  crude,
  adjusted,
  spanners = c("Crude", "Adjusted"),
  theme = "jama"
)

jama_table$table
```

|  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|
| Characteristic | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race | 189 |  |  | 189 |  |  |
|  White |  | Ref. |  |  | Ref. |  |
|  Black |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |

``` r


custom_theme_table <- merge_tables(
  crude,
  adjusted,
  spanners = c("Crude", "Adjusted"),
  theme = c("zebra", "compact", "labels_bold")
)

custom_theme_table$table
```

|  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|
| Characteristic | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race | 189 |  |  | 189 |  |  |
|  White |  | Ref. |  |  | Ref. |  |
|  Black |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability | 189 |  |  | 189 |  |  |
|  No |  | Ref. |  |  | Ref. |  |
|  Yes |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |

## Fine-tune with flextable

The merged table is an ordinary `flextable` stored in `$table`. Anything
the [flextable](https://ardata-fr.github.io/flextable-book/) package can
do can be applied to it: fonts, colours, widths, alignment or
highlighting particular cells. Take the table out, change it, and keep
the result.

``` r

library(flextable)

ft <- table_2_final$table

ft <- ft |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 9, part = "all") |>
  color(color = "#0F5C5F", part = "header") |>
  bg(bg = "#EEF6F6", part = "header") |>
  align(j = 2:ncol_keys(ft), align = "center", part = "all") |>
  autofit()

ft
```

|  | Low birth weight |  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|----|----|
| Characteristic | Normal BW, N=130 | Low BW, N=59 | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race |  |  | 189 |  |  | 189 |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | Ref. |  |  | Ref. |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy |  |  | 189 |  |  | 189 |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | Ref. |  |  | Ref. |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension |  |  | 189 |  |  | 189 |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | Ref. |  |  | Ref. |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability |  |  | 189 |  |  | 189 |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | Ref. |  |  | Ref. |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |  |  |
| Odds ratios from logistic regression; adjusted estimates include all listed characteristics. |  |  |  |  |  |  |  |  |

Table 2. Factors associated with low birth weight {.table .cl-d8bad469
quarto-disable-processing="true"}

To change particular cells, refer to columns by their keys and to rows
by number or by a condition on the cell values. Each block of a merged
table has its own keys, ending in `_p1`, `_p2`, `_p3` and so on, in the
order the tables were merged.

``` r

ft$col_keys
#> [1] "Characteristic"          "Normal.BW_p1"           
#> [3] "Low.BW_p1"               "N_p2"                   
#> [5] "OR..95..CI._p2"          "p.value_p2"             
#> [7] "N_p3"                    "Adjusted.OR..95..CI._p3"
#> [9] "p.value_p3"
```

For example, to bold the adjusted p-values below 0.05 (the third block):

``` r

adjusted_p <- ft$col_keys[startsWith(ft$col_keys, "p.value") & endsWith(ft$col_keys, "_p3")]
p_values <- suppressWarnings(as.numeric(sub("<", "", ft$body$dataset[[adjusted_p]])))

ft <- bold(ft, i = which(p_values < 0.05), j = adjusted_p, part = "body")
ft
```

|  | Low birth weight |  | Crude |  |  | Adjusted |  |  |
|----|----|----|----|----|----|----|----|----|
| Characteristic | Normal BW, N=130 | Low BW, N=59 | N | OR (95% CI) | p-value | N | Adjusted OR (95% CI) | p-value |
| Maternal age (years) | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89–1.01) | 0.105 | 189 | 0.98 (0.92–1.05) | 0.606 |
| Maternal weight (lb) | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97–1.00) | 0.023 | 189 | 0.98 (0.97–1.00) | 0.018 |
| Race |  |  | 189 |  |  | 189 |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | Ref. |  |  | Ref. |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94–5.77) | 0.068 |  | 3.60 (1.28–10.10) | 0.015 |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96–3.74) | 0.067 |  | 2.46 (1.05–5.77) | 0.038 |
| Smoking during pregnancy |  |  | 189 |  |  | 189 |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | Ref. |  |  | Ref. |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08–3.78) | 0.028 |  | 2.79 (1.29–6.05) | 0.009 |
| History of hypertension |  |  | 189 |  |  | 189 |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | Ref. |  |  | Ref. |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02–11.09) | 0.046 |  | 6.41 (1.66–24.72) | 0.007 |
| Uterine irritability |  |  | 189 |  |  | 189 |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | Ref. |  |  | Ref. |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14–5.83) | 0.023 |  | 2.45 (1.02–5.90) | 0.046 |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |  |  |
| N = 189 complete observations included in the model. |  |  |  |  |  |  |  |  |
| Odds ratios from logistic regression; adjusted estimates include all listed characteristics. |  |  |  |  |  |  |  |  |

Table 2. Factors associated with low birth weight {.table .cl-d8bad469
quarto-disable-processing="true"}

Changes made this way apply to the flextable only. Keep the original
`gtregression` object if you want to merge it again later, and apply
flextable styling as the final step.

For HTML-first work, use `format = "gt"` in
[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
and style the result with the [gt](https://gt.rstudio.com/) package in
the same way.

## Save the result

[`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md)
accepts the `gtregression` object or a styled flextable. The file type
is set by `format`.

``` r

save_table(table_2_final, filename = "table-2", format = "docx")
save_table(ft, filename = "table-2-styled", format = "docx")
save_table(table_2_final, filename = "table-2", format = "html")
```

Several tables can go into one Word document with
[`save_docx()`](https://gtregression.thinkdenominator.com/reference/save_docx.md):

``` r

save_docx(
  tables = list(table_2_final, primary),
  filename = "results",
  titles = c("Table 2. Low birth weight", "Table 3. Sensitivity analyses")
)
```

## Which tables can be merged?

| Can be merged | Notes |
|----|----|
| [`descriptive_table()`](https://gtregression.thinkdenominator.com/reference/descriptive_table.md) | Use `show_dichotomous` consistently. |
| [`uni_reg()`](https://gtregression.thinkdenominator.com/reference/uni_reg.md), [`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md) and the stratified functions | Use `show_ref` consistently. |
| [`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md), [`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md), [`competing_risk_reg()`](https://gtregression.thinkdenominator.com/reference/competing_risk_reg.md) | Crude and adjusted pairs, or sensitivity analyses. |
| [`ordinal_reg()`](https://gtregression.thinkdenominator.com/reference/ordinal_reg.md), [`multinomial_reg()`](https://gtregression.thinkdenominator.com/reference/multinomial_reg.md), [`multilevel_reg()`](https://gtregression.thinkdenominator.com/reference/multilevel_reg.md), [`conditional_logit_reg()`](https://gtregression.thinkdenominator.com/reference/conditional_logit_reg.md) | Each keeps its own estimate type and notes. |
| A merged table | Merge again, or relabel with [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md). |

[`risk_difference()`](https://gtregression.thinkdenominator.com/reference/risk_difference.md)
tables are designed to stand alone and are not merged; see [Risk
Difference and
NNT](https://gtregression.thinkdenominator.com/articles/risk-difference.md).
