# Customize, Merge, and Export

Make the output look like it belongs in the final report. Rename labels,
merge tables, and save tables or plots.

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
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

birthwt_exposures <- c("age", "lwt", "race", "smoke", "ht", "ui")

attr(birthwt_data$age, "label") <- "Maternal age"
attr(birthwt_data$lwt, "label") <- "Maternal weight"
attr(birthwt_data$race, "label") <- "Maternal race"
attr(birthwt_data$smoke, "label") <- "Smoking during pregnancy"
attr(birthwt_data$ht, "label") <- "Hypertension"
attr(birthwt_data$ui, "label") <- "Uterine irritability"

birthwt_desc <- descriptive_table(
  birthwt_data,
  exposures = birthwt_exposures,
  by = low
)
birthwt_uni <- uni_reg(
  birthwt_data,
  outcome = low,
  exposures = birthwt_exposures,
  approach = logit
)
birthwt_multi <- multi_reg(
  birthwt_data,
  outcome = low,
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit
)
```

## Customize Labels

If labels already live on the data, `gtregression` uses them
automatically.
[`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md)
is still useful for journal-specific wording, compact headers, captions,
and caveats. Use raw variable names on the left side of
`variable_labels` and `level_labels`; this keeps customisation stable
even when the visible table already shows prettier labels.

Footnotes and caveats are styled compactly by default across flextable
and gt outputs, which keeps abbreviation notes and adjustment notes
readable without making final tables unnecessarily tall.

``` r

birthwt_custom <- modify_table(
  birthwt_multi,
  variable_labels = c(
    smoke = "Smoked during pregnancy",
    ht = "History of hypertension",
    ui = "Uterine irritability"
  ),
  level_labels = list(
    smoke = c(Yes = "Smoker"),
    ht = c(Yes = "Hypertensive")
  ),
  header_labels = c(estimate = "Adjusted OR", p.value = "P"),
  caption = "Adjusted regression for low birth weight",
  caveat = "Adjusted for maternal age, maternal weight, and maternal race."
)

birthwt_custom$table
```

| Characteristic | Adjusted OR | P |
|----|----|----|
| Smoked during pregnancy |  |  |
| No | Ref. |  |
|  Smoker | 2.87 (1.36–6.04) | 0.006 |
| History of hypertension |  |  |
| No | Ref. |  |
|  Hypertensive | 5.99 (1.51–23.79) | 0.011 |
| Uterine irritability |  |  |
| No | Ref. |  |
|  Yes | 2.27 (0.98–5.24) | 0.055 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |
| Adjusted for maternal age, maternal weight, and maternal race. |  |  |

Adjusted regression for low birth weight {.table .cl-2d670660
quarto-disable-processing="true"}

## Merge Tables

[`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md)
combines descriptive, crude, and adjusted results. Matching is based on
the original variable names, so merged tables remain aligned even when
the visible labels differ across input tables.

``` r

birthwt_merged <- merge_tables(
  birthwt_desc,
  birthwt_uni,
  birthwt_multi,
  spanners = c("Descriptive", "Crude", "Adjusted")
)

birthwt_merged$table
```

|  | Descriptive |  | Crude |  |  | Adjusted |  |
|----|----|----|----|----|----|----|----|
| Characteristic | Normal BW | Low BW | N | OR (95% CI) | p-value | Adjusted OR (95% CI) | p-value |
| Maternal age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89-1.01) | 0.105 |  |  |
| Maternal weight | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97-1.00) | 0.023 |  |  |
| Maternal race |  |  | 189 |  |  |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | Ref. |  |  |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94-5.77) | 0.068 |  |  |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96-3.74) | 0.067 |  |  |
| Smoking during pregnancy |  |  | 189 |  |  |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | Ref. |  | Ref. |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08-3.78) | 0.028 | 2.87 (1.36–6.04) | 0.006 |
| Hypertension |  |  | 189 |  |  |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | Ref. |  | Ref. |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02-11.09) | 0.046 | 5.99 (1.51–23.79) | 0.011 |
| Uterine irritability |  |  | 189 |  |  |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | Ref. |  | Ref. |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14-5.83) | 0.023 | 2.27 (0.98–5.24) | 0.055 |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |
| Adjusted for age, lwt, and race |  |  |  |  |  |  |  |

The merged table can be polished after merging too.

``` r

birthwt_merged_paper <- modify_table(
  birthwt_merged,
  variable_labels = c(
    age = "Maternal age",
    lwt = "Maternal weight",
    race = "Maternal race",
    smoke = "Smoking during pregnancy",
    ht = "Hypertension",
    ui = "Uterine irritability"
  ),
  caption = "Clinical profile and regression estimates for low birth weight",
  caveat = "Adjusted estimates are adjusted for maternal age, maternal weight, and maternal race."
)

birthwt_merged_paper$table
```

|  | Descriptive |  | Crude |  |  | Adjusted |  |
|----|----|----|----|----|----|----|----|
| Characteristic | Normal BW | Low BW | N | OR (95% CI) | p-value | Adjusted OR (95% CI) | p-value |
| Maternal age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89-1.01) | 0.105 |  |  |
| Maternal weight | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97-1.00) | 0.023 |  |  |
| Maternal race |  |  | 189 |  |  |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | Ref. |  |  |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94-5.77) | 0.068 |  |  |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96-3.74) | 0.067 |  |  |
| Smoking during pregnancy |  |  | 189 |  |  |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | Ref. |  | Ref. |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08-3.78) | 0.028 | 2.87 (1.36–6.04) | 0.006 |
| Hypertension |  |  | 189 |  |  |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | Ref. |  | Ref. |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02-11.09) | 0.046 | 5.99 (1.51–23.79) | 0.011 |
| Uterine irritability |  |  | 189 |  |  |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | Ref. |  | Ref. |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14-5.83) | 0.023 | 2.27 (0.98–5.24) | 0.055 |
| Adjusted estimates are adjusted for maternal age, maternal weight, and maternal race. |  |  |  |  |  |  |  |

Clinical profile and regression estimates for low birth weight {.table
.cl-2de9b13c quarto-disable-processing="true"}

## Save Outputs

When no directory is supplied, save helpers use
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). This keeps examples
and tests CRAN-safe while still returning the file path invisibly.

``` r

table_path <- save_table(
  birthwt_merged_paper,
  filename = "birthwt-table",
  format = html
)

birthwt_plot <- plot_reg(
  birthwt_multi,
  title = "Adjusted Regression for Low Birth Weight"
)

plot_path <- save_plot(
  birthwt_plot,
  filename = "birthwt-forest",
  format = png
)
```

## Word Reports

`flextable` is the default table engine, so Word export works naturally.
If a table was created as `format = gt`, save it as HTML/PDF or recreate
it with `format = flextable` before sending it to
[`save_docx()`](https://thinkdenominator.github.io/gtregression/reference/save_docx.md).
Wide tables are fitted to a standard Word page by default; use
`table_width` when your document has different margins or landscape
orientation.

``` r

birthwt_multi_ft <- multi_reg(
  birthwt_data,
  outcome = low,
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit,
  format = flextable
)

docx_path <- save_docx(
  tables = list(birthwt_multi_ft),
  filename = "birthwt-report",
  titles = "Adjusted Regression",
  table_width = 6.5
)
```

## What To Inspect

- [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md):
  changed labels, caption, and caveat.
- [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md):
  `$table`, `$table_display`, and `$footnotes`.
- [`save_table()`](https://thinkdenominator.github.io/gtregression/reference/save_table.md),
  [`save_plot()`](https://thinkdenominator.github.io/gtregression/reference/save_plot.md),
  [`save_docx()`](https://thinkdenominator.github.io/gtregression/reference/save_docx.md):
  invisibly returned file paths.
- Raw variable names remain available for relabelling, merging, and
  testing even when display labels are used.
