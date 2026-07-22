# Customize, Merge, and Export

## Customize, Merge, and Export

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

birthwt_desc <- descriptive_table(
  birthwt_data,
  exposures = c("age", "lwt", "race", "smoke", "ht", "ui"),
  by = "low"
)
birthwt_uni <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "race", "smoke", "ht", "ui"),
  approach = "logit"
)
birthwt_multi <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = "logit"
)
```

### Customize Labels

``` r

birthwt_custom <- modify_table(
  birthwt_multi,
  variable_labels = c(
    smoke = "Smoking during pregnancy",
    ht = "Hypertension",
    ui = "Uterine irritability"
  ),
  header_labels = c(estimate = "Adjusted OR", p.value = "P"),
  caption = "Adjusted regression for low birth weight"
)

birthwt_custom$table
```

| Characteristic | Adjusted OR | P |
|----|----|----|
| Hypertension |  |  |
| No | — |  |
|  Yes | 5.99 (1.51–23.79) | 0.011 |
| Smoking during pregnancy |  |  |
| No | — |  |
|  Yes | 2.87 (1.36–6.04) | 0.006 |
| Uterine irritability |  |  |
| No | — |  |
|  Yes | 2.27 (0.98–5.24) | 0.055 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |

Adjusted regression for low birth weight {.table .cl-e58ba690
quarto-disable-processing="true"}

### Merge Tables

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
| age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 189 | 0.95 (0.89-1.01) | 0.105 |  |  |
| ht |  |  | 189 |  |  |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |  | -- |  | — |  |
|  Yes | 5 (3.8%) | 7 (11.9%) |  | 3.37 (1.02-11.09) | 0.046 | 5.99 (1.51–23.79) | 0.011 |
| lwt | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 189 | 0.99 (0.97-1.00) | 0.023 |  |  |
| race |  |  | 189 |  |  |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |  | -- |  |  |  |
|  Black | 15 (11.5%) | 11 (18.6%) |  | 2.33 (0.94-5.77) | 0.068 |  |  |
|  Other | 42 (32.3%) | 25 (42.4%) |  | 1.89 (0.96-3.74) | 0.067 |  |  |
| smoke |  |  | 189 |  |  |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |  | -- |  | — |  |
|  Yes | 44 (33.8%) | 30 (50.8%) |  | 2.02 (1.08-3.78) | 0.028 | 2.87 (1.36–6.04) | 0.006 |
| ui |  |  | 189 |  |  |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |  | -- |  | — |  |
|  Yes | 14 (10.8%) | 14 (23.7%) |  | 2.58 (1.14-5.83) | 0.023 | 2.27 (0.98–5.24) | 0.055 |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |
| Adjusted for age, lwt, and race |  |  |  |  |  |  |  |

### Save Outputs

When no destination is supplied, save helpers use a temporary folder.

``` r

save_table(birthwt_merged, filename = "birthwt-table", format = "html")

birthwt_plot <- plot_reg(
  birthwt_multi,
  title = "Adjusted Regression for Low Birth Weight"
)

save_plot(birthwt_plot, filename = "birthwt-forest", format = "png")
```

### Word Reports

Use `format = "flextable"` when the destination is Word.

``` r

birthwt_multi_ft <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = "logit",
  format = "flextable"
)

save_docx(
  tables = list(birthwt_multi_ft),
  filename = "birthwt-report",
  titles = "Adjusted Regression"
)
```

### What To Inspect

- [`modify_table()`](https://thinkdenominator.github.io/gtregression/reference/modify_table.md):
  changed labels, caption, and caveat.
- [`merge_tables()`](https://thinkdenominator.github.io/gtregression/reference/merge_tables.md):
  `$table`, `$table_display`, and `$footnotes`.
- [`save_table()`](https://thinkdenominator.github.io/gtregression/reference/save_table.md),
  [`save_plot()`](https://thinkdenominator.github.io/gtregression/reference/save_plot.md),
  [`save_docx()`](https://thinkdenominator.github.io/gtregression/reference/save_docx.md):
  returned file paths.
