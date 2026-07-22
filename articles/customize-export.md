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
  approach = logit
)
birthwt_multi <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit
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
| Yes | 5.99 (1.51–23.79) | 0.011 |
| Smoking during pregnancy |  |  |
| No | — |  |
| Yes | 2.87 (1.36–6.04) | 0.006 |
| Uterine irritability |  |  |
| No | — |  |
| Yes | 2.27 (0.98–5.24) | 0.055 |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |

Adjusted regression for low birth weight {.table .gt_table
quarto-disable-processing="false" quarto-bootstrap="false"}

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

[TABLE]

### Save Outputs

When no destination is supplied, save helpers use a temporary folder.

``` r

save_table(birthwt_merged, filename = "birthwt-table", format = html)

birthwt_plot <- plot_reg(
  birthwt_multi,
  title = "Adjusted Regression for Low Birth Weight"
)

save_plot(birthwt_plot, filename = "birthwt-forest", format = png)
```

### Word Reports

Use `format = flextable` when the destination is Word.

``` r

birthwt_multi_ft <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht", "ui"),
  adjust_for = c("age", "lwt", "race"),
  approach = logit,
  format = flextable
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
