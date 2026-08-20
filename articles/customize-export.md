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
[`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
is still useful for journal-specific wording, compact headers, captions,
and caveats. Use raw variable names on the left side of
`variable_labels` and `level_labels`; this keeps customisation stable
even when the visible table already shows prettier labels.

Footnotes and caveats are styled compactly by default across flextable
and gt outputs, which keeps abbreviation notes and adjustment notes
readable without making final tables unnecessarily tall.

> **Figure placeholder: anatomy of a gtregression table.** Add the slide
> image here to identify the caption, headers, characteristic labels,
> category levels, sample-size columns, abbreviations, automatic
> adjustment note, and custom caveat. The intended asset path is
> `vignettes/figures/modify-table-anatomy.png`.

### Customisation Options At A Glance

| Table part | Argument | Default | What it changes | Example |
|----|----|---:|----|----|
| Characteristic label | `variable_labels` | `NULL` | Renames variable/header rows using internal variable names. | `c(age = "Maternal age")` |
| Category level | `level_labels` | `NULL` | Renames factor levels while retaining their indentation and reference category. | `list(smoke = c(Yes = "Smoker"))` |
| Column header | `header_labels` | `NULL` | Renames visible headers. Common aliases include `estimate`, `p.value`, and `N`. | `c(estimate = "Adjusted OR")` |
| Caption | `caption` | `NULL` | Adds a manuscript-style title above the table. | `"Factors associated with low birth weight"` |
| Characteristic emphasis | `bold_labels` | `TRUE` | Bolds variable/characteristic rows. | `FALSE` |
| Category emphasis | `bold_levels` | `FALSE` | Bolds category-level rows. | `TRUE` |
| Characteristic style | `italic_labels` | `FALSE` | Italicizes variable/characteristic rows. | `TRUE` |
| Category style | `italic_levels` | `FALSE` | Italicizes category-level rows. | `TRUE` |
| Displayed N columns | `remove_N` | `FALSE` | Removes visible sample-size columns where present. | `TRUE` |
| Complete-case note | `remove_N_obs` | `FALSE` | Removes the `N = ... complete observations` footnote. | `TRUE` |
| Abbreviations | `remove_abbreviations` | `FALSE` | Removes the abbreviations footnote only. | `TRUE` |
| Adjustment note | `remove_adjustment_note` | `FALSE` | Removes the automatic `Adjusted for ...` note. | `TRUE` |
| Extra footnote | `caveat` | `NULL` | Adds a final study-specific interpretation or manuscript note. | `"Estimates use complete-case analysis."` |

Use raw internal variable names on the left side of `variable_labels`
and `level_labels`. `remove_N` removes visible table columns, whereas
`remove_N_obs` removes only the complete-case footnote. When replacing
the automatic adjustment note, use `remove_adjustment_note = TRUE`
together with a custom `caveat`.

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

birthwt_custom
```

## Merge Tables

[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
combines descriptive, crude, and adjusted results. Matching is based on
the original variable names, so merged tables remain aligned even when
the visible labels differ across input tables.

Merged tables use `flextable` by default, including when one or more
input tables were created with `format = "gt"`. Use `format = "gt"`
explicitly for an HTML-first merged table.

### Keep Binary Rows Consistent

Before merging a descriptive, crude, and adjusted table, use the same
binary row layout in every input. The clearest publication layout keeps
both levels in the descriptive table and displays the regression
reference row:

``` r

birthwt_desc <- descriptive_table(
  birthwt_data,
  exposures = exposures,
  by = "low",
  show_dichotomous = "all_levels"
)

birthwt_uni <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = exposures,
  approach = "logit",
  show_ref = TRUE
)
```

Use `show_ref = TRUE` for the adjusted regression table as well. If one
table uses compact binary rows while another displays both levels,
[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
warns before merging because additional rows can otherwise appear.
Compact tables are also supported when used consistently: set
`show_dichotomous = "single_row"` and `show_ref = FALSE` across the
relevant tables.

[`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md)
carries the footnotes already present in each input table. Exact
duplicate notes are shown once, while table-specific notes, including
the adjustment note from
[`multi_reg()`](https://gtregression.thinkdenominator.com/reference/multi_reg.md),
are retained unchanged. The adjustment note uses the same display labels
as the multivariable table; labels supplied through variable metadata or
[`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md)
are therefore reflected in the note. The same rule applies to adjusted
[`cox_reg()`](https://gtregression.thinkdenominator.com/reference/cox_reg.md)
and
[`surv_reg()`](https://gtregression.thinkdenominator.com/reference/surv_reg.md)
tables.

Use `modify_table(remove_adjustment_note = TRUE, caveat = "...")` when a
custom manuscript note is preferred.

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
| Ref. = reference category. |  |  |  |  |  |  |  |
| Adjusted for Maternal age, Maternal weight, and Maternal race |  |  |  |  |  |  |  |
| N = 189 complete observations included in each adjusted model. |  |  |  |  |  |  |  |

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
| Categorical variables shown as n (%); percentages are by column. |  |  |  |  |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |  |  |  |  |
| Abbreviations: OR = Odds Ratio; CI = Confidence Interval. |  |  |  |  |  |  |  |
| Ref. = reference category. |  |  |  |  |  |  |  |
| Adjusted for Maternal age, Maternal weight, and Maternal race |  |  |  |  |  |  |  |
| N = 189 complete observations included in each adjusted model. |  |  |  |  |  |  |  |
| Adjusted estimates are adjusted for maternal age, maternal weight, and maternal race. |  |  |  |  |  |  |  |

Clinical profile and regression estimates for low birth weight {.table
.cl-af9a8544 quarto-disable-processing="true"}

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
[`save_docx()`](https://gtregression.thinkdenominator.com/reference/save_docx.md).
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

- [`modify_table()`](https://gtregression.thinkdenominator.com/reference/modify_table.md):
  changed labels, caption, and caveat.
- [`merge_tables()`](https://gtregression.thinkdenominator.com/reference/merge_tables.md):
  `$table`, `$table_display`, and `$footnotes`.
- [`save_table()`](https://gtregression.thinkdenominator.com/reference/save_table.md),
  [`save_plot()`](https://gtregression.thinkdenominator.com/reference/save_plot.md),
  [`save_docx()`](https://gtregression.thinkdenominator.com/reference/save_docx.md):
  invisibly returned file paths.
- Raw variable names remain available for relabelling, merging, and
  testing even when display labels are used.
