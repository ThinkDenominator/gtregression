# Descriptive Tables

## Descriptive Tables

Start with a table that people can actually read.
[`descriptive_table()`](https://thinkdenominator.github.io/gtregression/reference/descriptive_table.md)
creates publication-ready summaries for continuous, categorical, and
mixed exposure sets.

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

### Column Percentages

Use column percentages when the table is grouped by outcome or another
column. Continuous variables are shown as median (IQR) by default, and
variable labels are picked up automatically when variables have a
`"label"` attribute.

``` r

desc_column <- descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = "column",
  show_overall = "last",
  theme = clinical
)

desc_column$table
```

| Characteristic | Normal BW, N=130 | Low BW, N=59 | Overall, N=189 |
|----|----|----|----|
| Maternal age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 23.0 (19.0-26.0) |
| Maternal weight | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 121.0 (110.0-140.0) |
| Maternal race |  |  |  |
|  White | 73 (56.2%) | 23 (39.0%) | 96 (50.8%) |
|  Black | 15 (11.5%) | 11 (18.6%) | 26 (13.8%) |
|  Other | 42 (32.3%) | 25 (42.4%) | 67 (35.4%) |
| Smoking during pregnancy |  |  |  |
|  No | 86 (66.2%) | 29 (49.2%) | 115 (60.8%) |
|  Yes | 44 (33.8%) | 30 (50.8%) | 74 (39.2%) |
| Hypertension |  |  |  |
|  No | 125 (96.2%) | 52 (88.1%) | 177 (93.7%) |
|  Yes | 5 (3.8%) | 7 (11.9%) | 12 (6.3%) |
| Uterine irritability |  |  |  |
|  No | 116 (89.2%) | 45 (76.3%) | 161 (85.2%) |
|  Yes | 14 (10.8%) | 14 (23.7%) | 28 (14.8%) |
| Previous preterm labour |  |  |  |
|  No | 118 (90.8%) | 41 (69.5%) | 159 (84.1%) |
|  Yes | 12 (9.2%) | 18 (30.5%) | 30 (15.9%) |
| First trimester visits |  |  |  |
|  None | 64 (49.2%) | 36 (61.0%) | 100 (52.9%) |
|  One | 36 (27.7%) | 11 (18.6%) | 47 (24.9%) |
|  Two or more | 30 (23.1%) | 12 (20.3%) | 42 (22.2%) |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |

### Row Percentages

Use row percentages when the question is how each exposure level is
distributed across groups. Common option values can be written with or
without quotes.

``` r

descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = "row",
  show_overall = "first",
  show_missing = no,
  theme = striped
)$table
```

| Characteristic | Overall, N=189 | Normal BW, N=130 | Low BW, N=59 |
|----|----|----|----|
| Maternal age | 23.0 (19.0-26.0) | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) |
| Maternal weight | 121.0 (110.0-140.0) | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) |
| Maternal race |  |  |  |
|  White | 96 | 73 (76.0%) | 23 (24.0%) |
|  Black | 26 | 15 (57.7%) | 11 (42.3%) |
|  Other | 67 | 42 (62.7%) | 25 (37.3%) |
| Smoking during pregnancy |  |  |  |
|  No | 115 | 86 (74.8%) | 29 (25.2%) |
|  Yes | 74 | 44 (59.5%) | 30 (40.5%) |
| Hypertension |  |  |  |
|  No | 177 | 125 (70.6%) | 52 (29.4%) |
|  Yes | 12 | 5 (41.7%) | 7 (58.3%) |
| Uterine irritability |  |  |  |
|  No | 161 | 116 (72.0%) | 45 (28.0%) |
|  Yes | 28 | 14 (50.0%) | 14 (50.0%) |
| Previous preterm labour |  |  |  |
|  No | 159 | 118 (74.2%) | 41 (25.8%) |
|  Yes | 30 | 12 (40.0%) | 18 (60.0%) |
| First trimester visits |  |  |  |
|  None | 100 | 64 (64.0%) | 36 (36.0%) |
|  One | 47 | 36 (76.6%) | 11 (23.4%) |
|  Two or more | 42 | 30 (71.4%) | 12 (28.6%) |
| Categorical variables shown as n (%); percentages are by row (Overall shows counts). |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |

### Summary Choices

Use `statistic` when continuous variables need a different summary. A
single value applies to all numeric variables. A named vector lets you
mix summaries, including treating numeric ordinal variables as
categorical.

``` r

descriptive_table(
  data = birthwt_data,
  exposures = c("age", "lwt", "ftv", "smoke"),
  by = low,
  statistic = c(
    age = mean,
    lwt = median,
    ftv = categorical
  ),
  percent = column,
  show_missing = no
)$table
```

| Characteristic | Normal BW, N=130 | Low BW, N=59 |
|----|----|----|
| Maternal age | 23.7 (5.6) | 22.3 (4.5) |
| Maternal weight | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) |
| ftv |  |  |
|  0 | 64 (49.2%) | 36 (61.0%) |
|  1 | 36 (27.7%) | 11 (18.6%) |
|  2 | 23 (17.7%) | 7 (11.9%) |
|  3 | 3 (2.3%) | 4 (6.8%) |
|  4 | 3 (2.3%) | 1 (1.7%) |
|  6 | 1 (0.8%) | 0 (0.0%) |
| Smoking during pregnancy |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |
|  Yes | 44 (33.8%) | 30 (50.8%) |
| Categorical variables shown as n (%); percentages are by column. |  |  |
| Continuous summaries: age = Mean (SD); lwt = Median (IQR). |  |  |

The quoted form is equivalent and often clearer in saved scripts:

``` r

descriptive_table(
  data = birthwt_data,
  exposures = c("age", "lwt", "ftv", "smoke"),
  by = "low",
  statistic = c(
    age = "mean",
    lwt = "median",
    ftv = "categorical"
  )
)
```

### Output Format

`flextable` is the default because it behaves well in Word workflows.
Use `format = gt` when the output is mainly for HTML or pkgdown.

``` r

descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = "column",
  format = gt
)$table
```

| Characteristic | Normal BW, N=130 | Low BW, N=59 |
|----|----|----|
| Maternal age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) |
| Maternal weight | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) |
| Maternal race |  |  |
| White | 73 (56.2%) | 23 (39.0%) |
| Black | 15 (11.5%) | 11 (18.6%) |
| Other | 42 (32.3%) | 25 (42.4%) |
| Smoking during pregnancy |  |  |
| No | 86 (66.2%) | 29 (49.2%) |
| Yes | 44 (33.8%) | 30 (50.8%) |
| Hypertension |  |  |
| No | 125 (96.2%) | 52 (88.1%) |
| Yes | 5 (3.8%) | 7 (11.9%) |
| Uterine irritability |  |  |
| No | 116 (89.2%) | 45 (76.3%) |
| Yes | 14 (10.8%) | 14 (23.7%) |
| Previous preterm labour |  |  |
| No | 118 (90.8%) | 41 (69.5%) |
| Yes | 12 (9.2%) | 18 (30.5%) |
| First trimester visits |  |  |
| None | 64 (49.2%) | 36 (61.0%) |
| One | 36 (27.7%) | 11 (18.6%) |
| Two or more | 30 (23.1%) | 12 (20.3%) |
| Categorical variables shown as n (%); percentages are by column. |  |  |
| Continuous variables shown as Median (IQR). |  |  |

### What To Inspect

- `$table`: rendered `gt` or `flextable` output.
- `$table_body`: clean data behind the table.
- `$variable_labels`: labels used for display; raw variable names remain
  in `$table_body` for reliable merging and modification.
- `$format`: output format used by the table builder.
