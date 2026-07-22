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
```

### Column Percentages

Use column percentages when the table is grouped by outcome or another
column.

``` r

desc_column <- descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = column,
  show_overall = last,
  theme = clinical
)

desc_column$table
```

| Characteristic | Normal BW, N=130 | Low BW, N=59 | Overall, N=189 |
|----|----|----|----|
| age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) | 23.0 (19.0-26.0) |
| ftv_cat |  |  |  |
| None | 64 (49.2%) | 36 (61.0%) | 100 (52.9%) |
| One | 36 (27.7%) | 11 (18.6%) | 47 (24.9%) |
| Two or more | 30 (23.1%) | 12 (20.3%) | 42 (22.2%) |
| ht |  |  |  |
| No | 125 (96.2%) | 52 (88.1%) | 177 (93.7%) |
| Yes | 5 (3.8%) | 7 (11.9%) | 12 (6.3%) |
| lwt | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) | 121.0 (110.0-140.0) |
| ptl_cat |  |  |  |
| No | 118 (90.8%) | 41 (69.5%) | 159 (84.1%) |
| Yes | 12 (9.2%) | 18 (30.5%) | 30 (15.9%) |
| race |  |  |  |
| White | 73 (56.2%) | 23 (39.0%) | 96 (50.8%) |
| Black | 15 (11.5%) | 11 (18.6%) | 26 (13.8%) |
| Other | 42 (32.3%) | 25 (42.4%) | 67 (35.4%) |
| smoke |  |  |  |
| No | 86 (66.2%) | 29 (49.2%) | 115 (60.8%) |
| Yes | 44 (33.8%) | 30 (50.8%) | 74 (39.2%) |
| ui |  |  |  |
| No | 116 (89.2%) | 45 (76.3%) | 161 (85.2%) |
| Yes | 14 (10.8%) | 14 (23.7%) | 28 (14.8%) |
| Categorical variables shown as n (%); percentages are by column. |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |

### Row Percentages

Use row percentages when the question is how each exposure level is
distributed across groups.

``` r

descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = row,
  show_overall = first,
  show_missing = no,
  theme = striped
)$table
```

| Characteristic | Overall, N=189 | Normal BW, N=130 | Low BW, N=59 |
|----|----|----|----|
| age | 23.0 (19.0-26.0) | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) |
| ftv_cat |  |  |  |
| None | 100 | 64 (64.0%) | 36 (36.0%) |
| One | 47 | 36 (76.6%) | 11 (23.4%) |
| Two or more | 42 | 30 (71.4%) | 12 (28.6%) |
| ht |  |  |  |
| No | 177 | 125 (70.6%) | 52 (29.4%) |
| Yes | 12 | 5 (41.7%) | 7 (58.3%) |
| lwt | 121.0 (110.0-140.0) | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) |
| ptl_cat |  |  |  |
| No | 159 | 118 (74.2%) | 41 (25.8%) |
| Yes | 30 | 12 (40.0%) | 18 (60.0%) |
| race |  |  |  |
| White | 96 | 73 (76.0%) | 23 (24.0%) |
| Black | 26 | 15 (57.7%) | 11 (42.3%) |
| Other | 67 | 42 (62.7%) | 25 (37.3%) |
| smoke |  |  |  |
| No | 115 | 86 (74.8%) | 29 (25.2%) |
| Yes | 74 | 44 (59.5%) | 30 (40.5%) |
| ui |  |  |  |
| No | 161 | 116 (72.0%) | 45 (28.0%) |
| Yes | 28 | 14 (50.0%) | 14 (50.0%) |
| Categorical variables shown as n (%); percentages are by row (Overall shows counts). |  |  |  |
| Continuous variables shown as Median (IQR). |  |  |  |

### Word-Friendly Output

Switch to `format = flextable` when the next stop is a Word report.

``` r

descriptive_table(
  data = birthwt_data,
  exposures = birthwt_exposures,
  by = "low",
  percent = column,
  format = flextable
)$table
```

| Characteristic | Normal BW, N=130 | Low BW, N=59 |
|----|----|----|
| age | 23.0 (19.0-28.0) | 22.0 (19.5-25.0) |
| ftv_cat |  |  |
|  None | 64 (49.2%) | 36 (61.0%) |
|  One | 36 (27.7%) | 11 (18.6%) |
|  Two or more | 30 (23.1%) | 12 (20.3%) |
| ht |  |  |
|  No | 125 (96.2%) | 52 (88.1%) |
|  Yes | 5 (3.8%) | 7 (11.9%) |
| lwt | 123.5 (113.0-147.0) | 120.0 (104.0-130.0) |
| ptl_cat |  |  |
|  No | 118 (90.8%) | 41 (69.5%) |
|  Yes | 12 (9.2%) | 18 (30.5%) |
| race |  |  |
|  White | 73 (56.2%) | 23 (39.0%) |
|  Black | 15 (11.5%) | 11 (18.6%) |
|  Other | 42 (32.3%) | 25 (42.4%) |
| smoke |  |  |
|  No | 86 (66.2%) | 29 (49.2%) |
|  Yes | 44 (33.8%) | 30 (50.8%) |
| ui |  |  |
|  No | 116 (89.2%) | 45 (76.3%) |
|  Yes | 14 (10.8%) | 14 (23.7%) |
| Categorical variables shown as n (%); percentages are by column. |  |  |
| Continuous variables shown as Median (IQR). |  |  |

### What To Inspect

- `$table`: rendered `gt` or `flextable` output.
- `$table_body`: clean data behind the table.
- `$format`: output format used by the table builder.
