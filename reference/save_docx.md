# Save multiple tables and plots to a Word document

Saves a collection of `gtregression` tables, merged tables,
`gt_tbl`/`flextable` objects, and `ggplot2` plots into a single Word
document.

## Usage

``` r
save_docx(
  tables = NULL,
  plots = NULL,
  filename = "report.docx",
  titles = NULL,
  plot_width = 6,
  plot_height = 5
)
```

## Arguments

- tables:

  A list of tables. Each element may be a `gtregression` object,
  `merged_table` object, `gt_tbl`, or `flextable`.

- plots:

  A list of `ggplot2` plot objects.

- filename:

  File name for the output, with or without `.docx`. If no directory is
  supplied, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- titles:

  Optional character vector of titles for tables and plots in the order
  they are added.

- plot_width:

  Width of inserted plots in inches.

- plot_height:

  Height of inserted plots in inches.

## Value

Saves the Word document to disk. Invisibly returns the normalized file
path.

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

tbl <- uni_reg(
  data = birthwt_data,
  outcome = "low",
  exposures = c("age", "smoke"),
  approach = "logit",
  format = "flextable"
)

save_docx(tables = tbl, filename = tempfile("report"))
#> Word document saved at: /tmp/RtmpQEDMyA/report1a147b2d60c7.docx
```
