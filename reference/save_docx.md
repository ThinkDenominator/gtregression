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
  table_width = 6.5,
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

- table_width:

  Maximum table width in inches for Word export. The default `6.5` fits
  a standard portrait Word page with common margins. Use `NULL` to keep
  the original flextable widths.

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
#> Word document saved at: /tmp/RtmpaFHEEe/report1a042550409c.docx
```
