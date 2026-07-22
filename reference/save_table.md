# Save a single regression or summary table

Saves a `gtregression` table, merged table, `gt_tbl`, or `flextable` as
a Word, PDF, or HTML file.

## Usage

``` r
save_table(tbl, filename = "table", format = c("docx", "pdf", "html"))
```

## Arguments

- tbl:

  A `gtregression` object, `merged_table` object, `gt_tbl`, or
  `flextable`.

- filename:

  File name for the output. Extension is optional. If no directory is
  supplied, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- format:

  Output format. One of `"docx"`, `"pdf"`, or `"html"`.

## Value

Saves the file to disk. Invisibly returns the normalized file path.

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
  approach = logit
)

save_table(tbl, filename = tempfile("table"), format = html)
#> Table saved at: /tmp/RtmpS4lYYk/table1af144c3d3e2.html
```
