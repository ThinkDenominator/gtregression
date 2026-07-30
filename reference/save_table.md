# Save a single regression or summary table

Saves a `gtregression` table, merged table, `gt_tbl`, or `flextable` as
a Word, PDF, or HTML file.

## Usage

``` r
save_table(
  tbl,
  filename = "table",
  format = c("docx", "pdf", "html"),
  orientation = c("auto", "portrait", "landscape"),
  fit_width = TRUE,
  font_size = 9,
  min_font_size = 8
)
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

- orientation:

  Word page orientation for DOCX output. One of `"auto"`, `"portrait"`,
  or `"landscape"`. With `"auto"`, wide tables are saved in landscape
  orientation before any font-size reduction is attempted.

- fit_width:

  Logical. If `TRUE`, try to fit flextable DOCX output within the
  selected Word page width. If `FALSE`, keep the natural autofit table
  width.

- font_size:

  Requested font size for flextable DOCX output.

- min_font_size:

  Smallest font size allowed when fitting wide flextable DOCX output.
  The font size is never reduced below this value.

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
  approach = "logit"
)

save_table(tbl, filename = tempfile("table"), format = "html")
#> Table saved at: /tmp/RtmpHE56Fn/table1a9e77ae5e7d.html

# Wide Word tables can be saved in landscape orientation.
# \donttest{
save_table(
  tbl,
  filename = tempfile("table-wide"),
  format = "docx",
  orientation = "auto",
  fit_width = TRUE,
  font_size = 9,
  min_font_size = 8
)
#> Table saved at: /tmp/RtmpHE56Fn/table-wide1a9e4a24846f.docx
# }
```
