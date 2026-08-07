# Save a forest_reg() output

Saves a
[`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
output, or a compatible forestploter/grid object, to a fixed graphics
device. This is useful when the RStudio Viewer or operating-system
graphics device crops wide forest plots or compresses forest columns.

## Usage

``` r
save_forest(
  forest,
  filename = "forest",
  format = c("pdf", "png", "tiff", "jpg"),
  width = NULL,
  height = NULL,
  scale = 1,
  auto_size = TRUE,
  padding = 0.25,
  dpi = 300
)
```

## Arguments

- forest:

  A `gtregression_forest` object returned by
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md),
  or a compatible forestploter/grid object.

- filename:

  File name for the output, with or without extension. If no directory
  is supplied, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- format:

  Output format. One of `"pdf"`, `"png"`, `"tiff"`, or `"jpg"`.

- width, height:

  Optional export width and height in inches. If either is `NULL` and
  `auto_size = TRUE`, the size is measured from the intrinsic
  forestploter gtable dimensions. If measurement is not possible, a
  practical fallback is estimated from the number of rows and columns in
  the
  [`forest_reg()`](https://gtregression.thinkdenominator.com/reference/forest_reg.md)
  output.

- scale:

  Positive multiplier applied to the export width and height. This is a
  quick way to make a large forest plot roomier.

- auto_size:

  Logical. If `TRUE`, measure the intrinsic forest plot size when
  `width` or `height` is not supplied. If `FALSE`, use the fallback
  row/column-based sizing.

- padding:

  White space around the forest plot in inches.

- dpi:

  Resolution for raster formats.

## Value

Saves the file to disk. Invisibly returns the normalized file path.

## Examples

``` r
birthwt_data <- data_birthwt |>
  transform(
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
  )

uni_or <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("age", "smoke", "ht"),
  approach = "logit"
)

forest <- forest_reg(uni = uni_or)
save_forest(forest, filename = tempfile("forest"), format = "pdf")
#> Forest plot saved at: /tmp/RtmphvE3PL/forest1b9284dffda.pdf

# For large forest plots, increase width, height, scale, or padding.
save_forest(
  forest,
  filename = tempfile("forest-wide"),
  format = "png",
  scale = 1.2,
  padding = 0.35,
  dpi = 300
)
#> Forest plot saved at: /tmp/RtmphvE3PL/forest-wide1b922ef85b90.png
```
