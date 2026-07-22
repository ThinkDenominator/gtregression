# Save a single plot

Saves a `ggplot2` plot to a file in PNG, PDF, or JPG format.

## Usage

``` r
save_plot(
  plot,
  filename = "plot",
  format = c("png", "pdf", "jpg"),
  width = 8,
  height = 6,
  dpi = 300
)
```

## Arguments

- plot:

  A `ggplot2` object.

- filename:

  Name of the file to save, with or without extension. If no directory
  is supplied, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- format:

  Output format. One of `"png"`, `"pdf"`, or `"jpg"`.

- width:

  Width of the saved plot in inches.

- height:

  Height of the saved plot in inches.

- dpi:

  Resolution of the plot in dots per inch.

## Value

Saves the file to disk. Invisibly returns the normalized file path.

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point()

save_plot(p, filename = tempfile("plot"), format = "png")
#> Plot saved at: /tmp/RtmpyrO5nP/plot1a8e679da97b.png
```
