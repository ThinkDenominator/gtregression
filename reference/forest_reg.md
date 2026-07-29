# Draw a forest table from regression outputs

Wrapper around
[`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html)
that works directly with
[`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md)
output or with `gtregression` regression objects. It can show
descriptive columns and one or two model effect columns in a table-style
forest plot while leaving axis and interval drawing to forestploter.

## Usage

``` r
forest_reg(
  df = NULL,
  uni = NULL,
  multi = NULL,
  desc = NULL,
  theme = NULL,
  ci_col_width = 20,
  side = c("right", "left"),
  quiet = TRUE,
  effects = NULL,
  ticks_at = NULL,
  ticks_digits = NULL,
  xlim = NULL,
  ...
)
```

## Arguments

- df:

  Output of
  [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md).
  If `NULL`, it is built from `uni`, `multi`, and `desc`.

- uni, multi, desc:

  Optional `gtregression` objects to pass through to
  [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md).

- theme:

  Optional
  [`forestploter::forest_theme()`](https://rdrr.io/pkg/forestploter/man/forest_theme.html).
  If `NULL`, `forestploter` defaults are used. You may pass colors and
  styling either here or through `...`.

- ci_col_width:

  Numeric value, or length-2 numeric for two effect columns, controlling
  the blank spacer width used by `forestploter` for the
  confidence-interval plot column(s). Values greater than 1 are
  interpreted as approximate character counts. Values between 0 and 1
  are accepted for backward compatibility and converted to character
  counts.

- side:

  Character. For each effect, position of the plot relative to the
  effect-size text: `"left"` = plot first then text; `"right"` = text
  first then plot. The `Characteristic` column and descriptive columns
  remain on the left.

- quiet:

  Logical. Suppress forestploter warnings. Default = \`TRUE\`.

- effects:

  Optional effect labels passed to
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html).

- ticks_at:

  Optional numeric vector, or length-2 list for two effect columns,
  specifying x-axis tick positions. If `NULL`,
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html)
  chooses the default ticks.

- ticks_digits:

  Optional number of digits for x-axis tick labels.

- xlim:

  Optional numeric vector of length 2, or length-2 list for two effect
  columns, specifying x-axis limits. If `NULL`,
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html)
  chooses the default limits.

- ...:

  Passed to
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html).
  Common options include `title` and `footnote`.

## Value

A `gtregression_forest` object with elements:

- `plot`:

  The forest plot object.

- `data`:

  The plotting data sent to
  [`forestploter::forest()`](https://rdrr.io/pkg/forestploter/man/forest.html).

- `input_data`:

  The original
  [`forest_df()`](https://thinkdenominator.github.io/gtregression/reference/forest_df.md)
  data, including standard-error helper columns.

- `meta`:

  Model metadata, including reference line and x-axis transformation.

## Examples

``` r
birthwt_data <- data_birthwt |>
  transform(
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
    low = factor(low, levels = c(0, 1),
                 labels = c("Normal BW", "Low BW"))
  )

uni_or <- uni_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("age", "lwt", "smoke", "ht"),
  approach = "logit"
)
multi_or <- multi_reg(
  birthwt_data,
  outcome = "low",
  exposures = c("smoke", "ht"),
  adjust_for = c("age", "lwt"),
  approach = "logit"
)

forest_reg(uni = uni_or, multi = multi_or)


# If axis labels overlap, set x-axis limits and tick marks.
forest_reg(
  uni = uni_or,
  multi = multi_or,
  xlim = list(c(0.25, 8), c(0.25, 12)),
  ticks_at = list(c(0.5, 1, 2, 4, 8), c(0.5, 1, 2, 4, 8))
)


# If the forest plot panel is too narrow or too wide, tune ci_col_width.
forest_reg(
  uni = uni_or,
  multi = multi_or,
  ci_col_width = c(18, 22)
)
```
