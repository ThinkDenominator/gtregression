# Print gtregression objects (unified)

Prints the rendered table for any object produced by this package
(objects that include class `"gtregression"`), regardless of subtype
(`uni_reg`, `multi_reg`, `stratified_*`, `merged_table`,
`descriptive_table`, ...). If no rendered table is found, a compact
structure of the object (or its display data) is shown.

## Usage

``` r
# S3 method for class 'gtregression'
print(x, ...)
```

## Arguments

- x:

  An object with class `"gtregression"`.

- ...:

  Ignored. Present for compatibility with the generic.
