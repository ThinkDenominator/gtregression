# Access fields on gtregression objects with \`\$\`

Works for any object from this package, since they all carry class
\`"gtregression"\`. Returns NULL (quietly) if the field is not present.

## Usage

``` r
# S3 method for class 'gtregression'
x$name
```

## Arguments

- x:

  A `gtregression` object.

- name:

  Field name to access.

## Details

Common fields: - table, table_display, table_body - models,
model_summaries, reg_check - approach, format (or engine), source -
parts, spanners (for merged tables) - by, levels (for descriptive
tables)
