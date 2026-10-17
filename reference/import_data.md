# Import an analysis dataset

Read common tabular data formats through one consistent interface.
Optional packages are required only for the formats that use them.

## Usage

``` r
import_data(
  file,
  format = NULL,
  sheet = NULL,
  header = TRUE,
  sep = NULL,
  na = c("", "NA"),
  encoding = "UTF-8",
  name_repair = c("check_unique", "unique", "minimal"),
  ...
)
```

## Arguments

- file:

  Path to the data file.

- format:

  Optional file format when it cannot be inferred from \`file\`. Supply
  an extension without the leading dot, such as \`"csv"\` or \`"dta"\`.

- sheet:

  Excel sheet name or number. Used only for \`.xls\` and \`.xlsx\`
  files. The first sheet is used by default.

- header:

  Logical; whether delimited text files contain column names.

- sep:

  Optional delimiter for \`.csv\`, \`.tsv\`, and \`.txt\` files.
  Defaults to comma for CSV, tab for TSV, and whitespace for TXT.

- na:

  Character vector of strings interpreted as missing values.

- encoding:

  Character encoding used for delimited text files. With the default
  `"UTF-8"`, a file that is not valid UTF-8 (typically a Latin-1 or
  Windows-1252 export) is read as Latin-1 and a message says so; set
  `encoding` explicitly to override.

- name_repair:

  How to handle column names: \`"check_unique"\` preserves names and
  errors on blank or duplicated names; \`"unique"\` repairs them with
  \[make.names()\]; \`"minimal"\` preserves names without checking.

- ...:

  Additional arguments passed to the format-specific reader.

## Value

A data frame. Tibbles and labelled columns returned by \`readxl\` or
\`haven\` are retained.

## Details

Supported extensions are CSV (\`.csv\`), tab-separated or text files
(\`.tsv\`, \`.txt\`), Excel (\`.xls\`, \`.xlsx\`), R data (\`.rds\`),
Stata (\`.dta\`), SPSS (\`.sav\`, \`.zsav\`, \`.por\`), and SAS
(\`.sas7bdat\`, \`.xpt\`).

\`readxl\` is required for Excel files and \`haven\` is required for
Stata, SPSS, and SAS files. RDS files must contain a data frame or
tibble.

## Examples

``` r
csv_file <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(outcome = c(0, 1), age = c(45, 61)),
  csv_file,
  row.names = FALSE
)
imported <- import_data(csv_file)
names(imported)
#> [1] "outcome" "age"    
```
