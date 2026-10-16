## Manual real-time test: import_data()
## Package: gtregression 1.2
##
## Story:
## A user receives analysis files in several common formats and wants one
## predictable import function. This script creates safe temporary files, then
## imports them one at a time for visual inspection.
##
## How to use:
## Run section by section. All demonstration files are written to tempdir(),
## not to the package folder or the user's working directory.


## 0. Setup -------------------------------------------------------------------

## During development: devtools::load_all(".")
## After installation: library(gtregression)

library(gtregression)

source_data <- data.frame(
  id = 1:20,
  outcome = factor(rep(c("No", "Yes"), 10)),
  age = c(44, 62, 51, 55, 58, 47, 69, 53, 61, 49,
          66, 52, 57, 63, 46, 59, 71, 54, NA, 60),
  notes = rep(c("Complete", "Complete", "Missing", "NA"), 5)
)

source_data
str(source_data)


## 1. CSV ---------------------------------------------------------------------

csv_file <- tempfile(fileext = ".csv")
utils::write.csv(source_data, csv_file, row.names = FALSE, na = "Missing")

csv_data <- import_data(csv_file, na = c("", "NA", "Missing"))
csv_data
str(csv_data)


## 2. Extensionless Shiny-style upload ---------------------------------------

## Browser uploads often arrive at a temporary path without an extension.
## Supply format explicitly in this case.

upload_path <- tempfile()
utils::write.csv(source_data, upload_path, row.names = FALSE)

upload_data <- import_data(upload_path, format = "csv")
upload_data


## 3. TSV ---------------------------------------------------------------------

tsv_file <- tempfile(fileext = ".tsv")
utils::write.table(
  source_data,
  tsv_file,
  sep = "\t",
  row.names = FALSE,
  quote = TRUE
)

tsv_data <- import_data(tsv_file)
tsv_data


## 4. Custom-delimited text ---------------------------------------------------

txt_file <- tempfile(fileext = ".txt")
utils::write.table(
  source_data,
  txt_file,
  sep = ";",
  row.names = FALSE,
  quote = TRUE
)

txt_data <- import_data(txt_file, sep = ";")
txt_data


## 5. RDS ---------------------------------------------------------------------

rds_file <- tempfile(fileext = ".rds")
saveRDS(source_data, rds_file)

rds_data <- import_data(rds_file)
rds_data
str(rds_data)


## 6. Duplicate-name handling -------------------------------------------------

duplicate_file <- tempfile(fileext = ".csv")
writeLines(c("age,age", "40,41"), duplicate_file)

## Run this line alone and confirm the message recommends name_repair:
## import_data(duplicate_file)

repaired_data <- import_data(
  duplicate_file,
  name_repair = "unique"
)

repaired_data
names(repaired_data)


## 7. Excel files -------------------------------------------------------------

## readxl is needed to read Excel files. Use a real de-identified workbook:
##
## excel_data <- import_data("path/to/data.xlsx")
## excel_data
##
## selected_sheet <- import_data(
##   "path/to/data.xlsx",
##   sheet = "Analysis"
## )
## selected_sheet
##
## selected_sheet_number <- import_data("path/to/data.xls", sheet = 2)
## selected_sheet_number


## 8. Stata, SPSS, and SAS files ---------------------------------------------

## haven is needed for these formats. Repeat with de-identified real files:
##
## stata_data <- import_data("path/to/data.dta")
## spss_data <- import_data("path/to/data.sav")
## spss_compressed <- import_data("path/to/data.zsav")
## spss_portable <- import_data("path/to/data.por")
## sas_data <- import_data("path/to/data.sas7bdat")
## sas_transport <- import_data("path/to/data.xpt")
##
## str(stata_data)
## attributes(stata_data[[1]])
##
## Confirm that variable labels and value labels remain attached.


## 9. Unsupported files and invalid RDS --------------------------------------

unsupported_file <- tempfile(fileext = ".unsupported")
writeLines("not a supported dataset", unsupported_file)

## Run alone and inspect the friendly list of supported formats:
## import_data(unsupported_file)

invalid_rds <- tempfile(fileext = ".rds")
saveRDS(letters, invalid_rds)

## Run alone and confirm that a non-data-frame RDS is rejected clearly:
## import_data(invalid_rds)


## 10. Continue into gtregression ---------------------------------------------

csv_data$outcome <- factor(csv_data$outcome, levels = c("No", "Yes"))

descriptive_table(
  data = csv_data,
  exposures = age,
  by = outcome
)

uni_reg(
  data = csv_data,
  outcome = outcome,
  exposures = age,
  approach = logit
)


## 11. What to inspect --------------------------------------------------------

## - CSV, TSV, TXT, and RDS return data frames with expected names and rows.
## - An extensionless path works when format is supplied.
## - sep controls custom text delimiters.
## - na controls strings interpreted as missing values.
## - Duplicate names fail by default and repair reproducibly when requested.
## - Excel sheet selection works by name and number.
## - Labelled metadata is retained for haven-supported files.
## - Imported data can move directly into descriptive and regression functions.
