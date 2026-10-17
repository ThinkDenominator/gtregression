#' Import an analysis dataset
#'
#' Read common tabular data formats through one consistent interface. Optional
#' packages are required only for the formats that use them.
#'
#' @param file Path to the data file.
#' @param format Optional file format when it cannot be inferred from `file`.
#'   Supply an extension without the leading dot, such as `"csv"` or `"dta"`.
#' @param sheet Excel sheet name or number. Used only for `.xls` and `.xlsx`
#'   files. The first sheet is used by default.
#' @param header Logical; whether delimited text files contain column names.
#' @param sep Optional delimiter for `.csv`, `.tsv`, and `.txt` files. Defaults
#'   to comma for CSV, tab for TSV, and whitespace for TXT.
#' @param na Character vector of strings interpreted as missing values.
#' @param encoding Character encoding used for delimited text files. With the
#'   default \code{"UTF-8"}, a file that is not valid UTF-8 (typically a
#'   Latin-1 or Windows-1252 export) is read as Latin-1 and a message says so;
#'   set \code{encoding} explicitly to override.
#' @param name_repair How to handle column names: `"check_unique"` preserves
#'   names and errors on blank or duplicated names; `"unique"` repairs them
#'   with [make.names()]; `"minimal"` preserves names without checking.
#' @param ... Additional arguments passed to the format-specific reader.
#'
#' @return A data frame. Tibbles and labelled columns returned by `readxl` or
#'   `haven` are retained.
#'
#' @details
#' Supported extensions are CSV (`.csv`), tab-separated or text files (`.tsv`,
#' `.txt`), Excel (`.xls`, `.xlsx`), R data (`.rds`), Stata (`.dta`), SPSS
#' (`.sav`, `.zsav`, `.por`), and SAS (`.sas7bdat`, `.xpt`).
#'
#' `readxl` is required for Excel files and `haven` is required for Stata,
#' SPSS, and SAS files. RDS files must contain a data frame or tibble.
#'
#' @examples
#' csv_file <- tempfile(fileext = ".csv")
#' utils::write.csv(
#'   data.frame(outcome = c(0, 1), age = c(45, 61)),
#'   csv_file,
#'   row.names = FALSE
#' )
#' imported <- import_data(csv_file)
#' names(imported)
#'
#' @export
import_data <- function(file,
                        format = NULL,
                        sheet = NULL,
                        header = TRUE,
                        sep = NULL,
                        na = c("", "NA"),
                        encoding = "UTF-8",
                        name_repair = c("check_unique", "unique", "minimal"),
                        ...) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !nzchar(file)) {
    stop("`file` must be one non-empty file path.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("The data file does not exist: ", file, call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` must identify a file, not a directory.", call. = FALSE)
  }
  if (!is.logical(header) || length(header) != 1L || is.na(header)) {
    stop("`header` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(na) || anyNA(na)) {
    stop("`na` must be a character vector without missing values.", call. = FALSE)
  }
  if (!is.character(encoding) || length(encoding) != 1L ||
      is.na(encoding) || !nzchar(encoding)) {
    stop("`encoding` must be one non-empty character value.", call. = FALSE)
  }

  name_repair <- match.arg(name_repair)
  if (!is.null(format) && (!is.character(format) || length(format) != 1L ||
                           is.na(format) || !nzchar(format))) {
    stop("`format` must be NULL or one non-empty character value.", call. = FALSE)
  }
  extension <- if (is.null(format)) tools::file_ext(file) else sub("^\\.", "", format)
  extension <- tolower(extension)
  supported <- c(
    "csv", "tsv", "txt", "xls", "xlsx", "rds", "dta", "sav",
    "zsav", "por", "sas7bdat", "xpt"
  )
  if (!extension %in% supported) {
    stop(
      "Unsupported data format `.", extension,
      "`. Supported extensions are: ",
      paste(paste0(".", supported), collapse = ", "), ".",
      call. = FALSE
    )
  }

  data <- switch(
    extension,
    csv = .import_delimited(
      file, header, if (is.null(sep)) "," else sep, na, encoding, ...
    ),
    tsv = .import_delimited(
      file, header, if (is.null(sep)) "\t" else sep, na, encoding, ...
    ),
    txt = .import_delimited(
      file, header, if (is.null(sep)) "" else sep, na, encoding, ...
    ),
    xls = .import_excel(file, sheet, na, ...),
    xlsx = .import_excel(file, sheet, na, ...),
    rds = do.call(readRDS, c(list(file = file), list(...))),
    dta = .import_haven("read_dta", file, ...),
    sav = .import_haven("read_sav", file, ...),
    zsav = .import_haven("read_sav", file, ...),
    por = .import_haven("read_por", file, ...),
    sas7bdat = .import_haven("read_sas", file, ...),
    xpt = .import_haven("read_xpt", file, ...)
  )

  if (!is.data.frame(data)) {
    stop("The imported object must be a data frame or tibble.", call. = FALSE)
  }
  .repair_import_names(data, name_repair)
}

#' @keywords internal
#' @noRd
.import_delimited <- function(file, header, sep, na, encoding, ...) {
  dots <- list(...)
  if (is.null(dots$quote)) dots$quote <- "\""
  args <- list(
    file = file,
    header = header,
    sep = sep,
    na.strings = na,
    fileEncoding = encoding,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = ""
  )
  args[names(dots)] <- dots

  # A Latin-1 (or Windows-1252) file read as UTF-8 makes read.table() stop at
  # the first invalid byte and return truncated data with only a warning.
  # Detect invalid UTF-8 before reading and fall back to Latin-1, saying so.
  latin1_fallback <- FALSE
  declared_utf8 <- is.character(args$fileEncoding) &&
    toupper(gsub("[^A-Za-z0-9]", "", args$fileEncoding)) %in% c("UTF8", "UTF8BOM")
  if (declared_utf8 && !.import_is_valid_utf8(file)) {
    # Read the bytes as they are, mark them as Latin-1 and convert to UTF-8
    # afterwards (independent of the session locale).
    args$fileEncoding <- ""
    args$encoding <- "latin1"
    latin1_fallback <- TRUE
    message(
      "import_data(): `", basename(file), "` is not valid UTF-8; it was read ",
      "as Latin-1 (ISO-8859-1). If characters look wrong, set `encoding` ",
      "explicitly, for example encoding = \"windows-1252\"."
    )
  }
  out <- do.call(utils::read.table, args)
  if (latin1_fallback) {
    to_utf8 <- function(x) {
      converted <- iconv(x, from = "latin1", to = "UTF-8")
      ifelse(is.na(converted) & !is.na(x), x, converted)
    }
    names(out) <- to_utf8(names(out))
    for (j in seq_along(out)) {
      if (is.character(out[[j]])) {
        out[[j]] <- to_utf8(out[[j]])
      } else if (is.factor(out[[j]])) {
        levels(out[[j]]) <- to_utf8(levels(out[[j]]))
      }
    }
  }
  out
}

#' Does a text file contain only valid UTF-8?
#' @keywords internal
#' @noRd
.import_is_valid_utf8 <- function(file) {
  size <- file.size(file)
  if (is.na(size) || size == 0) return(TRUE)
  bytes <- tryCatch(
    readBin(file, what = "raw", n = size),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(bytes)) return(TRUE)
  bytes <- bytes[bytes != as.raw(0L)]
  if (!length(bytes)) return(TRUE)
  isTRUE(validUTF8(rawToChar(bytes)))
}

#' @keywords internal
#' @noRd
.import_excel <- function(file, sheet, na, ...) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to import Excel files.", call. = FALSE)
  }
  args <- list(path = file, na = na, .name_repair = "minimal")
  if (!is.null(sheet)) args$sheet <- sheet
  do.call(readxl::read_excel, c(args, list(...)))
}

#' @keywords internal
#' @noRd
.import_haven <- function(reader, file, ...) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "Package 'haven' is required to import Stata, SPSS, or SAS files.",
      call. = FALSE
    )
  }
  do.call(getExportedValue("haven", reader), c(list(file = file), list(...)))
}

#' @keywords internal
#' @noRd
.repair_import_names <- function(data, name_repair) {
  current <- names(data)
  invalid <- !length(current) || anyNA(current) || any(!nzchar(current)) ||
    anyDuplicated(current)
  if (identical(name_repair, "check_unique") && invalid) {
    stop(
      "Imported column names must be non-empty and unique. Use ",
      "`name_repair = \"unique\"` to repair them reproducibly.",
      call. = FALSE
    )
  }
  if (identical(name_repair, "unique")) {
    names(data) <- make.names(current, unique = TRUE)
  }
  data
}
