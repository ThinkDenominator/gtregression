# R/print.R

#' Print gtregression objects (unified)
#'
#' Prints the rendered table for any object produced by this package
#' (objects that include class \code{"gtregression"}), regardless of subtype
#' (\code{uni_reg}, \code{multi_reg}, \code{stratified_*}, \code{merged_table},
#' \code{descriptive_table}, ...). If no rendered table is found, a compact
#' structure of the object (or its display data) is shown.
#'
#' @param x An object with class \code{"gtregression"}.
#' @param ... Ignored. Present for compatibility with the generic.
#'
#' @export
print.gtregression <- function(x, ...) {
  # Preferred inner table if present
  tb <- NULL
  if (!is.null(x$table)) tb <- x$table
  # In case someone passes the raw gt/flextable itself but it still has the class
  if (is.null(tb) && (inherits(x, "gt_tbl") || inherits(x, "flextable"))) tb <- x

  if (inherits(tb, "gt_tbl") || inherits(tb, "flextable")) {
    print(tb)
  } else if (!is.null(tb)) {
    # Some other table-like object
    print(tb)
  } else if (!is.null(x$table_display)) {
    # Fallback: show a compact peek of the display data so users see "something"
    utils::str(utils::head(x$table_display, 10))
  } else {
    # Last resort
    utils::str(x)
  }
  invisible(x)
}
