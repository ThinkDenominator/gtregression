#' Save a Single Regression Table
#'
#' Saves a gtsummary table as a Word, PDF, or HTML file
#'
#' @param tbl A gtsummary object (e.g., tbl_regression(), tbl_summary()).
#' @param filename File name to save the output. Extension is optional.
#' @param format Output format. One of "docx", "pdf", or "html".
#'
#' @return Saves the file to a temporary directory (if no path is given). Does not return an object.
#'
#' @importFrom gtsummary as_gt
#' @importFrom gt gtsave
#' @export
#'
#' @examples
#' \donttest{
#' model <- glm(mpg ~ hp + wt, data = mtcars)
#' tbl <- gtsummary::tbl_regression(model)
#' save_table(tbl, filename = file.path(tempdir(), "regression_table"), format = "docx")
#' }
save_table <- function(tbl, filename = "table",
                       format = c("docx", "pdf", "html")) {
  format <- match.arg(format)

  # Add extension if missing
  if (!grepl(paste0("\\.", format, "$"), filename)) {
    filename <- paste0(filename, ".", format)
  }

  # Use tempdir if path is not absolute
  if (!grepl("/", filename, fixed = TRUE) && !grepl("\\\\", filename)) {
    filename <- file.path(tempdir(), filename)
  }

  gt_tbl <- gtsummary::as_gt(tbl)
  gt::gtsave(gt_tbl, filename = filename)

  message("Table saved at: ", normalizePath(filename))
}

#' Save a Single Plot
#'
#' Saves a ggplot2 plot to a file in PNG, PDF, or JPG format.
#'
#' @param plot A ggplot2 object.
#' @param filename Name of the file to save, with or without extension.
#' @param format Output format. One of "png", "pdf", or "jpg".
#' @param width Width of the saved plot in inches.
#' @param height Height of the saved plot in inches.
#' @param dpi Resolution of the plot in dots per inch (default is 300).
#'
#' @return Saves the file to a temporary directory (if no path is given).
#'
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' save_plot(p, filename = file.path(tempdir(), "scatterplot"), format = "png")
#' }
save_plot <- function(plot, filename = "plot", format = c("png", "pdf", "jpg"),
                      width = 8, height = 6, dpi = 300) {
  format <- match.arg(format)

  # Add extension if missing
  if (!grepl(paste0("\\.", format, "$"), filename)) {
    filename <- paste0(filename, ".", format)
  }

  # Use tempdir if path is not absolute
  if (!grepl("/", filename, fixed = TRUE) && !grepl("\\\\", filename)) {
    filename <- file.path(tempdir(), filename)
  }

  ggplot2::ggsave(filename = filename, plot = plot, device = format,
                  width = width, height = height, dpi = dpi)

  message("Plot saved at: ", normalizePath(filename))
}



#' Save Multiple Tables and Plots to a Word Document
#'
#' Saves a collection of gtsummary tables and ggplot2 plots into a .docx file.
#'
#' @param tables A list of gtsummary tables.
#' @param plots A list of ggplot2 plot objects.
#' @param filename File name for the output (with or without .docx extension).
#' @param titles Optional. A character vector of titles.
#'
#' @return A Word document saved to a temporary directory (if no path is given). No object is returned.
#'
#' @export
#' @importFrom officer read_docx body_add_par body_add_gg
#' @importFrom gtsummary as_flex_table
#' @importFrom flextable body_add_flextable
#'
#' @examples
#' \donttest{
#' library(gtsummary)
#' library(ggplot2)
#' tbl <- tbl_regression(glm(mpg ~ hp + wt, data = mtcars))
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' save_docx(
#'   tables = list(tbl),
#'   plots = list(p),
#'   filename = file.path(tempdir(), "report.docx"),
#'   titles = c("Table 1: Regression", "Figure 1: Scatterplot")
#' )
#' }
save_docx <- function(tables = NULL, plots = NULL,
                      filename = "report.docx", titles = NULL) {
  # Add .docx if missing
  if (!grepl("\\.docx$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".docx")
  }

  # Use tempdir if path is not absolute
  if (!grepl("/", filename, fixed = TRUE) && !grepl("\\\\", filename)) {
    filename <- file.path(tempdir(), filename)
  }

  doc <- officer::read_docx()

  n_items <- length(tables) + length(plots)
  if (!is.null(titles) && length(titles) != n_items) {
    warning("Length of titles does not match number of tables + plots. Titles will be ignored.")
    titles <- NULL
  }

  idx <- 1

  # Add tables
  if (!is.null(tables)) {
    for (tbl in tables) {
      if (!inherits(tbl, "gtsummary"))
        stop("All elements in 'tables' must be gtsummary objects.")
      ft <- gtsummary::as_flex_table(tbl)

      if (!is.null(titles)) {
        doc <- officer::body_add_par(doc, titles[[idx]], style = "heading 1")
        idx <- idx + 1
      }

      doc <- flextable::body_add_flextable(doc, ft)
      doc <- officer::body_add_par(doc, "") # Blank line
    }
  }

  # Add plots
  if (!is.null(plots)) {
    for (p in plots) {
      if (!inherits(p, "ggplot"))
        stop("All elements in 'plots' must be ggplot2 objects.")

      if (!is.null(titles)) {
        doc <- officer::body_add_par(doc, titles[[idx]], style = "heading 1")
        idx <- idx + 1
      }

      doc <- officer::body_add_gg(doc, value = p, width = 6, height = 5)
      doc <- officer::body_add_par(doc, "") # Blank line
    }
  }

  print(doc, target = filename)

  message("Word document saved at: ", normalizePath(filename))
  message("If tables or plots extend beyond the page, consider switching to landscape layout in Word (Layout > Orientation > Landscape).")
}
