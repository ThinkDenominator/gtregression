#' Save a Single Regression Table
#'
#' Save a gtsummary table as Word, PDF, or HTML file.
#'
#' @param tbl A gtsummary table.
#' @param filename File name (with or without extension).
#' @param format One of `"docx"`, `"pdf"`, or `"html"`.
#'
#' @return A saved file in the working directory.
#' @export
#' @importFrom gt gtsave
#' @importFrom gtsummary as_gt
#' @examples
#' \dontrun{
#' tbl <- gtsummary::tbl_regression(glm(mpg ~ hp + wt, data = mtcars))
#' save_table(tbl, filename = "regression_table", format = "docx")
#' }
save_table <- function(tbl, filename = "table", format = c("docx", "pdf", "html")) {
  format <- match.arg(format)

  # Add extension if missing
  if (!grepl(paste0("\\.", format, "$"), filename)) {
    filename <- paste0(filename, ".", format)
  }

  gt_tbl <- gtsummary::as_gt(tbl)
  gt::gtsave(gt_tbl, filename = filename)

  message("Table saved as: '", filename, "' in the working directory.")
}

#' Save a Single Plot
#'
#' Save a ggplot object as PNG, PDF, or JPG.
#'
#' @param plot A ggplot object.
#' @param filename File name (with or without extension).
#' @param format One of `"png"`, `"pdf"`, or `"jpg"`.
#' @param width Width of the plot in inches.
#' @param height Height of the plot in inches.
#' @param dpi Resolution of the plot (default is 300).
#'
#' @return A saved plot file in the working directory.
#' @export
#' @importFrom ggplot2 ggsave
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
#' save_plot(p, filename = "scatterplot", format = "png")
#' }
save_plot <- function(plot, filename = "plot", format = c("png", "pdf", "jpg"), width = 8, height = 6, dpi = 300) {
  format <- match.arg(format)

  # Add extension if missing
  if (!grepl(paste0("\\.", format, "$"), filename)) {
    filename <- paste0(filename, ".", format)
  }

  ggplot2::ggsave(filename = filename, plot = plot, device = format, width = width, height = height, dpi = dpi)

  message("Plot saved as: '", filename, "' in the working directory.")
}

#' Save Multiple Tables and Plots to a Word Document
#'
#' Save multiple gtsummary tables and ggplot2 plots to a Word document.
#'
#' @param tables A list of gtsummary tables.
#' @param plots A list of ggplot2 plots.
#' @param filename File name (with or without `.docx` extension).
#' @param titles Optional. A character vector of titles before each table or plot.
#'
#' @return A Word document saved in the working directory.
#' @export
#' @importFrom officer read_docx body_add_par body_add_gg
#' @importFrom gtsummary as_flex_table
#' @importFrom flextable as_flextable body_add_flextable
#' @importFrom ggplot2 ggplot
#' @examples
#' \dontrun{
#' tbl <- gtsummary::tbl_regression(glm(mpg ~ hp + wt, data = mtcars))
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) + ggplot2::geom_point()
#' save_docx(
#'   tables = list(tbl),
#'   plots = list(p),
#'   filename = "report.docx",
#'   titles = c("Table 1: Regression", "Figure 1: Scatterplot")
#' )
#' }
save_docx <- function(tables = NULL, plots = NULL, filename = "report.docx", titles = NULL) {
  # Add .docx if missing
  if (!grepl("\\.docx$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".docx")
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
      if (!inherits(tbl, "gtsummary")) stop("All elements in 'tables' must be gtsummary objects.")
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
      if (!inherits(p, "ggplot")) stop("All elements in 'plots' must be ggplot2 objects.")

      if (!is.null(titles)) {
        doc <- officer::body_add_par(doc, titles[[idx]], style = "heading 1")
        idx <- idx + 1
      }

      doc <- officer::body_add_gg(doc, value = p, width = 6, height = 5)
      doc <- officer::body_add_par(doc, "") # Blank line
    }
  }

  print(doc, target = filename)
  message("Word document saved as: '", filename, "' in the working directory.")
}
