# -------------------------------------------------------------------
# Save helpers
# -------------------------------------------------------------------

#' Resolve a table object for saving
#' @keywords internal
#' @noRd
.resolve_table_object <- function(tbl) {
  if (inherits(tbl, c("gtregression", "merged_table")) && !is.null(tbl[["table"]])) {
    return(tbl[["table"]])
  }

  if (inherits(tbl, "gt_tbl") || inherits(tbl, "flextable")) {
    return(tbl)
  }

  stop(
    "`tbl` must be a gtregression object, merged_table object, gt_tbl, or flextable.",
    call. = FALSE
  )
}

#' Normalize output filename
#' @keywords internal
#' @noRd
.normalize_save_path <- function(filename, ext) {
  stopifnot(is.character(filename), length(filename) == 1L, nzchar(filename))
  stopifnot(is.character(ext), length(ext) == 1L, nzchar(ext))

  if (!grepl(paste0("\\.", ext, "$"), filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".", ext)
  }

  has_path <- grepl("/", filename, fixed = TRUE) || grepl("\\\\", filename)
  if (!has_path) {
    filename <- file.path(tempdir(), filename)
  }

  filename
}

# -------------------------------------------------------------------
# save_table
# -------------------------------------------------------------------

#' Save a single regression or summary table
#'
#' Saves a \code{gtregression} table, merged table, \code{gt_tbl}, or
#' \code{flextable} as a Word, PDF, or HTML file.
#'
#' @param tbl A \code{gtregression} object, \code{merged_table} object,
#'   \code{gt_tbl}, or \code{flextable}.
#' @param filename File name for the output. Extension is optional.
#' @param format Output format. One of \code{"docx"}, \code{"pdf"}, or
#'   \code{"html"}.
#'
#' @return Saves the file to disk. Invisibly returns the normalized file path.
#' @export
save_table <- function(tbl,
                       filename = "table",
                       format = c("docx", "pdf", "html")) {
  format <- match.arg(format)
  filename <- .normalize_save_path(filename, format)

  obj <- .resolve_table_object(tbl)

  if (inherits(plot, "ggplot")) {
    if (requireNamespace("ggtext", quietly = TRUE)) {
      loadNamespace("ggtext")
    }
  }

  if (inherits(obj, "gt_tbl")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required to save gt tables.", call. = FALSE)
    }

    gt::gtsave(data = obj, filename = filename)

  } else if (inherits(obj, "flextable")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Package 'flextable' is required to save flextable objects.", call. = FALSE)
    }

    if (identical(format, "docx")) {
      flextable::save_as_docx(obj, path = filename)
    } else if (identical(format, "html")) {
      flextable::save_as_html(obj, path = filename)
    } else {
      stop(
        "Saving flextable objects as PDF is not directly supported. Save as DOCX or HTML instead.",
        call. = FALSE
      )
    }

  } else {
    stop("Unsupported table type.", call. = FALSE)
  }

  message("Table saved at: ", normalizePath(filename))
  invisible(normalizePath(filename))
}

# -------------------------------------------------------------------
# save_plot
# -------------------------------------------------------------------

#' Save a single plot
#'
#' Saves a \code{ggplot2} plot to a file in PNG, PDF, or JPG format.
#'
#' @param plot A \code{ggplot2} object.
#' @param filename Name of the file to save, with or without extension.
#' @param format Output format. One of \code{"png"}, \code{"pdf"}, or \code{"jpg"}.
#' @param width Width of the saved plot in inches.
#' @param height Height of the saved plot in inches.
#' @param dpi Resolution of the plot in dots per inch.
#'
#' @return Saves the file to disk. Invisibly returns the normalized file path.
#' @importFrom ggplot2 ggsave
#' @export
save_plot <- function(plot,
                      filename = "plot",
                      format = c("png", "pdf", "jpg"),
                      width = 8,
                      height = 6,
                      dpi = 300) {
  format <- match.arg(format)

  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }

  filename <- .normalize_save_path(filename, format)

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = format,
    width = width,
    height = height,
    dpi = dpi
  )

  message("Plot saved at: ", normalizePath(filename))
  invisible(normalizePath(filename))
}

# -------------------------------------------------------------------
# save_docx
# -------------------------------------------------------------------

#' Save multiple tables and plots to a Word document
#'
#' Saves a collection of \code{gtregression} tables, merged tables,
#' \code{gt_tbl}/\code{flextable} objects, and \code{ggplot2} plots into a
#' single Word document.
#'
#' @param tables A list of tables. Each element may be a \code{gtregression}
#'   object, \code{merged_table} object, \code{gt_tbl}, or \code{flextable}.
#' @param plots A list of \code{ggplot2} plot objects.
#' @param filename File name for the output, with or without \code{.docx}.
#' @param titles Optional character vector of titles for tables and plots in
#'   the order they are added.
#' @param plot_width Width of inserted plots in inches.
#' @param plot_height Height of inserted plots in inches.
#'
#' @return Saves the Word document to disk. Invisibly returns the normalized
#'   file path.
#' @export
#' @importFrom officer read_docx body_add_par body_add_gg
#' @importFrom flextable body_add_flextable
save_docx <- function(tables = NULL,
                      plots = NULL,
                      filename = "report.docx",
                      titles = NULL,
                      plot_width = 6,
                      plot_height = 5) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required.", call. = FALSE)
  }
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required.", call. = FALSE)
  }

  filename <- .normalize_save_path(filename, "docx")

  n_tables <- if (is.null(tables)) 0L else length(tables)
  n_plots  <- if (is.null(plots)) 0L else length(plots)
  n_items  <- n_tables + n_plots

  if (n_items == 0L) {
    stop("Provide at least one table or plot.", call. = FALSE)
  }

  if (!is.null(titles) && length(titles) != n_items) {
    warning(
      "Length of `titles` does not match the number of tables + plots. Titles will be ignored.",
      call. = FALSE
    )
    titles <- NULL
  }

  doc <- officer::read_docx()
  idx <- 1L

  if (!is.null(tables)) {
    for (tbl in tables) {
      obj <- .resolve_table_object(tbl)

      if (!is.null(titles)) {
        doc <- officer::body_add_par(doc, titles[[idx]], style = "heading 1")
        idx <- idx + 1L
      }

      if (inherits(obj, "flextable")) {
        ft <- obj
      } else if (inherits(obj, "gt_tbl")) {
        stop(
          "DOCX export currently supports flextable-based tables directly. For gt tables, save as HTML/PDF with save_table(), or create the table with format = 'flextable'.",
          call. = FALSE
        )
      } else {
        stop("Unsupported table type for DOCX export.", call. = FALSE)
      }

      doc <- flextable::body_add_flextable(doc, ft)
      doc <- officer::body_add_par(doc, "")
    }
  }

  if (!is.null(plots)) {
    for (p in plots) {
      if (!inherits(p, "ggplot")) {
        stop("All elements in `plots` must be ggplot2 objects.", call. = FALSE)
      }

      if (!is.null(titles)) {
        doc <- officer::body_add_par(doc, titles[[idx]], style = "heading 1")
        idx <- idx + 1L
      }

      doc <- officer::body_add_gg(
        doc,
        value = p,
        width = plot_width,
        height = plot_height
      )
      doc <- officer::body_add_par(doc, "")
    }
  }

  print(doc, target = filename)

  message("Word document saved at: ", normalizePath(filename))
  invisible(normalizePath(filename))
}
