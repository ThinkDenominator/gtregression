# -------------------------------------------------------------------
# Saving and export utilities
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

#' Check whether an object can be exported as a table
#' @keywords internal
#' @noRd
.is_save_table_like <- function(x) {
  inherits(x, c("gtregression", "merged_table", "gt_tbl", "flextable"))
}

#' Normalize output filename
#' @keywords internal
#' @noRd
.normalize_save_path <- function(filename, ext) {
  if (!is.character(filename) || length(filename) != 1L ||
      is.na(filename) || !nzchar(filename)) {
    stop("`filename` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(ext) || length(ext) != 1L ||
      is.na(ext) || !nzchar(ext)) {
    stop("`ext` must be a single non-empty character string.", call. = FALSE)
  }

  if (!grepl(paste0("\\.", ext, "$"), filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".", ext)
  }

  has_path <- grepl("/", filename, fixed = TRUE) || grepl("\\\\", filename)
  if (!has_path) {
    filename <- file.path(tempdir(), filename)
  }

  filename
}

#' Validate DOCX table sizing options
#' @keywords internal
#' @noRd
.validate_docx_table_sizing <- function(fit_width = TRUE,
                                        font_size = 9,
                                        min_font_size = 8) {
  if (!is.logical(fit_width) || length(fit_width) != 1L || is.na(fit_width)) {
    stop("`fit_width` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(font_size) || length(font_size) != 1L ||
      is.na(font_size) || font_size <= 0) {
    stop("`font_size` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(min_font_size) || length(min_font_size) != 1L ||
      is.na(min_font_size) || min_font_size <= 0) {
    stop("`min_font_size` must be a single positive number.", call. = FALSE)
  }
  if (min_font_size > font_size) {
    stop("`min_font_size` must be less than or equal to `font_size`.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Measure a flextable width in inches
#' @keywords internal
#' @noRd
.flextable_col_widths <- function(ft) {
  widths <- ft$body$colwidths
  if (is.null(widths)) {
    return(NULL)
  }
  widths
}

#' Measure a flextable width in inches
#' @keywords internal
#' @noRd
.flextable_width <- function(ft) {
  widths <- .flextable_col_widths(ft)
  if (is.null(widths)) {
    dims <- tryCatch(flextable::flextable_dim(ft), error = function(e) NULL)
    if (is.null(dims) || is.null(dims$widths)) {
      return(NA_real_)
    }
    width <- sum(dims$widths, na.rm = TRUE)
  } else {
    width <- sum(widths, na.rm = TRUE)
  }
  if (!is.finite(width) || width <= 0) NA_real_ else width
}

#' Scale flextable columns to a target width without reducing font size
#' @keywords internal
#' @noRd
.scale_flextable_width <- function(ft, table_width) {
  widths <- .flextable_col_widths(ft)
  if (is.null(widths)) {
    return(ft)
  }

  current_width <- sum(widths, na.rm = TRUE)
  if (!is.finite(current_width) || current_width <= 0 ||
      current_width <= table_width) {
    return(ft)
  }

  scaled <- widths * (table_width / current_width)
  col_keys <- names(scaled)
  if (is.null(col_keys) || any(!nzchar(col_keys))) {
    col_keys <- ft$col_keys
  }
  if (is.null(col_keys) || length(col_keys) != length(scaled)) {
    return(ft)
  }

  out <- ft
  for (i in seq_along(scaled)) {
    out <- tryCatch(
      flextable::width(out, j = col_keys[[i]], width = scaled[[i]]),
      error = function(e) out
    )
  }
  out
}

#' Fit flextable output to a Word page width without unsafe font shrinking
#' @keywords internal
#' @noRd
.fit_flextable_docx_width <- function(ft,
                                      table_width = 6.5,
                                      fit_width = TRUE,
                                      font_size = 9,
                                      min_font_size = 8,
                                      warn = TRUE) {
  .validate_docx_table_sizing(
    fit_width = fit_width,
    font_size = font_size,
    min_font_size = min_font_size
  )
  if (is.null(table_width)) {
    return(flextable::autofit(flextable::fontsize(ft, size = font_size, part = "all")))
  }
  if (!is.numeric(table_width) || length(table_width) != 1L ||
      is.na(table_width) || table_width <= 0) {
    stop("`table_width` must be NULL or a single positive number.", call. = FALSE)
  }

  ft_requested <- flextable::autofit(flextable::fontsize(ft, size = font_size, part = "all"))
  if (!isTRUE(fit_width)) {
    return(ft_requested)
  }

  fitted <- .scale_flextable_width(ft_requested, table_width)
  fitted_width <- .flextable_width(fitted)
  if (is.finite(fitted_width) && fitted_width <= table_width) {
    return(fitted)
  }

  if (font_size > min_font_size) {
    for (size in seq(font_size - 1, min_font_size, by = -1)) {
      candidate <- flextable::autofit(flextable::fontsize(ft, size = size, part = "all"))
      candidate <- .scale_flextable_width(candidate, table_width)
      candidate_width <- .flextable_width(candidate)
      if (is.finite(candidate_width) && candidate_width <= table_width) {
        return(candidate)
      }
    }
  }

  if (isTRUE(warn)) {
    warning(
      "The table is wider than the selected DOCX page width. ",
      "Saving with flextable::autofit() at the requested font size instead of ",
      "reducing below `min_font_size`. Consider landscape orientation, fewer columns, ",
      "a smaller `font_size`, or `fit_width = FALSE`.",
      call. = FALSE
    )
  }

  ft_requested
}

#' Resolve DOCX orientation from table width
#' @keywords internal
#' @noRd
.resolve_docx_orientation <- function(ft,
                                      orientation = "auto",
                                      portrait_width = 6.5,
                                      landscape_width = 9,
                                      wide_col_threshold = 6L) {
  orientation <- match.arg(orientation, c("auto", "portrait", "landscape"))

  if (!identical(orientation, "auto")) {
    return(orientation)
  }

  measured_width <- .flextable_width(flextable::autofit(ft))
  col_keys <- if (!is.null(ft$col_keys)) ft$col_keys else character(0)
  n_cols <- length(col_keys)

  if ((is.finite(measured_width) && measured_width > portrait_width) ||
      n_cols > wide_col_threshold) {
    return("landscape")
  }

  "portrait"
}

#' Prepare a flextable and Word section for DOCX export
#' @keywords internal
#' @noRd
.prepare_flextable_docx <- function(ft,
                                    orientation = c("auto", "portrait", "landscape"),
                                    fit_width = TRUE,
                                    font_size = 9,
                                    min_font_size = 8) {
  orientation <- match.arg(orientation)
  .validate_docx_table_sizing(
    fit_width = fit_width,
    font_size = font_size,
    min_font_size = min_font_size
  )

  resolved_orientation <- .resolve_docx_orientation(
    ft = flextable::fontsize(ft, size = font_size, part = "all"),
    orientation = orientation
  )
  page_width <- if (identical(resolved_orientation, "landscape")) 9 else 6.5
  section <- officer::prop_section(
    page_size = officer::page_size(orient = resolved_orientation)
  )

  list(
    table = .fit_flextable_docx_width(
      ft,
      table_width = page_width,
      fit_width = fit_width,
      font_size = font_size,
      min_font_size = min_font_size
    ),
    orientation = resolved_orientation,
    page_width = page_width,
    section = section
  )
}

#' Resolve a forest plot object for saving
#' @keywords internal
#' @noRd
.resolve_forest_object <- function(forest) {
  if (inherits(forest, "gtregression_forest") && !is.null(forest[["plot"]])) {
    return(forest[["plot"]])
  }

  if (inherits(forest, c("forestplot", "gtable", "gTree", "grob"))) {
    return(forest)
  }

  stop(
    "`forest` must be a forest_reg() object or a compatible forestploter/grid object.",
    call. = FALSE
  )
}

#' Estimate a practical export canvas for forest plots
#' @keywords internal
#' @noRd
.measure_forest_grob <- function(plot) {
  if (is.null(plot[["widths"]]) || is.null(plot[["heights"]])) {
    return(NULL)
  }

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = tmp, width = 20, height = 20)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)

  grid::grid.newpage()
  width <- grid::convertWidth(sum(plot$widths), unitTo = "in", valueOnly = TRUE)
  height <- grid::convertHeight(sum(plot$heights), unitTo = "in", valueOnly = TRUE)

  if (!is.finite(width) || !is.finite(height) || width <= 0 || height <= 0) {
    return(NULL)
  }

  list(width = width, height = height)
}

#' Estimate a practical export canvas for forest plots
#' @keywords internal
#' @noRd
.forest_export_dimensions <- function(forest, plot, width = NULL, height = NULL,
                                      scale = 1, padding = 0.25,
                                      auto_size = TRUE) {
  if (!is.numeric(scale) || length(scale) != 1L || is.na(scale) || scale <= 0) {
    stop("`scale` must be a single positive number.", call. = FALSE)
  }
  if (!is.logical(auto_size) || length(auto_size) != 1L || is.na(auto_size)) {
    stop("`auto_size` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(width) &&
      (!is.numeric(width) || length(width) != 1L || is.na(width) || width <= 0)) {
    stop("`width` must be NULL or a single positive number.", call. = FALSE)
  }
  if (!is.null(height) &&
      (!is.numeric(height) || length(height) != 1L || is.na(height) || height <= 0)) {
    stop("`height` must be NULL or a single positive number.", call. = FALSE)
  }

  if (isTRUE(auto_size) && (is.null(width) || is.null(height))) {
    measured <- .measure_forest_grob(plot)
    if (!is.null(measured)) {
      if (is.null(width)) width <- measured$width + (2 * padding)
      if (is.null(height)) height <- measured$height + (2 * padding)
    }
  }

  if (is.null(width) || is.null(height)) {
    plot_data <- if (inherits(forest, "gtregression_forest")) forest[["data"]] else NULL

    if (is.data.frame(plot_data) && nrow(plot_data) > 0L) {
      n_rows <- nrow(plot_data)
      n_cols <- ncol(plot_data)
      n_ci_cols <- sum(grepl("^\\s+$", names(plot_data)))
      longest_label <- if ("Characteristic" %in% names(plot_data)) {
        max(nchar(as.character(plot_data$Characteristic)), na.rm = TRUE)
      } else {
        20
      }
      if (!is.finite(longest_label)) longest_label <- 20

      auto_width <- 3.4 + (0.95 * n_cols) + (1.4 * n_ci_cols) +
        (0.035 * min(longest_label, 60))
      auto_height <- 1.2 + (0.32 * n_rows)
    } else {
      auto_width <- 10
      auto_height <- 7
    }

    if (is.null(width)) width <- auto_width
    if (is.null(height)) height <- auto_height
  }

  list(width = width * scale, height = height * scale)
}

#' Draw a forest plot on the active graphics device
#' @keywords internal
#' @noRd
.draw_forest_export <- function(plot, padding = 0.25) {
  if (!is.numeric(padding) || length(padding) != 1L ||
      is.na(padding) || padding < 0) {
    stop("`padding` must be a single non-negative number.", call. = FALSE)
  }

  grid::grid.newpage()
  if (padding > 0) {
    grid::pushViewport(grid::viewport(
      width = grid::unit(1, "npc") - grid::unit(2 * padding, "in"),
      height = grid::unit(1, "npc") - grid::unit(2 * padding, "in")
    ))
    on.exit(grid::popViewport(), add = TRUE)
  }
  grid::grid.draw(plot)
  invisible(plot)
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
#' @param filename File name for the output. Extension is optional. If no
#'   directory is supplied, the file is saved in \code{tempdir()}.
#' @param format Output format. One of \code{"docx"}, \code{"pdf"}, or
#'   \code{"html"}.
#' @param orientation Word page orientation for DOCX output. One of
#'   \code{"auto"}, \code{"portrait"}, or \code{"landscape"}. With
#'   \code{"auto"}, wide tables are saved in landscape orientation before any
#'   font-size reduction is attempted.
#' @param fit_width Logical. If \code{TRUE}, try to fit flextable DOCX output
#'   within the selected Word page width. If \code{FALSE}, keep the natural
#'   autofit table width.
#' @param font_size Requested font size for flextable DOCX output.
#' @param min_font_size Smallest font size allowed when fitting wide flextable
#'   DOCX output. The font size is never reduced below this value.
#'
#' @return Saves the file to disk. Invisibly returns the normalized file path.
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   dplyr::mutate(
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
#'   )
#'
#' tbl <- uni_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "smoke"),
#'   approach = "logit"
#' )
#'
#' save_table(tbl, filename = tempfile("table"), format = "html")
#'
#' # Wide Word tables can be saved in landscape orientation.
#' \donttest{
#' save_table(
#'   tbl,
#'   filename = tempfile("table-wide"),
#'   format = "docx",
#'   orientation = "auto",
#'   fit_width = TRUE,
#'   font_size = 9,
#'   min_font_size = 8
#' )
#' }
#' @export
save_table <- function(tbl,
                       filename = "table",
                       format = c("docx", "pdf", "html"),
                       orientation = c("auto", "portrait", "landscape"),
                       fit_width = TRUE,
                       font_size = 9,
                       min_font_size = 8) {
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("docx", "pdf", "html"))
  format <- match.arg(format)
  orientation <- .choice_arg(
    substitute(orientation),
    env = parent.frame(),
    choices = c("auto", "portrait", "landscape")
  )
  orientation <- match.arg(orientation, c("auto", "portrait", "landscape"))
  filename <- .normalize_save_path(filename, format)

  obj <- .resolve_table_object(tbl)

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
      docx <- .prepare_flextable_docx(
        obj,
        orientation = orientation,
        fit_width = fit_width,
        font_size = font_size,
        min_font_size = min_font_size
      )
      flextable::save_as_docx(
        docx$table,
        path = filename,
        pr_section = docx$section
      )
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
#' @param filename Name of the file to save, with or without extension. If no
#'   directory is supplied, the file is saved in \code{tempdir()}.
#' @param format Output format. One of \code{"png"}, \code{"pdf"}, or \code{"jpg"}.
#' @param width Width of the saved plot in inches.
#' @param height Height of the saved plot in inches.
#' @param dpi Resolution of the plot in dots per inch.
#'
#' @return Saves the file to disk. Invisibly returns the normalized file path.
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' save_plot(p, filename = tempfile("plot"), format = "png")
#' @importFrom ggplot2 ggsave
#' @export
save_plot <- function(plot,
                      filename = "plot",
                      format = c("png", "pdf", "jpg"),
                      width = 8,
                      height = 6,
                      dpi = 300) {
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("png", "pdf", "jpg"))
  format <- match.arg(format)

  if (inherits(plot, "gtregression_forest")) {
    stop("Use `save_forest()` to save forest_reg() outputs.", call. = FALSE)
  }
  if (!inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot2 object.", call. = FALSE)
  }
  if (!is.numeric(width) || length(width) != 1L || is.na(width) || width <= 0) {
    stop("`width` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(height) || length(height) != 1L || is.na(height) || height <= 0) {
    stop("`height` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(dpi) || length(dpi) != 1L || is.na(dpi) || dpi <= 0) {
    stop("`dpi` must be a single positive number.", call. = FALSE)
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
# save_forest
# -------------------------------------------------------------------

#' Save a forest_reg() output
#'
#' Saves a \code{forest_reg()} output, or a compatible
#' \pkg{forestploter}/grid object, to a fixed graphics device. This is useful
#' when the RStudio Viewer or operating-system graphics device crops wide forest
#' plots or compresses forest columns.
#'
#' @param forest A \code{gtregression_forest} object returned by
#'   \code{forest_reg()}, or a compatible \pkg{forestploter}/grid object.
#' @param filename File name for the output, with or without extension. If no
#'   directory is supplied, the file is saved in \code{tempdir()}.
#' @param format Output format. One of \code{"pdf"}, \code{"png"},
#'   \code{"tiff"}, or \code{"jpg"}.
#' @param width,height Optional export width and height in inches. If either is
#'   \code{NULL} and \code{auto_size = TRUE}, the size is measured from the
#'   intrinsic \pkg{forestploter} gtable dimensions. If measurement is not
#'   possible, a practical fallback is estimated from the number of rows and
#'   columns in the \code{forest_reg()} output.
#' @param scale Positive multiplier applied to the export width and height. This
#'   is a quick way to make a large forest plot roomier.
#' @param auto_size Logical. If \code{TRUE}, measure the intrinsic forest plot
#'   size when \code{width} or \code{height} is not supplied. If \code{FALSE},
#'   use the fallback row/column-based sizing.
#' @param padding White space around the forest plot in inches.
#' @param dpi Resolution for raster formats.
#'
#' @return Saves the file to disk. Invisibly returns the normalized file path.
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   transform(
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
#'   )
#'
#' uni_or <- uni_reg(
#'   birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "smoke", "ht"),
#'   approach = "logit"
#' )
#'
#' forest <- forest_reg(uni = uni_or)
#' save_forest(forest, filename = tempfile("forest"), format = "pdf")
#'
#' # For large forest plots, increase width, height, scale, or padding.
#' save_forest(
#'   forest,
#'   filename = tempfile("forest-wide"),
#'   format = "png",
#'   scale = 1.2,
#'   padding = 0.35,
#'   dpi = 300
#' )
#' @export
#' @importFrom grid grid.newpage grid.draw pushViewport popViewport viewport unit convertWidth convertHeight
save_forest <- function(forest,
                        filename = "forest",
                        format = c("pdf", "png", "tiff", "jpg"),
                        width = NULL,
                        height = NULL,
                        scale = 1,
                        auto_size = TRUE,
                        padding = 0.25,
                        dpi = 300) {
  format <- .choice_arg(substitute(format), env = parent.frame(), choices = c("pdf", "png", "tiff", "jpg"))
  format <- match.arg(format)

  if (!is.numeric(padding) || length(padding) != 1L ||
      is.na(padding) || padding < 0) {
    stop("`padding` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(dpi) || length(dpi) != 1L || is.na(dpi) || dpi <= 0) {
    stop("`dpi` must be a single positive number.", call. = FALSE)
  }

  plot <- .resolve_forest_object(forest)
  dims <- .forest_export_dimensions(
    forest,
    plot = plot,
    width = width,
    height = height,
    scale = scale,
    padding = padding,
    auto_size = auto_size
  )
  width <- dims$width
  height <- dims$height

  if (padding * 2 >= min(width, height)) {
    stop("`padding` is too large for the requested export width/height.", call. = FALSE)
  }

  filename <- .normalize_save_path(filename, format)

  device_open <- FALSE
  if (identical(format, "pdf")) {
    grDevices::pdf(filename, width = width, height = height, onefile = FALSE)
  } else if (identical(format, "png")) {
    grDevices::png(filename, width = width, height = height, units = "in", res = dpi)
  } else if (identical(format, "jpg")) {
    grDevices::jpeg(filename, width = width, height = height, units = "in", res = dpi)
  } else {
    grDevices::tiff(filename, width = width, height = height, units = "in",
                    res = dpi, compression = "lzw")
  }
  device_open <- TRUE
  on.exit(if (device_open) grDevices::dev.off(), add = TRUE)

  .draw_forest_export(plot, padding = padding)
  grDevices::dev.off()
  device_open <- FALSE

  message("Forest plot saved at: ", normalizePath(filename))
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
#' @param filename File name for the output, with or without \code{.docx}. If no
#'   directory is supplied, the file is saved in \code{tempdir()}.
#' @param titles Optional character vector of titles for tables and plots in
#'   the order they are added.
#' @param table_width Maximum table width in inches for Word export. The default
#'   \code{6.5} fits a standard portrait Word page with common margins. Use
#'   \code{NULL} to keep the original flextable widths.
#' @param plot_width Width of inserted plots in inches.
#' @param plot_height Height of inserted plots in inches.
#'
#' @return Saves the Word document to disk. Invisibly returns the normalized
#'   file path.
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   dplyr::mutate(
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
#'   )
#'
#' tbl <- uni_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "smoke"),
#'   approach = "logit",
#'   format = "flextable"
#' )
#'
#' save_docx(tables = tbl, filename = tempfile("report"))
#' @export
#' @importFrom officer read_docx body_add_par body_add_gg
#' @importFrom flextable body_add_flextable
save_docx <- function(tables = NULL,
                      plots = NULL,
                      filename = "report.docx",
                      titles = NULL,
                      table_width = 6.5,
                      plot_width = 6,
                      plot_height = 5) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required.", call. = FALSE)
  }

  filename <- .normalize_save_path(filename, "docx")

  if (!is.null(tables) && .is_save_table_like(tables)) {
    tables <- list(tables)
  }
  if (!is.null(plots) && inherits(plots, "ggplot")) {
    plots <- list(plots)
  }
  if (!is.null(tables) && (is.data.frame(tables) || !is.list(tables))) {
    stop("`tables` must be NULL, a table object, or a list of table objects.", call. = FALSE)
  }
  if (!is.null(plots) && (is.data.frame(plots) || !is.list(plots))) {
    stop("`plots` must be NULL, a ggplot2 object, or a list of ggplot2 objects.", call. = FALSE)
  }
  if (!is.numeric(plot_width) || length(plot_width) != 1L ||
      is.na(plot_width) || plot_width <= 0) {
    stop("`plot_width` must be a single positive number.", call. = FALSE)
  }
  if (!is.null(table_width) &&
      (!is.numeric(table_width) || length(table_width) != 1L ||
       is.na(table_width) || table_width <= 0)) {
    stop("`table_width` must be NULL or a single positive number.", call. = FALSE)
  }
  if (!is.numeric(plot_height) || length(plot_height) != 1L ||
      is.na(plot_height) || plot_height <= 0) {
    stop("`plot_height` must be a single positive number.", call. = FALSE)
  }

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
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Package 'flextable' is required to add tables to a DOCX file.", call. = FALSE)
    }

    for (tbl in tables) {
      obj <- .resolve_table_object(tbl)

      if (!is.null(titles)) {
        doc <- officer::body_add_par(doc, titles[[idx]], style = "heading 1")
        idx <- idx + 1L
      }

      if (inherits(obj, "flextable")) {
        ft <- .fit_flextable_docx_width(obj, table_width = table_width)
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
