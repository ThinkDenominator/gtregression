#' Prepare linear-regression diagnostics for console display
#'
#' @keywords internal
#' @noRd
.as_reg_check_result <- function(x, format = c("flextable", "gt")) {
  has_diagnostics <- function(item) {
    is.data.frame(item) || (
      is.list(item) && any(vapply(item, has_diagnostics, logical(1), USE.NAMES = FALSE))
    )
  }

  if (!is.list(x) || !length(x) || !has_diagnostics(x)) {
    return(x)
  }

  attr(x, "reg_check_format") <- match.arg(format)
  class(x) <- unique(c("gtregression_reg_check", class(x)))
  x
}

#' Combine nested linear-regression diagnostics into a display data frame
#'
#' @keywords internal
#' @noRd
.reg_check_display_data <- function(x) {
  rows <- list()

  collect <- function(item, path = character(0)) {
    if (is.data.frame(item)) {
      out <- item
      if (length(path) >= 2L) {
        out$Stratum <- path[[1L]]
      }
      rows[[length(rows) + 1L]] <<- out
      return(invisible(NULL))
    }

    if (is.list(item)) {
      nms <- names(item)
      if (is.null(nms)) nms <- rep("", length(item))
      for (i in seq_along(item)) {
        collect(item[[i]], c(path, nms[[i]]))
      }
    }
    invisible(NULL)
  }

  collect(unclass(x))
  if (!length(rows)) return(NULL)

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  preferred <- c("Stratum", "Exposure", "Test", "Statistic", "Interpretation")
  out[, c(intersect(preferred, names(out)), setdiff(names(out), preferred)), drop = FALSE]
}

#' Print linear-regression diagnostics as a publication-ready table
#'
#' @param x A diagnostic list stored in a gtregression regression result.
#' @param ... Unused.
#'
#' @return The diagnostic object, invisibly.
#' @keywords internal
#' @export
print.gtregression_reg_check <- function(x, ...) {
  display <- .reg_check_display_data(x)
  if (is.null(display) || !nrow(display)) {
    print(unclass(x))
    return(invisible(x))
  }

  format <- attr(x, "reg_check_format", exact = TRUE)
  if (identical(format, "gt")) {
    out <- gt::gt(display) |>
      gt::tab_header(title = "Linear regression diagnostics") |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_source_note(
        paste0(
          "Diagnostics are screening checks and should be interpreted alongside ",
          "residual plots and study context."
        )
      ) |>
      .compact_gt_source_notes()
    out <- gt::cols_align(
      out,
      align = "left",
      columns = intersect(c("Stratum", "Exposure", "Test", "Interpretation"), names(display))
    )
    if ("Statistic" %in% names(display)) {
      out <- gt::cols_align(out, align = "center", columns = "Statistic")
    }
  } else {
    out <- flextable::flextable(display)
    out <- flextable::set_caption(out, caption = "Linear regression diagnostics")
    out <- flextable::bold(out, part = "header", bold = TRUE)
    out <- flextable::align(
      out,
      j = intersect(c("Stratum", "Exposure", "Test", "Interpretation"), names(display)),
      align = "left",
      part = "all"
    )
    if ("Statistic" %in% names(display)) {
      out <- flextable::align(out, j = "Statistic", align = "center", part = "all")
    }
    out <- flextable::add_footer_lines(
      out,
      values = "Diagnostics are screening checks and should be interpreted alongside residual plots and study context."
    )
    out <- .compact_flex_footer(out)
    out <- flextable::italic(out, italic = TRUE, part = "footer")
    out <- flextable::autofit(out)
  }

  print(out)
  invisible(x)
}
