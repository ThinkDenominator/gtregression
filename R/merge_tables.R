# -------------------------------------------------------------------
# merge_tables helpers
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.theme_presets_merge <- list(
  minimal  = c("plain", "lines", "labels_bold"),
  clinical = c("plain", "labels_bold", "compact"),
  striped  = c("zebra", "labels_bold", "compact"),
  shaded   = c("header_shaded", "labels_bold", "lines"),
  jama     = c("plain", "lines", "labels_bold", "compact")
)

#' @keywords internal
#' @noRd
.resolve_theme_merge <- function(theme) {
  if (length(theme) == 1 && !is.na(theme) &&
      theme %in% names(.theme_presets_merge)) {
    return(.theme_presets_merge[[theme]])
  }
  unique(tolower(theme))
}

#' Build canonical row-id map for a table_display object
#'
#' Rules:
#' - header followed immediately by another header (or end of table)
#'   is treated as a continuous/main-effect row and gets id var||var
#' - non-header rows get id current_var||level
#'
#' @keywords internal
#' @noRd
.canonical_map <- function(df) {
  stopifnot(all(c("Characteristic", "is_header") %in% names(df)))

  ids <- rep(NA_character_, nrow(df))
  current_var <- NA_character_

  for (i in seq_len(nrow(df))) {
    ch <- trimws(df$Characteristic[i])
    ih <- isTRUE(df$is_header[i])

    if (ih) {
      current_var <- ch

      # continuous/main-effect row lives on the header line
      if (i == nrow(df) || isTRUE(df$is_header[i + 1])) {
        ids[i] <- paste0(current_var, "||", current_var)
      }
    } else {
      lvl <- trimws(df$Characteristic[i])
      lvl <- sub("\\s*\\((Ref\\.|ref)\\)$", "", lvl)
      ids[i] <- paste0(current_var, "||", lvl)
    }
  }

  content_cols <- setdiff(names(df), c("Characteristic", "is_header"))
  if ("N" %in% content_cols) {
    content_cols <- c("N", setdiff(content_cols, "N"))
  }

  lookup <- new.env(parent = emptyenv())
  for (i in which(!is.na(ids))) {
    lookup[[ids[i]]] <- df[i, content_cols, drop = FALSE]
  }

  list(
    row_ids_order = ids,
    content_cols = content_cols,
    lookup = lookup
  )
}

#' Build merged skeleton using the union of canonical row ids
#' across all input tables
#' @keywords internal
#' @noRd
.build_merged_skeleton <- function(tbls) {
  maps <- lapply(tbls, function(x) .canonical_map(x[["table_display"]]))

  all_ids <- unique(unlist(
    lapply(maps, `[[`, "row_ids_order"),
    use.names = FALSE
  ))
  all_ids <- all_ids[!is.na(all_ids)]

  rows <- lapply(all_ids, function(id) {
    parts <- strsplit(id, "\\|\\|")[[1]]
    var <- parts[1]
    lvl <- parts[2]

    is_header <- identical(var, lvl)
    char <- if (is_header) var else paste0("  ", lvl)

    data.frame(
      Characteristic = char,
      is_header = is_header,
      row_id = id,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Collect footnotes from gtregression objects
#' @keywords internal
#' @noRd
.collect_footnotes <- function(obj) {
  out <- character()

  approach <- obj[["approach"]]
  if (!is.null(approach) && exists(".abbrev_note", mode = "function")) {
    out <- c(out, .abbrev_note(approach))
  }

  models <- obj[["models"]]
  is_single_multi <- inherits(obj, "multi_reg") &&
    !is.null(models) &&
    length(models) == 1L &&
    !is.null(models[[1]])

  if (is_single_multi) {
    n_used <- tryCatch(
      stats::nobs(models[[1]]),
      error = function(e) NA_integer_
    )
    if (!is.na(n_used)) {
      out <- c(
        out,
        paste0(
          "N = ", n_used,
          " complete observations included in the multivariable model"
        )
      )
    }
  }

  foots <- obj[["footnotes"]]
  if (!is.null(foots)) {
    out <- c(out, foots)
  }

  unique(out[nzchar(out)])
}

# -------------------------------------------------------------------
# merge_tables main
# -------------------------------------------------------------------

#' Merge gtregression tables and preserve structure and notes
#'
#' @param ... Two or more \code{gtregression} objects containing
#'   \code{$table_display}.
#' @param spanners Character vector of spanner labels, one per table.
#'   If \code{NULL}, defaults to \code{"Table 1"}, \code{"Table 2"}, etc.
#' @param theme Merge theme preset or vector of primitives.
#'
#' @return A merged table object of class \code{c("merged_table", ...)}.
#' @export
merge_tables <- function(..., spanners = NULL, theme = "minimal") {
  tbls <- list(...)

  if (length(tbls) < 2L) {
    stop("Provide at least two tables to merge.", call. = FALSE)
  }

  ok <- vapply(
    tbls,
    function(x) inherits(x, "gtregression") && !is.null(x[["table_display"]]),
    logical(1)
  )
  if (!all(ok)) {
    stop(
      "All inputs must be outputs of this package (contain $table_display).",
      call. = FALSE
    )
  }

  engines <- vapply(
    tbls,
    function(x) {
      fmt <- x[["format"]]
      tbl <- x[["table"]]

      if (!is.null(fmt)) {
        return(tolower(fmt))
      }
      if (inherits(tbl, "gt_tbl")) {
        return("gt")
      }
      if (inherits(tbl, "flextable")) {
        return("flextable")
      }
      NA_character_
    },
    character(1)
  )

  if (anyNA(engines) || length(unique(engines)) != 1L) {
    stop(
      "All inputs must use the same engine ('gt' or 'flextable').",
      call. = FALSE
    )
  }

  engine <- unique(engines)
  theme <- .resolve_theme_merge(theme)

  if (is.null(spanners)) {
    spanners <- paste("Table", seq_along(tbls))
  }
  if (length(spanners) != length(tbls)) {
    stop("Length of `spanners` must equal number of tables.", call. = FALSE)
  }

  maps <- lapply(tbls, function(x) .canonical_map(x[["table_display"]]))
  skeleton <- .build_merged_skeleton(tbls)
  base_ids <- skeleton$row_id

  merged_cols <- list()
  col_labels <- list()

  for (i in seq_along(tbls)) {
    mi <- maps[[i]]

    blank_row <- as.data.frame(
      as.list(rep("", length(mi$content_cols))),
      stringsAsFactors = FALSE
    )
    names(blank_row) <- mi$content_cols

    aligned <- do.call(
      rbind,
      lapply(seq_along(base_ids), function(r) {
        rid <- base_ids[r]
        got <- mi$lookup[[rid]]
        if (is.null(got)) {
          blank_row
        } else {
          got[, mi$content_cols, drop = FALSE]
        }
      })
    )

    safe_names <- make.unique(
      paste0(make.names(mi$content_cols, unique = FALSE), "_p", i)
    )
    names(aligned) <- safe_names
    merged_cols[[i]] <- aligned
    col_labels[[i]] <- setNames(mi$content_cols, nm = safe_names)
  }

  merged_display <- cbind(
    skeleton[, c("Characteristic", "is_header"), drop = FALSE],
    do.call(cbind, merged_cols)
  )

  footnotes <- unique(unlist(
    lapply(tbls, .collect_footnotes),
    use.names = FALSE
  ))
  footnotes <- footnotes[nzchar(footnotes)]

  # ---------------- gt helpers ----------------

  apply_theme_gt <- function(tb, df, theme) {
    tb <- gt::tab_style(
      tb,
      gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    )

    tb <- gt::tab_style(
      tb,
      gt::cell_text(weight = "bold"),
      gt::cells_body(rows = df$is_header %in% TRUE, columns = "Characteristic")
    )

    tb <- gt::tab_style(
      tb,
      gt::cell_text(indent = gt::px(12)),
      gt::cells_body(rows = df$is_header %in% FALSE, columns = "Characteristic")
    )

    if ("header_shaded" %in% theme) {
      tb <- gt::tab_options(
        tb,
        column_labels.background.color = "#f6f8fa"
      )
    }

    if ("zebra" %in% theme) {
      tb <- gt::opt_row_striping(tb)
    }

    if ("lines" %in% theme) {
      tb <- gt::tab_style(
        tb,
        gt::cell_borders(
          sides = "top",
          color = "#DADADA",
          weight = gt::px(1)
        ),
        gt::cells_body(rows = TRUE)
      )

      tb <- gt::tab_options(
        tb,
        table.border.top.style = "solid",
        table.border.top.color = "#DADADA",
        table.border.bottom.style = "solid",
        table.border.bottom.color = "#DADADA"
      )
    }

    if ("compact" %in% theme) {
      tb <- gt::tab_options(tb, data_row.padding = gt::px(2))
    }

    tb
  }

  build_gt_merge <- function(df, spanners, col_labels, theme, footnotes) {
    stopifnot(requireNamespace("gt", quietly = TRUE))

    content_cols <- unlist(lapply(col_labels, names), use.names = FALSE)
    out <- df[, c("Characteristic", content_cols), drop = FALSE]

    labels_list <- c(
      list(Characteristic = "Characteristic"),
      setNames(
        as.list(unlist(col_labels, use.names = FALSE)),
        nm = unlist(lapply(col_labels, names), use.names = FALSE)
      )
    )

    tb <- gt::gt(out) |>
      gt::cols_label(.list = labels_list) |>
      gt::cols_align("left", columns = "Characteristic") |>
      gt::cols_align(
        "center",
        columns = setdiff(names(out), "Characteristic")
      ) |>
      gt::sub_missing(
        columns = setdiff(names(out), "Characteristic"),
        missing_text = ""
      ) |>
      gt::tab_options(
        table.font.names = "system-ui",
        data_row.padding = gt::px(4),
        heading.background.color = "white",
        table.background.color = "white"
      )

    start <- 2L
    for (i in seq_along(spanners)) {
      k <- length(col_labels[[i]])
      if (k > 0) {
        tb <- gt::tab_spanner(
          tb,
          label = spanners[i],
          columns = names(out)[start:(start + k - 1L)]
        )
        start <- start + k
      }
    }

    tb <- gt::tab_style(
      tb,
      gt::cell_text(weight = "bold"),
      locations = gt::cells_column_spanners()
    )

    tb <- apply_theme_gt(tb, df, theme)

    if (length(footnotes)) {
      for (ln in footnotes) {
        tb <- gt::tab_source_note(tb, source_note = ln)
      }
      tb <- gt::tab_options(tb, source_notes.padding = gt::px(2))
    }

    tb
  }

  # ---------------- flextable helpers ----------------

  build_flex_merge <- function(df, spanners, col_labels, theme, footnotes) {
    stopifnot(requireNamespace("flextable", quietly = TRUE))

    content_cols <- unlist(lapply(col_labels, names), use.names = FALSE)
    out <- df[, c("Characteristic", content_cols), drop = FALSE]
    suppressWarnings(out[is.na(out)] <- "")

    keys <- colnames(out)
    labs <- c("Characteristic", unlist(col_labels, use.names = FALSE))

    ft <- flextable::flextable(out)
    ft <- flextable::set_header_labels(ft, .labels = stats::setNames(labs, keys))
    ft <- flextable::align(ft, j = "Characteristic", align = "left", part = "all")
    ft <- flextable::align(
      ft,
      j = setdiff(keys, "Characteristic"),
      align = "center",
      part = "all"
    )
    ft <- flextable::bold(ft, part = "header", bold = TRUE)

    widths <- c(1, vapply(col_labels, length, integer(1)))
    ft <- flextable::add_header_row(
      ft,
      values = c("", spanners),
      colwidths = widths
    )
    ft <- flextable::bold(ft, part = "header", bold = TRUE)

    hdr_idx <- which(df$is_header %in% TRUE)
    lvl_idx <- which(df$is_header %in% FALSE)

    if (length(hdr_idx)) {
      ft <- flextable::bold(
        ft,
        i = hdr_idx,
        j = "Characteristic",
        bold = TRUE,
        part = "body"
      )
    }

    if (length(lvl_idx)) {
      ft <- flextable::padding(
        ft,
        i = lvl_idx,
        j = "Characteristic",
        padding.left = 14,
        part = "body"
      )
    }

    if ("header_shaded" %in% theme) {
      ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
    }

    if ("zebra" %in% theme && length(lvl_idx)) {
      ft <- flextable::bg(
        ft,
        i = lvl_idx[seq(1, length(lvl_idx), by = 2)],
        bg = "#f6f8fa",
        part = "body"
      )
    }

    if ("lines" %in% theme) {
      n_body <- nrow(out)
      if (!is.null(n_body) && n_body > 0) {
        ft <- flextable::hline(ft, i = 1, part = "body")
        ft <- flextable::hline(ft, i = n_body, part = "body")
      }
    }

    if ("compact" %in% theme) {
      ft <- flextable::padding(ft, padding = 2, part = "body")
    }

    ft <- flextable::autofit(ft)

    if (length(footnotes)) {
      ft <- flextable::add_footer_lines(ft, values = footnotes)
      ft <- flextable::align(ft, part = "footer", align = "left")
      ft <- flextable::padding(
        ft,
        part = "footer",
        padding.top = 0,
        padding.bottom = 0
      )
      ft <- flextable::line_spacing(ft, part = "footer", space = 0.9)
    }

    ft
  }

  merged_tbl <- if (engine == "gt") {
    build_gt_merge(merged_display, spanners, col_labels, theme, footnotes)
  } else {
    build_flex_merge(merged_display, spanners, col_labels, theme, footnotes)
  }

  engine_class <- if (engine == "gt") "gt_merge" else "ft_merge"

  part_sources <- vapply(
    tbls,
    function(x) {
      src <- x[["source"]]
      if (!is.null(src)) src else NA_character_
    },
    character(1)
  )

  part_formats <- vapply(
    tbls,
    function(x) {
      fmt <- x[["format"]]
      if (!is.null(fmt)) fmt else NA_character_
    },
    character(1)
  )

  res <- list(
    table = merged_tbl,
    table_display = merged_display,
    spanners = spanners,
    engine = engine,
    footnotes = footnotes,
    part_sources = part_sources,
    part_formats = part_formats,
    source = "merge_tables"
  )

  class(res) <- c("merged_table", engine_class, "list")
  res
}

# -------------------------------------------------------------------
# print method
# -------------------------------------------------------------------

#' @export
print.merged_table <- function(x, ...) {
  tbl <- x[["table"]]
  if (inherits(tbl, "gt_tbl") || inherits(tbl, "flextable")) {
    base::print(tbl)
  } else {
    utils::str(x)
  }
  invisible(x)
}
