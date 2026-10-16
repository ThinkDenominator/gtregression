# -------------------------------------------------------------------
# merge_tables internals
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
  .validate_theme_arg(theme)
  if (length(theme) == 1 && !is.na(theme) &&
      theme %in% names(.theme_presets_merge)) {
    return(.theme_presets_merge[[theme]])
  }
  unique(tolower(theme))
}

#' Build canonical row-id map for a table_display object
#'
#' Rules:
#' - every header row is retained
#' - header followed immediately by another header (or end of table) is treated
#'   as a continuous/main-effect row and gets id var||var
#' - categorical headers get id var||__header__
#' - non-header rows get id current_var||level
#'
#' @keywords internal
#' @noRd
.canonical_row_ids <- function(obj, df) {
  body <- obj[["table_body"]]

  row_exposure <- attr(df, "row_exposure", exact = TRUE)
  if (is.data.frame(body) &&
      all(c("exposure", "level", "ref") %in% names(body)) &&
      length(row_exposure) == nrow(df)) {
    ids <- character(nrow(df))
    for (v in unique(row_exposure)) {
      display_idx <- which(row_exposure == v)
      rows <- body[body$exposure == v, , drop = FALSE]
      if (!nrow(rows)) next

      header_idx <- display_idx[df$is_header[display_idx] %in% TRUE]
      level_idx <- setdiff(display_idx, header_idx)
      is_categorical <- any(rows$ref %in% TRUE)

      if (length(header_idx)) {
        if (is_categorical && nrow(rows) == 2L && length(display_idx) == 1L) {
          non_reference <- rows$level[!rows$ref]
          ids[header_idx] <- paste0(v, "||", non_reference[1L])
        } else if (is_categorical) {
          ids[header_idx] <- paste0(v, "||__header__")
        } else {
          ids[header_idx] <- paste0(v, "||", v)
        }
      }

      if (length(level_idx)) {
        visible_labels <- trimws(as.character(df$Characteristic[level_idx]))
        visible_labels <- sub("\\s*\\((Ref\\.|ref)\\)$", "", visible_labels)
        body_levels <- as.character(rows$level)
        matched <- match(visible_labels, body_levels)
        if (anyNA(matched) && length(level_idx) <= length(body_levels)) {
          matched[is.na(matched)] <- seq_along(body_levels)[is.na(matched)]
        }
        ids[level_idx] <- paste0(v, "||", body_levels[matched])
      }
    }
    if (all(nzchar(ids))) return(ids)
  }

  if (is.data.frame(body) && all(c("var", "level", "type") %in% names(body))) {
    ids <- character()
    for (v in unique(body$var)) {
      rows <- body[body$var == v, , drop = FALSE]
      data_rows <- rows[rows$type != "header", , drop = FALSE]

      if (!nrow(data_rows)) {
        ids <- c(ids, paste0(v, "||", v))
      } else if (all(data_rows$type == "continuous")) {
        ids <- c(ids, paste0(v, "||", v))
      } else if (any(data_rows$type == "continuous")) {
        # Continuous variable with a "(Missing)" row: the summary sits on the
        # variable row (id var||var) and the missing count on its own row.
        ids <- c(ids, paste0(v, "||", data_rows$level))
      } else if (any(data_rows$type == "dichotomous")) {
        ids <- c(ids, paste0(v, "||", data_rows$level))
      } else {
        ids <- c(
          ids,
          paste0(v, "||__header__"),
          paste0(v, "||", data_rows$level)
        )
      }
    }
    if (length(ids) == nrow(df)) {
      return(ids)
    }
  }

  if (is.data.frame(body) && all(c("exposure", "level", "ref") %in% names(body))) {
    ids <- character()
    for (v in unique(body$exposure)) {
      rows <- body[body$exposure == v, , drop = FALSE]
      is_fact <- any(rows$ref %in% TRUE)

      if (!is_fact && nrow(rows) == 1L) {
        ids <- c(ids, paste0(v, "||", v))
      } else {
        levels <- rows$level
        if (!is_fact) {
          levels <- levels[levels != v]
        }
        ids <- c(
          ids,
          paste0(v, "||__header__"),
          paste0(v, "||", levels)
        )
      }
    }
    if (length(ids) == nrow(df)) {
      return(ids)
    }
  }

  NULL
}

.canonical_map <- function(obj) {
  df <- if (is.list(obj) && !is.null(obj[["table_display"]])) {
    obj[["table_display"]]
  } else {
    obj
  }
  stopifnot(all(c("Characteristic", "is_header") %in% names(df)))

  ids <- .canonical_row_ids(obj, df)

  if (is.null(ids)) {
    ids <- rep(NA_character_, nrow(df))
    current_var <- NA_character_
    # Tables without `table_body` (stratified outputs) still record the raw
    # variable of every display row; prefer it to the visible label so that
    # labelled data align with the other inputs.
    row_exposure <- attr(df, "row_exposure", exact = TRUE)
    if (length(row_exposure) != nrow(df)) row_exposure <- NULL

    for (i in seq_len(nrow(df))) {
      ch <- trimws(df$Characteristic[i])
      ih <- isTRUE(df$is_header[i])

      if (ih) {
        current_var <- if (is.null(row_exposure)) ch else as.character(row_exposure[i])

        # Continuous/main-effect rows live on the header line; categorical
        # headers are structural rows and should still be preserved.
        if (i == nrow(df) || isTRUE(df$is_header[i + 1])) {
          ids[i] <- paste0(current_var, "||", current_var)
        } else {
          ids[i] <- paste0(current_var, "||__header__")
        }
      } else {
        lvl <- trimws(df$Characteristic[i])
        lvl <- sub("\\s*\\((Ref\\.|ref)\\)$", "", lvl)
        ids[i] <- paste0(current_var, "||", lvl)
      }
    }
  }

  spec <- .header_spec(obj)
  content_cols <- spec$cols

  lookup <- new.env(parent = emptyenv())
  for (i in which(!is.na(ids))) {
    lookup[[ids[i]]] <- df[i, content_cols, drop = FALSE]
  }

  labels <- stats::setNames(as.character(df$Characteristic), ids)
  headers <- stats::setNames(as.logical(df$is_header), ids)

  list(
    row_ids_order = ids,
    content_cols = content_cols,
    lookup = lookup,
    labels = labels,
    headers = headers,
    spec = spec
  )
}

#' First element of a stored `format` value, lower case
#'
#' Some objects store the unresolved default `c("flextable", "gt")`.
#' @keywords internal
#' @noRd
.format1 <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  tolower(as.character(x)[1L])
}

#' Text of one part (header / body / footer) of a flextable
#' @keywords internal
#' @noRd
.flex_part_text <- function(ft, part) {
  p <- ft[[part]]
  if (is.null(p) || is.null(p$dataset) || !nrow(p$dataset)) return(NULL)
  d <- p$content$content$data
  if (is.null(d)) d <- p$content$data
  if (is.null(d)) return(NULL)
  matrix(
    vapply(d, function(chunk) paste(chunk$txt, collapse = ""), character(1)),
    nrow = nrow(d),
    dimnames = list(NULL, colnames(d))
  )
}

#' Read column labels and spanners from a rendered gt or flextable
#'
#' Returns `cols` (content columns in display order), `labels` (visible
#' column labels named by column) and `levels` (spanner rows, innermost first;
#' each a list with `label` and `cols`).
#' @keywords internal
#' @noRd
.rendered_header <- function(tbl) {
  if (inherits(tbl, "flextable")) {
    m <- .flex_part_text(tbl, "header")
    keys <- tbl$col_keys
    if (is.null(m) || !all(keys %in% colnames(m))) return(NULL)
    m <- m[, keys, drop = FALSE]
    spans <- tbl$header$spans$rows
    content <- setdiff(keys, "Characteristic")
    labels <- stats::setNames(unname(m[nrow(m), content]), content)
    levels <- list()
    if (nrow(m) > 1L) {
      for (r in rev(seq_len(nrow(m) - 1L))) {
        level_labels <- character()
        level_cols <- list()
        j <- 1L
        while (j <= length(keys)) {
          width <- if (is.matrix(spans) && all(dim(spans) == dim(m))) {
            max(1L, as.integer(spans[r, j]))
          } else {
            run <- 1L
            while (j + run <= length(keys) && identical(m[r, j + run], m[r, j])) {
              run <- run + 1L
            }
            run
          }
          group <- setdiff(keys[j:min(length(keys), j + width - 1L)], "Characteristic")
          if (nzchar(trimws(m[r, j])) && length(group)) {
            level_labels <- c(level_labels, unname(m[r, j]))
            level_cols[[length(level_cols) + 1L]] <- group
          }
          j <- j + width
        }
        if (length(level_labels)) {
          levels[[length(levels) + 1L]] <- list(label = level_labels, cols = level_cols)
        }
      }
    }
    return(list(cols = content, labels = labels, levels = levels))
  }

  if (inherits(tbl, "gt_tbl")) {
    boxhead <- tbl[["_boxhead"]]
    if (is.null(boxhead)) return(NULL)
    boxhead <- boxhead[!boxhead$type %in% "hidden", , drop = FALSE]
    keys <- as.character(boxhead$var)
    text <- vapply(boxhead$column_label, function(z) {
      paste(as.character(z), collapse = "")
    }, character(1))
    keep <- keys != "Characteristic"
    content <- keys[keep]
    labels <- stats::setNames(text[keep], content)
    levels <- list()
    spanners <- tbl[["_spanners"]]
    if (!is.null(spanners) && nrow(spanners)) {
      spanner_level <- spanners$spanner_level
      if (is.null(spanner_level)) spanner_level <- rep(1L, nrow(spanners))
      for (lv in sort(unique(spanner_level))) {
        rows <- which(spanner_level == lv)
        level_labels <- vapply(spanners$spanner_label[rows], function(z) {
          paste(as.character(z), collapse = "")
        }, character(1))
        level_cols <- lapply(spanners$vars[rows], function(v) {
          content[content %in% as.character(v)]
        })
        first <- vapply(level_cols, function(v) {
          if (length(v)) match(v[1L], content) else NA_integer_
        }, integer(1))
        keep_rows <- !is.na(first)
        ord <- order(first[keep_rows])
        levels[[length(levels) + 1L]] <- list(
          label = level_labels[keep_rows][ord],
          cols = level_cols[keep_rows][ord]
        )
      }
    }
    return(list(cols = content, labels = labels, levels = levels))
  }

  NULL
}

#' Visible header of a package table: columns, labels and spanners
#'
#' The rendered table is the most reliable record of what the reader sees
#' (stratified, multinomial and merged tables do not store their labels).
#' Falls back to stored `header_labels` or the display column names.
#' @keywords internal
#' @noRd
.header_spec <- function(obj) {
  display <- obj[["table_display"]]
  all_cols <- setdiff(names(display), c("Characteristic", "is_header"))

  rendered <- tryCatch(.rendered_header(obj[["table"]]), error = function(e) NULL)
  if (!is.null(rendered) && length(rendered$cols) &&
      all(rendered$cols %in% all_cols)) {
    return(rendered)
  }

  cols <- all_cols
  if ("N" %in% cols) cols <- c("N", setdiff(cols, "N"))
  labels <- stats::setNames(cols, cols)
  stored <- obj[["header_labels"]]
  if (!is.null(stored)) {
    matched <- intersect(cols, names(stored))
    labels[matched] <- as.character(stored[matched])
  }
  levels <- list()
  if (inherits(obj, c("merged_table", "multinomial_reg")) &&
      !is.null(obj[["column_labels"]])) {
    stored <- unlist(unname(obj[["column_labels"]]))
    matched <- intersect(cols, names(stored))
    labels[matched] <- as.character(stored[matched])
    if (!is.null(obj[["spanners"]])) {
      groups <- lapply(obj[["column_labels"]], function(x) intersect(names(x), cols))
      keep <- lengths(groups) > 0L
      levels <- list(list(label = obj[["spanners"]][keep], cols = groups[keep]))
    }
  }
  list(cols = cols, labels = labels, levels = levels)
}

#' Footnotes shown under a rendered gt or flextable
#' @keywords internal
#' @noRd
.rendered_footnotes <- function(tbl) {
  out <- character()
  if (inherits(tbl, "flextable")) {
    m <- .flex_part_text(tbl, "footer")
    if (!is.null(m)) out <- unname(m[, 1L])
  } else if (inherits(tbl, "gt_tbl")) {
    notes <- tbl[["_source_notes"]]
    if (length(notes)) {
      out <- unlist(lapply(notes, function(z) {
        strsplit(as.character(z), "  \n", fixed = TRUE)
      }), use.names = FALSE)
    }
  }
  out <- as.character(out)
  out[!is.na(out) & nzchar(out)]
}

#' Recover visible source-table column labels
#'
#' Some source tables use display labels that differ from their internal
#' column names. Descriptive tables, for example, append the group sample size
#' to each header. Preserve those labels when rebuilding a merged table.
#'
#' @keywords internal
#' @noRd
.source_column_labels <- function(obj, content_cols) {
  spec <- .header_spec(obj)
  labels <- stats::setNames(content_cols, content_cols)
  matched <- intersect(content_cols, names(spec$labels))
  labels[matched] <- as.character(spec$labels[matched])
  unname(labels)
}

#' Align compact binary regression rows to descriptive level rows
#'
#' Regression tables that do not display reference rows may represent a binary
#' exposure as one compact row (var||var). When merged with a descriptive table
#' that has explicit level rows (var||No, var||Yes), the compact row should live
#' on the non-reference level rather than creating a separate variable row.
#'
#' @keywords internal
#' @noRd
.align_compact_binary_maps <- function(maps) {
  parse_ids <- function(ids) {
    parts <- strsplit(ids, "\\|\\|")
    data.frame(
      id = ids,
      var = vapply(parts, `[`, character(1), 1),
      level = vapply(parts, `[`, character(1), 2),
      stringsAsFactors = FALSE
    )
  }

  all_ids <- unique(unlist(lapply(maps, `[[`, "row_ids_order"), use.names = FALSE))
  all_ids <- all_ids[!is.na(all_ids)]
  id_df <- parse_ids(all_ids)

  positive_values <- c(
    "yes", "y", "true", "1", "present", "positive", "exposed",
    "case", "event"
  )

  choose_level <- function(var, current_label) {
    candidates <- id_df$level[
      id_df$var == var &
        !id_df$level %in% c(var, "__header__")
    ]
    candidates <- unique(candidates[nzchar(candidates)])
    if (!length(candidates)) {
      return(NA_character_)
    }

    current_label <- trimws(current_label)
    label_match <- candidates[tolower(candidates) == tolower(current_label)]
    if (length(label_match)) {
      return(label_match[1])
    }

    positive_match <- candidates[tolower(candidates) %in% positive_values]
    if (length(positive_match)) {
      return(positive_match[1])
    }

    if (length(candidates) == 2L) {
      return(candidates[2])
    }

    NA_character_
  }

  remapped <- character()

  for (m in seq_along(maps)) {
    ids <- maps[[m]]$row_ids_order
    if (!length(ids)) {
      next
    }

    parsed <- parse_ids(ids)
    compact <- which(parsed$var == parsed$level)

    for (i in compact) {
      from <- parsed$id[i]
      value <- maps[[m]]$lookup[[from]]
      if (is.null(value)) {
        next
      }

      label <- unname(maps[[m]]$labels[from])
      target_level <- choose_level(parsed$var[i], label)
      if (is.na(target_level)) {
        next
      }

      to <- paste0(parsed$var[i], "||", target_level)
      if (identical(from, to)) {
        next
      }

      maps[[m]]$lookup[[to]] <- value
      rm(list = from, envir = maps[[m]]$lookup)

      maps[[m]]$row_ids_order[maps[[m]]$row_ids_order == from] <- to
      label_idx <- which(names(maps[[m]]$labels) == from)
      names(maps[[m]]$labels)[label_idx] <- to
      if (!is.null(maps[[m]]$headers)) {
        header_idx <- which(names(maps[[m]]$headers) == from)
        names(maps[[m]]$headers)[header_idx] <- to
        # Stays flagged as a compact (header-line) row; whether it is shown
        # as a level row is decided by `.resolve_compact_rows()`.
        maps[[m]]$headers[header_idx] <- TRUE
      }
      remapped <- c(remapped, parsed$var[i])
    }
  }

  list(
    maps = maps,
    remapped = unique(remapped)
  )
}

#' Decide how compact binary rows are displayed in the merged table
#'
#' A compact row carries a binary variable on a single line (regression tables
#' with `show_ref = FALSE`, descriptive tables with
#' `show_dichotomous = "single_row"`). Its canonical id is `var||level`.
#'
#' * When no input shows the variable with a header and level rows, and all
#'   inputs report the same level, the row keeps the variable label.
#' * When another input contributes a header row, the compact row becomes an
#'   indented level row and its N moves to the header row, where expanded
#'   tables print N.
#' * When compact inputs report different levels (different reference
#'   categories), a header row is added and each table's reference level is
#'   marked "Ref.".
#'
#' @keywords internal
#' @noRd
.resolve_compact_rows <- function(maps, tbls) {
  var_of <- function(ids) sub("\\|\\|.*$", "", ids)
  level_of <- function(ids) sub("^.*?\\|\\|", "", ids, perl = TRUE)
  blank_like <- function(x) if (is.character(x)) "" else x[NA_integer_]

  compact_pos <- lapply(maps, function(map) {
    ids <- map$row_ids_order
    which(
      !is.na(ids) & unname(map$headers) %in% TRUE &
        level_of(ids) != var_of(ids) & level_of(ids) != "__header__"
    )
  })
  variables <- unique(unlist(lapply(seq_along(maps), function(m) {
    var_of(maps[[m]]$row_ids_order[compact_pos[[m]]])
  }), use.names = FALSE))
  added_ref <- FALSE

  info <- lapply(stats::setNames(variables, variables), function(v) {
    header_id <- paste0(v, "||__header__")
    list(
      has_header = any(vapply(maps, function(map) {
        header_id %in% map$row_ids_order
      }, logical(1))),
      compact_ids = unique(unlist(lapply(seq_along(maps), function(m) {
        ids <- maps[[m]]$row_ids_order[compact_pos[[m]]]
        ids[var_of(ids) == v]
      }), use.names = FALSE)),
      other_levels = any(vapply(seq_along(maps), function(m) {
        ids <- maps[[m]]$row_ids_order
        pos <- which(!is.na(ids) & var_of(ids) == v &
                       !level_of(ids) %in% c(v, "__header__"))
        length(setdiff(pos, compact_pos[[m]])) > 0L
      }, logical(1)))
    )
  })

  for (v in variables) {
    header_id <- paste0(v, "||__header__")
    has_header <- info[[v]]$has_header
    compact_ids <- info[[v]]$compact_ids
    other_levels <- info[[v]]$other_levels

    # All inputs agree on one compact line: keep it, with the variable label.
    if (!has_header && length(compact_ids) == 1L && !other_levels) next

    for (m in seq_along(maps)) {
      map <- maps[[m]]
      ids <- map$row_ids_order
      k <- which(!is.na(ids) & ids %in% compact_ids &
                   unname(map$headers) %in% TRUE)
      if (!length(k)) next
      k <- k[1L]
      id <- ids[k]
      level <- level_of(id)
      row <- map$lookup[[id]]
      variable_label <- unname(map$labels[k])

      order_ids <- c(header_id, id)
      order_labels <- c(variable_label, paste0("  ", level))
      order_headers <- c(TRUE, FALSE)

      if (!is.null(row)) {
        n_cols <- intersect("N", names(row))
        if (length(n_cols)) {
          header_row <- row
          for (column in setdiff(names(row), n_cols)) {
            header_row[[column]] <- blank_like(row[[column]])
          }
          for (column in n_cols) row[[column]] <- blank_like(row[[column]])
          map$lookup[[header_id]] <- header_row
          map$lookup[[id]] <- row
        }

        body <- tbls[[m]][["table_body"]]
        if (!has_header && is.data.frame(body) &&
            all(c("exposure", "level", "ref") %in% names(body))) {
          body_rows <- body[body$exposure == v, , drop = FALSE]
          ref_level <- as.character(body_rows$level[body_rows$ref %in% TRUE])
          ref_id <- paste0(v, "||", ref_level[1L])
          effect_cols <- grep("95% CI", names(row), value = TRUE, fixed = TRUE)
          if (length(ref_level) == 1L && !ref_id %in% ids &&
              ref_id %in% compact_ids && length(effect_cols)) {
            ref_row <- row
            for (column in names(row)) ref_row[[column]] <- blank_like(row[[column]])
            for (column in effect_cols) ref_row[[column]] <- "Ref."
            map$lookup[[ref_id]] <- ref_row
            ref_first <- match(ref_level[1L], body_rows$level) <
              match(level, body_rows$level)
            if (isTRUE(ref_first)) {
              order_ids <- c(header_id, ref_id, id)
              order_labels <- c(variable_label, paste0("  ", ref_level[1L]),
                                paste0("  ", level))
            } else {
              order_ids <- c(order_ids, ref_id)
              order_labels <- c(order_labels, paste0("  ", ref_level[1L]))
            }
            order_headers <- c(TRUE, FALSE, FALSE)
            added_ref <- TRUE
          }
        }
      }

      before <- seq_len(k - 1L)
      after <- if (k < length(ids)) (k + 1L):length(ids) else integer()
      map$row_ids_order <- c(ids[before], order_ids, ids[after])
      map$labels <- c(
        map$labels[before],
        stats::setNames(order_labels, order_ids),
        map$labels[after]
      )
      map$headers <- c(
        map$headers[before],
        stats::setNames(order_headers, order_ids),
        map$headers[after]
      )
      maps[[m]] <- map
    }
  }

  list(maps = maps, added_ref = added_ref)
}

#' Inspect binary row representations across tables before merging.
#'
#' - `compact` indicates the variable is represented by one row (for example
#'   `smoke` as a single row in regression output with reference rows hidden).
#' - `expanded` indicates the variable is represented with explicit levels,
#'   e.g. `smoke = No`, `smoke = Yes`.
#'
#' @keywords internal
#' @noRd
.binary_mode_profile <- function(tbl) {
  body <- tbl[["table_body"]]
  display <- tbl[["table_display"]]
  if (!is.data.frame(body) || !nrow(body)) {
    return(list(compact = character(), expanded = character()))
  }
  body <- as.data.frame(body, stringsAsFactors = FALSE)
  if (is.data.frame(display)) {
    display <- as.data.frame(display, stringsAsFactors = FALSE)
  }

  compact <- character()
  expanded <- character()

  if (all(c("var", "type", "level") %in% names(body))) {
    for (v in unique(body$var)) {
      rows <- body[which(body$var == v), , drop = FALSE]
      data_rows <- rows[which(rows$type != "header"), , drop = FALSE]

      if (any(data_rows$type %in% "dichotomous")) {
        compact <- c(compact, v)
        next
      }

      level_rows <- data_rows[
        which(data_rows$type %in% c(
          "categorical", "dichotomous", "categorical_missing"
        )),
        ,
        drop = FALSE
      ]
      if (nrow(level_rows) == 0L) {
        next
      }

      level_vals <- trimws(as.character(level_rows$level))
      level_vals <- level_vals[nzchar(level_vals)]
      if (length(level_vals) %in% c(2L, 3L)) {
        # Two levels for all-level binary, or two levels + (Missing).
        expanded <- c(expanded, v)
      }
    }
    return(list(compact = unique(compact), expanded = unique(expanded)))
  }

  if (all(c("exposure", "level", "ref") %in% names(body))) {
    for (v in unique(body$exposure)) {
      rows <- body[which(body$exposure == v), , drop = FALSE]
      is_factor <- any(rows$ref %in% TRUE)

      if (is_factor && is.data.frame(display) &&
          all(c("Characteristic", "is_header") %in% names(display))) {
        header <- which(
          display$is_header %in% TRUE &
            trimws(as.character(display$Characteristic)) == v
        )
        if (length(header) == 1L) {
          next_header <- which(
            display$is_header %in% TRUE & seq_len(nrow(display)) > header
          )
          last_row <- if (length(next_header)) {
            next_header[1L] - 1L
          } else {
            nrow(display)
          }
          visible_levels <- if (header < last_row) {
            sum(!display$is_header[(header + 1L):last_row])
          } else {
            0L
          }

          # `show_ref = FALSE` represents a binary predictor on its header row.
          if (visible_levels == 0L && nrow(rows) == 2L) {
            compact <- c(compact, v)
          } else {
            expanded <- c(expanded, v)
          }
          next
        }
      }

      if (nrow(rows) == 1L) {
        # Regression compact binary row usually has level different from variable
        # name because reference rows are removed.
        lev <- as.character(rows$level[[1]])
        if (!identical(trimws(lev), trimws(v))) {
          compact <- c(compact, v)
        }
      } else if (is_factor && any(!rows$ref %in% TRUE)) {
        expanded <- c(expanded, v)
      }
    }
  }

  list(compact = unique(compact), expanded = unique(expanded))
}

#' Warn when binary rows are mixed as compact versus expanded across inputs.
#'
#' @keywords internal
#' @noRd
.binary_merge_mismatch <- function(tbls) {
  profiles <- lapply(tbls, .binary_mode_profile)

  compact_vars <- list()
  expanded_vars <- list()

  for (i in seq_along(profiles)) {
    profile <- profiles[[i]]

    for (v in profile$compact) {
      previous <- compact_vars[[v]]
      if (is.null(previous)) {
        previous <- integer()
      }
      compact_vars[[v]] <- unique(c(previous, i))
    }

    for (v in profile$expanded) {
      previous <- expanded_vars[[v]]
      if (is.null(previous)) {
        previous <- integer()
      }
      expanded_vars[[v]] <- unique(c(previous, i))
    }
  }

  vars_compact <- names(compact_vars)
  vars_expanded <- names(expanded_vars)
  mismatch <- intersect(vars_compact, vars_expanded)

  unique(mismatch)
}

#' Warn when binary rows are mixed as compact versus expanded across inputs.
#'
#' @keywords internal
#' @noRd
.warn_binary_merge_mode <- function(tbls) {
  mismatch <- .binary_merge_mismatch(tbls)

  if (!length(mismatch)) return(invisible(NULL))

  warning(
    "merge_tables(): binary display mode mismatch for variable(s): ",
    paste(mismatch, collapse = ", "),
    ". When merging descriptive, univariable, and multivariable tables, use ",
    "`show_dichotomous = \"all_levels\"` in descriptive_table() and ",
    "`show_ref = TRUE` in every regression table to prevent extra binary rows. ",
    "Alternatively, use compact display consistently: `show_dichotomous = ",
    "\"single_row\"` and `show_ref = FALSE`.",
    call. = FALSE
  )
}

#' Build merged skeleton using the union of canonical row ids
#' across all input tables
#' @keywords internal
#' @noRd
.build_merged_skeleton <- function(maps) {
  encountered_ids <- unique(unlist(
    lapply(maps, `[[`, "row_ids_order"),
    use.names = FALSE
  ))
  encountered_ids <- encountered_ids[!is.na(encountered_ids)]

  parsed <- strsplit(encountered_ids, "\\|\\|")
  variables <- unique(vapply(parsed, `[`, character(1), 1L))
  all_ids <- unlist(lapply(variables, function(variable) {
    map_ids <- lapply(maps, function(map) {
      ids <- map$row_ids_order
      ids[vapply(strsplit(ids, "\\|\\|"), `[`, character(1), 1L) == variable]
    })
    lengths_by_map <- lengths(map_ids)
    base <- if (any(lengths_by_map)) map_ids[[which.max(lengths_by_map)]] else character()
    variable_ids <- unique(c(base, unlist(map_ids, use.names = FALSE)))
    header <- variable_ids[grepl("\\|\\|__header__$", variable_ids)]
    c(header, setdiff(variable_ids, header))
  }), use.names = FALSE)

  rows <- lapply(all_ids, function(id) {
    parts <- strsplit(id, "\\|\\|")[[1]]
    var <- parts[1]
    lvl <- parts[2]

    is_source_header <- any(vapply(
      maps,
      function(map) {
        if (is.null(map$headers)) {
          return(FALSE)
        }
        isTRUE(unname(map$headers[id]))
      },
      logical(1)
    ))
    is_header <- identical(var, lvl) || identical(lvl, "__header__") ||
      is_source_header
    label <- NA_character_
    for (map in maps) {
      hit <- unname(map$labels[id])
      if (!is.na(hit) && nzchar(hit)) {
        label <- hit
        break
      }
    }
    char <- if (!is.na(label)) label else if (is_header) var else paste0("  ", lvl)

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
  # Prefer the notes created by the source function. A merge must preserve the
  # source table's reporting decisions rather than recreate them.
  stored_footnotes <- obj[["footnotes"]]
  if (!is.null(stored_footnotes)) {
    stored_footnotes <- as.character(stored_footnotes)
    return(unique(stored_footnotes[nzchar(stored_footnotes)]))
  }

  # Stratified tables do not store their notes; read what the reader sees.
  rendered <- tryCatch(
    .rendered_footnotes(obj[["table"]]),
    error = function(e) character()
  )
  if (length(rendered)) {
    return(unique(rendered))
  }

  # Compatibility fallback for objects created by earlier gtregression
  # versions that did not retain their rendered notes.
  out <- character()

  approach <- obj[["approach"]]
  if (!is.null(approach) && exists(".abbrev_note", mode = "function")) {
    out <- c(out, .abbrev_note(approach))
  }

  adjust_for <- obj[["adjust_for"]]
  if (!is.null(adjust_for) && length(adjust_for) &&
      exists(".adjustment_note", mode = "function")) {
    out <- c(out, .adjustment_note(adjust_for))
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
          " complete observations included in the model."
        )
      )
    }
  }

  unique(out[nzchar(out)])
}

#' Combine the footnotes of several tables
#'
#' Exact duplicates are shown once, notes of the same kind (same text before
#' the first colon, e.g. "Outcome comparison: ...") are kept together, and
#' several abbreviation lines are combined into one.
#' @keywords internal
#' @noRd
.merge_footnotes <- function(note_list) {
  kind_of <- function(x) {
    ifelse(grepl("^[^:]{1,40}: ", x), sub(": .*$", "", x), NA_character_)
  }
  out <- character()
  for (notes in note_list) {
    notes <- as.character(notes)
    for (note in notes[!is.na(notes) & nzchar(notes)]) {
      if (note %in% out) next
      kind <- kind_of(note)
      same <- which(!is.na(kind) & kind_of(out) %in% kind)
      if (length(same) && !.is_abbreviation_note(note)) {
        out <- append(out, note, after = max(same))
      } else {
        out <- c(out, note)
      }
    }
  }
  .combine_abbreviation_notes(out)
}

#' Add spanner rows to a flextable header
#'
#' `levels` is a list of spanner rows, innermost first; each has `label` and
#' `cols` (a list of column-key vectors).
#' @keywords internal
#' @noRd
.add_spanner_rows_flex <- function(ft, keys, levels) {
  for (level in levels) {
    values <- character()
    widths <- integer()
    j <- 1L
    while (j <= length(keys)) {
      group <- which(vapply(level$cols, function(x) keys[j] %in% x, logical(1)))
      if (length(group)) {
        width <- 1L
        while (j + width <= length(keys) &&
               keys[j + width] %in% level$cols[[group[1L]]]) {
          width <- width + 1L
        }
        values <- c(values, level$label[group[1L]])
        widths <- c(widths, width)
        j <- j + width
      } else {
        values <- c(values, "")
        widths <- c(widths, 1L)
        j <- j + 1L
      }
    }
    ft <- flextable::add_header_row(ft, values = values, colwidths = widths)
  }
  ft
}

#' Add spanner rows to a gt table (unique ids allow repeated labels)
#' @keywords internal
#' @noRd
.add_spanner_rows_gt <- function(tb, levels) {
  used <- character()
  for (l in seq_along(levels)) {
    level <- levels[[l]]
    for (g in seq_along(level$label)) {
      if (!length(level$cols[[g]])) next
      id <- level$label[g]
      if (id %in% used) id <- paste0(id, "__", l, "_", g)
      used <- c(used, id)
      tb <- gt::tab_spanner(
        tb,
        label = level$label[g],
        columns = dplyr::all_of(level$cols[[g]]),
        id = id,
        level = l
      )
    }
  }
  tb
}

#' Build a gt table from a display frame with labels, spanners and notes
#' @keywords internal
#' @noRd
.render_gt_table <- function(df, cols, labels, levels, theme, footnotes,
                             caption = NULL) {
  stopifnot(requireNamespace("gt", quietly = TRUE))

  out <- as.data.frame(df, stringsAsFactors = FALSE)[
    , c("Characteristic", cols), drop = FALSE
  ]
  labels_list <- c(
    list(Characteristic = "Characteristic"),
    stats::setNames(as.list(unname(as.character(labels[cols]))), cols)
  )

  tb <- gt::gt(out) |>
    gt::cols_label(.list = labels_list) |>
    gt::cols_align("left", columns = "Characteristic") |>
    gt::cols_align("center", columns = dplyr::all_of(cols)) |>
    gt::sub_missing(columns = dplyr::all_of(cols), missing_text = "") |>
    gt::tab_options(
      table.font.names = "system-ui",
      data_row.padding = gt::px(4),
      heading.background.color = "white",
      table.background.color = "white"
    )

  tb <- .add_spanner_rows_gt(tb, levels)
  tb <- gt::tab_style(
    tb,
    gt::cell_text(weight = "bold"),
    locations = gt::cells_column_spanners()
  )
  tb <- gt::tab_style(
    tb,
    gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  )
  tb <- gt::tab_style(
    tb,
    gt::cell_text(indent = gt::px(12)),
    gt::cells_body(rows = df$is_header %in% FALSE, columns = "Characteristic")
  )
  tb <- .apply_theme_gt(tb, df, theme)

  if (!is.null(caption)) tb <- gt::tab_caption(tb, caption = caption)
  # One plain-text source note per line: text such as `smoke*race` must not
  # be interpreted as markdown.
  for (line in footnotes) {
    tb <- gt::tab_source_note(tb, source_note = line)
  }
  if (length(footnotes)) tb <- .compact_gt_source_notes(tb)

  tb
}

#' Build a flextable from a display frame with labels, spanners and notes
#'
#' `vanilla = TRUE` applies the same theme function as the single regression
#' tables; otherwise the lighter merged / descriptive styling is used.
#' @keywords internal
#' @noRd
.render_flex_table <- function(df, cols, labels, levels, theme, footnotes,
                               caption = NULL, vanilla = FALSE,
                               body_top_rule = TRUE) {
  stopifnot(requireNamespace("flextable", quietly = TRUE))

  out <- as.data.frame(df, stringsAsFactors = FALSE)[
    , c("Characteristic", cols), drop = FALSE
  ]
  out[] <- lapply(out, function(x) {
    x[is.na(x)] <- ""
    x
  })
  keys <- colnames(out)
  labs <- c("Characteristic", unname(as.character(labels[cols])))

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(labs, keys))
  ft <- .add_spanner_rows_flex(ft, keys, levels)

  if (isTRUE(vanilla)) {
    ft <- .apply_theme_flex(ft, df, theme)
    ft <- flextable::bold(ft, part = "header", bold = TRUE)
  } else {
    ft <- flextable::align(ft, j = "Characteristic", align = "left", part = "all")
    ft <- flextable::align(ft, j = cols, align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header", bold = TRUE)

    hdr_idx <- which(df$is_header %in% TRUE)
    lvl_idx <- which(df$is_header %in% FALSE)

    if (length(hdr_idx)) {
      ft <- flextable::bold(ft, i = hdr_idx, j = "Characteristic",
                            bold = TRUE, part = "body")
    }
    if (length(lvl_idx)) {
      ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic",
                               padding.left = 14, part = "body")
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
    if ("lines" %in% theme && nrow(out) > 0L) {
      # Same rules as the single tables: the header keeps its top rule, a
      # thin rule separates header and body, and one closes the body.
      if (isTRUE(body_top_rule)) ft <- flextable::hline_top(ft, part = "body")
      ft <- flextable::hline(ft, i = nrow(out), part = "body")
    }
    if ("compact" %in% theme) {
      # Vertical padding only: the left padding carries the level indent.
      ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2,
                               part = "body")
    }
  }

  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption = caption)
  ft <- flextable::autofit(ft)

  if (length(footnotes)) {
    ft <- flextable::add_footer_lines(ft, values = footnotes)
    ft <- .compact_flex_footer(ft)
  }

  ft
}

# -------------------------------------------------------------------
# merge_tables main
# -------------------------------------------------------------------

#' Merge two or more tables side by side
#'
#' @param ... Two or more \code{gtregression} objects containing
#'   \code{$table_display}.
#' @param spanners Character vector of spanner labels, one per table.
#'   If \code{NULL}, defaults to \code{"Table 1"}, \code{"Table 2"}, etc.
#' @param theme Merge theme preset or vector of primitives.
#' @param format Output table format. One of \code{"flextable"} (default) or
#'   \code{"gt"}. The merged display is rebuilt in this format, independently
#'   of the formats used by the input tables.
#' @details
#' Binary variables should use the same row display across all input tables.
#' For a descriptive, univariable, and multivariable merge, the clearest
#' publication layout is usually \code{show_dichotomous = "all_levels"} in
#' \code{descriptive_table()} and \code{show_ref = TRUE} in each regression
#' function. Mixing these settings can create additional binary rows; in that
#' situation \code{merge_tables()} issues a warning with the compatible
#' settings.
#'
#' Footnotes created by each input table are retained unchanged. Exact duplicate
#' notes are shown once in the merged table.
#'
#' Tables of the same type can be merged directly. This is useful for
#' sensitivity analyses such as complete-case, best-case, and worst-case
#' models. Supply one table per scenario and use \code{spanners} to identify
#' each analysis; rows are aligned by their underlying variable and level.
#'
#' @return A merged table object of class
#'   \code{c("gtregression", "merged_table", ...)}.
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   dplyr::mutate(
#'     race = factor(race, levels = c(1, 2, 3),
#'                   labels = c("White", "Black", "Other")),
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
#'   )
#'
#' uni_tbl <- uni_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "lwt", "smoke", "ht"),
#'   approach = "logit"
#' )
#'
#' multi_tbl <- multi_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("smoke", "ht"),
#'   adjust_for = c("age", "lwt"),
#'   approach = "logit"
#' )
#'
#' merge_tables(
#'   uni_tbl,
#'   multi_tbl,
#'   spanners = c("Univariable", "Adjusted")
#' )
#'
#' merge_tables(
#'   multi_tbl,
#'   multi_tbl,
#'   spanners = c("Primary analysis", "Sensitivity analysis")
#' )
#' @export
merge_tables <- function(...,
                         spanners = NULL,
                         theme = "minimal",
                         format = c("flextable", "gt")) {
  theme <- .choice_arg(substitute(theme), env = parent.frame())
  format <- .choice_arg(
    substitute(format),
    env = parent.frame(),
    choices = c("flextable", "gt")
  )
  format <- match.arg(format, c("flextable", "gt"))

  tbls <- list(...)
  .rd_reject(tbls, "merge_tables")

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
      # A stored format may be the unresolved default c("flextable", "gt").
      fmt <- .format1(x[["format"]])
      tbl <- x[["table"]]

      if (inherits(tbl, "gt_tbl")) {
        return("gt")
      }
      if (inherits(tbl, "flextable")) {
        return("flextable")
      }
      fmt
    },
    character(1)
  )

  if (anyNA(engines) || length(unique(engines)) != 1L) {
    stop(
      "All inputs must use the same engine ('gt' or 'flextable').",
      call. = FALSE
    )
  }

  engine <- format
  theme <- .resolve_theme_merge(theme)

  if (is.null(spanners)) {
    spanners <- paste("Table", seq_along(tbls))
  }
  if (length(spanners) != length(tbls)) {
    stop("Length of `spanners` must equal number of tables.", call. = FALSE)
  }
  if (!is.character(spanners) || anyNA(spanners) || any(!nzchar(trimws(spanners)))) {
    stop("`spanners` must contain one non-empty character label per table.",
         call. = FALSE)
  }

  .warn_binary_merge_mode(tbls)
  binary_mismatch <- .binary_merge_mismatch(tbls)

  maps <- lapply(tbls, .canonical_map)
  aligned <- .align_compact_binary_maps(maps)
  maps <- aligned$maps
  aligned_mismatch <- intersect(aligned$remapped, binary_mismatch)
  if (length(aligned_mismatch)) {
    message(
      "merge_tables(): compact binary regression rows were aligned to ",
      "descriptive level rows for ",
      paste(aligned_mismatch, collapse = ", "),
      ". To display Ref., use `show_ref = TRUE` in the regression ",
      "function before merging."
    )
  }
  resolved <- .resolve_compact_rows(maps, tbls)
  maps <- resolved$maps
  skeleton <- .build_merged_skeleton(maps)
  base_ids <- skeleton$row_id

  merged_cols <- list()
  col_labels <- list()
  inner_spanners <- vector("list", length(tbls))

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
    source_labels <- unname(as.character(mi$spec$labels[mi$content_cols]))

    # Effect columns: never print the literal "NA (NA-NA)" of a non-estimable
    # coefficient, and use one confidence-interval separator in every column.
    effect_cols <- which(
      grepl("95% CI", source_labels, fixed = TRUE) |
        grepl("95% CI", mi$content_cols, fixed = TRUE)
    )
    for (j in effect_cols) {
      aligned[[j]] <- .tidy_effect_cells(aligned[[j]])
    }

    merged_cols[[i]] <- aligned
    col_labels[[i]] <- setNames(source_labels, nm = safe_names)

    # Spanners of the source table (strata, outcome comparisons, or the
    # spanners of an already merged table) become an inner spanner row.
    key_map <- stats::setNames(safe_names, mi$content_cols)
    inner_spanners[[i]] <- lapply(mi$spec$levels, function(level) {
      list(
        label = level$label,
        cols = lapply(level$cols, function(x) unname(key_map[x]))
      )
    })
  }

  merged_display <- cbind(
    skeleton[, c("Characteristic", "is_header"), drop = FALSE],
    do.call(cbind, merged_cols)
  )
  rownames(merged_display) <- NULL

  footnotes <- .merge_footnotes(lapply(tbls, .collect_footnotes))
  if (isTRUE(resolved$added_ref) && !.ref_note() %in% footnotes) {
    footnotes <- c(footnotes, .ref_note())
  }

  # Header rows: source spanners (innermost first), then one spanner per table.
  n_inner <- max(0L, lengths(inner_spanners))
  header_levels <- lapply(seq_len(n_inner), function(l) {
    parts <- lapply(inner_spanners, function(x) if (length(x) >= l) x[[l]] else NULL)
    list(
      label = unlist(lapply(parts, `[[`, "label"), use.names = FALSE),
      cols = unlist(lapply(parts, `[[`, "cols"), recursive = FALSE)
    )
  })
  has_cols <- lengths(col_labels) > 0L
  header_levels[[length(header_levels) + 1L]] <- list(
    label = spanners[has_cols],
    cols = lapply(col_labels[has_cols], names)
  )

  all_cols <- unlist(lapply(col_labels, names), use.names = FALSE)
  all_labels <- stats::setNames(unlist(col_labels, use.names = FALSE), all_cols)

  merged_tbl <- if (engine == "gt") {
    .render_gt_table(merged_display, all_cols, all_labels, header_levels,
                     theme, footnotes)
  } else {
    .render_flex_table(merged_display, all_cols, all_labels, header_levels,
                       theme, footnotes)
  }

  engine_class <- if (engine == "gt") "gt_merge" else "ft_merge"

  part_sources <- vapply(
    tbls,
    function(x) {
      src <- x[["source"]]
      if (!is.null(src)) as.character(src)[1L] else NA_character_
    },
    character(1)
  )

  part_formats <- vapply(tbls, function(x) .format1(x[["format"]]), character(1))

  res <- list(
    table = merged_tbl,
    table_display = merged_display,
    column_labels = col_labels,
    spanners = spanners,
    inner_spanners = inner_spanners,
    engine = engine,
    format = format,
    theme = theme,
    footnotes = footnotes,
    part_sources = part_sources,
    part_formats = part_formats,
    source = "merge_tables"
  )

  class(res) <- c("gtregression", "merged_table", engine_class, "list")
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
