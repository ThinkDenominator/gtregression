# --- small theme resolver used here ------------------------------------------
.theme_presets_merge <- list(
  minimal  = c("plain","lines","labels_bold"),
  clinical = c("plain","labels_bold","compact"),
  striped  = c("zebra","labels_bold","compact"),
  shaded   = c("header_shaded","labels_bold","lines"),
  jama     = c("plain","lines","labels_bold","compact")
)
.resolve_theme_merge <- function(theme) {
  if (length(theme) == 1 && !is.na(theme) && theme %in% names(.theme_presets_merge))
    return(.theme_presets_merge[[theme]])
  unique(tolower(theme))
}

# --- canonical row-id map (keeps panel columns; puts N first if present) -----
.canonical_map <- function(df) {
  stopifnot(all(c("Characteristic","is_header") %in% names(df)))
  ids <- rep(NA_character_, nrow(df))
  current_var <- NA_character_
  for (i in seq_len(nrow(df))) {
    ch <- df$Characteristic[i]; ih <- isTRUE(df$is_header[i])
    if (ih) {
      current_var <- ch
      if (i == nrow(df) || isTRUE(df$is_header[i + 1])) {
        ids[i] <- paste0(current_var, "||", current_var)  # continuous lives here
      }
    } else {
      lvl <- sub("\\s*\\(Ref\\.\\)$", "", trimws(ch))
      ids[i] <- paste0(current_var, "||", lvl)
    }
  }
  content_cols <- setdiff(names(df), c("Characteristic","is_header"))
  # ✅ ensure N appears first inside each panel if present
  if ("N" %in% content_cols)
    content_cols <- c("N", setdiff(content_cols, "N"))
  lookup <- new.env(parent = emptyenv())
  for (i in which(!is.na(ids))) {
    # keep only the content columns in this exact order
    lookup[[ ids[i] ]] <- df[i, content_cols, drop = FALSE]
  }
  list(row_ids_order = ids, content_cols = content_cols, lookup = lookup)
}

# --- collect footnotes from parts (abbrev + multi N + descriptive notes) -----
.collect_footnotes <- function(obj) {
  out <- character()

  # Abbrev from regression tables
  if (!is.null(obj$approach) && exists(".abbrev_note", mode = "function"))
    out <- c(out, .abbrev_note(obj$approach))

  # N-note from multivariable (if present)
  if (identifies <- (inherits(obj, "multi_reg") && !is.null(obj$models) &&
                     !is.null(obj$models$multivariable_model))) {
    n_used <- tryCatch(stats::nobs(obj$models$multivariable_model), error = function(e) NA_integer_)
    if (!is.na(n_used))
      out <- c(out, paste0("N = ", n_used, " complete observations included in the multivariable model"))
  }

  # Descriptive notes: we store them directly on the object
  if (!is.null(obj$footnotes)) out <- c(out, obj$footnotes)

  unique(out[nzchar(out)])
}



#' Merge tables (descriptive / uni / multi) and preserve look & notes
#'
#' @param ... package tables with $table_display (same engine)
#' @param spanners labels over each panel
#' @param theme merge theme (preset or primitives)
#' @export
merge_tables <- function(..., spanners = NULL, theme = "minimal") {
  tbls <- list(...)
  if (length(tbls) < 2) stop("Provide at least two tables to merge.", call. = FALSE)

  ok <- vapply(tbls, function(x) inherits(x, "gtregression") && !is.null(x$table_display), logical(1))
  if (!all(ok)) stop("All inputs must be outputs of this package (contain $table_display).", call. = FALSE)

  engines <- vapply(tbls, function(x) {
    if (!is.null(x$format)) return(tolower(x$format))
    if (inherits(x$table, "gt_tbl")) return("gt")
    if (inherits(x$table, "flextable")) return("flextable")
    NA_character_
  }, character(1))
  if (anyNA(engines) || length(unique(engines)) != 1L)
    stop("All inputs must use the same engine ('gt' or 'flextable').", call. = FALSE)
  engine <- unique(engines)
  theme  <- .resolve_theme_merge(theme)

  if (is.null(spanners)) spanners <- paste("Table", seq_along(tbls))
  if (length(spanners) != length(tbls))
    stop("Length of `spanners` must equal number of tables.", call. = FALSE)

  base_disp <- tbls[[1]]$table_display
  base_ids  <- .canonical_map(base_disp)$row_ids_order

  maps <- lapply(tbls, function(x) .canonical_map(x$table_display))
  merged_cols <- list(); col_labels <- list()

  for (i in seq_along(tbls)) {
    mi <- maps[[i]]
    blank_row <- as.data.frame(as.list(rep("", length(mi$content_cols))), stringsAsFactors = FALSE)
    names(blank_row) <- mi$content_cols

    aligned <- do.call(rbind, lapply(seq_along(base_ids), function(r) {
      rid <- base_ids[r]
      if (is.na(rid)) return(blank_row)
      got <- mi$lookup[[ rid ]]
      if (is.null(got)) blank_row else got[, mi$content_cols, drop = FALSE]
    }))

    # internal, unique, syntactic names per panel; labels kept separately
    safe_names <- make.unique(paste0(make.names(mi$content_cols, unique = FALSE), "_p", i))
    names(aligned) <- safe_names
    merged_cols[[i]] <- aligned

    # pretty labels (so we show "p-value", not "pvalue.1")
    col_labels[[i]] <- setNames(mi$content_cols, nm = safe_names)
  }

  merged_display <- cbind(
    base_disp[, c("Characteristic","is_header"), drop = FALSE],
    do.call(cbind, merged_cols)
  )

  # gather footnotes from all parts
  footnotes <- unique(unlist(lapply(tbls, .collect_footnotes), use.names = FALSE))
  footnotes <- footnotes[nzchar(footnotes)]

  # -------- builders (gt / flextable) ----------------------------------------
  apply_theme_gt <- function(tb, df, theme) {
    tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                        locations = gt::cells_column_labels())
    tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                        gt::cells_body(rows = df$is_header %in% TRUE, columns = "Characteristic"))
    tb <- gt::tab_style(tb, gt::cell_text(indent = gt::px(12)),
                        gt::cells_body(rows = df$is_header %in% FALSE, columns = "Characteristic"))
    if ("header_shaded" %in% theme) tb <- gt::tab_options(tb, column_labels.background.color = "#f6f8fa")
    if ("zebra" %in% theme) tb <- gt::opt_row_striping(tb)
    if ("lines" %in% theme) {
      tb <- gt::tab_style(tb, gt::cell_borders(sides = "top", color = "#DADADA", weight = gt::px(1)),
                          gt::cells_body(rows = TRUE))
      tb <- gt::tab_options(tb,
                            table.border.top.style = "solid",  table.border.top.color = "#DADADA",
                            table.border.bottom.style = "solid", table.border.bottom.color = "#DADADA")
    }
    if ("compact" %in% theme) tb <- gt::tab_options(tb, data_row.padding = gt::px(2))
    tb
  }

  build_gt_merge <- function(df, spanners, col_labels, theme, footnotes) {
    stopifnot(requireNamespace("gt", quietly = TRUE))
    content_cols <- unlist(lapply(col_labels, names), use.names = FALSE)
    out <- df[, c("Characteristic", content_cols), drop = FALSE]

    labels_list <- c(list(Characteristic = "Characteristic"),
                     setNames(as.list(unlist(col_labels, use.names = FALSE)),
                              nm = unlist(lapply(col_labels, names), use.names = FALSE)))

    tb <- gt::gt(out) |>
      gt::cols_label(.list = labels_list) |>
      gt::cols_align("left",   columns = "Characteristic") |>
      gt::cols_align("center", columns = setdiff(names(out), "Characteristic")) |>
      gt::sub_missing(columns = setdiff(names(out), "Characteristic"), missing_text = "") |>
      gt::tab_options(
        table.font.names         = "system-ui",
        data_row.padding         = gt::px(4),
        heading.background.color = "white",
        table.background.color   = "white"
      )

    # spanners per input
    start <- 2L
    for (i in seq_along(spanners)) {
      k <- length(col_labels[[i]])
      if (k > 0) {
        tb <- gt::tab_spanner(tb, label = spanners[i], columns = names(out)[start:(start + k - 1L)])
        start <- start + k
      }
    }
    tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                        locations = gt::cells_column_spanners())
    tb <- apply_theme_gt(tb, df, theme)

    # right before 'tb' is returned in build_gt_merge()
    if (length(footnotes)) {
      for (ln in footnotes) tb <- gt::tab_source_note(tb, source_note = ln)
      tb <- gt::tab_options(tb, source_notes.padding = gt::px(2))
    }

    tb
  }

  build_flex_merge <- function(df, spanners, col_labels, theme, footnotes) {
    stopifnot(requireNamespace("flextable", quietly = TRUE))
    content_cols <- unlist(lapply(col_labels, names), use.names = FALSE)
    out <- df[, c("Characteristic", content_cols), drop = FALSE]
    # remove NA display
    suppressWarnings(out[is.na(out)] <- "")

    keys  <- colnames(out)
    labs  <- c("Characteristic", unlist(col_labels, use.names = FALSE))

    ft <- flextable::flextable(out)
    ft <- flextable::set_header_labels(ft, .labels = setNames(labs, keys))
    ft <- flextable::align(ft, j = "Characteristic", align = "left",  part = "all")
    ft <- flextable::align(ft, j = setdiff(keys, "Characteristic"), align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header", bold = TRUE)
    # spanner row
    widths <- c(1, vapply(col_labels, length, integer(1)))
    ft <- flextable::add_header_row(ft, values = c("", spanners), colwidths = widths)
    ft <- flextable::bold(ft, part = "header", bold = TRUE)
    # bold/indent body
    hdr_idx <- which(df$is_header %in% TRUE)
    lvl_idx <- which(df$is_header %in% FALSE)
    if (length(hdr_idx)) ft <- flextable::bold(ft, i = hdr_idx, j = "Characteristic", bold = TRUE, part = "body")
    if (length(lvl_idx)) ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic", padding.left = 14, part = "body")
    # themes
    if ("header_shaded" %in% theme) ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
    if ("zebra" %in% theme && length(lvl_idx)) {
      ft <- flextable::bg(ft, i = lvl_idx[seq(1, length(lvl_idx), by = 2)], bg = "#f6f8fa", part = "body")
    }
    if ("lines" %in% theme) {
      n_body <- nrow(out); if (!is.null(n_body) && n_body > 0) {
        ft <- flextable::hline(ft, i = 1, part = "body")
        ft <- flextable::hline(ft, i = n_body, part = "body")
      }
    }
    if ("compact" %in% theme) ft <- flextable::padding(ft, padding = 2, part = "body")
    ft <- flextable::colformat_na(ft, na_str = "")
    ft <- flextable::autofit(ft)

    if (length(footnotes)) {
      ft <- flextable::add_footer_lines(ft, values = footnotes)
      ft <- flextable::align(ft, part = "footer", align = "left")
      ft <- flextable::padding(ft, part = "footer", padding.top = 0, padding.bottom = 0)
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
  res <- list(
    table         = merged_tbl,
    table_display = merged_display,
    parts         = tbls,
    spanners      = spanners,
    engine        = engine,
    source        = "merge_tables"
  )
  class(res) <- c("merged_table","gtregression", engine_class, class(res))
  res
}

# safe print
#' @export
print.merged_table <- function(x, ...) {
  tbl <- x[["table"]]
  if (inherits(tbl, "gt_tbl") || inherits(tbl, "flextable")) base::print(tbl) else utils::str(x)
  invisible(x)
}
