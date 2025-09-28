# ---------- helper: safely relabel display df ----------
#' @keywords internal
#' @noRd
.relabel_display <- function(display_df,
                             variable_labels = NULL,
                             level_labels    = NULL) {
  df <- as.data.frame(display_df, stringsAsFactors = FALSE)
  stopifnot(all(c("Characteristic","is_header") %in% names(df)))

  # carry variable name down so we can match levels
  var_id <- character(nrow(df))
  cur <- NA_character_
  for (i in seq_len(nrow(df))) {
    if (isTRUE(df$is_header[i])) cur <- trimws(df$Characteristic[i])
    var_id[i] <- cur
  }

  # variable (header) relabel
  if (!is.null(variable_labels)) {
    hdr_idx <- which(df$is_header %in% TRUE)
    if (length(hdr_idx)) {
      old_hdr <- trimws(df$Characteristic[hdr_idx])
      idx <- match(old_hdr, names(variable_labels))
      repl <- variable_labels[idx]
      df$Characteristic[hdr_idx] <- ifelse(is.na(idx), df$Characteristic[hdr_idx], unname(repl))
    }
  }

  # level relabel (keep indent)
  if (!is.null(level_labels)) {
    lvl_idx <- which(df$is_header %in% FALSE)
    if (length(lvl_idx)) {
      base_lvl <- sub("^\\s+", "", df$Characteristic[lvl_idx])
      df$Characteristic[lvl_idx] <- mapply(
        FUN = function(v, lv, original) {
          ll <- level_labels[[v]]
          if (is.null(ll)) return(original)
          hit <- match(lv, names(ll))
          if (is.na(hit)) return(original)
          paste0("  ", unname(ll[hit]))
        },
        v = var_id[lvl_idx], lv = base_lvl, original = df$Characteristic[lvl_idx],
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      )
    }
  }

  df
}

# ---------- main: modify labels/headers/notes and rebuild (gtregression only) ----------
#' Modify Regression/Descriptive Tables (labels, headers, caption, notes)
#'
#' Works with objects created by this package (class `"gtregression"`:
#' `uni_reg()`, `multi_reg()`, `descriptive_table()`, `merge_tables()`).
#' No `gtsummary` dependency or fallback.
#'
#' @param gt_table Table object produced by this package (must contain `$table_display`).
#' @param variable_labels Named character vector: `c(old_var = "New label", ...)`.
#' @param level_labels Named list for factor levels:
#'   `list(var1 = c(old = "New", ...), var2 = c(...))`.
#' @param header_labels Named vector to rename visible headers, e.g.
#'   `c("OR (95% CI)" = "Crude OR", "p-value" = "P")`.
#' @param caption Optional caption/title.
#' @param bold_labels Logical; bold variable (header) rows in the body.
#' @param bold_levels Logical; bold factor level rows in the body.
#' @param remove_N Logical; if `TRUE`, drops the `N` column for univariate package tables.
#' @param remove_N_obs Logical; if `TRUE`, suppresses multivariable complete-case footnote.
#' @param remove_abbreviations Logical; if `TRUE`, removes the Abbreviations footnote line.
#' @param caveat Optional extra footnote.
#' @return The modified table object (same class as input).
#' @export
modify_table <- function(gt_table,
                         variable_labels = NULL,
                         level_labels = NULL,
                         header_labels = NULL,
                         caption = NULL,
                         bold_labels = FALSE,
                         bold_levels = FALSE,
                         remove_N = FALSE,
                         remove_N_obs = FALSE,
                         remove_abbreviations = FALSE,
                         caveat = NULL) {
  tbl <- gt_table

  # --------- accept only our objects ----------
  is_pkg_obj <- inherits(tbl, "gtregression") && !is.null(tbl$table_display)
  if (!is_pkg_obj) {
    stop("`modify_table()` expects a gtregression object from this package (with $table_display).",
         call. = FALSE)
  }

  # --------- relabel display safely ----------
  tbl$table_display <- .relabel_display(
    tbl$table_display,
    variable_labels = variable_labels,
    level_labels    = level_labels
  )

  # --------- optionally drop N column (for uni tables) ----------
  if (isTRUE(remove_N) && "N" %in% names(tbl$table_display)) {
    tbl$table_display$N <- NULL
  }

  # --------- collect/compose footnotes ----------
  footnotes <- character(0)
  # existing notes attached to object or inner table
  ft_attr1 <- attr(tbl, "footnotes", exact = TRUE)
  if (!is.null(ft_attr1)) footnotes <- c(footnotes, ft_attr1)
  ft_attr2 <- if (!is.null(tbl$table)) attr(tbl$table, "footnotes", exact = TRUE) else NULL
  if (!is.null(ft_attr2)) footnotes <- c(footnotes, ft_attr2)
  # abbreviations line (unless removed)
  if (!isTRUE(remove_abbreviations) && !is.null(tbl$approach) &&
      exists(".abbrev_note", mode = "function")) {
    footnotes <- c(footnotes, .abbrev_note(tbl$approach))
  }
  # multivariable N note (unless removed)
  if (!isTRUE(remove_N_obs) && inherits(tbl, "multi_reg")) {
    if (!is.null(tbl$models) && !is.null(tbl$models$multivariable_model)) {
      n_used <- tryCatch(stats::nobs(tbl$models$multivariable_model),
                         error = function(e) NA_integer_)
      if (!is.na(n_used)) {
        footnotes <- c(footnotes,
                       paste0("N = ", n_used,
                              " complete observations included in the multivariable model"))
      }
    }
  }
  # default descriptive notes (if none)
  if (!length(footnotes) && inherits(tbl, "descriptive_table")) {
    footnotes <- c(
      "Categorical variables shown as n (%).",
      "Continuous variables shown as Median (IQR)."
    )
  }
  # user caveat last
  if (!is.null(caveat) && nzchar(caveat)) footnotes <- c(footnotes, caveat)
  footnotes <- unique(footnotes[nzchar(footnotes)])

  # --------- reorder visible columns for univariate: N → Effect → p-value ----------
  reorder_univariate_cols <- function(display_df) {
    stopifnot(all(c("Characteristic","is_header") %in% names(display_df)))
    cols <- names(display_df)
    body_cols <- setdiff(cols, c("Characteristic","is_header"))
    if (length(body_cols)) {
      # classic uni layout has exactly one effect col + optional N + p-value
      hasN   <- "N" %in% body_cols
      hasP   <- "p-value" %in% body_cols
      effect <- setdiff(body_cols, c("N","p-value"))
      if (hasP && length(effect) == 1L) {
        ord <- c("Characteristic", if (hasN) "N", effect, "p-value")
        return(display_df[, ord, drop = FALSE])
      }
    }
    # otherwise: drop is_header and keep as-is
    display_df[, setdiff(names(display_df), "is_header"), drop = FALSE]
  }

  # --------- builders (gt / flextable) ----------
  .build_pkg_gt <- function(display_df, header_labels, footnotes) {
    stopifnot(requireNamespace("gt", quietly = TRUE))
    out <- reorder_univariate_cols(display_df)

    # header label mapping: gt expects a list(colname = "New Label")
    if (!is.null(header_labels)) {
      lab_list <- as.list(header_labels)
      names(lab_list) <- names(header_labels)
    } else {
      lab_list <- NULL
    }

    tb <- gt::gt(out) |>
      gt::cols_label(.list = lab_list) |>
      gt::cols_align("left",   columns = "Characteristic") |>
      gt::cols_align("center", columns = setdiff(names(out), "Characteristic")) |>
      gt::sub_missing(columns = setdiff(names(out), "Characteristic"), missing_text = "") |>
      gt::tab_options(
        table.font.names         = "system-ui",
        data_row.padding         = gt::px(4),
        heading.background.color = "white",
        table.background.color   = "white",
        source_notes.padding     = gt::px(2)
      )

    # bold column labels
    tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                        locations = gt::cells_column_labels())
    # bold variable headers (on demand; note: many tables already bold by default)
    if (isTRUE(bold_labels)) {
      hdr_idx <- which(display_df$is_header %in% TRUE)
      if (length(hdr_idx)) {
        tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                            locations = gt::cells_body(rows = hdr_idx, columns = "Characteristic"))
      }
    }
    # optional bold levels
    if (isTRUE(bold_levels)) {
      lvl_idx <- which(display_df$is_header %in% FALSE)
      if (length(lvl_idx)) {
        tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                            locations = gt::cells_body(rows = lvl_idx, columns = "Characteristic"))
      }
    }
    # indent levels
    tb <- gt::tab_style(tb, gt::cell_text(indent = gt::px(12)),
                        locations = gt::cells_body(rows = display_df$is_header %in% FALSE,
                                                   columns = "Characteristic"))
    # caption
    if (!is.null(caption)) tb <- gt::tab_caption(tb, caption = caption)
    # footnotes
    if (length(footnotes)) {
      tb <- gt::tab_source_note(tb, source_note = gt::md(paste(footnotes, collapse = "  \n")))
    }
    tb
  }

  .build_pkg_flex <- function(display_df, header_labels, footnotes) {
    stopifnot(requireNamespace("flextable", quietly = TRUE))
    out <- reorder_univariate_cols(display_df)

    # NA -> "" (older flextable compat)
    out[] <- lapply(out, function(x) { x[is.na(x)] <- ""; x })

    # rename headers by changing column names (no reordering)
    if (!is.null(header_labels)) {
      nn <- colnames(out)
      hit <- nn %in% names(header_labels)
      nn[hit] <- unname(header_labels[nn[hit]])
      colnames(out) <- nn
    }

    ft <- flextable::flextable(out)
    ft <- flextable::align(ft, j = "Characteristic", align = "left",  part = "all")
    ft <- flextable::align(ft, j = setdiff(names(out), "Characteristic"), align = "center", part = "all")
    ft <- flextable::bold(ft, part = "header", bold = TRUE)

    # bold variable headers (on demand)
    if (isTRUE(bold_labels)) {
      hdr_idx <- which(display_df$is_header %in% TRUE)
      if (length(hdr_idx)) ft <- flextable::bold(ft, i = hdr_idx, j = "Characteristic", bold = TRUE, part = "body")
    }
    # optional bold levels
    if (isTRUE(bold_levels)) {
      lvl_idx <- which(display_df$is_header %in% FALSE)
      if (length(lvl_idx)) ft <- flextable::bold(ft, i = lvl_idx, j = "Characteristic", bold = TRUE, part = "body")
    }
    # indent levels
    lvl_idx <- which(display_df$is_header %in% FALSE)
    if (length(lvl_idx)) ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic",
                                                  padding.left = 14, part = "body")
    # caption
    if (!is.null(caption)) ft <- flextable::set_caption(ft, caption = caption)
    # footnotes
    if (length(footnotes)) {
      ft <- flextable::add_footer_lines(ft, values = footnotes)
      ft <- flextable::align(ft, part = "footer", align = "left")
      ft <- flextable::padding(ft, part = "footer", padding.top = 0, padding.bottom = 0)
      ft <- flextable::line_spacing(ft, part = "footer", space = 0.9)
    }

    ft <- flextable::autofit(ft)
    ft
  }

  # --------- rebuild rendered table ----------
  if (identical(tolower(tbl$format), "gt") || inherits(tbl$table, "gt_tbl")) {
    tbl$table <- .build_pkg_gt(tbl$table_display, header_labels, footnotes)
  } else if (identical(tolower(tbl$format), "flextable") || inherits(tbl$table, "flextable")) {
    tbl$table <- .build_pkg_flex(tbl$table_display, header_labels, footnotes)
  }

  tbl
}
