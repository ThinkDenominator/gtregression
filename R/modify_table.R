# ---------- helper: safely relabel display df ----------
#' @keywords internal
#' @noRd
.relabel_display <- function(display_df,
                             variable_labels = NULL,
                             level_labels    = NULL) {
  df <- as.data.frame(display_df, stringsAsFactors = FALSE)
  stopifnot(all(c("Characteristic","is_header") %in% names(df)))

  .must_be_named_character(variable_labels, "variable_labels", allow_null = TRUE)
  if (!is.null(level_labels) &&
      (!is.list(level_labels) || is.null(names(level_labels)) ||
       any(!nzchar(names(level_labels))))) {
    stop("`level_labels` must be a named list.", call. = FALSE)
  }
  if (!is.null(level_labels)) {
    for (nm in names(level_labels)) {
      .must_be_named_character(level_labels[[nm]], paste0("level_labels$", nm), allow_null = FALSE)
    }
  }

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

#' Validate named character vector inputs
#' @keywords internal
#' @noRd
.must_be_named_character <- function(x, arg, allow_null = TRUE) {
  if (is.null(x) && isTRUE(allow_null)) {
    return(invisible(TRUE))
  }
  if (!is.character(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop("`", arg, "` must be a named character vector.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Normalize common header aliases to visible table_display names
#' @keywords internal
#' @noRd
.normalize_header_labels <- function(header_labels, display_df, approach = NULL) {
  .must_be_named_character(header_labels, "header_labels", allow_null = TRUE)
  if (is.null(header_labels)) {
    return(NULL)
  }

  out <- header_labels
  effect_cols <- setdiff(names(display_df), c("Characteristic", "is_header", "N", "p-value"))
  effect_col <- if (length(effect_cols) == 1L) effect_cols else NA_character_
  aliases <- c(
    estimate = effect_col,
    p.value = "p-value",
    p_value = "p-value",
    pvalue = "p-value",
    N = "N"
  )

  for (nm in names(out)) {
    if (!nm %in% names(display_df) && nm %in% names(aliases) && !is.na(aliases[[nm]])) {
      names(out)[names(out) == nm] <- aliases[[nm]]
    }
  }

  bad <- setdiff(names(out), names(display_df))
  if (length(bad)) {
    stop(
      "`header_labels` contains columns not found in the table: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

#' Validate logical scalar inputs
#' @keywords internal
#' @noRd
.must_be_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---------- main: modify labels/headers/notes and rebuild (gtregression only) ----------
#' Modify Regression/Descriptive Tables (labels, headers, caption, notes)
#'
#' Works with objects created by this package (class \code{"gtregression"}):
#' \code{uni_reg()}, \code{multi_reg()}, \code{descriptive_table()},
#' and \code{merge_tables()}.
#' No \pkg{gtsummary} dependency or fallback.
#'
#' @param gt_table Table object produced by this package (must contain
#'   \code{$table_display}).
#' @param variable_labels Named character vector, for example
#'   \code{c(old_var = "New label", ...)}.
#' @param level_labels Named list for factor levels:
#'   \code{list(var1 = c(old = "New", ...), var2 = c(...))}.
#' @param header_labels Named character vector to rename visible headers, e.g.
#'   \code{c("OR (95\% CI)" = "Crude OR", "p-value" = "P")}. Common aliases
#'   such as \code{estimate}, \code{p.value}, and \code{N} are also accepted.
#' @param caption Optional caption/title.
#' @param bold_labels Logical; bold variable (header) rows in the body.
#' @param bold_levels Logical; bold factor level rows in the body.
#' @param remove_N Logical; if \code{TRUE}, drops the \code{N} column for
#'   univariate package tables.
#' @param remove_N_obs Logical; if \code{TRUE}, suppresses multivariable
#'   complete-case footnote.
#' @param remove_abbreviations Logical; if \code{TRUE}, removes the
#'   Abbreviations footnote line.
#' @param caveat Optional extra footnote.
#' @return The modified table object (same class as input).
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   dplyr::mutate(
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
#'   )
#'
#' tbl <- uni_reg(
#'   data = birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "smoke", "ht"),
#'   approach = "logit"
#' )
#'
#' modify_table(
#'   tbl,
#'   variable_labels = c(age = "Maternal age", smoke = "Smoking"),
#'   level_labels = list(smoke = c(Yes = "Smoker")),
#'   header_labels = c(estimate = "Crude OR", p.value = "P"),
#'   caption = "Univariable regression for low birth weight"
#' )$table
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

  .must_be_named_character(variable_labels, "variable_labels", allow_null = TRUE)
  .must_be_named_character(header_labels, "header_labels", allow_null = TRUE)
  .must_be_flag(bold_labels, "bold_labels")
  .must_be_flag(bold_levels, "bold_levels")
  .must_be_flag(remove_N, "remove_N")
  .must_be_flag(remove_N_obs, "remove_N_obs")
  .must_be_flag(remove_abbreviations, "remove_abbreviations")
  if (!is.null(caption) && (!is.character(caption) || length(caption) != 1L || is.na(caption))) {
    stop("`caption` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.null(caveat) && (!is.character(caveat) || length(caveat) != 1L || is.na(caveat))) {
    stop("`caveat` must be NULL or a single character string.", call. = FALSE)
  }

  # --------- relabel display safely ----------
  tbl$table_display <- .relabel_display(
    tbl$table_display,
    variable_labels = variable_labels,
    level_labels    = level_labels
  )

  header_labels <- .normalize_header_labels(header_labels, tbl$table_display, tbl$approach)

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
  tbl$footnotes <- footnotes
  if (!is.null(caption)) {
    tbl$caption <- caption
  }

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

    merged_labels <- NULL
    if (inherits(tbl, "merged_table") && !is.null(tbl$column_labels)) {
      merged_labels <- c(
        Characteristic = "Characteristic",
        unlist(tbl$column_labels, use.names = TRUE)
      )
      merged_labels <- merged_labels[names(merged_labels) %in% names(out)]
    }

    lab_values <- if (!is.null(merged_labels)) merged_labels else header_labels
    if (!is.null(header_labels)) {
      lab_values[names(header_labels)] <- unname(header_labels)
    }
    lab_list <- if (!is.null(lab_values)) as.list(lab_values) else NULL
    names(lab_list) <- names(lab_values)

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
    if (inherits(tbl, "merged_table") && !is.null(tbl$spanners) &&
        !is.null(tbl$column_labels)) {
      start <- 2L
      for (i in seq_along(tbl$spanners)) {
        cols_i <- names(tbl$column_labels[[i]])
        cols_i <- cols_i[cols_i %in% names(out)]
        k <- length(cols_i)
        if (k > 0) {
          tb <- gt::tab_spanner(
            tb,
            label = tbl$spanners[i],
            columns = names(out)[start:(start + k - 1L)]
          )
          start <- start + k
        }
      }
      tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                          locations = gt::cells_column_spanners())
    }
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

    merged_labels <- NULL
    if (inherits(tbl, "merged_table") && !is.null(tbl$column_labels)) {
      merged_labels <- c(
        Characteristic = "Characteristic",
        unlist(tbl$column_labels, use.names = TRUE)
      )
      merged_labels <- merged_labels[names(merged_labels) %in% names(out)]
    }

    labels <- stats::setNames(names(out), names(out))
    if (!is.null(merged_labels)) {
      labels[names(merged_labels)] <- unname(merged_labels)
    }
    if (!is.null(header_labels)) {
      labels[names(header_labels)] <- unname(header_labels)
    }

    ft <- flextable::flextable(out)
    ft <- flextable::set_header_labels(ft, values = labels)
    if (inherits(tbl, "merged_table") && !is.null(tbl$spanners) &&
        !is.null(tbl$column_labels)) {
      widths <- c(
        1L,
        vapply(tbl$column_labels, function(x) {
          sum(names(x) %in% names(out))
        }, integer(1))
      )
      keep <- c(TRUE, widths[-1] > 0)
      ft <- flextable::add_header_row(
        ft,
        values = c("", tbl$spanners)[keep],
        colwidths = widths[keep]
      )
    }
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
