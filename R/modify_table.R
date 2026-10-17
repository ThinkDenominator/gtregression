# ---------- display-label relabelling ----------
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
  raw_exposure <- attr(display_df, "row_exposure", exact = TRUE)
  if (!is.null(raw_exposure) && length(raw_exposure) == nrow(df)) {
    var_id <- as.character(raw_exposure)
  } else {
    var_id <- character(nrow(df))
    cur <- NA_character_
    for (i in seq_len(nrow(df))) {
      if (isTRUE(df$is_header[i])) cur <- trimws(df$Characteristic[i])
      var_id[i] <- cur
    }
  }

  # variable (header) relabel
  if (!is.null(variable_labels)) {
    hdr_idx <- which(df$is_header %in% TRUE)
    if (length(hdr_idx)) {
      old_hdr <- var_id[hdr_idx]
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

  .attach_display_metadata(
    df,
    row_exposure = var_id,
    variable_labels = attr(display_df, "variable_labels", exact = TRUE)
  )
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
.normalize_header_labels <- function(header_labels, display_df, approach = NULL,
                                     visible_labels = NULL) {
  .must_be_named_character(header_labels, "header_labels", allow_null = TRUE)
  if (is.null(header_labels)) {
    return(NULL)
  }

  cols <- setdiff(names(display_df), c("Characteristic", "is_header"))
  visible <- stats::setNames(cols, cols)
  if (!is.null(visible_labels)) {
    matched <- intersect(cols, names(visible_labels))
    visible[matched] <- as.character(visible_labels[matched])
  }
  is_n <- visible == "N" | cols == "N"
  is_p <- visible == "p-value" | cols == "p-value"
  is_events <- visible == "Events"
  aliases <- list(
    estimate = cols[!is_n & !is_p & !is_events],
    p.value = cols[is_p],
    p_value = cols[is_p],
    pvalue = cols[is_p],
    N = cols[is_n]
  )

  out <- character()
  bad <- character()
  for (i in seq_along(header_labels)) {
    nm <- names(header_labels)[i]
    targets <- if (nm %in% names(display_df)) {
      nm
    } else if (nm %in% names(aliases) && length(aliases[[nm]])) {
      aliases[[nm]]
    } else if (nm %in% visible) {
      # A visible label (e.g. "OR (95% CI)" in a merged or multinomial table)
      # addresses every column that shows it.
      cols[visible == nm]
    } else {
      character()
    }
    if (!length(targets)) {
      bad <- c(bad, nm)
      next
    }
    out[targets] <- unname(header_labels[[i]])
  }

  if (length(bad)) {
    stop(
      "`header_labels` contains columns not found in the table: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

#' Drop display columns while preserving row metadata
#' @keywords internal
#' @noRd
.drop_display_columns <- function(display_df, cols) {
  cols <- intersect(cols, names(display_df))
  if (!length(cols)) {
    return(display_df)
  }

  out <- display_df[, setdiff(names(display_df), cols), drop = FALSE]
  attr(out, "row_exposure") <- attr(display_df, "row_exposure", exact = TRUE)
  attr(out, "variable_labels") <- attr(display_df, "variable_labels", exact = TRUE)
  out
}

#' Check for stratified regression output classes
#' @keywords internal
#' @noRd
.is_stratified_regression_output <- function(tbl) {
  inherits(tbl, "stratified_uni_reg") ||
    inherits(tbl, "stratified_multi_reg") ||
    inherits(tbl, "stratified_cox_reg") ||
    inherits(tbl, "stratified_surv_reg")
}

#' Pull a shared stratified effect label from header aliases
#' @keywords internal
#' @noRd
.stratified_effect_label <- function(header_labels, default) {
  if (is.null(header_labels)) {
    return(default)
  }

  allowed <- c("estimate", default)
  bad <- setdiff(names(header_labels), allowed)
  if (length(bad)) {
    stop(
      "`header_labels` contains columns not found in the stratified table: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  if ("estimate" %in% names(header_labels)) {
    return(unname(header_labels[["estimate"]]))
  }
  if (default %in% names(header_labels)) {
    return(unname(header_labels[[default]]))
  }

  default
}

#' Rebuild a modified stratified table with the original strata builders
#' @keywords internal
#' @noRd
.rebuild_modified_stratified_table <- function(tbl, display_df, footnotes, header_labels = NULL) {
  if (!.is_stratified_regression_output(tbl)) {
    return(NULL)
  }

  spanners <- tbl$levels
  theme <- .resolve_theme(if (!is.null(tbl$theme)) tbl$theme else "minimal")
  format <- .format1(tbl$format)
  if (is.na(format)) {
    format <- if (inherits(tbl$table, "gt_tbl")) "gt" else "flextable"
  }

  if (inherits(tbl, "stratified_uni_reg")) {
    effect_label <- .stratified_effect_label(header_labels, .get_effect_label(tbl$approach))
    if (identical(format, "gt")) {
      return(.build_gt_strata_wide_uni(display_df, spanners, effect_label, theme, footnotes))
    }
    return(.build_flex_strata_wide_uni(display_df, spanners, effect_label, theme, footnotes))
  }

  if (inherits(tbl, "stratified_multi_reg")) {
    effect_label <- .stratified_effect_label(
      header_labels,
      paste("Adjusted", .get_effect_label(tbl$approach))
    )
    if (identical(format, "gt")) {
      return(.build_gt_strata_wide_multi(display_df, spanners, effect_label, theme, footnotes))
    }
    return(.build_flex_strata_wide_multi(display_df, spanners, effect_label, theme, footnotes))
  }

  if (inherits(tbl, "stratified_cox_reg")) {
    effect_label <- .stratified_effect_label(
      header_labels,
      if (isTRUE(tbl$adjusted_mode) || isTRUE(tbl$multivariable)) {
      "Adjusted HR (95% CI)"
    } else {
      "HR (95% CI)"
      }
    )
    builder <- if (isTRUE(tbl$adjusted_mode) || isTRUE(tbl$multivariable)) {
      if (identical(format, "gt")) .build_gt_strata_wide_multi else .build_flex_strata_wide_multi
    } else {
      if (identical(format, "gt")) .build_gt_strata_wide_uni else .build_flex_strata_wide_uni
    }
    return(builder(display_df, spanners, effect_label, theme, footnotes))
  }

  if (inherits(tbl, "stratified_surv_reg")) {
    effect_label <- .stratified_effect_label(
      header_labels,
      if (isTRUE(tbl$adjusted_mode) || isTRUE(tbl$multivariable)) {
      "Adjusted Time Ratio (95% CI)"
    } else {
      "Time Ratio (95% CI)"
      }
    )
    builder <- if (isTRUE(tbl$adjusted_mode) || isTRUE(tbl$multivariable)) {
      if (identical(format, "gt")) .build_gt_strata_wide_multi else .build_flex_strata_wide_multi
    } else {
      if (identical(format, "gt")) .build_gt_strata_wide_uni else .build_flex_strata_wide_uni
    }
    return(builder(display_df, spanners, effect_label, theme, footnotes))
  }

  NULL
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
#' Modify table labels, headers, captions and notes
#'
#' Works with objects created by this package (class \code{"gtregression"}):
#' \code{uni_reg()}, \code{multi_reg()}, \code{cox_reg()}, \code{surv_reg()},
#' stratified regression outputs, \code{descriptive_table()}, and
#' \code{merge_tables()}.
#' No \pkg{gtsummary} dependency or fallback.
#'
#' For \code{risk_difference()} tables only the generic operations are
#' available: \code{caption}, \code{caveat}, \code{variable_labels} (outcome,
#' exposure and adjustment variables), \code{level_labels} (the two groups and
#' the event), \code{header_labels} and \code{remove_abbreviations}. The
#' other arguments are refused with an error and nothing is changed.
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
#' @param bold_labels Logical; bold variable (header) rows in the body. Defaults
#'   to \code{TRUE} to preserve the package table hierarchy.
#' @param bold_levels Logical; bold factor level rows in the body.
#' @param italic_labels Logical; italicize variable (header) rows in the body.
#' @param italic_levels Logical; italicize factor level rows in the body.
#' @param remove_N Logical; if \code{TRUE}, drops displayed \code{N} columns
#'   from univariable and stratified package tables. For stratified survival
#'   outputs, event columns are retained unless the original table was created
#'   with \code{show_sample = "none"}.
#'   Footnotes that only explain the removed column are dropped as well; the
#'   complete-observation footnote (\code{N = ... complete observations
#'   included in the model.}) is kept. Use \code{remove_N_obs} to remove it.
#' @param remove_N_obs Logical; if \code{TRUE}, suppresses the
#'   complete-observation (complete-case) footnote, in every wording used by
#'   the package tables.
#' @param remove_abbreviations Logical; if \code{TRUE}, removes the
#'   Abbreviations footnote line.
#' @param remove_adjustment_note Logical; if \code{TRUE}, removes the automatic
#'   \code{Adjusted for ...} footnote. Use \code{caveat} to add customised
#'   wording.
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
#'   approach = "logit",
#'   format = "gt"
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
                         bold_labels = TRUE,
                         bold_levels = FALSE,
                         italic_labels = FALSE,
                         italic_levels = FALSE,
                         remove_N = FALSE,
                         remove_N_obs = FALSE,
                         remove_abbreviations = FALSE,
                         remove_adjustment_note = FALSE,
                         caveat = NULL) {
  tbl <- gt_table

  # --------- accept only our objects ----------
  is_pkg_obj <- inherits(tbl, "gtregression") && !is.null(tbl$table_display)
  if (!is_pkg_obj) {
    stop("`modify_table()` expects a gtregression object from this package (with $table_display).",
         call. = FALSE)
  }
  if (inherits(tbl, "mediation_analysis")) {
    stop(
      "`modify_table()` does not support `mediation_analysis()` results. ",
      "Edit the mediation table with flextable or gt functions on `$table`.",
      call. = FALSE
    )
  }

  .must_be_named_character(variable_labels, "variable_labels", allow_null = TRUE)
  .must_be_named_character(header_labels, "header_labels", allow_null = TRUE)
  .must_be_flag(bold_labels, "bold_labels")
  .must_be_flag(bold_levels, "bold_levels")
  .must_be_flag(italic_labels, "italic_labels")
  .must_be_flag(italic_levels, "italic_levels")
  .must_be_flag(remove_N, "remove_N")
  .must_be_flag(remove_N_obs, "remove_N_obs")
  .must_be_flag(remove_abbreviations, "remove_abbreviations")
  .must_be_flag(remove_adjustment_note, "remove_adjustment_note")
  if (!is.null(caption) && (!is.character(caption) || length(caption) != 1L || is.na(caption))) {
    stop("`caption` must be NULL or a single character string.", call. = FALSE)
  }
  if (!is.null(caveat) && (!is.character(caveat) || length(caveat) != 1L || is.na(caveat))) {
    stop("`caveat` must be NULL or a single character string.", call. = FALSE)
  }

  # risk_difference() tables have their own layout: only the safe generic
  # operations are applied, by a dedicated helper.
  if (inherits(tbl, "risk_difference")) {
    unsupported <- c(
      if (!isTRUE(bold_labels)) "bold_labels",
      if (isTRUE(bold_levels)) "bold_levels",
      if (isTRUE(italic_labels)) "italic_labels",
      if (isTRUE(italic_levels)) "italic_levels",
      if (isTRUE(remove_N)) "remove_N",
      if (isTRUE(remove_N_obs)) "remove_N_obs",
      if (isTRUE(remove_adjustment_note)) "remove_adjustment_note"
    )
    return(.rd_modify_table(
      tbl,
      variable_labels = variable_labels,
      level_labels = level_labels,
      header_labels = header_labels,
      caption = caption,
      caveat = caveat,
      remove_abbreviations = remove_abbreviations,
      unsupported = unsupported
    ))
  }

  # --------- what the reader currently sees (before any change) ----------
  # Column order, visible labels, spanners and notes are read from the source
  # object so that a call without arguments leaves the content unchanged.
  header_spec <- .header_spec(tbl)
  source_footnotes <- .collect_footnotes(tbl)

  # --------- relabel display safely ----------
  tbl$table_display <- .relabel_display(
    tbl$table_display,
    variable_labels = variable_labels,
    level_labels    = level_labels
  )
  if (!is.null(variable_labels)) {
    current_labels <- tbl$variable_labels
    if (is.null(current_labels)) current_labels <- character(0)
    current_labels[names(variable_labels)] <- unname(variable_labels)
    tbl$variable_labels <- current_labels
    attr(tbl$table_display, "variable_labels") <- current_labels
  }

  # --------- optionally drop N columns ----------
  if (isTRUE(remove_N)) {
    display_names <- names(tbl$table_display)
    labelled_n <- names(header_spec$labels)[header_spec$labels == "N"]
    n_cols <- display_names[
      display_names == "N" |
        startsWith(display_names, "..N__") |
        display_names %in% labelled_n
    ]
    tbl$table_display <- .drop_display_columns(tbl$table_display, n_cols)
  }

  if (!.is_stratified_regression_output(tbl)) {
    header_labels <- .normalize_header_labels(
      header_labels, tbl$table_display, tbl$approach,
      visible_labels = header_spec$labels
    )
  }

  # --------- collect/compose footnotes ----------
  # Start from the notes of the source table, in their original order.
  footnotes <- source_footnotes
  # Some older objects attach notes to the object or rendered inner table.
  ft_attr1 <- attr(tbl, "footnotes", exact = TRUE)
  if (!is.null(ft_attr1)) footnotes <- c(footnotes, ft_attr1)
  ft_attr2 <- if (!is.null(tbl$table)) attr(tbl$table, "footnotes", exact = TRUE) else NULL
  if (!is.null(ft_attr2)) footnotes <- c(footnotes, ft_attr2)

  # Replace a note in place (keeping note order) or append it when absent.
  set_note <- function(notes, pattern, value) {
    hit <- grep(pattern, notes)
    if (!length(hit)) return(c(notes, value))
    notes[hit[1L]] <- value
    if (length(hit) > 1L) notes <- notes[-hit[-1L]]
    notes
  }

  # abbreviations line: added only when the source table has none
  if (!isTRUE(remove_abbreviations) && !is.null(tbl$approach) &&
      !any(.is_abbreviation_note(footnotes)) &&
      exists(".abbrev_note", mode = "function")) {
    footnotes <- c(footnotes, .abbrev_note(tbl$approach))
  }
  if (isTRUE(remove_abbreviations)) {
    # Ordinal, conditional-logit and multinomial tables write this line
    # without the "Abbreviations:" prefix.
    footnotes <- footnotes[!.is_abbreviation_note(footnotes)]
  }
  # multivariable N note: added only when the source table has none
  if (!isTRUE(remove_N_obs) && inherits(tbl, "multi_reg") &&
      !any(.is_n_note(footnotes))) {
    if (!is.null(tbl$models) && !is.null(tbl$models$multivariable_model)) {
      n_used <- tryCatch(stats::nobs(tbl$models$multivariable_model),
                         error = function(e) NA_integer_)
      if (!is.na(n_used)) {
        footnotes <- c(footnotes,
                       paste0("N = ", n_used,
                              " complete observations included in the model."))
      }
    }
  }
  if (isTRUE(remove_N_obs)) {
    # every wording of the complete-observation note
    footnotes <- footnotes[!.is_n_note(footnotes)]
  }
  if (isTRUE(remove_N)) {
    # notes that only explain the N column that was just removed
    footnotes <- footnotes[!.is_n_note(footnotes, column_only = TRUE)]
  }
  # default descriptive notes (if none)
  if (!length(footnotes) && inherits(tbl, "descriptive_table")) {
    footnotes <- c(
      "Categorical variables shown as n (%).",
      "Continuous variables shown as Median (IQR)."
    )
  }
  # Keep the notes that name variables aligned with the labels currently
  # visible in the table. Notes are rewritten only when labels are supplied,
  # and stay in their original position.
  adjustment_note_classes <- c(
    "multi_reg", "cox_reg", "surv_reg", "conditional_logit_reg",
    "competing_risk_reg", "ordinal_reg", "multinomial_reg", "multilevel_reg"
  )
  relabelled <- !is.null(variable_labels)
  if (inherits(tbl, adjustment_note_classes) &&
      !is.null(tbl$adjust_for) &&
      length(tbl$adjust_for) && exists(".display_adjustment_note", mode = "function") &&
      (relabelled || !any(grepl("^Adjusted for ", footnotes)))) {
    footnotes <- set_note(
      footnotes, "^Adjusted for ",
      .display_adjustment_note(tbl$adjust_for, tbl$variable_labels)
    )
  }
  if (inherits(tbl, "conditional_logit_reg") &&
      !is.null(tbl$matched_set) && length(tbl$matched_set) == 1L &&
      (relabelled || !any(grepl("^Matched by set identifier:", footnotes)))) {
    matched_label <- .label_var(tbl$matched_set, tbl$variable_labels)
    footnotes <- set_note(
      footnotes, "^Matched by set identifier:",
      paste0("Matched by set identifier: ", matched_label, ".")
    )
  }
  if (inherits(tbl, "multilevel_reg") &&
      !is.null(tbl$group) && length(tbl$group) == 1L &&
      (relabelled || !any(grepl("^Random-intercept model clustered by ", footnotes)))) {
    group_label <- .label_var(tbl$group, tbl$variable_labels)
    footnotes <- set_note(
      footnotes, "^Random-intercept model clustered by ",
      paste0("Random-intercept model clustered by ", group_label, ".")
    )
  }
  if (isTRUE(remove_adjustment_note)) {
    footnotes <- footnotes[!grepl("^Adjusted for ", footnotes)]
  }
  # user caveat last
  if (!is.null(caveat) && nzchar(caveat)) footnotes <- c(footnotes, caveat)
  footnotes <- unique(footnotes[nzchar(footnotes)])
  tbl$footnotes <- footnotes
  if (!is.null(caption)) {
    tbl$caption <- caption
  }

  # --------- visible columns, labels and spanners ----------
  display_cols <- setdiff(names(tbl$table_display), c("Characteristic", "is_header"))
  cols <- header_spec$cols[header_spec$cols %in% display_cols]
  labels <- header_spec$labels[cols]
  if (!is.null(header_labels)) {
    renamed <- intersect(names(header_labels), cols)
    labels[renamed] <- unname(header_labels[renamed])
  }
  levels <- lapply(header_spec$levels, function(level) {
    groups <- lapply(level$cols, function(x) x[x %in% cols])
    keep <- lengths(groups) > 0L
    list(label = level$label[keep], cols = groups[keep])
  })
  levels <- levels[vapply(levels, function(level) length(level$label) > 0L, logical(1))]

  theme <- .resolve_theme(if (!is.null(tbl$theme)) tbl$theme else "minimal")
  # Single regression tables use the same flextable theme function as their
  # source builder; descriptive, multinomial and merged tables keep the
  # lighter styling of theirs.
  single_regression <- !length(levels) &&
    !inherits(tbl, c("descriptive_table", "merged_table", "multinomial_reg"))

  if (inherits(tbl, c("merged_table", "multinomial_reg"))) {
    if (!is.null(tbl$column_labels)) {
      tbl$column_labels <- lapply(tbl$column_labels, function(x) {
        x <- x[names(x) %in% cols]
        x[] <- unname(labels[names(x)])
        x
      })
    }
  } else if (!is.null(tbl$header_labels)) {
    kept <- intersect(names(tbl$header_labels), cols)
    tbl$header_labels <- tbl$header_labels[kept]
    tbl$header_labels[kept] <- unname(labels[kept])
  }

  apply_body_text_style <- function(rendered, display_df) {
    header_rows <- which(display_df$is_header %in% TRUE)
    level_rows <- which(display_df$is_header %in% FALSE)

    if (inherits(rendered, "gt_tbl")) {
      if (length(header_rows)) {
        rendered <- gt::tab_style(
          rendered,
          style = gt::cell_text(
            weight = if (isTRUE(bold_labels)) "bold" else "normal",
            style = if (isTRUE(italic_labels)) "italic" else "normal"
          ),
          locations = gt::cells_body(rows = header_rows, columns = "Characteristic")
        )
      }
      if (length(level_rows)) {
        rendered <- gt::tab_style(
          rendered,
          style = gt::cell_text(
            weight = if (isTRUE(bold_levels)) "bold" else "normal",
            style = if (isTRUE(italic_levels)) "italic" else "normal"
          ),
          locations = gt::cells_body(rows = level_rows, columns = "Characteristic")
        )
      }
      return(rendered)
    }

    if (inherits(rendered, "flextable")) {
      if (length(header_rows)) {
        rendered <- flextable::bold(
          rendered, i = header_rows, j = "Characteristic",
          bold = isTRUE(bold_labels), part = "body"
        )
        rendered <- flextable::italic(
          rendered, i = header_rows, j = "Characteristic",
          italic = isTRUE(italic_labels), part = "body"
        )
      }
      if (length(level_rows)) {
        rendered <- flextable::bold(
          rendered, i = level_rows, j = "Characteristic",
          bold = isTRUE(bold_levels), part = "body"
        )
        rendered <- flextable::italic(
          rendered, i = level_rows, j = "Characteristic",
          italic = isTRUE(italic_levels), part = "body"
        )
      }
    }

    rendered
  }

  # --------- rebuild rendered table ----------
  format <- .format1(tbl$format)
  strata_table <- .rebuild_modified_stratified_table(tbl, tbl$table_display, footnotes, header_labels)
  if (!is.null(strata_table)) {
    if (!is.null(caption)) {
      strata_table <- if (inherits(strata_table, "gt_tbl")) {
        gt::tab_caption(strata_table, caption = caption)
      } else {
        flextable::set_caption(strata_table, caption = caption)
      }
    }
    tbl$table <- strata_table
  } else if (inherits(tbl$table, "gt_tbl") ||
             (!inherits(tbl$table, "flextable") && identical(format, "gt"))) {
    tbl$table <- .render_gt_table(
      tbl$table_display, cols, labels, levels, theme, footnotes,
      caption = caption
    )
  } else if (inherits(tbl$table, "flextable") || identical(format, "flextable")) {
    tbl$table <- .render_flex_table(
      tbl$table_display, cols, labels, levels, theme, footnotes,
      caption = caption, vanilla = single_regression,
      body_top_rule = !inherits(tbl, "descriptive_table")
    )
  }

  tbl$table <- apply_body_text_style(tbl$table, tbl$table_display)

  tbl
}
