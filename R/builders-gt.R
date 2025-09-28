#' @keywords internal
#' @noRd
.apply_theme_gt <- function(tb, df, theme) {
  if ("header_shaded" %in% theme) {
    tb <- gt::tab_options(tb, column_labels.background.color = "#f6f8fa")
  }
  if ("zebra" %in% theme) tb <- gt::opt_row_striping(tb)
  if ("lines" %in% theme) {
    tb <- gt::tab_style(
      tb,
      style = list(gt::cell_borders(sides = "top", color = "#DADADA", weight = gt::px(1))),
      locations = gt::cells_body(rows = TRUE)
    )
    tb <- gt::tab_options(tb,
                          table.border.top.style = "solid",   table.border.top.color = "#DADADA",
                          table.border.bottom.style = "solid",table.border.bottom.color = "#DADADA")
  }
  # bold exposure labels by default
  hdr_idx <- which(df$is_header %in% TRUE)
  if (length(hdr_idx)) {
    tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                        gt::cells_body(rows = hdr_idx, columns = "Characteristic"))
  }
  if ("compact" %in% theme) tb <- gt::tab_options(tb, data_row.padding = gt::px(2))
  tb
}
#' @keywords internal
#' @noRd
.build_gt <- function(df, effect_label, source_note, theme) {
  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install 'gt' or use format='flextable'.", call. = FALSE)
  .must_be_display_df(df)

  df$N <- ifelse(df$is_header, df$N, NA_integer_)
  out <- df[, c("Characteristic", "N", effect_label, "p-value"), drop = FALSE]

  tb <- gt::gt(out) |>
    gt::cols_align("left",   columns = "Characteristic") |>
    gt::cols_align("center", columns = c("N", effect_label, "p-value")) |>
    gt::sub_missing(columns = "N", missing_text = "") |>
    gt::tab_options(
      table.font.names         = "system-ui",
      data_row.padding         = gt::px(4),
      heading.background.color = "white",
      table.background.color   = "white"
    )

  # indent levels
  tb <- gt::tab_style(tb, gt::cell_text(indent = gt::px(12)),
                      gt::cells_body(rows = df$is_header %in% FALSE, columns = "Characteristic"))
  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      locations = gt::cells_column_labels())
  # try to bold spanners if present
  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_spanners())

  tb <- .apply_theme_gt(tb, df, theme)
  tb <- gt::tab_source_note(tb, source_note = source_note)
  tb
}
#' @keywords internal
#' @noRd
.build_gt_multi <- function(df, effect_label, footnotes, theme) {
  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install 'gt' or use format='flextable'.", call. = FALSE)

  out <- df[, c("Characteristic", effect_label, "p-value"), drop = FALSE]

  tb <- gt::gt(out) |>
    gt::cols_align("left",   columns = "Characteristic") |>
    gt::cols_align("center", columns = c(effect_label, "p-value")) |>
    gt::tab_options(
      table.font.names         = "system-ui",
      data_row.padding         = gt::px(4),
      heading.background.color = "white",
      table.background.color   = "white"
    )

  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      gt::cells_body(rows = df$is_header %in% TRUE, columns = "Characteristic"))
  tb <- gt::tab_style(tb, gt::cell_text(indent = gt::px(12)),
                      gt::cells_body(rows = df$is_header %in% FALSE, columns = "Characteristic"))
  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      locations = gt::cells_column_labels())
  # try to bold spanners if present
  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      locations = gt::cells_column_spanners())

  tb <- .apply_theme_gt(tb, df, theme)

  # two separate lines in the footer
  tb <- gt::tab_source_note(tb, source_note = footnotes)
  tb
}
#' GT builder for stratified wide tables (3 cols per stratum)
#' @keywords internal
.build_gt_strata_wide_uni <- function(df, spanners, effect_label, theme, footnotes) {
  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install 'gt' or use format='flextable'.", call. = FALSE)

  block_ids <- unlist(lapply(seq_along(spanners), function(i) {
    nm <- sub("^.*=\\s*", "", spanners[i])
    c(paste0("..N__", nm), paste0("..eff__", nm), paste0("..p__", nm))
  }))
  out <- df[, c("Characteristic", block_ids), drop = FALSE]

  sub_header <- c("Characteristic",
                  unlist(rep(list(c("N", effect_label, "p-value")),
                             length(spanners))))

  tb <- gt::gt(out) |>
    gt::cols_label(.list = setNames(sub_header, names(out))) |>
    gt::cols_align("left",   columns = "Characteristic") |>
    gt::cols_align("center", columns = setdiff(names(out), "Characteristic")) |>
    gt::tab_options(
      table.font.names         = "system-ui",
      data_row.padding         = gt::px(4),
      heading.background.color = "white",
      table.background.color   = "white"
    )

  # bold column labels
  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      locations = gt::cells_column_labels())

  # add spanners (3 columns each)
  start <- 2L
  for (i in seq_along(spanners)) {
    cols <- names(out)[start:(start+2)]
    tb <- gt::tab_spanner(tb, label = spanners[i], columns = cols)
    start <- start + 3L
  }
  # bold spanners after creating them
  tb <- tryCatch(
    gt::tab_style(tb, gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_spanners(spanners = spanners)),
    error = function(e) gt::tab_style(tb, gt::cell_text(weight = "bold"),
                                      locations = gt::cells_column_spanners())
  )

  # exposure headers bold; levels indented
  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      gt::cells_body(rows = df$is_header %in% TRUE, columns = "Characteristic"))
  tb <- gt::tab_style(tb, gt::cell_text(indent = gt::px(12)),
                      gt::cells_body(rows = df$is_header %in% FALSE, columns = "Characteristic"))

  # themes
  if ("header_shaded" %in% theme) tb <- gt::tab_options(tb, column_labels.background.color = "#f6f8fa")
  if ("zebra" %in% theme) tb <- gt::opt_row_striping(tb)
  if ("lines" %in% theme) {
    tb <- gt::tab_style(tb, gt::cell_borders(sides = "top", color = "#DADADA", weight = gt::px(1)),
                        gt::cells_body(rows = TRUE))
    tb <- gt::tab_options(tb,
                          table.border.top.style = "solid",   table.border.top.color = "#DADADA",
                          table.border.bottom.style = "solid",table.border.bottom.color = "#DADADA")
  }
  if ("compact" %in% theme) tb <- gt::tab_options(tb, data_row.padding = gt::px(2))

  tb <- gt::tab_source_note(tb, source_note = footnotes)
  tb
}
#' @keywords internal
.build_gt_strata_wide_multi <- function(df, spanners, effect_label_adj, theme, footnotes) {
  if (!requireNamespace("gt", quietly = TRUE))
    stop("Install 'gt' or use format='flextable'.", call. = FALSE)

  block_ids <- unlist(lapply(spanners, function(s) {
    nm <- sub("^.*=\\s*", "", s)
    c(paste0("..eff__", nm), paste0("..p__", nm))
  }))
  out <- df[, c("Characteristic", block_ids), drop = FALSE]

  sub_header <- c("Characteristic",
                  unlist(rep(list(c(effect_label_adj, "p-value")), length(spanners))))

  tb <- gt::gt(out) |>
    gt::cols_label(.list = setNames(sub_header, names(out))) |>
    gt::cols_align("left",   columns = "Characteristic") |>
    gt::cols_align("center", columns = setdiff(names(out), "Characteristic")) |>
    gt::tab_options(
      table.font.names         = "system-ui",
      data_row.padding         = gt::px(4),
      heading.background.color = "white",
      table.background.color   = "white"
    )

  tb <- gt::tab_style(tb, gt::cell_text(weight = "bold"),
                      locations = gt::cells_column_labels())

  start <- 2L
  for (i in seq_along(spanners)) {
    cols <- names(out)[start:(start+1)]
    tb <- gt::tab_spanner(tb, label = spanners[i], columns = cols)
    start <- start + 2L
  }
  tb <- tryCatch(
    gt::tab_style(tb, gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_spanners(spanners = spanners)),
    error = function(e) gt::tab_style(tb, gt::cell_text(weight = "bold"),
                                      locations = gt::cells_column_spanners())
  )

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
                          table.border.top.style = "solid",   table.border.top.color = "#DADADA",
                          table.border.bottom.style = "solid",table.border.bottom.color = "#DADADA")
  }
  if ("compact" %in% theme) tb <- gt::tab_options(tb, data_row.padding = gt::px(2))

  gt::tab_source_note(tb, source_note = footnotes)
}
