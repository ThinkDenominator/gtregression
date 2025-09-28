#' @keywords internal
#' @noRd
.apply_theme_flex <- function(ft, df, theme) {
  ft <- flextable::theme_vanilla(ft)
  hdr_idx <- which(df$is_header %in% TRUE)
  lvl_idx <- which(df$is_header %in% FALSE)

  # indent level rows (flextable ignores leading spaces)
  if (length(lvl_idx)) ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic", padding.left = 14)

  if ("header_shaded" %in% theme) {
    ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  }
  if ("zebra" %in% theme && length(lvl_idx)) {
    ft <- flextable::bg(ft, i = lvl_idx[seq(1, length(lvl_idx), by = 2)], bg = "#f6f8fa")
  }
  # lines — use hline
  if ("lines" %in% theme) {
    n_body <- nrow(ft$body$dataset)
    if (!is.null(n_body) && n_body > 0) {
      ft <- flextable::hline(ft, i = 1, part = "body")
      ft <- flextable::hline(ft, i = n_body, part = "body")
    }
  }
  # bold exposure labels by default
  if (length(hdr_idx)) ft <- flextable::bold(ft, i = hdr_idx, j = "Characteristic", bold = TRUE)

  if ("compact" %in% theme) ft <- flextable::padding(ft, padding = 2)
  ft
}

#' @keywords internal
#' @noRd

.build_flextable <- function(df, effect_label, source_note, theme) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Install 'flextable' or use format='gt'.", call. = FALSE)
  .must_be_display_df(df)

  df$N <- ifelse(df$is_header, df$N, NA_integer_)
  out <- df[, c("Characteristic", "N", effect_label, "p-value"), drop = FALSE]
  out$N <- ifelse(is.na(out$N), "", as.character(out$N))

  ft <- flextable::flextable(out)
  ft <- flextable::align(ft, j = "Characteristic", align = "left")
  ft <- flextable::align(ft, j = c("N", effect_label, "p-value"), align = "center")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)

  ft <- .apply_theme_flex(ft, df, theme)
  ft <- flextable::autofit(ft)

  ft <- flextable::add_footer_lines(ft, values = source_note)
  ft <- flextable::align(ft, part = "footer", align = "left")
  ft
}
#' @keywords internal
#' @noRd

.build_flextable_multi <- function(df, effect_label, footnotes, theme) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Install 'flextable' or use format='gt'.", call. = FALSE)

  out <- df[, c("Characteristic", effect_label, "p-value"), drop = FALSE]
  ft <- flextable::flextable(out)
  ft <- flextable::align(ft, j = "Characteristic", align = "left")
  ft <- flextable::align(ft, j = c(effect_label, "p-value"), align = "center")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)

  hdr_idx <- which(df$is_header %in% TRUE)
  lvl_idx <- which(df$is_header %in% FALSE)
  if (length(lvl_idx)) ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic", padding.left = 14)
  ft <- .apply_theme_flex(ft, df, theme)

  ft <- flextable::autofit(ft)

  # two separate lines in the footer
  ft <- flextable::add_footer_lines(ft, values = footnotes)
  ft <- flextable::align(ft, part = "footer", align = "left")
  ft
}
#' Flextable builder for stratified wide tables (3 cols per stratum)
#' @keywords internal
.build_flex_strata_wide_uni <- function(df, spanners, effect_label, theme, footnotes) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Install 'flextable' or use format='gt'.", call. = FALSE)

  block_ids <- unlist(lapply(seq_along(spanners), function(i) {
    nm <- sub("^.*=\\s*", "", spanners[i])
    c(paste0("..N__", nm), paste0("..eff__", nm), paste0("..p__", nm))
  }))
  out <- df[, c("Characteristic", block_ids), drop = FALSE]

  sub_header <- c("Characteristic",
                  unlist(rep(list(c("N", effect_label, "p-value")),
                             length(spanners))))

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, values = setNames(sub_header, names(out)))
  ft <- flextable::add_header_row(ft, values = c("", spanners), colwidths = c(1, rep(3, length(spanners))))
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::align(ft, j = "Characteristic", align = "left",  part = "all")
  ft <- flextable::align(ft, j = setdiff(names(out), "Characteristic"), align = "center", part = "all")

  # body styling (NO row-index offset)
  lvl_idx <- which(df$is_header %in% FALSE)
  hdr_idx <- which(df$is_header %in% TRUE)
  if (length(lvl_idx)) ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic",
                                                padding.left = 14, part = "body")
  if (length(hdr_idx)) ft <- flextable::bold(ft, i = hdr_idx, j = "Characteristic",
                                             bold = TRUE, part = "body")

  # themes
  if ("header_shaded" %in% theme) ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  if ("zebra" %in% theme && length(lvl_idx)) {
    ft <- flextable::bg(ft, i = lvl_idx[seq(1, length(lvl_idx), by = 2)], bg = "#f6f8fa", part = "body")
  }
  if ("lines" %in% theme) {
    n_body <- nrow(df)
    if (!is.null(n_body) && n_body > 0) {
      ft <- flextable::hline(ft, i = 1,      part = "body")
      ft <- flextable::hline(ft, i = n_body, part = "body")
    }
  }
  if ("compact" %in% theme) ft <- flextable::padding(ft, padding = 2, part = "body")

  ft <- flextable::autofit(ft)
  ft <- flextable::add_footer_lines(ft, values = c(.abbrev_note)) # caller passes a vector; see orchestrator
  ft <- flextable::align(ft, part = "footer", align = "left")
  ft
}

#' Flextable builder for stratified MULTIVARIABLE wide tables (2 cols/stratum)
#' @keywords internal
.build_flex_strata_wide_multi <- function(df, spanners, effect_label_adj, theme, footnotes) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Install 'flextable' or use format='gt'.", call. = FALSE)

  block_ids <- unlist(lapply(spanners, function(s) {
    nm <- sub("^.*=\\s*", "", s)
    c(paste0("..eff__", nm), paste0("..p__", nm))
  }))
  out <- df[, c("Characteristic", block_ids), drop = FALSE]

  sub_header <- c("Characteristic",
                  unlist(rep(list(c(effect_label_adj, "p-value")), length(spanners))))

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, values = setNames(sub_header, names(out)))
  ft <- flextable::add_header_row(ft, values = c("", spanners), colwidths = c(1, rep(2, length(spanners))))
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::align(ft, j = "Characteristic", align = "left",  part = "all")
  ft <- flextable::align(ft, j = setdiff(names(out), "Characteristic"), align = "center", part = "all")

  lvl_idx <- which(df$is_header %in% FALSE)
  hdr_idx <- which(df$is_header %in% TRUE)
  if (length(lvl_idx)) ft <- flextable::padding(ft, i = lvl_idx, j = "Characteristic", padding.left = 14, part = "body")
  if (length(hdr_idx)) ft <- flextable::bold(ft, i = hdr_idx, j = "Characteristic", bold = TRUE, part = "body")

  if ("header_shaded" %in% theme) ft <- flextable::bg(ft, part = "header", bg = "#f6f8fa")
  if ("zebra" %in% theme && length(lvl_idx)) {
    ft <- flextable::bg(ft, i = lvl_idx[seq(1, length(lvl_idx), by = 2)], bg = "#f6f8fa", part = "body")
  }
  if ("lines" %in% theme) {
    n_body <- nrow(df)
    if (!is.null(n_body) && n_body > 0) {
      ft <- flextable::hline(ft, i = 1,      part = "body")
      ft <- flextable::hline(ft, i = n_body, part = "body")
    }
  }
  if ("compact" %in% theme) ft <- flextable::padding(ft, padding = 2, part = "body")

  ft <- flextable::autofit(ft)
  ft <- flextable::add_footer_lines(ft, values = footnotes)
  ft <- flextable::align(ft, part = "footer", align = "left")
  ft
}
