#' Draw a Forest Plot (Publication-Ready)
#'
#' Wrapper around `forestploter::forest()` that works directly with
#' `forest_df()` output or with raw regression objects.
#'
#' @param df Output of `forest_df()`. If `NULL`, will be built from (uni, multi, desc).
#' @param uni,multi,desc Optional gtregression objects to pass through to `forest_df()`.
#' @param theme Optional `forestploter::forest_theme()`. If `NULL`, a sensible default is used.
#'   You may pass colors and styling either here (e.g., `ci_col`, `refline_gp`) or via `...`.
#' @param ci_col_width Numeric or length-2 numeric. Relative width of the CI column(s).
#'   A vector like `c(0.22, 0.26)` lets you tune uni vs adjusted columns separately.
#' @param side Character. For each effect, position of the plot relative to the effect-size text:
#'   `"left"` = plot first then text; `"right"` = text first then plot.
#'   **Note:** The `Characteristic` column (and any descriptive/summary columns) always remains on the left.
#' @param bold_headers Logical. Bold the exposure headers (non-indented rows) in the first column. Default `TRUE`.
#' @param quiet Logical. Suppress forestploter warnings. Default = `TRUE`.
#' @param ... Passed to `forestploter::forest()`. Common options include:
#'   `ci_col`, `point_col`, `point_shape`, `rowheight`, `ticks_at`, `title`, `footnote`.
#'
#' @return A `gtregression_forest` object with elements:
#'   - `plot`: the forest plot
#'   - `data`: the input data frame (post-processed order, no `se_*` columns)
#'   - `meta`: model metadata
#' @export
forest_reg <- function(df = NULL, uni = NULL, multi = NULL, desc = NULL,
                       theme = NULL,
                       ci_col_width = 0.25,
                       side = c("right", "left"),
                       quiet = TRUE,
                       ...) {

  side <- match.arg(side)

  # build df if needed
  if (is.null(df)) {
    stopifnot(inherits(uni, "gtregression"))
    df <- forest_df(uni = uni, multi = multi, desc = desc)
  }

  # detect meta
  meta <- attr(df, "forest_meta")
  if (is.null(meta)) {
    meta <- list(x_trans = "none", ref_line = 0)
  }

  # default theme
  if (is.null(theme)) {
    theme <- forestploter::forest_theme(
      base_size   = 11,
      refline_gp  = grid::gpar(lty = 2, col = "grey60"),
      ci_lwd      = 2,
      ci_col      = "black",
      header_fill = "#f6f8fa"
    )
  }

  # choose columns
  has_multi <- !is.null(attr(df, "est2"))
  if (has_multi) {
    est <- list(attr(df, "est"),  attr(df, "est2"))
    lo  <- list(attr(df, "lo"),   attr(df, "lo2"))
    hi  <- list(attr(df, "hi"),   attr(df, "hi2"))
    se  <- list(df$se_uni, df$se_adj)
    ci_col <- c(which(names(df) == " "), which(names(df) == "  "))
  } else {
    est <- attr(df, "est")
    lo  <- attr(df, "lo")
    hi  <- attr(df, "hi")
    se  <- df$se_uni
    ci_col <- which(names(df) == " ")
  }



  # --- per-plot xlim (one per CI column) ---
  if (has_multi) {
    # plot 1 (univariate)
    lo1 <- lo[[1]]; hi1 <- hi[[1]]
    f1  <- is.finite(lo1) & is.finite(hi1)
    r1  <- range(c(lo1[f1], hi1[f1], meta$ref_line), na.rm = TRUE)
    if (!all(is.finite(r1))) r1 <- if (meta$x_trans %in% c("log","log2","log10")) c(0.5, 2) else c(-1, 1)
    d1  <- diff(r1); if (!is.finite(d1) || d1 == 0) d1 <- 0.1
    xlim1 <- c(r1[1] - d1 * 0.10, r1[2] + d1 * 0.10)
    if (meta$x_trans %in% c("log","log2","log10")) xlim1[1] <- max(xlim1[1], 1e-6)

    # plot 2 (adjusted)
    lo2 <- lo[[2]]; hi2 <- hi[[2]]
    f2  <- is.finite(lo2) & is.finite(hi2)
    r2  <- range(c(lo2[f2], hi2[f2], meta$ref_line), na.rm = TRUE)
    if (!all(is.finite(r2))) r2 <- if (meta$x_trans %in% c("log","log2","log10")) c(0.5, 2) else c(-1, 1)
    d2  <- diff(r2); if (!is.finite(d2) || d2 == 0) d2 <- 0.1
    xlim2 <- c(r2[1] - d2 * 0.10, r2[2] + d2 * 0.10)
    if (meta$x_trans %in% c("log","log2","log10")) xlim2[1] <- max(xlim2[1], 1e-6)

    # pass a list of xlims (one per plot)
    xlim <- list(xlim1, xlim2)

  } else {
    # single plot
    f   <- is.finite(lo) & is.finite(hi)
    r   <- range(c(lo[f], hi[f], meta$ref_line), na.rm = TRUE)
    if (!all(is.finite(r))) r <- if (meta$x_trans %in% c("log","log2","log10")) c(0.5, 2) else c(-1, 1)
    d   <- diff(r); if (!is.finite(d) || d == 0) d <- 0.1
    xlim <- c(r[1] - d * 0.10, r[2] + d * 0.10)
    if (meta$x_trans %in% c("log","log2","log10")) xlim[1] <- max(xlim[1], 1e-6)
  }


  # weights based on SE (smaller SE → bigger boxes)
  weights <- if (is.list(se)) {
    lapply(se, function(s) {
      ifelse(is.finite(s) & s > 0,
             scales::rescale(1/s, to = c(0.6, 1.6), na.rm = TRUE),
             NA_real_)
    })
  } else {
    ifelse(is.finite(se) & se > 0,
           scales::rescale(1/se, to = c(0.6, 1.6), na.rm = TRUE),
           NA_real_)
  }
  df_plot <- df[, setdiff(names(df), c("se_uni","se_adj")), drop = FALSE]

  # --- expand effect labels by renaming columns (no helpers) ---
  nm    <- names(df_plot)
  oldnm <- nm  # keep a copy for matching

  short <- c("OR (95% CI)", "RR (95% CI)", "IRR (95% CI)", "Beta (95% CI)")
  long  <- c("Odds Ratio (95% CI)",
             "Risk Ratio (95% CI)",
             "Incidence Rate Ratio (95% CI)",
             "Linear Regression Coefficient (95% CI)")

  # exact matches (unadjusted)
  idx <- match(oldnm, short)
  nm[!is.na(idx)] <- long[idx[!is.na(idx)]]

  # exact matches with "Adjusted " prefix
  idx2 <- match(oldnm, paste0("Adjusted ", short))
  nm[!is.na(idx2)] <- paste0("Adjusted ", long[idx2[!is.na(idx2)]])

  names(df_plot) <- nm
  # --- position the plot columns per `side` and recompute ci_col ---
  # --- enforce layout rules: Characteristics/desc on the left; plot/text order by `side` ---

  nm <- names(df_plot)

  # 1) Identify CI text columns and anchor (plot) columns
  ci_cols_all <- grep("\\(95% CI\\)$", nm, value = TRUE)        # e.g., "Odds Ratio (95% CI)", "Adjusted Odds Ratio (95% CI)"
  anchor_cols <- intersect(c(" ", "  "), nm)                    # " " = uni plot, "  " = adj plot if present

  # Split CI text into unadjusted vs adjusted (if present)
  ci_uni <- setdiff(ci_cols_all, ci_cols_all[grepl("^Adjusted\\s+", ci_cols_all)])
  ci_uni <- ci_uni[1] %||% NA_character_

  ci_adj <- ci_cols_all[grepl("^Adjusted\\s+", ci_cols_all)]
  ci_adj <- ci_adj[1] %||% NA_character_

  # 2) Characteristic + desc columns = everything that's not an anchor or CI text
  left_cols <- setdiff(nm, c(anchor_cols, ci_cols_all))   # preserve original order

  # 3) Build the effect-area blocks based on `side`
  blocks <- character(0)

  # Univariate block (if columns exist)
  if (!is.na(ci_uni) && " " %in% anchor_cols) {
    blocks <- c(blocks, if (side == "left") c(" ", ci_uni) else c(ci_uni, " "))
  }

  # Adjusted block (if columns exist)
  if (!is.na(ci_adj) && "  " %in% anchor_cols) {
    blocks <- c(blocks, if (side == "left") c("  ", ci_adj) else c(ci_adj, "  "))
  }

  # 4) Reorder df_plot: left (Characteristic/desc) | effect blocks
  df_plot <- df_plot[, c(left_cols, blocks), drop = FALSE]

  # 5) Recompute ci_column indices (must be the plot-anchor columns in the same order as in `blocks`)
  anchor_in_blocks <- blocks[blocks %in% c(" ", "  ")]
  ci_col <- match(anchor_in_blocks, names(df_plot))


  draw <- function() forestploter::forest(
    data       = df_plot,
    est        = est,
    lower      = lo,
    upper      = hi,
    sizes      = weights,
    ci_column  = ci_col,
    x_trans    = meta$x_trans,
    ref_line   = meta$ref_line,
    xlim       = xlim,
    ci_col_width = ci_col_width,
    theme      = theme,
    side       = side,
    effects    = effects,
    ...
  )

  plt <- if (quiet) suppressWarnings(draw()) else draw()

  structure(list(plot = plt, data = df, meta = meta),
            class = c("gtregression_forest", "list"))
}

#' @export
print.gtregression_forest <- function(x, ..., autofit = TRUE) {
  print(x$plot, autofit = autofit)
  invisible(x)
}
