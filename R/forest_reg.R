#' Draw a forest table from regression outputs
#'
#' Wrapper around \code{forestploter::forest()} that works directly with
#' \code{forest_df()} output or with \code{gtregression} regression objects.
#' It can show descriptive columns and one or more model effect columns in a
#' table-style forest plot while leaving axis and interval drawing to
#' \pkg{forestploter}.
#'
#' @param df Output of \code{forest_df()}. If \code{NULL}, it is built from
#'   \code{uni}, \code{multi}, and \code{desc}.
#' @param uni,multi,desc Optional \code{gtregression} objects to pass through to
#'   \code{forest_df()}.
#' @param theme Optional \code{forestploter::forest_theme()}. If \code{NULL},
#'   \code{forestploter} defaults are used. You may pass colors and styling
#'   either here or through \code{...}.
#' @param ci_col_width Numeric value, or one value per effect column,
#'   controlling the blank spacer width used by \code{forestploter} for the
#'   confidence-interval plot column(s). Values greater than 1 are interpreted
#'   as approximate character counts. Values between 0 and 1 are accepted for
#'   backward compatibility and converted to character counts.
#' @param side Character. For each effect, position of the plot relative to the effect-size text:
#'   \code{"left"} = plot first then text; \code{"right"} = text first then plot.
#'   The \code{Characteristic} column and descriptive columns remain on the left.
#' @param quiet Logical. Suppress forestploter warnings. Default = `TRUE`.
#' @param effects Optional effect labels passed to \code{forestploter::forest()}.
#' @param ticks_at Optional numeric vector, or list of numeric vectors for
#'   multiple effect
#'   columns, specifying x-axis tick positions. If \code{NULL},
#'   \code{forestploter::forest()} chooses the default ticks.
#' @param ticks_digits Optional number of digits for x-axis tick labels.
#' @param xlim Optional numeric vector of length 2, or list of length-2 numeric
#'   vectors for multiple
#'   effect columns, specifying x-axis limits. If \code{NULL},
#'   \code{forestploter::forest()} chooses the default limits.
#' @param style_strata Logical. Retained for older vertically grouped
#'   stratified forest data. Current \code{forest_df()} stratified output places
#'   strata side by side, so no stratum header rows are styled.
#' @param strata_fill Character. Fill color used for older styled stratum header
#'   rows.
#' @param ... Passed to \code{forestploter::forest()}. Common options include
#'   \code{title} and \code{footnote}.
#'
#' @return A \code{gtregression_forest} object with elements:
#' \describe{
#'   \item{\code{plot}}{The forest plot object.}
#'   \item{\code{data}}{The plotting data sent to \code{forestploter::forest()}.}
#'   \item{\code{input_data}}{The original \code{forest_df()} data, including
#'   standard-error helper columns.}
#'   \item{\code{meta}}{Model metadata, including reference line and x-axis
#'   transformation.}
#' }
#' @importFrom forestploter forest
#' @examples
#' birthwt_data <- data_birthwt |>
#'   transform(
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1),
#'                  labels = c("Normal BW", "Low BW"))
#'   )
#'
#' uni_or <- uni_reg(
#'   birthwt_data,
#'   outcome = "low",
#'   exposures = c("age", "lwt", "smoke", "ht"),
#'   approach = "logit"
#' )
#' multi_or <- multi_reg(
#'   birthwt_data,
#'   outcome = "low",
#'   exposures = c("smoke", "ht"),
#'   adjust_for = c("age", "lwt"),
#'   approach = "logit"
#' )
#'
#' forest_reg(uni = uni_or, multi = multi_or)
#'
#' # If axis labels overlap, set x-axis limits and tick marks.
#' forest_reg(
#'   uni = uni_or,
#'   multi = multi_or,
#'   xlim = list(c(0.25, 8), c(0.25, 12)),
#'   ticks_at = list(c(0.5, 1, 2, 4, 8), c(0.5, 1, 2, 4, 8))
#' )
#'
#' # If the forest plot panel is too narrow or too wide, tune ci_col_width.
#' forest_reg(
#'   uni = uni_or,
#'   multi = multi_or,
#'   ci_col_width = c(18, 22)
#' )

#' @export
forest_reg <- function(df = NULL, uni = NULL, multi = NULL, desc = NULL,
                       theme = NULL,
                       ci_col_width = 20,
                       side = c("right", "left"),
                       quiet = TRUE,
                       effects = NULL,
                       ticks_at = NULL,
                       ticks_digits = NULL,
                       xlim = NULL,
                       style_strata = TRUE,
                       strata_fill = "#EAF2F1",
                       ...) {

  side <- .choice_arg(substitute(side), env = parent.frame(), choices = c("right", "left"))
  side <- match.arg(side)
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(effects) && !is.character(effects)) {
    stop("`effects` must be NULL or a character vector.", call. = FALSE)
  }
  if (!is.numeric(ci_col_width) || anyNA(ci_col_width) ||
      !length(ci_col_width) || any(ci_col_width <= 0)) {
    stop("`ci_col_width` must be a positive numeric value or numeric vector.",
         call. = FALSE)
  }
  ci_col_width <- ifelse(ci_col_width <= 1, ci_col_width * 80, ci_col_width)
  ci_col_width <- pmax(4L, as.integer(round(ci_col_width)))
  validate_ticks <- function(x, name) {
    if (is.null(x)) return(invisible(NULL))
    ok <- is.numeric(x) && length(x) > 0L && !anyNA(x)
    if (!ok) {
      stop("`", name, "` must be NULL or a numeric vector.", call. = FALSE)
    }
    invisible(NULL)
  }
  if (is.list(ticks_at)) {
    if (!length(ticks_at)) {
      stop("`ticks_at` must be NULL, a numeric vector, or a non-empty list of numeric vectors.",
           call. = FALSE)
    }
    lapply(ticks_at, validate_ticks, name = "ticks_at")
  } else {
    validate_ticks(ticks_at, "ticks_at")
  }
  validate_xlim <- function(x) {
    if (is.null(x)) return(invisible(NULL))
    ok <- is.numeric(x) && length(x) == 2L && !anyNA(x) && x[1] < x[2]
    if (!ok) {
      stop("`xlim` must be NULL, a numeric vector of length 2, or a non-empty list of numeric length-2 vectors.",
           call. = FALSE)
    }
    invisible(NULL)
  }
  if (is.list(xlim)) {
    if (!length(xlim)) {
      stop("`xlim` must be NULL, a numeric vector of length 2, or a non-empty list of numeric length-2 vectors.",
           call. = FALSE)
    }
    lapply(xlim, validate_xlim)
  } else {
    validate_xlim(xlim)
  }
  if (!is.null(ticks_digits) &&
      (!is.numeric(ticks_digits) || length(ticks_digits) != 1L ||
       is.na(ticks_digits) || ticks_digits < 0)) {
    stop("`ticks_digits` must be NULL or a single non-negative number.", call. = FALSE)
  }
  if (!is.null(ticks_digits)) ticks_digits <- as.integer(ticks_digits)
  if (!is.logical(style_strata) || length(style_strata) != 1L || is.na(style_strata)) {
    stop("`style_strata` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(strata_fill) || length(strata_fill) != 1L || is.na(strata_fill)) {
    stop("`strata_fill` must be a single character color.", call. = FALSE)
  }

  # build df if needed
  if (is.null(df)) {
    if (is.null(uni) && inherits(multi, "gtregression")) {
      df <- forest_df(uni = multi, desc = desc)
    } else if (!inherits(uni, "gtregression")) {
      stop(
        "Provide `df = forest_df(...)`, a `gtregression` object in `uni`, or a `multi_reg()` object in `multi`.",
        call. = FALSE
      )
    } else {
      df <- forest_df(uni = uni, multi = multi, desc = desc)
    }
  } else if (!is.data.frame(df)) {
    stop("`df` must be a data frame created by `forest_df()`.", call. = FALSE)
  }

  if (!is.null(df) && !is.null(uni) && is.data.frame(uni)) {
    stop(
      "When `df` is supplied, do not pass another data frame as the second argument. ",
      "Use `forest_df(uni, multi, desc = ...)` to combine inputs before calling `forest_reg()`.",
      call. = FALSE
    )
  }

  if (!("Characteristic" %in% names(df))) {
    stop("`df` must contain a `Characteristic` column.", call. = FALSE)
  }
  if (is.null(attr(df, "est")) || !("se_uni" %in% names(df)) ||
      !is.numeric(df$se_uni)) {
    stop(
      "`df` does not contain forest plot estimates. ",
        "Use `forest_df()` with a compatible regression object; ",
        "a descriptive-only `forest_df()` cannot be drawn by `forest_reg()`.",
        call. = FALSE
    )
  }

  # detect meta
  meta <- attr(df, "forest_meta")
  if (is.null(meta)) {
    meta <- list(x_trans = "none", ref_line = 0)
  }

  # choose estimate sets
  effect_sets <- .forest_effect_sets(df)
  est <- if (length(effect_sets$est) == 1L) effect_sets$est[[1]] else effect_sets$est
  lo <- if (length(effect_sets$lo) == 1L) effect_sets$lo[[1]] else effect_sets$lo
  hi <- if (length(effect_sets$hi) == 1L) effect_sets$hi[[1]] else effect_sets$hi

  df_plot <- df[, setdiff(names(df), c("se_uni","se_adj")), drop = FALSE]

  # --- position the plot columns per `side` and recompute ci_col ---
  # --- enforce layout rules: Characteristics/desc on the left; plot/text order by `side` ---

  nm <- names(df_plot)

  # 1) Identify CI text columns and anchor (plot) columns
  ci_cols_all <- grep("\\(95% CI\\)$", nm, value = TRUE)
  anchor_cols <- effect_sets$anchors
  text_cols <- effect_sets$text_cols
  if (!length(anchor_cols) || !length(text_cols) ||
      length(anchor_cols) != length(text_cols)) {
    stop("`df` does not contain matched forest plot columns.", call. = FALSE)
  }
  if (length(ci_col_width) == 1L) {
    ci_col_width <- rep(ci_col_width, length(anchor_cols))
  } else if (length(ci_col_width) != length(anchor_cols)) {
    stop(
      "`ci_col_width` must be length 1 or have one value per effect column.",
      call. = FALSE
    )
  }
  for (i in seq_along(anchor_cols)) {
    df_plot[[anchor_cols[i]]] <- paste(rep(" ", ci_col_width[i]), collapse = "")
  }

  # 2) Characteristic + desc columns = everything that's not an anchor or CI text
  left_cols <- setdiff(nm, c(anchor_cols, ci_cols_all))   # preserve original order

  # 3) Build the effect-area blocks based on `side`
  blocks <- character(0)
  for (i in seq_along(anchor_cols)) {
    blocks <- c(
      blocks,
      if (side == "left") c(anchor_cols[i], text_cols[i]) else c(text_cols[i], anchor_cols[i])
    )
  }

  # 4) Reorder df_plot: left (Characteristic/desc) | effect blocks
  df_plot <- df_plot[, c(left_cols, blocks), drop = FALSE]

  # 5) Recompute ci_column indices (must be the plot-anchor columns in the same order as in `blocks`)
  anchor_in_blocks <- blocks[blocks %in% anchor_cols]
  ci_col <- match(anchor_in_blocks, names(df_plot))

  draw <- function() forestploter::forest(
    data       = df_plot,
    est        = est,
    lower      = lo,
    upper      = hi,
    ci_column  = ci_col,
    x_trans    = meta$x_trans,
    ref_line   = meta$ref_line,
    xlim       = xlim,
    ticks_at   = ticks_at,
    ticks_digits = ticks_digits,
    theme      = theme,
    effects    = effects,
    ...
  )

  plt <- if (quiet) suppressWarnings(draw()) else draw()

  strata_rows <- .forest_stratum_rows(df_plot, meta)
  if (isTRUE(style_strata) && length(strata_rows)) {
    plt <- .forest_style_stratum_rows(
      plot = plt,
      rows = strata_rows,
      cols = seq_along(df_plot),
      fill = strata_fill
    )
  }

  meta$ticks_at <- ticks_at
  meta$ticks_digits <- ticks_digits
  meta$xlim <- xlim
  meta$ci_col_width <- ci_col_width
  meta$strata_rows <- strata_rows
  meta$style_strata <- style_strata
  meta$strata_fill <- strata_fill

  structure(list(plot = plt, data = df_plot, input_data = df, meta = meta),
            class = c("gtregression_forest", "list"))
}

.forest_effect_sets <- function(df) {
  custom <- attr(df, "forest_estimates", exact = TRUE)
  if (!is.null(custom)) {
    out <- list(
      est = custom$est,
      lo = custom$lo,
      hi = custom$hi,
      anchors = custom$anchors,
      text_cols = custom$text_cols
    )
    if (length(out$est) && length(out$lo) && length(out$hi) &&
        length(out$anchors) && length(out$text_cols)) {
      return(out)
    }
  }

  ci_cols_all <- grep("\\(95% CI\\)$", names(df), value = TRUE)
  anchor_cols <- names(df)[grepl("^ +$", names(df))]

  if (!is.null(attr(df, "est2", exact = TRUE))) {
    text_cols <- character(0)
    ci_uni <- setdiff(ci_cols_all, ci_cols_all[grepl("^Adjusted\\s+", ci_cols_all)])
    ci_adj <- ci_cols_all[grepl("^Adjusted\\s+", ci_cols_all)]
    text_cols <- c(ci_uni[1], ci_adj[1])
    text_cols <- text_cols[!is.na(text_cols)]

    return(list(
      est = list(attr(df, "est", exact = TRUE), attr(df, "est2", exact = TRUE)),
      lo = list(attr(df, "lo", exact = TRUE), attr(df, "lo2", exact = TRUE)),
      hi = list(attr(df, "hi", exact = TRUE), attr(df, "hi2", exact = TRUE)),
      anchors = intersect(c(" ", "  "), anchor_cols),
      text_cols = text_cols
    ))
  }

  text_col <- ci_cols_all[1]
  list(
    est = list(attr(df, "est", exact = TRUE)),
    lo = list(attr(df, "lo", exact = TRUE)),
    hi = list(attr(df, "hi", exact = TRUE)),
    anchors = anchor_cols[1],
    text_cols = text_col
  )
}

.forest_stratum_rows <- function(df_plot, meta) {
  if (!isTRUE(meta$stratified) || is.null(meta$stratifier) || is.null(meta$strata)) {
    return(integer())
  }

  if (isTRUE(meta$side_by_side_strata)) {
    return(integer())
  }

  labels <- paste0(meta$stratifier, " = ", meta$strata)
  which(df_plot$Characteristic %in% labels)
}

.forest_style_stratum_rows <- function(plot, rows, cols, fill) {
  for (row in rows) {
    plot <- forestploter::edit_plot(
      plot,
      row = row,
      col = cols,
      part = "body",
      which = "background",
      gp = grid::gpar(fill = fill, col = NA)
    )
    plot <- forestploter::edit_plot(
      plot,
      row = row,
      col = cols,
      part = "body",
      which = "text",
      gp = grid::gpar(fontface = "bold")
    )
  }
  plot
}

#' @export
print.gtregression_forest <- function(x, ..., autofit = FALSE) {
  if (!is.logical(autofit) || length(autofit) != 1L || is.na(autofit)) {
    stop("`autofit` must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(autofit)) {
    warning(
      "`autofit = TRUE` is ignored for `forest_reg()` because it can compress ",
      "forest plot columns and cause overlap. Printing with `autofit = FALSE`. ",
      "Use a wider graphics device or export canvas for more side space.",
      call. = FALSE
    )
    autofit <- FALSE
  }
  print(x$plot, autofit = autofit)
  invisible(x)
}
