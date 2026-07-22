#' Draw a publication-ready forest plot
#'
#' Wrapper around \code{forestploter::forest()} that works directly with
#' \code{forest_df()} output or with \code{gtregression} regression objects.
#' It can show descriptive columns and one or two model effect columns in a
#' table-style forest plot.
#'
#' @param df Output of \code{forest_df()}. If \code{NULL}, it is built from
#'   \code{uni}, \code{multi}, and \code{desc}.
#' @param uni,multi,desc Optional \code{gtregression} objects to pass through to
#'   \code{forest_df()}.
#' @param theme Optional \code{forestploter::forest_theme()}. If \code{NULL}, a
#'   sensible default is used. You may pass colors and styling either here or
#'   through \code{...}.
#' @param ci_col_width Numeric or length-2 numeric. Relative width of the CI column(s).
#'   A vector like \code{c(0.22, 0.26)} lets you tune unadjusted and adjusted
#'   columns separately.
#' @param side Character. For each effect, position of the plot relative to the effect-size text:
#'   \code{"left"} = plot first then text; \code{"right"} = text first then plot.
#'   The \code{Characteristic} column and descriptive columns remain on the left.
#' @param quiet Logical. Suppress forestploter warnings. Default = `TRUE`.
#' @param effects Optional effect labels passed to \code{forestploter::forest()}.
#' @param ticks_at Optional numeric vector, or length-2 list for two effect
#'   columns, specifying x-axis tick positions. If \code{NULL},
#'   \code{forestploter::forest()} chooses the default ticks.
#' @param ticks_digits Optional number of digits for x-axis tick labels.
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
#' @importFrom forestploter forest forest_theme
#' @importFrom grid gpar
#' @importFrom scales rescale
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

#' @export
forest_reg <- function(df = NULL, uni = NULL, multi = NULL, desc = NULL,
                       theme = NULL,
                       ci_col_width = 0.25,
                       side = c("right", "left"),
                       quiet = TRUE,
                       effects = NULL,
                       ticks_at = NULL,
                       ticks_digits = NULL,
                       ...) {

  side <- .choice_arg(substitute(side), env = parent.frame(), choices = c("right", "left"))
  side <- match.arg(side)
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(ci_col_width) || anyNA(ci_col_width) ||
      !length(ci_col_width) || !length(ci_col_width) %in% c(1L, 2L) ||
      any(ci_col_width <= 0)) {
    stop("`ci_col_width` must be a positive numeric value or length-2 numeric vector.",
         call. = FALSE)
  }
  if (!is.null(effects) && !is.character(effects)) {
    stop("`effects` must be NULL or a character vector.", call. = FALSE)
  }
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
  if (!is.null(ticks_digits) &&
      (!is.numeric(ticks_digits) || length(ticks_digits) != 1L ||
       is.na(ticks_digits) || ticks_digits < 0)) {
    stop("`ticks_digits` must be NULL or a single non-negative number.", call. = FALSE)
  }
  if (!is.null(ticks_digits)) ticks_digits <- as.integer(ticks_digits)

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
      "Use `forest_df()` with a `uni_reg()` or `multi_reg()` object; ",
      "a descriptive-only `forest_df()` cannot be drawn by `forest_reg()`.",
      call. = FALSE
    )
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



  xlim <- NULL


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

  # Multi-only inputs have one plot anchor but an adjusted effect label.
  if (is.na(ci_uni) && !is.na(ci_adj) && " " %in% anchor_cols && !"  " %in% anchor_cols) {
    ci_uni <- ci_adj
    ci_adj <- NA_character_
  }

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
    ticks_at   = ticks_at,
    ticks_digits = ticks_digits,
    ci_col_width = ci_col_width,
    theme      = theme,
    side       = side,
    effects    = effects,
    ...
  )

  plt <- if (quiet) suppressWarnings(draw()) else draw()

  meta$ticks_at <- ticks_at
  meta$ticks_digits <- ticks_digits

  structure(list(plot = plt, data = df_plot, input_data = df, meta = meta),
            class = c("gtregression_forest", "list"))
}

#' @export
print.gtregression_forest <- function(x, ..., autofit = TRUE) {
  print(x$plot, autofit = autofit)
  invisible(x)
}
