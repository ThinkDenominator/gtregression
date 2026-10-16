# Internal structure engine for the GT Regression Data > Structure screen.
# Base R only. It returns structured values (not printed text) so the app can
# render a compact overview, objective "worth a look" features, a variable
# table and a per-variable detail.

gt_ds_type <- function(x) {
  if (inherits(x, "Date")) return("Date")
  if (inherits(x, "POSIXt")) return("Date-time")
  if (is.ordered(x)) return("Ordered")
  if (is.factor(x)) return("Factor")
  if (is.logical(x)) return("Logical")
  if (is.numeric(x)) return("Numeric")
  if (is.character(x)) return("Text")
  "Other"
}

# Display separators are returned as control-character tokens and turned into
# HTML entities by gt_ds_html(), so output is identical in any locale
# (including C/POSIX), with no non-ASCII bytes in strings.
gt_ds_dot <- "\001"
gt_ds_pm <- "\002"
gt_ds_dash <- "\003"

gt_ds_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("\001", "&middot;", x, fixed = TRUE)
  x <- gsub("\002", "&plusmn;", x, fixed = TRUE)
  gsub("\003", "&ndash;", x, fixed = TRUE)
}

# Plain-text form (tests, logs).
gt_ds_text <- function(x) {
  x <- gsub("\001", "-", x, fixed = TRUE)
  x <- gsub("\002", "+/-", x, fixed = TRUE)
  gsub("\003", "-", x, fixed = TRUE)
}

# Compact number for display: 3 significant digits for non-integers,
# thousands separators for large values.
gt_ds_num <- function(x, digits = 3) {
  if (!length(x) || is.na(x)) return("NA")
  if (!is.finite(x)) return(format(x))
  if (abs(x - round(x)) < 1e-9 && abs(x) < 1e15) return(format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
  value <- signif(x, digits)
  if (abs(value) >= 1e6 || (abs(value) < 1e-3 && value != 0)) return(format(value, scientific = TRUE, digits = digits))
  format(value, big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

gt_ds_pct <- function(n, d) {
  if (!d) return("0%")
  p <- 100 * n / d
  if (p > 0 && p < 1) return("<1%")
  paste0(round(p), "%")
}

# Number of rows that duplicate an earlier row. Radix ordering plus
# vectorised comparison of neighbours: the same answer as
# sum(duplicated(data)) at a fraction of the cost on large data.
gt_ds_duplicate_rows <- function(data) {
  n <- nrow(data)
  if (n < 2L || !ncol(data)) return(0L)
  atomic <- vapply(data, function(v) is.atomic(v) && is.null(dim(v)), logical(1))
  if (!all(atomic)) return(sum(duplicated(data)))
  o <- do.call(order, c(unname(as.list(data)), list(na.last = TRUE, method = "radix")))
  same <- rep(TRUE, n - 1L)
  for (v in data) {
    s <- v[o]
    a <- s[-1L]
    b <- s[-n]
    eq <- (a == b) | (is.na(a) & is.na(b))
    eq[is.na(eq)] <- FALSE
    same <- same & eq
    if (!any(same)) break
  }
  sum(same)
}

# Frequency table in display order: factor level order (unused levels kept),
# FALSE/TRUE for logicals, otherwise descending frequency.
gt_ds_levels <- function(x) {
  valid <- x[!is.na(x)]
  if (is.factor(x)) {
    counts <- tabulate(as.integer(valid), nbins = nlevels(x))
    return(data.frame(level = levels(x), n = counts, stringsAsFactors = FALSE))
  }
  if (is.logical(x)) {
    return(data.frame(level = c("FALSE", "TRUE"), n = c(sum(!valid), sum(valid)), stringsAsFactors = FALSE))
  }
  tab <- table(as.character(valid))
  tab <- tab[order(-as.integer(tab), names(tab))]
  data.frame(level = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
}

gt_ds_is_01 <- function(x, valid) {
  is.numeric(x) && length(valid) > 0L && all(valid == 0 | valid == 1) && length(unique(valid)) == 2L
}

gt_ds_date_range <- function(valid, type) {
  lo <- min(valid)
  hi <- max(valid)
  fmt <- function(d) format(d, if (identical(type, "Date-time")) "%d %b %Y %H:%M" else "%d %b %Y")
  # Short spans show full dates; longer spans show months.
  span_days <- as.numeric(difftime(hi, lo, units = "days"))
  if (span_days <= 62) {
    return(paste(fmt(lo), gt_ds_dash, fmt(hi)))
  }
  paste(format(lo, "%b %Y"), gt_ds_dash, format(hi, "%b %Y"))
}

gt_ds_summary <- function(x, type, valid, levels = NULL) {
  if (!length(valid)) return("all missing")
  if (identical(type, "Numeric")) {
    if (gt_ds_is_01(x, valid)) {
      return(paste0("0 ", gt_ds_pct(sum(valid == 0), length(valid)), " ", gt_ds_dot, " 1 ", gt_ds_pct(sum(valid == 1), length(valid))))
    }
    if (length(valid) < 2L) return(gt_ds_num(valid[1]))
    return(paste(gt_ds_num(mean(valid)), gt_ds_pm, gt_ds_num(stats::sd(valid))))
  }
  if (type %in% c("Date", "Date-time")) return(gt_ds_date_range(valid, type))
  if (type %in% c("Factor", "Ordered", "Logical", "Text")) {
    lv <- levels %||% gt_ds_levels(x)
    if (identical(type, "Text") && nrow(lv) > 10L) {
      return(paste0(gt_ds_num(nrow(lv)), " different values"))
    }
    shown <- utils::head(lv, 3L)
    parts <- paste(shown$level, vapply(shown$n, gt_ds_pct, character(1), d = length(valid)))
    more <- nrow(lv) - nrow(shown)
    out <- paste(parts, collapse = paste0(" ", gt_ds_dot, " "))
    if (more > 0L) out <- paste0(out, " ", gt_ds_dot, " +", more, " more")
    return(out)
  }
  paste(class(x), collapse = ", ")
}

#' Structure of a dataset (internal)
#'
#' @param data A data frame.
#' @param missing_threshold Proportion of missing values above which a
#'   variable is listed under "worth a look".
#' @param id_min_rows Minimum non-missing rows before an all-unique
#'   integer or text variable is described as a possible identifier.
#' @return A list with `dataset` (overview values), `variables` (one row per
#'   variable) and `notes` (objective features, each with the variables).
#' @keywords internal
#' @noRd
gt_data_structure <- function(data, missing_threshold = 0.2, id_min_rows = 50L) {
  stopifnot(is.data.frame(data))
  n <- nrow(data)
  rows <- lapply(names(data), function(name) {
    x <- data[[name]]
    type <- gt_ds_type(x)
    is_missing <- is.na(x)
    missing <- sum(is_missing)
    valid <- x[!is_missing]
    distinct <- length(unique(valid))
    levels <- if (type %in% c("Factor", "Ordered", "Logical") || (identical(type, "Text") && distinct <= 10L)) gt_ds_levels(x) else NULL
    integer_like <- is.numeric(x) && length(valid) && all(is.finite(valid)) && all(valid == round(valid))
    data.frame(
      variable = name,
      type = type,
      label = as.character(attr(x, "label", exact = TRUE) %||% ""),
      missing = missing,
      distinct = distinct,
      summary = gt_ds_summary(x, type, valid, levels),
      constant = distinct <= 1L,
      high_missing = n > 0L && missing / n > missing_threshold,
      zero_one = gt_ds_is_01(x, valid),
      possible_id = length(valid) >= id_min_rows && distinct == length(valid) &&
        (integer_like || type %in% c("Text", "Factor")),
      stringsAsFactors = FALSE
    )
  })
  variables <- if (length(rows)) do.call(rbind, rows) else data.frame(
    variable = character(), type = character(), label = character(), missing = integer(),
    distinct = integer(), summary = character(), constant = logical(), high_missing = logical(),
    zero_one = logical(), possible_id = logical(), stringsAsFactors = FALSE
  )
  type_order <- c("Numeric", "Factor", "Ordered", "Text", "Logical", "Date", "Date-time", "Other")
  types <- table(factor(variables$type, levels = type_order))
  types <- types[types > 0]
  duplicates <- gt_ds_duplicate_rows(data)
  notes <- list(
    duplicate_rows = duplicates,
    possible_id = variables$variable[variables$possible_id],
    constant = variables$variable[variables$constant],
    high_missing = variables$variable[variables$high_missing],
    zero_one = variables$variable[variables$zero_one]
  )
  list(
    dataset = list(
      rows = n,
      variables = ncol(data),
      missing_cells = sum(variables$missing),
      complete_rows = if (ncol(data)) sum(stats::complete.cases(data)) else n,
      duplicate_rows = duplicates,
      types = stats::setNames(as.integer(types), names(types))
    ),
    variables = variables,
    notes = notes
  )
}

#' Detail for one variable (internal)
#'
#' @param x A vector (one column).
#' @return A list with `type`, `label`, `counts` (valid, missing, distinct),
#'   `stats` (named display strings), `levels` (data frame or NULL) and
#'   `histogram` (counts and breaks, numeric only).
#' @keywords internal
#' @noRd
gt_variable_detail <- function(x, max_levels = 50L) {
  type <- gt_ds_type(x)
  is_missing <- is.na(x)
  valid <- x[!is_missing]
  n <- length(x)
  out <- list(
    type = type,
    label = as.character(attr(x, "label", exact = TRUE) %||% ""),
    counts = c(valid = length(valid), missing = sum(is_missing), distinct = length(unique(valid))),
    missing_pct = gt_ds_pct(sum(is_missing), n),
    stats = character(),
    levels = NULL,
    levels_more = 0L,
    histogram = NULL
  )
  if (identical(type, "Numeric") && length(valid)) {
    finite <- valid[is.finite(valid)]
    if (length(finite)) {
      q <- stats::quantile(finite, c(0.25, 0.5, 0.75), names = FALSE, type = 7)
      out$stats <- c(
        Mean = gt_ds_num(mean(finite)),
        SD = if (length(finite) > 1L) gt_ds_num(stats::sd(finite)) else gt_ds_dash,
        Median = gt_ds_num(q[2]),
        IQR = paste(gt_ds_num(q[1]), gt_ds_dash, gt_ds_num(q[3])),
        Range = paste(gt_ds_num(min(finite)), gt_ds_dash, gt_ds_num(max(finite)))
      )
      names(out$stats)[5] <- paste0("Min", gt_ds_dash, "Max")
      if (length(unique(finite)) > 1L) {
        # Sturges-style bins on pretty breaks (as hist() would), without graphics.
        bins <- min(30L, max(5L, ceiling(log2(length(finite)) + 1)))
        breaks <- pretty(range(finite), n = bins, min.n = 1)
        counts <- tabulate(findInterval(finite, breaks, rightmost.closed = TRUE, left.open = TRUE, all.inside = TRUE), nbins = length(breaks) - 1L)
        out$histogram <- list(counts = counts, breaks = breaks)
      }
    }
    if (gt_ds_is_01(x, valid) || out$counts[["distinct"]] <= 10L) {
      lv <- gt_ds_levels(as.character(valid))
      lv <- lv[order(as.numeric(lv$level)), , drop = FALSE]
      out$levels <- lv
    }
  } else if (type %in% c("Factor", "Ordered", "Logical", "Text") && length(valid) + length(levels(x))) {
    lv <- gt_ds_levels(x)
    out$levels_more <- max(0L, nrow(lv) - max_levels)
    out$levels <- utils::head(lv, max_levels)
  } else if (type %in% c("Date", "Date-time") && length(valid)) {
    fmt <- if (identical(type, "Date-time")) "%d %b %Y %H:%M" else "%d %b %Y"
    out$stats <- c(
      Earliest = format(min(valid), fmt),
      Median = format(sort(valid)[ceiling(length(valid) / 2)], fmt),
      Latest = format(max(valid), fmt)
    )
  }
  if (!is.null(out$levels)) out$levels$pct <- vapply(out$levels$n, gt_ds_pct, character(1), d = length(valid))
  out
}
