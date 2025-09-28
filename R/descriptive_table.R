# ---------- minimal theme resolver ----------
.theme_presets_desc <- list(
  minimal  = c("plain","lines","labels_bold"),
  clinical = c("plain","labels_bold","compact"),
  striped  = c("zebra","labels_bold","compact"),
  shaded   = c("header_shaded","labels_bold","lines"),
  jama     = c("plain","lines","labels_bold","compact")
)
.resolve_theme_desc <- function(theme) {
  if (length(theme) == 1 && !is.na(theme) && theme %in% names(.theme_presets_desc))
    return(.theme_presets_desc[[theme]])
  unique(tolower(theme))
}

# ---------- descriptive_table ----------
#' Descriptive Summary Table (no gtsummary) using gt/flextable
#'
#' Publication-ready summary of categorical and continuous variables
#' (optionally stratified). Mimics the OG gtsummary style:
#' * column headers include N, e.g. "Overall, N=200"
#' * categorical rows shown as n (%)
#' * continuous rows default to Median (IQR) (footnote reflects summary)
#'
#' @param data data.frame
#' @param exposures character; variables to summarise
#' @param by optional single grouping variable
#' @param percent "column" (default) or "row"; aliases like "col"/"rows" accepted
#' @param digits integer; decimals for % and continuous stats (default 1)
#' @param show_missing "ifany" (default) or "no"
#' @param show_dichotomous "all_levels" (default) or "single_row"
#' @param show_overall "no" (default), "first", or "last"
#' @param statistic optional named vector per continuous var:
#'   values in {"mean","median","mode","count"}
#'   (default is "median" = Median (IQR))
#' @param value optional named list for single-row binaries (e.g., list(sex="Female"))
#' @param format "gt" (default) or "flextable"
#' @param theme preset or primitives
#'
#' @return list with class c("gtregression","descriptive_table", <engine>):
#'   \itemize{
#'     \item $table: gt_tbl or flextable
#'     \item $table_display: display-ready data
#'     \item $table_body: long audit data (var/level/type)
#'     \item metadata fields
#'   }
#' @export
descriptive_table <- function(data,
                              exposures,
                              by = NULL,
                              percent = c("column","row"),
                              digits = 1,
                              show_missing = c("ifany","no"),
                              show_dichotomous = c("all_levels","single_row"),
                              show_overall = c("no","first","last"),
                              statistic = NULL,
                              value = NULL,
                              format = c("gt","flextable"),
                              theme = c("minimal")) {

  # ---- normalize choices (accept aliases) ----
  percent <- tolower(percent[1])
  if (percent %in% c("col","cols","column","columns")) percent <- "column"
  if (percent %in% c("row","rows","rowwise"))          percent <- "row"
  show_missing     <- match.arg(tolower(show_missing[1]),     c("ifany","no"))
  show_dichotomous <- match.arg(tolower(show_dichotomous[1]), c("all_levels","single_row"))
  show_overall     <- match.arg(tolower(show_overall[1]),     c("no","first","last"))
  percent          <- match.arg(percent, c("column","row"))
  format           <- match.arg(tolower(format[1]), c("gt","flextable"))
  theme            <- .resolve_theme_desc(theme)

  # ---- checks ----
  stopifnot(is.character(exposures), length(exposures) >= 1)
  data <- as.data.frame(data)
  missing_vars <- setdiff(c(exposures, by), names(data))
  if (length(missing_vars)) stop("Variables not found: ", paste(missing_vars, collapse=", "), call.=FALSE)
  if (percent == "row" && is.null(by)) {
    warning("`percent = \"row\"` requires `by`; using column-wise % instead.")
    percent <- "column"
  }

  # ---- helpers ----
  fmt_num <- function(x, d = digits) formatC(x, digits = d, format="f", big.mark=",")
  fmt_pct <- function(p, d = digits) if (is.na(p)) "" else paste0(fmt_num(100*p, d), "%")
  is_binary <- function(x) { ux <- unique(x[!is.na(x)]); length(ux) == 2 }
  pick_single_level <- function(x) {
    ux <- unique(x[!is.na(x)]); if (length(ux) != 2) return(NA_character_)
    if (is.factor(x)) return(levels(x)[2])
    if (is.logical(x)) return("TRUE")
    if (is.numeric(x)) return(as.character(sort(ux)[2]))
    sort(as.character(ux))[2]
  }
  # default continuous summary: median (IQR)
  cont_summary <- function(x, stat = "median", d = digits) {
    x <- x[!is.na(x)]
    if (!length(x)) return("")
    stat <- match.arg(stat, c("mean","median","mode","count"))
    if (stat == "mean") {
      m <- mean(x); s <- stats::sd(x)
      paste0(fmt_num(m, d), " (", fmt_num(s, d), ")")
    } else if (stat == "median") {
      md <- stats::median(x); q <- stats::quantile(x, c(.25,.75), names=FALSE)
      paste0(fmt_num(md, d), " (", fmt_num(q[1], d), "–", fmt_num(q[2], d), ")")
    } else if (stat == "mode") {
      tab <- sort(table(x), decreasing = TRUE); md <- as.numeric(names(tab)[1])
      paste0(fmt_num(md, d))
    } else {
      paste0("N = ", length(x))
    }
  }
  stat_label <- function(s) switch(s,
                                   mean   = "Mean (SD)",
                                   median = "Median (IQR)",
                                   mode   = "Mode",
                                   count  = "N",
                                   "Median (IQR)"
  )
  get_user_stat <- function(v) {
    if (is.null(statistic) || is.null(names(statistic))) return(NA_character_)
    idx <- match(v, names(statistic)); if (is.na(idx)) NA_character_ else statistic[[idx]]
  }
  get_single_level <- function(vname) {
    if (is.null(value) || is.null(names(value))) return(NA_character_)
    idx <- match(vname, names(value)); if (is.na(idx)) NA_character_ else as.character(value[[idx]])
  }

  # ---- by levels and group Ns (for headers) ----
  by_levels <- NULL
  group_names <- "Overall"
  group_idx   <- list(Overall = rep(TRUE, nrow(data)))
  if (!is.null(by)) {
    bv <- data[[by]]
    by_levels <- if (is.factor(bv)) levels(stats::na.omit(bv)) else unique(stats::na.omit(bv))
    group_names <- as.character(by_levels)
    group_idx   <- lapply(by_levels, function(g) (data[[by]] == g) & !is.na(data[[by]]))
    names(group_idx) <- group_names
    if (show_overall != "no") {
      where <- if (show_overall == "first") 1L else length(group_names) + 1L
      group_names <- append(group_names, values = "Overall", after = where - 1L)
      group_idx   <- append(group_idx,   values = list(Overall = rep(TRUE, nrow(data))), after = where - 1L)
    }
  }
  group_Ns <- vapply(group_names, function(gn) sum(!is.na(data[[exposures[1]]][group_idx[[gn]]])) , numeric(1))
  # N in header should reflect total non-missing per group considering all variables;
  # to mirror gtsummary we use N of the grouping column (or all rows). Safer approach:
  group_Ns <- vapply(group_names, function(gn) sum(rowSums(!is.na(data[exposures])[group_idx[[gn]], , drop=FALSE]) > 0), numeric(1))

  # ---- construct long body (header + levels; NO N CELLS) ----
  body_rows <- list()
  for (v in exposures) {
    x <- data[[v]]
    # header row
    body_rows[[length(body_rows)+1]] <- data.frame(
      var=v, level=NA_character_, type="header", stringsAsFactors=FALSE
    )
    is_cat <- is.factor(x) || is.character(x)
    is_bin <- is_binary(x)

    if (!is_cat && !is_bin) {
      body_rows[[length(body_rows)+1]] <- data.frame(
        var=v, level=v, type="continuous", stringsAsFactors=FALSE
      ); next
    }
    levels_here <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
    add_missing <- (show_missing == "ifany") && any(is.na(x))
    if (is_bin && show_dichotomous == "single_row") {
      pick <- get_single_level(v); if (is.na(pick) || !nzchar(pick)) pick <- pick_single_level(x)
      levels_here <- intersect(levels_here, pick)
    }
    for (lv in levels_here) {
      body_rows[[length(body_rows)+1]] <- data.frame(
        var=v, level=lv, type="categorical", stringsAsFactors=FALSE
      )
    }
    if (add_missing) {
      body_rows[[length(body_rows)+1]] <- data.frame(
        var=v, level="(Missing)", type="categorical_missing", stringsAsFactors=FALSE
      )
    }
  }
  table_body <- do.call(rbind, body_rows)

  # ---- display skeleton ----
  display <- do.call(rbind, lapply(split(table_body, table_body$var), function(df) {
    hdr <- data.frame(
      Characteristic = unique(df$var),
      is_header = TRUE, var = unique(df$var), level = NA_character_,
      type = "header", stringsAsFactors=FALSE
    )
    levs <- subset(df, type != "header")
    if (!nrow(levs)) return(hdr)
    levs$Characteristic <- ifelse(levs$type=="continuous", unique(df$var), paste0("  ", levs$level))
    levs$is_header <- FALSE
    levs <- levs[, c("Characteristic","is_header","var","level","type"), drop=FALSE]
    rbind(hdr, levs)
  }))
  rownames(display) <- NULL

  # ---- fill cells (no header cells content) ----
  add_group_col <- function(df, grp_idx) {
    cells <- character(nrow(df))
    for (i in seq_len(nrow(df))) {
      typ <- df$type[i]; v <- df$var[i]; lv <- df$level[i]
      x <- data[[v]][grp_idx]

      if (typ == "header") { cells[i] <- "" ; next }

      if (typ == "continuous") {
        st <- get_user_stat(v); if (is.na(st) || !nzchar(st)) st <- "median"
        cells[i] <- cont_summary(x, stat = st, d = digits)
        next
      }
      if (typ == "categorical_missing") {
        n <- sum(is.na(data[[v]][grp_idx]))
        denom <- sum(!is.na(data[[v]][grp_idx]))
        p <- if (denom > 0) n/denom else NA_real_
        cells[i] <- paste0(n, " (", fmt_pct(p, digits), ")"); next
      }
      n <- sum(as.character(x) == as.character(lv), na.rm=TRUE)
      if (is.null(by) || percent == "column") {
        denom <- sum(!is.na(x))
        p <- if (denom > 0) n/denom else NA_real_
        cells[i] <- paste0(n, " (", fmt_pct(p, digits), ")")
      } else {
        cells[i] <- as.character(n) # temporarily; will convert to row %
      }
    }
    cells
  }
  for (gn in group_names) {
    display[[gn]] <- add_group_col(display, group_idx[[gn]])
  }

  # row % conversion
  if (!is.null(by) && percent == "row") {
    grp_cols <- setdiff(group_names, "Overall")
    for (i in seq_len(nrow(display))) {
      if (display$type[i] %in% c("categorical","categorical_missing") && !display$is_header[i]) {
        v <- display$var[i]; lv <- display$level[i]
        counts <- integer(length(grp_cols))
        for (j in seq_along(grp_cols)) {
          idx <- group_idx[[grp_cols[j]]]; x <- data[[v]][idx]
          counts[j] <- if (display$type[i] == "categorical_missing") sum(is.na(x)) else
            sum(as.character(x) == as.character(lv), na.rm=TRUE)
        }
        row_tot <- sum(counts)
        for (j in seq_along(grp_cols)) {
          p <- if (row_tot > 0) counts[j]/row_tot else NA_real_
          display[[grp_cols[j]]][i] <- paste0(counts[j], " (", fmt_pct(p, digits), ")")
        }
        if ("Overall" %in% group_names) display[["Overall"]][i] <- as.character(row_tot)
      }
    }
  }

  # ---- header labels with N ----
  header_labels <- setNames(
    object = paste0(group_names, ", N=", vapply(group_names, function(gn) {
      # Use non-missing over all exposures per group as a pragmatic denominator
      sum(rowSums(!is.na(data[exposures])[group_idx[[gn]], , drop=FALSE]) > 0)
    }, numeric(1))),
    nm = group_names
  )

  # ---- footnotes ----
  # build continuous stat note
  cont_vars <- exposures[!vapply(data[exposures], function(x) is.factor(x) || is.character(x) || is_binary(x), logical(1))]
  stat_used <- if (length(cont_vars)) {
    lbls <- vapply(cont_vars, function(v) stat_label(ifelse(is.na(get_user_stat(v)),"median", get_user_stat(v))), character(1))
    if (length(unique(lbls)) == 1L) {
      paste0("Continuous variables shown as ", unique(lbls), ".")
    } else {
      paste0("Continuous summaries: ",
             paste(paste0(cont_vars, " = ", lbls), collapse = "; "), ".")
    }
  } else NULL

  pct_note <- if (!is.null(by)) {
    if (percent == "column") "Categorical variables shown as n (%); percentages are by column."
    else "Categorical variables shown as n (%); percentages are by row (Overall shows counts)."
  } else {
    "Categorical variables shown as n (%)."
  }

  foot <- c(pct_note, stat_used)

  # ---- builders (gt / flextable) ----
  build_gt <- function(display_df, header_labels, footnote_lines, theme) {
    if (!requireNamespace("gt", quietly = TRUE))
      stop("Install 'gt' or use format='flextable'.", call. = FALSE)
    out <- display_df[, c("Characteristic", names(header_labels)), drop=FALSE]
    tb <- gt::gt(out) |>
      gt::cols_label(.list = c(list(Characteristic = "Characteristic"), as.list(header_labels))) |>
      gt::cols_align("left",   columns = "Characteristic") |>
      gt::cols_align("center", columns = setdiff(names(out), "Characteristic")) |>
      gt::tab_options(
        table.font.names         = "system-ui",
        data_row.padding         = gt::px(4),
        heading.background.color = "white",
        table.background.color   = "white"
      )
    # bold labels + spanners, bold var headers; indent levels
    tb <- gt::tab_style(tb, gt::cell_text(weight="bold"), locations = gt::cells_column_labels())
    tb <- gt::tab_style(tb, gt::cell_text(weight="bold"),
                        gt::cells_body(rows = display_df$is_header %in% TRUE, columns = "Characteristic"))
    tb <- gt::tab_style(tb, gt::cell_text(indent = gt::px(12)),
                        gt::cells_body(rows = display_df$is_header %in% FALSE, columns = "Characteristic"))
    if ("header_shaded" %in% theme) tb <- gt::tab_options(tb, column_labels.background.color = "#f6f8fa")
    if ("zebra" %in% theme) tb <- gt::opt_row_striping(tb)
    if ("lines" %in% theme) {
      tb <- gt::tab_style(tb, gt::cell_borders(sides="top", color="#DADADA", weight=gt::px(1)),
                          gt::cells_body(rows=TRUE))
      tb <- gt::tab_options(tb,
                            table.border.top.style="solid",    table.border.top.color="#DADADA",
                            table.border.bottom.style="solid", table.border.bottom.color="#DADADA")
    }
    if ("compact" %in% theme) tb <- gt::tab_options(tb, data_row.padding = gt::px(2))
    if (length(footnote_lines)) tb <- gt::tab_source_note(tb, source_note = footnote_lines)
    tb
  }

  build_flex <- function(display_df, header_labels, footnote_lines, theme) {
    if (!requireNamespace("flextable", quietly = TRUE))
      stop("Install 'flextable' or use format='gt'.", call. = FALSE)
    out <- display_df[, c("Characteristic", names(header_labels)), drop=FALSE]
    colnames(out) <- c("Characteristic", unname(header_labels))
    ft <- flextable::flextable(out)
    ft <- flextable::align(ft, j="Characteristic", align="left",  part="all")
    ft <- flextable::align(ft, j=setdiff(names(out), "Characteristic"), align="center", part="all")
    ft <- flextable::bold(ft, part="header", bold=TRUE)
    hdr_idx <- which(display_df$is_header %in% TRUE)
    lvl_idx <- which(display_df$is_header %in% FALSE)
    if (length(hdr_idx)) ft <- flextable::bold(ft, i=hdr_idx, j="Characteristic", bold=TRUE, part="body")
    if (length(lvl_idx)) ft <- flextable::padding(ft, i=lvl_idx, j="Characteristic", padding.left=14, part="body")
    if ("header_shaded" %in% theme) ft <- flextable::bg(ft, part="header", bg="#f6f8fa")
    if ("zebra" %in% theme && length(lvl_idx)) {
      ft <- flextable::bg(ft, i=lvl_idx[seq(1,length(lvl_idx),by=2)], bg="#f6f8fa", part="body")
    }
    if ("lines" %in% theme) {
      n_body <- nrow(out); if (!is.null(n_body) && n_body>0) {
        ft <- flextable::hline(ft, i=1, part="body")
        ft <- flextable::hline(ft, i=n_body, part="body")
      }
    }
    if ("compact" %in% theme) ft <- flextable::padding(ft, padding=2, part="body")
    ft <- flextable::autofit(ft)
    if (length(footnote_lines)) {
      ft <- flextable::add_footer_lines(ft, values=footnote_lines)
      ft <- flextable::align(ft, part="footer", align="left")
    }
    ft
  }
  # Build a human label for continuous stats (change if your function differs)
  cont_label <- "Median (IQR)"  # or compute from your options

  # Auto footnotes for descriptive table
  .desc_build_notes <- function(by, percent, cont_label, single_row) {
    notes <- c("Categorical variables shown as n (%).")

    if (!is.null(by)) {
      if (identical(percent, "row")) {
        notes <- c(notes, "Percentages are within-row across groups; Overall shows counts for categorical rows.")
      } else {
        notes <- c(notes, "Percentages are within-column within groups.")
      }
    } else {
      notes <- c(notes, "Percentages are overall (single column).")
    }

    notes <- c(notes, paste0("Continuous variables shown as ", cont_label, "."))
    if (isTRUE(single_row)) {
      notes <- c(notes, "Binary variables displayed as a single level (n %).")
    }
    notes
  }

  # attach footnotes as an attribute (picked up by merge_tables)
  desc_notes <- .desc_build_notes(
    by          = by,
    percent     = percent,
    cont_label  = cont_label,
    single_row  = identical(show_dichotomous, "single_row")
  )



  tbl <- if (format == "gt") build_gt(display, header_labels, foot, theme)
  else                build_flex(display, header_labels, foot, theme)

  res <- list(
    table         = tbl,
    table_display = display[, c("Characteristic","is_header", names(header_labels)), drop=FALSE],
    table_body    = table_body,
    by            = by,
    levels        = by_levels,
    format        = format,
    source        = "descriptive_table"
  )
  # keep the same notes the table shows
  res$footnotes <- unique(c(foot))

  engine_class <- if (format == "gt") "gt_desc" else "ft_desc"
  class(res) <- c("gtregression","descriptive_table", engine_class, class(res))
  res
}


