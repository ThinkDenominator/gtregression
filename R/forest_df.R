# =========================
# DESC EXTRACTOR (INTERNAL)
# =========================
.df_from_desc <- function(desc, drop_is_header = TRUE) {
  if (!is.list(desc) || is.null(desc$table_display) || is.null(desc$table_body)) {
    stop("`desc` must be a descriptive_table object with `table_display` and `table_body`.",
         call. = FALSE)
  }
  td <- desc$table_display
  tb <- desc$table_body

  hdr <- if ("is_header" %in% names(td)) as.logical(td$is_header) else rep(FALSE, nrow(td))

  expo <- NULL
  if (is.data.frame(tb) && all(c("var", "type") %in% names(tb))) {
    skeleton <- do.call(rbind, lapply(split(tb, tb$var), function(x) {
      levs <- x[x$type != "header", , drop = FALSE]
      if (!nrow(levs)) {
        return(data.frame(exposure = unique(x$var), stringsAsFactors = FALSE))
      }
      if (all(levs$type == "continuous")) {
        return(data.frame(exposure = unique(x$var), stringsAsFactors = FALSE))
      }
      data.frame(
        exposure = c(unique(x$var), rep(unique(x$var), nrow(levs))),
        stringsAsFactors = FALSE
      )
    }))
    if (nrow(skeleton) == nrow(td)) {
      expo <- as.character(skeleton$exposure)
    }
  }

  if (is.null(expo)) {
    expo <- as.character(td$Characteristic)
    expo[!hdr] <- NA_character_
    last <- NA_character_
    for (i in seq_along(expo)) {
      if (!is.na(expo[i]) && nzchar(expo[i])) last <- expo[i] else expo[i] <- last
    }
  }

  out <- td
  out$exposure <- expo

  out$Characteristic <- as.character(out$Characteristic)
  out$exposure       <- as.character(out$exposure)
  if (drop_is_header && "is_header" %in% names(out)) out$is_header <- NULL
  rownames(out) <- NULL
  out
}

.merge_forest_desc <- function(left, desc) {
  ddf <- if (is.list(desc) && !is.null(desc$table_display)) .df_from_desc(desc) else desc
  stopifnot(is.data.frame(ddf), all(c("Characteristic","exposure") %in% names(ddf)))

  joined <- dplyr::left_join(left, ddf, by = c("exposure","Characteristic"))
  desc_cols <- setdiff(names(ddf), c("Characteristic", "exposure"))
  desc_cols <- desc_cols[desc_cols %in% names(joined)]
  if (!length(desc_cols)) return(joined)

  fallback <- ddf[!duplicated(ddf$exposure), c("exposure", desc_cols), drop = FALSE]
  fallback <- fallback[match(joined$exposure, fallback$exposure), , drop = FALSE]

  for (col in desc_cols) {
    needs_value <- is.na(joined[[col]]) | !nzchar(as.character(joined[[col]]))
    fallback_value <- fallback[[col]]
    has_fallback <- !is.na(fallback_value) & nzchar(as.character(fallback_value))
    idx <- needs_value & has_fallback
    joined[[col]][idx] <- fallback_value[idx]
  }

  joined
}

# =========================================
# CLASS-BASED INPUT PICKER (ORDER-AGNOSTIC)
# =========================================
.pick_inputs <- function(uni, multi = NULL, desc = NULL) {
  inputs <- Filter(Negate(is.null), list(uni = uni, multi = multi, desc = desc))

  is_uni   <- function(x) inherits(x, "gtregression") && any(c("uni_reg","gt_uni") %in% class(x))
  is_multi <- function(x) inherits(x, "gtregression") && any(c("multi_reg","gt_multi") %in% class(x))
  is_desc  <- function(x) any(c("descriptive_table","gt_desc") %in% class(x)) ||
    (is.list(x) && !is.null(x$table_display) && !is.null(x$table_body)) ||
    (is.data.frame(x) && "Characteristic" %in% names(x))

  got_uni <- got_multi <- got_desc <- NULL

  for (nm in names(inputs)) {
    x <- inputs[[nm]]
    if (is_uni(x))   { if (!is.null(got_uni))  stop("Two univariate objects supplied.", call. = FALSE); got_uni <- x;  next }
    if (is_multi(x)) { if (!is.null(got_multi)) stop("Two multivariable objects supplied.", call. = FALSE); got_multi <- x; next }
    if (is_desc(x))  { if (!is.null(got_desc))  stop("Two descriptive tables supplied.", call. = FALSE); got_desc <- x;  next }
    stop("Unknown input type for argument '", nm, "'.", call. = FALSE)
  }

  list(uni = got_uni, multi = got_multi, desc = got_desc)
}

#' Build a compatible data frame for forest plots
#'
#' Creates the tabular input used by \code{forest_reg()} from \code{uni_reg()},
#' \code{multi_reg()}, and optionally \code{descriptive_table()} objects. This
#' function is useful when users want to inspect, edit, or reuse the exact data
#' that will be passed to the forest plot.
#'
#' @param uni A \code{gtregression} object from \code{uni_reg()}. If
#'   \code{multi} is supplied without \code{uni}, the multivariable object is
#'   plotted as a single effect column.
#' @param multi Optional \code{gtregression} object from \code{multi_reg()}.
#' @param desc Optional descriptive table object from \code{descriptive_table()}.
#' @param digits Number of digits used when formatting confidence intervals.
#'
#' @return A data frame with display columns, formatted effect-size columns,
#'   standard-error columns, and plotting attributes used by \code{forest_reg()}
#'   to draw confidence intervals.
#'
#' @examples
#' birthwt_data <- data_birthwt |>
#'   dplyr::mutate(
#'     smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
#'     ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
#'     low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
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
#' forest_data <- forest_df(uni_or, multi_or)
#' head(forest_data)
#' @export
forest_df <- function(uni, multi = NULL, desc = NULL, digits = 2) {
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }
  digits <- as.integer(digits)

  `%||%`   <- function(a,b) if (!is.null(a)) a else b
  approach <- function(o) .normalize_approach(o$approach %||% "logit")
  is_ratio <- function(a) .normalize_approach(a) %in% c("logit","logbinomial","poisson","robpoisson","negbin",
                                   "margstd_boot","margstd_delta")
  effname  <- function(a) {
    a <- .normalize_approach(a)
    if (a == "linear") return("Beta")
    if (a %in% c("poisson","negbin")) return("IRR")
    if (a %in% c("logbinomial","robpoisson","margstd_boot","margstd_delta")) return("RR")
    "OR"
  }
  fmt_ci <- function(e,l,h,d){
    out <- rep("", length(e))
    ok <- is.finite(e) & is.finite(l) & is.finite(h)
    out[ok] <- sprintf(paste0("%.",d,"f (%.",d,"f-%." ,d,"f)"), e[ok], l[ok], h[ok])
    out
  }
  read_tb <- function(x){
    tb <- x$table_body
    need <- c("exposure","level","estimate","conf.low","conf.high","p.value","ref")
    miss <- setdiff(need, names(tb))
    if (length(miss)) stop("Missing columns in table_body: ", paste(miss, collapse=", "), call. = FALSE)
    transform(tb[ , need, drop=FALSE],
              ref       = as.logical(ref),
              estimate  = as.numeric(estimate),
              conf.low  = as.numeric(conf.low),
              conf.high = as.numeric(conf.high))
  }
  .se_from_ci <- function(est, lo, hi, ratio = TRUE) {
    se <- rep(NA_real_, length(est))
    ok <- is.finite(est) & is.finite(lo) & is.finite(hi)
    if (ratio) se[ok] <- (log(hi[ok]) - log(lo[ok])) / (2 * 1.96)
    else       se[ok] <- (hi[ok] - lo[ok]) / (2 * 1.96)
    se
  }

  picked <- .pick_inputs(uni, multi, desc)
  uni   <- picked$uni
  multi <- picked$multi
  desc  <- picked$desc

  multi_only <- is.null(uni) && !is.null(multi)
  if (is.null(uni) && !is.null(multi) && inherits(multi, "gtregression")) {
    uni   <- multi
    multi <- NULL
  }
  if (is.null(uni) && is.null(multi) && !is.null(desc)) return(.df_from_desc(desc))

  has_multi <- !is.null(multi)
  u    <- read_tb(uni)
  app1 <- approach(uni)
  eff1 <- if (isTRUE(multi_only)) paste("Adjusted", effname(app1)) else effname(app1)
  if (has_multi) {
    m    <- read_tb(multi)
    app2 <- approach(multi); eff2 <- effname(app2)
  }

  ref_line <- if (is_ratio(app1)) 1 else 0

  # Build layout from univariate
  u$is_cont <- (u$exposure == u$level)
  blocks <- list()

  if (any(u$is_cont)) {
    cu <- u[u$is_cont, , drop = FALSE]
    cu$..row_type <- "level"
    cu$..label    <- cu$exposure
    blocks[[length(blocks)+1]] <- cu
  }
  for (ex in unique(u$exposure[!u$is_cont])) {
    df  <- u[!u$is_cont & u$exposure == ex, , drop=FALSE]
    hdr <- df[1, , drop=FALSE]
    hdr$level      <- ex
    hdr$estimate   <- hdr$conf.low <- hdr$conf.high <- hdr$p.value <- NA_real_
    hdr$ref        <- FALSE
    hdr$..row_type <- "header"
    hdr$..label    <- ex
    df$..row_type  <- "level"
    df$..label     <- paste0("  ", df$level)
    blocks[[length(blocks)+1]] <- hdr
    blocks[[length(blocks)+1]] <- df
  }
  lay <- dplyr::bind_rows(blocks)

  # Reference rows lie on ref_line (text "--" added later)
  is_ref <- lay$ref & lay$..row_type == "level"
  lay$estimate [is_ref] <- ref_line
  lay$conf.low [is_ref] <- ref_line
  lay$conf.high[is_ref] <- ref_line

  # Align multi
  if (has_multi) {
    key_m <- paste(m$exposure, m$level, sep="||")
    key_l <- paste(lay$exposure, lay$level, sep="||")
    pos   <- match(key_l, key_m)
    est2 <- lo2 <- hi2 <- rep(NA_real_, nrow(lay))
    hit <- which(!is.na(pos))
    est2[hit] <- m$estimate[pos[hit]]
    lo2 [hit] <- m$conf.low[pos[hit]]
    hi2 [hit] <- m$conf.high[pos[hit]]
    est2[is_ref] <- ifelse(is.na(est2[is_ref]), est2[is_ref], ref_line)
    lo2 [is_ref] <- ifelse(is.na(lo2 [is_ref]), lo2 [is_ref], ref_line)
    hi2 [is_ref] <- ifelse(is.na(hi2 [is_ref]), hi2 [is_ref], ref_line)
  }

  # Left table + optional desc merge
  left <- data.frame(
    Characteristic   = lay$..label,
    exposure         = lay$exposure,
    stringsAsFactors = FALSE
  )
  if (!is.null(desc)) {
    left <- .merge_forest_desc(left, desc)
  }
  is_header <- lay$..row_type == "header"
  # CI text cols + BLANK anchor headers
  if (has_multi) {
    left[[" "]] <- ""   # uni anchor
    ci1 <- fmt_ci(lay$estimate, lay$conf.low, lay$conf.high, digits)
    ci1[is_header | is_ref] <- ""              # <-- make headers & refs blank
    ci1[is_ref] <- "--"
    left[[paste0(eff1, " (95% CI)")]] <- ci1

    left[["  "]] <- ""  # adj anchor
    ci2 <- fmt_ci(est2, lo2, hi2, digits)
    ci2[is_header | is_ref] <- ""              # <-- same here
    ci2[is_ref] <- "--"
    left[[paste0("Adjusted ", eff2, " (95% CI)")]] <- ci2
  } else {
    left[[" "]] <- ""
    ci1 <- fmt_ci(lay$estimate, lay$conf.low, lay$conf.high, digits)
    ci1[is_header | is_ref] <- ""              # <-- and here
    ci1[is_ref] <- "--"
    left[[paste0(eff1, " (95% CI)")]] <- ci1
  }


  # Remove helper join key from visible cols
  left <- left[, setdiff(names(left), "exposure"), drop = FALSE]

  # Standard errors (not for display)
  is_header <- lay$..row_type == "header"
  se_uni <- .se_from_ci(lay$estimate, lay$conf.low, lay$conf.high, ratio = is_ratio(app1))
  se_uni[is_header | is_ref] <- NA_real_
  left$se_uni <- se_uni

  if (has_multi) {
    se_adj <- .se_from_ci(est2, lo2, hi2, ratio = is_ratio(app2))
    se_adj[is_header | is_ref] <- NA_real_
    left$se_adj <- se_adj
  }

  # Keep only minimal, plotting-relevant attributes (optional).
  # If you want *no* attributes at all, comment these three lines too.
  attr(left, "est") <- lay$estimate
  attr(left, "lo")  <- lay$conf.low
  attr(left, "hi")  <- lay$conf.high
  if (has_multi) {
    attr(left, "est2") <- est2
    attr(left, "lo2")  <- lo2
    attr(left, "hi2")  <- hi2
  }

  attr(left, "forest_meta") <- list(
    x_trans = if (is_ratio(app1)) "log" else "none",
    ref_line = ref_line
  )

  left
}
