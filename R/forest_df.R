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

  expo <- attr(td, "row_exposure", exact = TRUE)
  if (!is.null(expo) && length(expo) == nrow(td)) {
    expo <- as.character(expo)
  } else {
    expo <- NULL
  }
  if (is.data.frame(tb) && all(c("var", "type") %in% names(tb))) {
    skeleton <- do.call(rbind, lapply(unique(tb$var), function(var) {
      x <- tb[tb$var == var, , drop = FALSE]
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

  left$..forest_desc_order <- seq_len(nrow(left))
  joined <- dplyr::left_join(left, ddf, by = c("exposure","Characteristic"))
  joined <- joined[order(joined$..forest_desc_order), , drop = FALSE]
  desc_cols <- setdiff(names(ddf), c("Characteristic", "exposure"))
  desc_cols <- desc_cols[desc_cols %in% names(joined)]
  if (!length(desc_cols)) {
    joined$..forest_desc_order <- NULL
    return(joined)
  }

  fallback <- ddf[!duplicated(ddf$exposure), c("exposure", desc_cols), drop = FALSE]
  fallback <- fallback[match(joined$exposure, fallback$exposure), , drop = FALSE]

  for (col in desc_cols) {
    needs_value <- is.na(joined[[col]]) | !nzchar(as.character(joined[[col]]))
    fallback_value <- fallback[[col]]
    has_fallback <- !is.na(fallback_value) & nzchar(as.character(fallback_value))
    continuous_or_header_row <- joined$Characteristic == joined$exposure
    idx <- needs_value & has_fallback & continuous_or_header_row
    joined[[col]][idx] <- fallback_value[idx]
  }

  joined$..forest_desc_order <- NULL
  joined
}

# =========================================
# CLASS-BASED INPUT PICKER (ORDER-AGNOSTIC)
# =========================================
.pick_inputs <- function(uni, multi = NULL, desc = NULL) {
  inputs <- Filter(Negate(is.null), list(uni = uni, multi = multi, desc = desc))

  is_uni   <- function(x) inherits(x, "gtregression") &&
    any(c("uni_reg", "gt_uni", "cox_reg", "gt_cox", "surv_reg", "gt_surv",
          "conditional_logit_reg", "gt_conditional_logit",
          "ft_conditional_logit", "competing_risk_reg",
          "gt_competing_risk", "ft_competing_risk", "multilevel_reg",
          "gt_multilevel", "ft_multilevel", "ordinal_reg",
          "gt_ordinal", "ft_ordinal", "multinomial_outcome") %in% class(x)) &&
    !.is_adjusted_reg_output(x)
  is_multi <- function(x) inherits(x, "gtregression") &&
    (any(c("multi_reg", "gt_multi") %in% class(x)) ||
       (any(c("cox_reg", "gt_cox", "surv_reg", "gt_surv",
                "conditional_logit_reg", "gt_conditional_logit",
                "ft_conditional_logit", "competing_risk_reg",
                "gt_competing_risk", "ft_competing_risk", "multilevel_reg",
                "gt_multilevel", "ft_multilevel", "ordinal_reg",
                "gt_ordinal", "ft_ordinal", "multinomial_outcome") %in% class(x)) &&
          .is_adjusted_reg_output(x)))
  is_desc  <- function(x) any(c("descriptive_table","gt_desc") %in% class(x)) ||
    (is.list(x) && !is.null(x$table_display) && !is.null(x$table_body)) ||
    (is.data.frame(x) && "Characteristic" %in% names(x))

  got_uni <- got_multi <- got_desc <- NULL

  for (nm in names(inputs)) {
    x <- inputs[[nm]]
    if (is_uni(x)) {
      if (!is.null(got_uni)) stop("Two univariate objects supplied.", call. = FALSE)
      got_uni <- x
      next
    }
    if (is_multi(x)) {
      if (!is.null(got_multi)) {
        # Exposure-specific adjusted models next to the single multivariable
        # model of the same family: the exposure-specific result is drawn
        # first and the multivariable model second.
        pair <- .forest_adjusted_pair(got_multi, x)
        if (is.null(pair) || !is.null(got_uni)) {
          stop("Two multivariable objects supplied.", call. = FALSE)
        }
        got_uni <- pair$first
        got_multi <- pair$second
        next
      }
      got_multi <- x
      next
    }
    if (is_desc(x)) {
      if (!is.null(got_desc)) stop("Two descriptive tables supplied.", call. = FALSE)
      got_desc <- x
      next
    }
    stop("Unknown input type for argument '", nm, "'.", call. = FALSE)
  }

  list(uni = got_uni, multi = got_multi, desc = got_desc)
}

# A result fitted as one model containing every exposure.
.forest_is_single_model <- function(x) {
  isTRUE(x$multivariable) ||
    (identical(x$source, "multi_reg") && !isTRUE(x$adjusted_mode))
}

# Two adjusted results that can be shown side by side: exposure-specific
# adjusted models and the single multivariable model of the same family.
.forest_adjusted_pair <- function(a, b) {
  same_family <- !is.null(a$source) && identical(a$source, b$source)
  if (!same_family) return(NULL)
  single_a <- .forest_is_single_model(a)
  single_b <- .forest_is_single_model(b)
  if (single_a == single_b) return(NULL)
  if (single_a) list(first = b, second = a) else list(first = a, second = b)
}

# The effect column header shown in a result's own table, for example
# "Matched OR (95% CI)" or "Adjusted common OR (95% CI)".
.forest_effect_header <- function(x) {
  td <- tryCatch(x$table_display, error = function(e) NULL)
  if (!is.data.frame(td)) return(NULL)
  hits <- grep("\\(95% CI\\)$", names(td), value = TRUE)
  if (length(hits) == 1L) hits else NULL
}

.is_stratified_forest_input <- function(x) {
  inherits(x, "gtregression") &&
    !is.null(x$per_stratum) &&
    any(c(
      "stratified_uni_reg",
      "stratified_multi_reg",
      "stratified_cox_reg",
      "stratified_surv_reg"
    ) %in% class(x))
}

.forest_or <- function(a, b) {
  if (!is.null(a)) a else b
}

.forest_stratum_object <- function(parent, x) {
  if (inherits(x, "gtregression")) {
    return(x)
  }

  out <- x
  out$approach <- parent$approach
  out$variable_labels <- parent$variable_labels
  out$adjusted_mode <- .forest_or(parent$adjusted_mode, x$adjusted_mode)
  out$multivariable <- .forest_or(parent$multivariable, x$multivariable)

  if (inherits(parent, "stratified_multi_reg")) {
    class(out) <- c("gtregression", "multi_reg", "gt_multi", class(out))
  } else if (inherits(parent, "stratified_cox_reg")) {
    class(out) <- c("gtregression", "cox_reg", "gt_cox", class(out))
  } else if (inherits(parent, "stratified_surv_reg")) {
    class(out) <- c("gtregression", "surv_reg", "gt_surv", class(out))
  }

  out
}

.forest_df_from_stratified <- function(x, digits = 2) {
  strata <- x$per_stratum
  if (!length(strata)) {
    stop("`stratified` regression objects must contain at least one stratum.", call. = FALSE)
  }

  labels <- names(strata)
  if (is.null(labels) || any(!nzchar(labels))) {
    labels <- paste0("Stratum ", seq_along(strata))
  }

  stratifier <- .forest_or(x$by, "Stratum")
  parts <- lapply(strata, function(item) {
    stratum_object <- .forest_stratum_object(x, item)
    forest_df(stratum_object, digits = digits)
  })

  first <- parts[[1]]
  text_cols_first <- grep("\\(95% CI\\)$", names(first), value = TRUE)
  if (length(text_cols_first) != 1L) {
    stop(
      "Stratified forest plots expect one effect column per stratum. ",
      "Use a single stratified regression object created by the package stratified functions.",
      call. = FALSE
    )
  }

  helper_cols <- c("se_uni", "se_adj")
  anchor_cols_first <- names(first)[grepl("^ +$", names(first))]
  left_cols <- setdiff(names(first), c(text_cols_first, anchor_cols_first, helper_cols))
  out <- first[, left_cols, drop = FALSE]

  est_list <- lo_list <- hi_list <- vector("list", length(parts))
  anchor_names <- character(length(parts))
  text_names <- character(length(parts))

  for (i in seq_along(parts)) {
    part <- parts[[i]]
    text_cols <- grep("\\(95% CI\\)$", names(part), value = TRUE)
    if (length(text_cols) != 1L) {
      stop(
        "Stratified forest plots expect one effect column per stratum. ",
        "Use a single stratified regression object created by the package stratified functions.",
        call. = FALSE
      )
    }

    effect_label <- text_cols[[1]]
    text_name <- paste0(labels[[i]], "\n", effect_label)
    anchor_name <- paste(rep(" ", i), collapse = "")

    out[[text_name]] <- part[[effect_label]]
    out[[anchor_name]] <- ""

    est_list[[i]] <- attr(part, "est", exact = TRUE)
    lo_list[[i]] <- attr(part, "lo", exact = TRUE)
    hi_list[[i]] <- attr(part, "hi", exact = TRUE)
    anchor_names[[i]] <- anchor_name
    text_names[[i]] <- text_name
  }

  if ("se_uni" %in% names(first)) {
    out$se_uni <- first$se_uni
  }

  # Keep the old single/two-model attributes for backward compatibility, while
  # also exposing a general list for side-by-side stratified plots.
  attr(out, "est") <- est_list[[1]]
  attr(out, "lo") <- lo_list[[1]]
  attr(out, "hi") <- hi_list[[1]]
  if (length(est_list) >= 2L) {
    attr(out, "est2") <- est_list[[2]]
    attr(out, "lo2") <- lo_list[[2]]
    attr(out, "hi2") <- hi_list[[2]]
  }
  attr(out, "forest_estimates") <- list(
    est = est_list,
    lo = lo_list,
    hi = hi_list,
    anchors = anchor_names,
    text_cols = text_names
  )

  meta <- .forest_or(attr(out, "forest_meta", exact = TRUE), list())
  first_meta <- attr(first, "forest_meta", exact = TRUE)
  if (!is.null(first_meta$x_trans)) meta$x_trans <- first_meta$x_trans
  if (!is.null(first_meta$ref_line)) meta$ref_line <- first_meta$ref_line
  meta$stratified <- TRUE
  meta$stratifier <- stratifier
  meta$strata <- labels
  meta$side_by_side_strata <- TRUE
  meta$effect_sets <- list(anchors = anchor_names, text_cols = text_names)
  attr(out, "forest_meta") <- meta
  out
}

#' Build a compatible data frame for forest plots
#'
#' Creates the tabular input used by \code{forest_reg()} from \code{uni_reg()},
#' \code{multi_reg()}, and optionally \code{descriptive_table()} objects. This
#' function is useful when users want to inspect, edit, or reuse the exact data
#' that will be passed to the forest plot. Stratified regression objects are
#' also supported when supplied one at a time; the returned data frame keeps the
#' characteristic rows once and places strata side by side as separate effect
#' columns for \code{forest_reg()}.
#'
#' @param uni A \code{gtregression} regression object, usually the crude
#'   results (for example \code{uni_reg()}, \code{cox_reg()},
#'   \code{conditional_logit_reg()} or \code{ordinal_reg()}). If \code{multi}
#'   is supplied without \code{uni}, the adjusted object is plotted as a single
#'   effect column.
#' @param multi Optional adjusted object of the same model family, for example
#'   \code{multi_reg()} or a model fitted with \code{multivariable = TRUE}.
#'   Exposure-specific adjusted results (\code{adjust_for}) and the single
#'   multivariable model can also be shown together; the second column is
#'   then labelled "Mutually adjusted".
#' @param desc Optional descriptive table object from \code{descriptive_table()}.
#' @param digits Number of digits used when formatting confidence intervals.
#'
#' @details Effect columns use the same header as the source tables, for
#'   example "Matched OR (95\% CI)" for \code{conditional_logit_reg()} and
#'   "Common OR (95\% CI)" for \code{ordinal_reg()}, and the estimates and
#'   intervals are taken unchanged from each result's \code{table_body}.
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
forest_df <- function(uni = NULL, multi = NULL, desc = NULL, digits = 2) {
  .rd_reject(list(uni, multi, desc), "forest_df")
  if (is.null(uni) && is.null(multi) && is.null(desc)) {
    stop("Supply at least one result to `uni`, `multi` or `desc`.", call. = FALSE)
  }
  if (inherits(uni, "multinomial_reg") || inherits(multi, "multinomial_reg")) {
    stop(
      "A multinomial model has multiple outcome comparisons. Pass one ",
      "`$per_outcome` component to `forest_df()`.",
      call. = FALSE
    )
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }
  digits <- as.integer(digits)

  inputs_raw <- list(uni = uni, multi = multi, desc = desc)
  supplied <- Filter(Negate(is.null), inputs_raw)
  stratified_inputs <- Filter(.is_stratified_forest_input, supplied)
  if (length(stratified_inputs)) {
    if (length(stratified_inputs) > 1L || length(supplied) > 1L) {
      stop(
        "forest_df() supports one stratified regression object at a time. ",
        "Do not combine stratified objects with `multi`, `desc`, or another stratified object.",
        call. = FALSE
      )
    }
    return(.forest_df_from_stratified(stratified_inputs[[1]], digits = digits))
  }

  `%||%`   <- function(a,b) if (!is.null(a)) a else b
  approach <- function(o) .normalize_approach(o$approach %||% "logit")
  is_ratio <- function(a) .normalize_approach(a) %in% c(
    "logit", "firth", "conditional_logit", "ordinal_logit",
    "multinomial_logit",
    "logbinomial", "poisson",
    "robpoisson", "negbin",
                                   "cox", "fine_gray", "survreg", "margstd_boot","margstd_delta")
  effname  <- function(a) {
    a <- .normalize_approach(a)
    if (a == "linear") return("Beta")
    if (a == "cox") return("HR")
    if (a == "fine_gray") return("SHR")
    if (a == "survreg") return("Time Ratio")
    if (a %in% c("poisson","negbin")) return("IRR")
    if (a %in% c("logbinomial","robpoisson","margstd_boot","margstd_delta")) return("RR")
    if (a == "conditional_logit") return("Matched OR")
    if (a == "ordinal_logit") return("Common OR")
    "OR"
  }
  adjusted_effname <- function(a) {
    e <- effname(a)
    if (e %in% c("Matched OR", "Common OR")) e <- paste0(tolower(substr(e, 1, 1)), substring(e, 2))
    paste("Adjusted", e)
  }
  fmt_ci <- function(e,l,h,d){
    out <- rep("", length(e))
    fmt_num <- function(x) {
      ifelse(
        is.na(x), "",
        ifelse(
          is.infinite(x), ifelse(x > 0, "\u221e", "-\u221e"),
          sprintf(paste0("%.", d, "f"), x)
        )
      )
    }
    ok <- !is.na(e) & !is.na(l) & !is.na(h)
    out[ok] <- paste0(
      fmt_num(e[ok]), " (", fmt_num(l[ok]), "\u2013", fmt_num(h[ok]), ")"
    )
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
  .se_from_ci <- function(est, lo, hi, ratio = TRUE, crit = stats::qnorm(0.975)) {
    se <- rep(NA_real_, length(est))
    crit <- rep_len(crit, length(est))
    ok <- is.finite(est) & is.finite(lo) & is.finite(hi) & is.finite(crit)
    if (ratio) se[ok] <- (log(hi[ok]) - log(lo[ok])) / (2 * crit[ok])
    else       se[ok] <- (hi[ok] - lo[ok]) / (2 * crit[ok])
    se
  }
  # Critical value used for the 95% CI of each row. Linear models (`lm`)
  # report t-based intervals, so the standard error must be recovered with
  # the t quantile on the residual degrees of freedom of the model that
  # produced the row; every other approach uses Wald (normal) intervals.
  .ci_crit <- function(x, exposure) {
    z <- stats::qnorm(0.975)
    out <- rep(z, length(exposure))
    models <- tryCatch(x$models, error = function(e) NULL)
    if (!is.list(models) || !length(models)) return(out)
    one <- function(fit) {
      if (!inherits(fit, "lm") || inherits(fit, "glm")) return(z)
      df_resid <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)
      if (length(df_resid) == 1L && is.finite(df_resid) && df_resid > 0) {
        return(stats::qt(0.975, df = df_resid))
      }
      z
    }
    crit <- vapply(models, one, numeric(1))
    if (length(crit) == 1L) return(rep(unname(crit), length(exposure)))
    pos <- match(as.character(exposure), names(models))
    out[!is.na(pos)] <- unname(crit[pos[!is.na(pos)]])
    out
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
  eff1 <- if (isTRUE(multi_only)) adjusted_effname(app1) else effname(app1)
  if (has_multi) {
    m    <- read_tb(multi)
    app2 <- approach(multi); eff2 <- effname(app2)
  }

  ref_line <- if (is_ratio(app1)) 1 else 0

  # Build the layout from the crude table; rows that exist only in the
  # adjusted table (an extra level, interaction rows, extra variables) are
  # placed inside their own variable block instead of being dropped.
  u$..adj_only <- FALSE
  exposures <- unique(as.character(u$exposure))
  if (has_multi) exposures <- unique(c(exposures, as.character(m$exposure)))
  blocks <- list()

  for (ex in exposures) {
    df <- u[u$exposure == ex, , drop = FALSE]
    is_factor <- any(df$ref %in% TRUE)

    if (has_multi) {
      mx <- m[m$exposure == ex, , drop = FALSE]
      is_factor <- is_factor || any(mx$ref %in% TRUE)
      for (i in seq_len(nrow(mx))) {
        if (as.character(mx$level[i]) %in% as.character(df$level)) next
        extra <- mx[i, , drop = FALSE]
        extra$estimate <- extra$conf.low <- extra$conf.high <- extra$p.value <- NA_real_
        extra$ref <- FALSE
        extra$..adj_only <- TRUE
        anchor <- if (i > 1L) match(as.character(mx$level[i - 1L]), as.character(df$level)) else 0L
        if (is.na(anchor)) anchor <- nrow(df)
        df <- rbind(
          df[seq_len(anchor), , drop = FALSE],
          extra[, names(df), drop = FALSE],
          df[seq_len(nrow(df) - anchor) + anchor, , drop = FALSE]
        )
      }
    }

    is_main <- as.character(df$exposure) == as.character(df$level)
    if (!is_factor && any(is_main)) {
      # continuous / main-effect row first, extra (interaction) rows indented
      df <- rbind(df[is_main, , drop = FALSE], df[!is_main, , drop = FALSE])
      df$..row_type <- "level"
      df$..label <- ifelse(
        as.character(df$exposure) == as.character(df$level),
        as.character(df$exposure),
        paste0("  ", df$level)
      )
      blocks[[length(blocks) + 1L]] <- df
    } else {
      hdr <- df[1, , drop = FALSE]
      hdr$level <- ex
      hdr$estimate <- hdr$conf.low <- hdr$conf.high <- hdr$p.value <- NA_real_
      hdr$ref <- FALSE
      hdr$..adj_only <- FALSE
      hdr$..row_type <- "header"
      hdr$..label <- ex
      df$..row_type <- "level"
      df$..label <- paste0("  ", df$level)
      blocks[[length(blocks) + 1L]] <- hdr
      blocks[[length(blocks) + 1L]] <- df
    }
  }
  lay <- dplyr::bind_rows(blocks)
  lay$..row_order <- seq_len(nrow(lay))

  # Reference rows lie on ref_line (text "Ref." added later)
  is_ref <- lay$ref & lay$..row_type == "level"
  lay$estimate [is_ref] <- ref_line
  lay$conf.low [is_ref] <- ref_line
  lay$conf.high[is_ref] <- ref_line

  # Align multi
  if (has_multi) {
    key_m <- paste(m$exposure, m$level, sep="||")
    key_l <- paste(lay$exposure, lay$level, sep="||")
    pos   <- match(key_l, key_m)
    pos[lay$..row_type == "header"] <- NA_integer_
    est2 <- lo2 <- hi2 <- rep(NA_real_, nrow(lay))
    hit <- which(!is.na(pos))
    has_adj_row <- !is.na(pos)
    est2[hit] <- m$estimate[pos[hit]]
    lo2 [hit] <- m$conf.low[pos[hit]]
    hi2 [hit] <- m$conf.high[pos[hit]]
    is_ref_adj <- rep(FALSE, nrow(lay))
    is_ref_adj[hit] <- m$ref[pos[hit]] %in% TRUE
    est2[is_ref_adj] <- ref_line
    lo2 [is_ref_adj] <- ref_line
    hi2 [is_ref_adj] <- ref_line
  }

  # Effect column headers follow each result's own table header, so a forest
  # plot names the estimate exactly as the table does.
  header1 <- .forest_effect_header(uni)
  if (is.null(header1)) header1 <- paste0(eff1, " (95% CI)")
  if (has_multi) {
    header2 <- .forest_effect_header(multi)
    if (is.null(header2)) header2 <- paste0(adjusted_effname(app2), " (95% CI)")
    if (identical(header1, header2)) {
      header2 <- if (grepl("^Adjusted ", header2)) {
        sub("^Adjusted ", "Mutually adjusted ", header2)
      } else {
        paste("Mutually adjusted", header2)
      }
    }
  }

  # Left table + optional desc merge
  forest_blank <- paste(rep(" ", 20), collapse = " ")
  label_map <- uni$variable_labels
  if (has_multi) {
    extra_labels <- multi$variable_labels
    extra_labels <- extra_labels[setdiff(names(extra_labels), names(label_map))]
    if (length(extra_labels)) label_map <- c(label_map, extra_labels)
  }
  left <- data.frame(
    Characteristic   = lay$..label,
    exposure         = lay$exposure,
    ..row_order      = lay$..row_order,
    stringsAsFactors = FALSE
  )
  if (!is.null(desc)) {
    left <- .merge_forest_desc(left, desc)
  }
  left <- left[order(left$..row_order), , drop = FALSE]
  label_idx <- lay$..row_type == "header" | (lay$exposure == lay$level)
  left$Characteristic[label_idx] <- vapply(
    lay$exposure[label_idx],
    .label_var,
    character(1),
    label_map = label_map
  )
  # Level labels follow the labels shown in the source tables, so levels
  # relabelled with modify_table(level_labels = ) keep their new text.
  level_idx <- which(lay$..row_type == "level" & lay$exposure != lay$level)
  if (length(level_idx)) {
    shown_level <- function(obj, ex, level) {
      if (is.null(obj) || is.null(obj$table_display)) return(NA_character_)
      rows <- obj$table_body[obj$table_body$exposure == ex, , drop = FALSE]
      if (!nrow(rows)) return(NA_character_)
      labels <- tryCatch(.plot_display_labels(obj, ex, rows), error = function(e) NULL)
      if (is.null(labels)) return(NA_character_)
      unname(labels$levels[level])
    }
    for (i in level_idx) {
      ex_i <- as.character(lay$exposure[i])
      level_i <- as.character(lay$level[i])
      shown <- shown_level(uni, ex_i, level_i)
      if (is.na(shown) && has_multi) shown <- shown_level(multi, ex_i, level_i)
      if (!is.na(shown) && nzchar(shown)) {
        left$Characteristic[i] <- paste0("  ", shown)
      }
    }
  }
  is_header <- lay$..row_type == "header"
  # CI text cols + BLANK anchor headers
  if (has_multi) {
    left[[" "]] <- forest_blank   # uni anchor
    ci1 <- fmt_ci(lay$estimate, lay$conf.low, lay$conf.high, digits)
    ci1[is_header | is_ref] <- ""              # <-- make headers & refs blank
    ci1[is_ref] <- "Ref."
    left[[header1]] <- ci1

    left[["  "]] <- forest_blank  # adj anchor
    ci2 <- fmt_ci(est2, lo2, hi2, digits)
    ci2[is_header | is_ref_adj] <- ""
    ci2[is_ref_adj] <- "Ref."
    left[[header2]] <- ci2
  } else {
    left[[" "]] <- forest_blank
    ci1 <- fmt_ci(lay$estimate, lay$conf.low, lay$conf.high, digits)
    ci1[is_header | is_ref] <- ""              # <-- and here
    ci1[is_ref] <- "Ref."
    left[[header1]] <- ci1
  }


  # Remove helper join key from visible cols
  left <- left[, setdiff(names(left), c("exposure", "..row_order")), drop = FALSE]

  # Standard errors (not for display)
  is_header <- lay$..row_type == "header"
  se_uni <- .se_from_ci(lay$estimate, lay$conf.low, lay$conf.high,
                        ratio = is_ratio(app1),
                        crit = .ci_crit(uni, lay$exposure))
  se_uni[is_header | is_ref] <- NA_real_
  left$se_uni <- se_uni

  if (has_multi) {
    se_adj <- .se_from_ci(est2, lo2, hi2, ratio = is_ratio(app2),
                          crit = .ci_crit(multi, lay$exposure))
    se_adj[is_header | is_ref_adj] <- NA_real_
    left$se_adj <- se_adj
  }

  display_cols <- setdiff(names(left), c("se_uni", "se_adj"))
  for (col in display_cols) {
    left[[col]] <- as.character(left[[col]])
    left[[col]][is.na(left[[col]])] <- ""
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

  attr(left, "forest_estimates") <- if (has_multi) {
    list(
      est = list(lay$estimate, est2), lo = list(lay$conf.low, lo2),
      hi = list(lay$conf.high, hi2), anchors = c(" ", "  "),
      text_cols = c(header1, header2)
    )
  } else {
    list(
      est = list(lay$estimate), lo = list(lay$conf.low), hi = list(lay$conf.high),
      anchors = " ", text_cols = header1
    )
  }

  attr(left, "forest_meta") <- list(
    x_trans = if (is_ratio(app1)) "log" else "none",
    ref_line = ref_line
  )

  left
}
