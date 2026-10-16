#' Compose footnotes for univariate strata
#' @keywords internal
.footnotes_uni_strata <- function(approach) {
  c(.abbrev_note(approach))
}

#' Describe the stratification variable once without repeating it in headings
#' @keywords internal
#' @noRd
.stratified_by_note <- function(stratifier, label = NULL) {
  # `label`: display label of the stratifier (its "label" attribute), when
  # available; otherwise the variable name is shown.
  shown <- stratifier
  if (is.character(label) && length(label) == 1L && !is.na(label) &&
      nzchar(label)) {
    shown <- unname(label)
  }
  paste0("Stratified by: ", shown, ".")
}

#' Display label of a stratifier variable (falls back to its name)
#' @keywords internal
#' @noRd
.stratifier_label <- function(data, stratifier) {
  out <- tryCatch(
    unname(.var_label_map(data, stratifier)[[1L]]),
    error = function(e) NULL
  )
  if (is.character(out) && length(out) == 1L && !is.na(out) && nzchar(out)) {
    out
  } else {
    stratifier
  }
}

#' Compose footnotes for multivariable strata (abbrev + per-stratum N)
#' @keywords internal
.footnotes_multi_strata <- function(approach, fits_by_stratum, stratifier) {
  n_lines <- vapply(names(fits_by_stratum), function(k) {
    n_used <- tryCatch(stats::nobs(fits_by_stratum[[k]]), error = function(e) NA_integer_)
    if (!is.na(n_used)) {
      paste0(stratifier, " = ", k, ": N = ", n_used,
             " complete observations included in the multivariable model")
    } else {
      paste0(stratifier, " = ", k, ": complete observations included in the multivariable model")
    }
  }, character(1))
  c(.abbrev_note(approach), n_lines)
}
#' @keywords internal
#' @noRd
.ref_note <- function() {
  "Ref. = reference category."
}

#' @keywords internal
#' @noRd
.validate_show_ref <- function(show_ref) {
  if (!is.logical(show_ref) || length(show_ref) != 1L || is.na(show_ref)) {
    stop("`show_ref` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(show_ref)
}

#' Determine whether a reference row remains visible
#' @keywords internal
#' @noRd
.has_visible_reference <- function(table_body, show_ref) {
  if (is.null(table_body) ||
      !all(c("exposure", "ref") %in% names(table_body)) ||
      !any(table_body$ref %in% TRUE, na.rm = TRUE)) {
    return(FALSE)
  }

  if (isTRUE(show_ref)) {
    return(TRUE)
  }

  groups <- split(table_body, table_body$exposure)
  any(vapply(groups, function(x) {
    levels <- unique(x$level[!is.na(x$level)])
    any(x$ref %in% TRUE, na.rm = TRUE) && length(levels) > 2L
  }, logical(1)))
}

#' Determine whether binary reference rows are compacted
#' @keywords internal
#' @noRd
.has_hidden_binary_reference <- function(table_body, show_ref) {
  if (isTRUE(show_ref) ||
      is.null(table_body) ||
      !all(c("exposure", "ref") %in% names(table_body))) {
    return(FALSE)
  }

  groups <- split(table_body, table_body$exposure)
  any(vapply(groups, function(x) {
    levels <- unique(x$level[!is.na(x$level)])
    length(levels) == 2L && any(x$ref %in% TRUE, na.rm = TRUE)
  }, logical(1)))
}

#' @keywords internal
#' @noRd
.message_hidden_ref_rows <- function(function_name, table_body, show_ref) {
  if (!.has_hidden_binary_reference(table_body, show_ref)) {
    return(invisible(FALSE))
  }

  message(
    function_name,
    "(): reference rows are hidden for binary variables. ",
    "Reference rows are retained for categorical variables with more than two levels. ",
    "To display Ref., use `show_ref = TRUE`."
  )

  invisible(TRUE)
}
#' @keywords internal
#' @noRd
.adjustment_note <- function(adjust_for) {
  adjust_for <- unname(adjust_for)

  if (length(adjust_for) == 1) {
    return(paste0("Adjusted for ", adjust_for))
  }

  if (length(adjust_for) == 2) {
    return(paste0("Adjusted for ", adjust_for[1], " and ", adjust_for[2]))
  }

  paste0(
    "Adjusted for ",
    paste(adjust_for[-length(adjust_for)], collapse = ", "),
    ", and ",
    adjust_for[length(adjust_for)]
  )
}

#' Compose an adjustment note using displayed variable labels
#' @keywords internal
#' @noRd
.display_adjustment_note <- function(adjust_for, variable_labels = NULL) {
  display_vars <- vapply(
    adjust_for,
    .label_var,
    character(1),
    label_map = variable_labels
  )
  .adjustment_note(display_vars)
}

#' @keywords internal
#' @noRd
.interaction_note <- function(interaction) {
  paste0("Model includes interaction term: ", interaction)
}
#' @keywords internal
#' @noRd
.n_note_multi_strata <- function(stratifier, n_by_stratum) {
  if (!length(n_by_stratum)) {
    return("N reflects complete observations used in each stratum-specific model")
  }

  # Each element holds the N of every model fitted in the stratum. Values may
  # arrive as numbers or as text (including an already formatted range such
  # as "79-97"), so every integer found is used and nothing is coerced with
  # a warning.
  count_values <- function(x) {
    if (is.list(x)) x <- unlist(x, use.names = FALSE)
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x)) {
      x <- x[is.finite(x)]
      return(unique(round(x)))
    }
    x <- as.character(x)
    x <- x[!is.na(x)]
    found <- unlist(regmatches(x, gregexpr("[0-9][0-9,]*", x)), use.names = FALSE)
    found <- gsub(",", "", found, fixed = TRUE)
    found <- found[nzchar(found)]
    unique(as.numeric(found))
  }
  show <- function(v) format(v, big.mark = "", scientific = FALSE, trim = TRUE)

  parts <- vapply(names(n_by_stratum), function(stratum) {
    values <- count_values(n_by_stratum[[stratum]])
    if (!length(values)) return(paste0(stratum, ": N unavailable"))
    if (length(values) == 1L) {
      return(paste0(stratum, ": N = ", show(values)))
    }
    paste0(stratum, ": N = ", show(min(values)), "\u2013", show(max(values)),
           " across models")
  }, character(1))

  paste0(
    "Complete observations included by ",
    stratifier,
    " stratum: ",
    paste(parts, collapse = "; ")
  )
}
