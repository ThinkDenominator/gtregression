#' Compose footnotes for univariate strata
#' @keywords internal
.footnotes_uni_strata <- function(approach) {
  c(.abbrev_note(approach))
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

  parts <- paste0(
    names(n_by_stratum),
    ": N = ",
    unlist(n_by_stratum, use.names = FALSE)
  )

  paste0(
    "Complete observations included by ",
    stratifier,
    " stratum: ",
    paste(parts, collapse = "; ")
  )
}
