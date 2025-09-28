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
