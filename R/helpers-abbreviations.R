#' @keywords internal
#' @noRd
.abbrev_note <- function(approach) {
  approach <- .normalize_approach(approach)
  note <- switch(
    approach,
    "linear"       = "Beta = Linear regression coefficient; CI = Confidence Interval.",
    "logit"        = "OR = Odds Ratio; CI = Confidence Interval.",
    "firth"        = "OR = Odds Ratio from Firth penalized logistic regression; CI = Confidence Interval.",
    "logbinomial" = "RR = Risk Ratio; CI = Confidence Interval.",
    "poisson"      = "IRR = Incidence Rate Ratio; CI = Confidence Interval.",
    "robpoisson"   = "RR = Relative Risk; CI = Confidence Interval.",
    "negbin"       = "IRR = Incidence Rate Ratio; CI = Confidence Interval.",
    "cox"          = "HR = Hazard Ratio; CI = Confidence Interval.",
    "fine_gray"    = paste0(
      "SHR = Subdistribution Hazard Ratio from Fine-Gray regression; ",
      "CI = Confidence Interval."
    ),
    "conditional_logit" = paste0(
      "Matched OR = matched Odds Ratio from conditional logistic regression; ",
      "CI = Confidence Interval."
    ),
    "ordinal_logit" = "OR = common Odds Ratio; CI = Confidence Interval.",
    "multinomial_logit" = "OR = Odds Ratio; CI = Confidence Interval.",
    "survreg"      = "Time Ratio = exponentiated accelerated failure time coefficient; CI = Confidence Interval.",
    "CI = Confidence Interval."
  )
  paste0("Abbreviations: ", note)
}
#' @keywords internal
#' @noRd
.is_ratio <- function(a) {
  .normalize_approach(a) %in% c(
    "logit", "firth", "conditional_logit", "ordinal_logit",
    "multinomial_logit",
    "logbinomial", "poisson",
    "robpoisson", "negbin", "cox", "fine_gray", "survreg"
  )
}
#' @keywords internal
#' @noRd
.get_effect_label <- function(a) switch(.normalize_approach(a),
                                        "linear"       = "Beta (95% CI)",
                                        "logit"        = "OR (95% CI)",
                                        "firth"        = "OR (95% CI)",
                                        "conditional_logit" = "Matched OR (95% CI)",
                                        "ordinal_logit" = "Common OR (95% CI)",
                                        "multinomial_logit" = "OR (95% CI)",
                                        "logbinomial" = "RR (95% CI)",
                                        "poisson"      = "IRR (95% CI)",
                                        "robpoisson"   = "RR (95% CI)",
                                        "negbin"       = "IRR (95% CI)",
                                        "cox"          = "HR (95% CI)",
                                        "fine_gray"    = "SHR (95% CI)",
                                        "survreg"      = "Time Ratio (95% CI)"
)
#' @keywords internal
#' @noRd
.fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format="f", digits=3)))

#' Identify abbreviation footnotes, with or without the "Abbreviations:" prefix
#'
#' Some model families write their abbreviation line without the prefix
#' (for example "OR = common Odds Ratio; CI = Confidence Interval.").
#' @keywords internal
#' @noRd
.is_abbreviation_note <- function(x) {
  x <- as.character(x)
  prefixed <- grepl("^Abbreviations:", x)
  bare <- grepl("^[A-Z][A-Za-z ]{0,30} = ", x) &
    !grepl("^(Ref\\.|N) = ", x) &
    grepl("(Ratio|Interval|coefficient)", x)
  prefixed | bare
}

#' Combine several abbreviation footnotes into one line
#'
#' Exact duplicate definitions are shown once and "CI" is listed last. A
#' single abbreviation line is returned unchanged.
#' @keywords internal
#' @noRd
.combine_abbreviation_notes <- function(notes) {
  notes <- as.character(notes)
  idx <- which(.is_abbreviation_note(notes))
  if (length(idx) < 2L) return(notes)

  items <- unlist(lapply(notes[idx], function(z) {
    z <- sub("^Abbreviations:\\s*", "", z)
    z <- sub("\\.\\s*$", "", z)
    trimws(strsplit(z, ";\\s*")[[1L]])
  }), use.names = FALSE)
  items <- unique(items[nzchar(items)])
  is_ci <- grepl("^CI = ", items)
  items <- c(items[!is_ci], items[is_ci])

  notes[idx[1L]] <- paste0("Abbreviations: ", paste(items, collapse = "; "), ".")
  notes[-idx[-1L]]
}

#' Identify footnotes that report or explain sample size (N)
#'
#' `column_only = TRUE` restricts the match to notes that explain a displayed
#' N column.
#' @keywords internal
#' @noRd
.is_n_note <- function(x, column_only = FALSE) {
  x <- as.character(x)
  column_note <- grepl("^N is the number of complete ", x) |
    grepl("^N reflects complete observations", x)
  if (isTRUE(column_only)) return(column_note)
  column_note |
    grepl("^N = .*complete observations", x) |
    grepl("^Complete observations included by ", x) |
    grepl("complete observations included in the (multivariable )?model", x) |
    grepl("complete observations included in each ", x)
}
