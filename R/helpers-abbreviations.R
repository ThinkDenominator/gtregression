#' @keywords internal
#' @noRd
.abbrev_note <- function(approach) {
  approach <- .normalize_approach(approach)
  note <- switch(
    approach,
    "linear"       = "Beta = Linear regression coefficient; CI = Confidence Interval.",
    "logit"        = "OR = Odds Ratio; CI = Confidence Interval.",
    "logbinomial" = "RR = Risk Ratio; CI = Confidence Interval.",
    "poisson"      = "IRR = Incidence Rate Ratio; CI = Confidence Interval.",
    "robpoisson"   = "RR = Relative Risk; CI = Confidence Interval.",
    "negbin"       = "IRR = Incidence Rate Ratio; CI = Confidence Interval.",
    "CI = Confidence Interval."
  )
  paste0("Abbreviations: ", note)
}
#' @keywords internal
#' @noRd
.is_ratio <- function(a) .normalize_approach(a) %in% c("logit","logbinomial","poisson", "robpoisson", "negbin")
#' @keywords internal
#' @noRd
.get_effect_label <- function(a) switch(.normalize_approach(a),
                                        "linear"       = "Beta (95% CI)",
                                        "logit"        = "OR (95% CI)",
                                        "logbinomial" = "RR (95% CI)",
                                        "poisson"      = "IRR (95% CI)",
                                        "robpoisson"   = "RR (95% CI)",
                                        "negbin"       = "IRR (95% CI)"
)
#' @keywords internal
#' @noRd
.fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format="f", digits=3)))
