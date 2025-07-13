#' Get Unadjusted Effect Label
#'
#' Returns the appropriate label (e.g., OR, IRR, RR, Beta) for unadjusted regression estimates.
#'
#' @param approach A character string for the regression approach. One of `"logit"`, `"log-binomial"`, `"poisson"`, `"robpoisson"`, `"linear"`.
#'
#' @return A character string for the effect label, formatted with markdown (e.g., `"**OR**"`).
#' @keywords internal
.get_effect_label <- function(approach) {
  switch(approach,
    "logit" = "**OR**",
    "poisson" = "**IRR**",
    "linear" = "**Beta**",
    "**RR**"
  )
}

#' Get Adjusted Effect Label
#'
#' Returns a markdown-formatted label for adjusted estimates (e.g., Adjusted OR).
#'
#' @param approach A character string for the regression approach.
#'
#' @return A character string label (e.g., `"**Adjusted IRR**"`).
#' @keywords internal

.get_effect_label_adjusted <- function(approach) {
  switch(approach,
    "logit" = "**Adjusted OR**",
    "poisson" = "**Adjusted IRR**",
    "linear" = "**Adjusted Beta**",
    "**Adjusted RR**"
  )
}

#' Get Abbreviation Explanation
#'
#' Returns a plain-language abbreviation string for the regression approach.
#'
#' @param approach A character string for the regression approach.
#'
#' @return A character string explaining the abbreviation (e.g., `"OR = Odds Ratio"`).
#' @keywords internal

.get_abbreviation <- function(approach) {
  switch(approach,
    "logit" = "OR = Odds Ratio",
    "log-binomial" = "RR = Relative Risk",
    "robpoisson" = "RR = Relative Risk",
    "poisson" = "IRR = Incidence Rate Ratio",
    "linear" = "Beta = Linear Regression Coefficient, CI = Confidence Interval",
    "RR = Relative Risk"
  )
}

#' Get Abbreviation to Remove
#'
#' Identifies which abbreviation string should be removed from the summary table,
#' if applicable for the given approach.
#'
#' @param approach A character string for the regression approach.
#'
#' @return A character string indicating the abbreviation to remove, or `""` if none.
#' @keywords internal

.get_remove_abbreviation <- function(approach) {
  switch(approach,
    "log-binomial" = "RR = Relative Risk",
    "logit" = "OR = Odds Ratio",
    "robpoisson" = "IRR = Incidence Rate Ratio",
    "poisson" = "IRR = Incidence Rate Ratio",
    "linear" = "CI = Confidence Interval",
    ""
  )
}
