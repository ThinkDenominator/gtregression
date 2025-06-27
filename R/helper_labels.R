
.get_effect_label <- function(approach) {
  switch(approach,
         "logit" = "**OR**",
         "poisson" = "**IRR**",
         "linear" = "**Beta**",
         "**RR**")
}

.get_effect_label_adjusted <- function(approach) {
  switch(approach,
         "logit" = "**Adjusted OR**",
         "poisson" = "**Adjusted IRR**",
         "linear" = "**Adjusted Beta**",
         "**Adjusted RR**")
}

.get_abbreviation <- function(approach) {
  switch(approach,
         "logit" = "OR = Odds Ratio",
         "log-binomial" = "RR = Relative Risk",
         "robpoisson" = "RR = Relative Risk",
         "poisson" = "IRR = Incidence Rate Ratio",
         "linear" = "Beta = Linear Regression Coefficient, CI = Confidence Interval",
         "RR = Relative Risk")
}

.get_remove_abbreviation <- function(approach) {
  switch(approach,
         "log-binomial" = "RR = Relative Risk",
         "logit" = "OR = Odds Ratio",
         "robpoisson" = "IRR = Incidence Rate Ratio",
         "poisson" = "IRR = Incidence Rate Ratio",
         "linear" = "CI = Confidence Interval",
         "")
}
