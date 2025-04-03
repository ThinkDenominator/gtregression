#' Epilepsy Treatment and Seizure Counts (`epilepsy`)
#'
#' A clinical dataset from a randomized trial investigating the effect of a drug on the number of seizures in patients with epilepsy.
#' Contains repeated measures data with treatment groups, baseline seizure counts, and follow-up counts.
#'
#' @format A data frame with 236 observations and 9 variables:
#' \describe{
#'   \item{y}{Number of seizures in a 2-week period (count)}
#'   \item{trt}{Treatment group (factor): \code{placebo} or \code{progabide}}
#'   \item{base}{Seizure count during baseline period (numeric)}
#'   \item{age}{Age of patient (numeric)}
#'   \item{V4}{Indicator for 4th visit (binary)}
#'   \item{subject}{Patient ID (factor)}
#'   \item{period}{Follow-up period number (integer)}
#'   \item{visit}{Visit number (factor)}
#'   \item{time}{Time in weeks (numeric)}
#' }
#'
#' @source \pkg{MASS} package. Original data from Thall and Vail (1990), *Biometrics*.
"epilepsy"
