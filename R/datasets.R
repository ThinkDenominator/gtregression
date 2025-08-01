#' Lung Cancer Trial Data
#'
#' Survival data from a clinical trial of lung cancer patients conducted
#'  by the Veteran's Administration.
#'
#' @format A data frame with 137 observations and 8 variables:
#' \describe{
#'   \item{trt}{Treatment group (1 = standard, 2 = test)}
#'   \item{celltype}{Cell type (squamous, smallcell, adeno, large)}
#'   \item{time}{Survival time (in days)}
#'   \item{status}{Censoring status (1 = died, 0 = censored)}
#'   \item{karno}{Karnofsky performance score (higher = better)}
#'   \item{diagtime}{Months from diagnosis to randomization}
#'   \item{age}{Age in years}
#'   \item{prior}{Prior therapy (0 = no, 10 = yes)}
#' }
#' @source \url{https://CRAN.R-project.org/package=survival}
#' @references Kalbfleisch JD and Prentice RL (1980).
#'  The Statistical Analysis of Failure Time Data.
"data_lungcancer"

#' Infertility Matched Case-Control Study
#'
#' investigating the relationship between infertility and abortions.
#'
#' @format A data frame with 248 observations and 8 variables:
#' \describe{
#'   \item{education}{Education level (0 = 0–5 years, 1 = 6–11 years,
#'   2 = 12+ years)}
#'   \item{age}{Age in years}
#'   \item{parity}{Number of prior pregnancies}
#'   \item{induced}{Number of induced abortions}
#'   \item{case}{Infertility case status (1 = case, 0 = control)}
#'   \item{spontaneous}{Number of spontaneous abortions}
#'   \item{stratum}{Matched set ID}
#'   \item{pooled.stratum}{Pooled stratum ID used for conditional regression}
#' }
#' @source
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/infert.html}
"data_infertility"


#' Epilepsy Treatment and Seizure Counts
#'
#' RCT on the effect of a drug on the seizures in patients with epilepsy.
#' Contains repeated measures data with treatment groups,
#' baseline seizure counts, and follow-up counts.
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
#'   \item{lbase}{Log of baseline seizures (numeric)}
#'   \item{lage}{Log of age (numeric)}
#' }
#'
#' @source \pkg{MASS} package. Original data from Thall and Vail (1990)
"data_epilepsy"


#' Birth Weight Data
#'
#' A dataset from the \pkg{MASS} package containing risk factors
#' associated with low birth weight (LBW) in newborns.
#' Originally collected at Baystate Medical Center,
#' Springfield, Massachusetts, USA.
#'
#' The outcome variable is binary (`low`): birth weight < 2500g (yes = 1)
#' or not (no = 0).
#'
#' @format A data frame with 189 observations and 10 variables:
#' \describe{
#'   \item{low}{Indicator for birth weight < 2500g (binary):
#'   \code{0 = normal}, \code{1 = low birth weight}}
#'   \item{age}{Mother's age in years (numeric)}
#'   \item{lwt}{Mother's weight in pounds at last menstrual period (numeric)}
#'   \item{race}{Mother's race (factor): \code{1 = White}, \code{2 = Black},
#'   \code{3 = Other}}
#'   \item{smoke}{Smoking status during pregnancy (binary): \code{0 = No},
#'    \code{1 = Yes}}
#'   \item{ptl}{Number of previous premature labors (integer)}
#'   \item{ht}{History of hypertension (binary): \code{0 = No}, \code{1 = Yes}}
#'   \item{ui}{Presence of uterine irritability (binary): \code{0 = No},
#'   \code{1 = Yes}}
#'   \item{ftv}{no of physician visits during the 1st trimester (integer, 0–6)}
#'   \item{bwt}{Birth weight in grams (numeric)}
#' }
#'
#' @source Hosmer, D.W., Lemeshow, S. (1989). *Applied Logistic Regression.*
#' New York: Wiley.
#' Also available in \pkg{MASS} and described in detail in its documentation.
"data_birthwt"


#' Student Absenteeism in Rural Schools
#'
#' This dataset contains observations on the number of days absent from school
#' for children in rural Australia,
#' along with student characteristics. It's commonly used to demonstrate count
#' models such as Poisson and Negative Binomial regression.
#'
#' @format A data frame with 146 observations and 5 variables:
#' \describe{
#'   \item{Eth}{Ethnicity (\code{"A"} = Aboriginal,
#'   \code{"N"} = Non-Aboriginal)}
#'   \item{Sex}{Sex (\code{"F"} or \code{"M"})}
#'   \item{Age}{Age group (\code{"F0", "F1", "F2", "F3"})}
#'   \item{Lrn}{Learner status (\code{"AL"} = average learner,
#'    \code{"SL"} = slow learner)}
#'   \item{Days}{Number of days absent from school (count outcome)}
#' }
#'
#' @source \pkg{MASS} package. See also Venables and Ripley (2002),
#'  *Modern Applied Statistics with S*.
#'
"data_gt_quin"

#' PimaIndians2 Diabetes Dataset
#'
#' A cleaned version of the original Pima Indians Diabetes dataset from
#' the `mlbench` package.
#' Useful for demonstrating regression approaches for binary outcomes.
#'
#' @format A data frame with 768 observations and 9 variables:
#' \describe{
#'   \item{pregnant}{Number of times pregnant}
#'   \item{glucose}{Plasma glucose concentration (glucose tolerance test)}
#'   \item{pressure}{Diastolic blood pressure (mm Hg)}
#'   \item{triceps}{Triceps skin fold thickness (mm)}
#'   \item{insulin}{2-Hour serum insulin (mu U/ml)}
#'   \item{mass}{Body mass index (BMI)}
#'   \item{pedigree}{Diabetes pedigree function}
#'   \item{age}{Age in years}
#'   \item{diabetes}{Factor indicating diabetes status (pos/neg)}
#' }
#'
#' @source \url{https://www.openml.org/d/37}
"data_PimaIndiansDiabetes"
