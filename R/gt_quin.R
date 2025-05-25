#' Student Absenteeism in Rural Schools (`gt_quin`)
#'
#' This dataset contains observations on the number of days absent from school for children in rural Australia,
#' along with student characteristics. It's commonly used to demonstrate count models such as Poisson and Negative Binomial regression.
#'
#' @format A data frame with 146 observations and 5 variables:
#' \describe{
#'   \item{Eth}{Ethnicity (\code{"A"} = Aboriginal, \code{"N"} = Non-Aboriginal)}
#'   \item{Sex}{Sex (\code{"F"} or \code{"M"})}
#'   \item{Age}{Age group (\code{"F0", "F1", "F2", "F3"})}
#'   \item{Lrn}{Learner status (\code{"AL"} = average learner, \code{"SL"} = slow learner)}
#'   \item{Days}{Number of days absent from school (count outcome)}
#' }
#'
#' @source \pkg{MASS} package. See also Venables and Ripley (2002), *Modern Applied Statistics with S*.
#'
#' @examples
#' glm(Days ~ Sex + Age + Eth, family = poisson, data = gt_quin)
"gt_quin"
