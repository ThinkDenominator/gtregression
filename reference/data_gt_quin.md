# Student Absenteeism in Rural Schools

This dataset contains observations on the number of days absent from
school for children in rural Australia, along with student
characteristics. It's commonly used to demonstrate count models such as
Poisson and Negative Binomial regression.

## Usage

``` r
data_gt_quin
```

## Format

A data frame with 146 observations and 5 variables:

- Eth:

  Ethnicity (`"A"` = Aboriginal, `"N"` = Non-Aboriginal)

- Sex:

  Sex (`"F"` or `"M"`)

- Age:

  Age group (`"F0", "F1", "F2", "F3"`)

- Lrn:

  Learner status (`"AL"` = average learner, `"SL"` = slow learner)

- Days:

  Number of days absent from school (count outcome)

## Source

MASS package. See also Venables and Ripley (2002), \*Modern Applied
Statistics with S\*.
