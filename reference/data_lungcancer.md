# Lung Cancer Trial Data

Survival data from a clinical trial of lung cancer patients conducted by
the Veteran's Administration.

## Usage

``` r
data_lungcancer
```

## Format

A data frame with 137 observations and 8 variables:

- trt:

  Treatment group (1 = standard, 2 = test)

- celltype:

  Cell type (squamous, smallcell, adeno, large)

- time:

  Survival time (in days)

- status:

  Censoring status (1 = died, 0 = censored)

- karno:

  Karnofsky performance score (higher = better)

- diagtime:

  Months from diagnosis to randomization

- age:

  Age in years

- prior:

  Prior therapy (0 = no, 10 = yes)

## Source

<https://CRAN.R-project.org/package=survival>

## References

Kalbfleisch JD and Prentice RL (1980). The Statistical Analysis of
Failure Time Data.
