# Epilepsy Treatment and Seizure Counts

RCT on the effect of a drug on the seizures in patients with epilepsy.
Contains repeated measures data with treatment groups, baseline seizure
counts, and follow-up counts.

## Usage

``` r
data_epilepsy
```

## Format

A data frame with 236 observations and 9 variables:

- y:

  Number of seizures in a 2-week period (count)

- trt:

  Treatment group (factor): `placebo` or `progabide`

- base:

  Seizure count during baseline period (numeric)

- age:

  Age of patient (numeric)

- V4:

  Indicator for 4th visit (binary)

- subject:

  Patient ID (factor)

- period:

  Follow-up period number (integer)

- lbase:

  Log of baseline seizures (numeric)

- lage:

  Log of age (numeric)

## Source

MASS package. Original data from Thall and Vail (1990)
