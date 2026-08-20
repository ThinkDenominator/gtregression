# Synthetic Diabetes Dataset

A synthetic replacement for the former diabetes dataset from the mlbench
package. It has the same structure and number of observations but does
not represent real people. Useful for demonstrating regression
approaches for binary outcomes.

## Usage

``` r
data_SynthDiabetes
```

## Format

A data frame with 768 observations and 9 variables:

- pregnant:

  Number of times pregnant

- glucose:

  Plasma glucose concentration (glucose tolerance test)

- pressure:

  Diastolic blood pressure (mm Hg)

- triceps:

  Triceps skin fold thickness (mm)

- insulin:

  2-Hour serum insulin (mu U/ml)

- mass:

  Body mass index (BMI)

- pedigree:

  Diabetes pedigree function

- age:

  Age in years

- diabetes:

  Factor indicating diabetes status (pos/neg)

## Source

[`mlbench::SynthDiabetes2`](https://rdrr.io/pkg/mlbench/man/SynthDiabetes.html),
available in mlbench version 2.1-11 or later.
