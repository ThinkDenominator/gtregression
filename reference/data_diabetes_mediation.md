# Diabetes Mediation Teaching Dataset

A health-related teaching dataset prepared from the Pima Indians
Diabetes data. It is designed for the practical question: could part of
the obesity and diabetes relationship operate through plasma glucose,
after adjustment for basic clinical covariates?

## Usage

``` r
data_diabetes_mediation
```

## Format

A data frame with variables:

- diabetes:

  Diabetes status (`No`, `Yes`)

- obesity:

  Obesity status based on BMI \>= 30 (`No`, `Yes`)

- glucose:

  Plasma glucose concentration

- bmi:

  Body mass index

- age:

  Age in years

- blood_pressure:

  Diastolic blood pressure

- pregnancies:

  Number of pregnancies

- diabetes_pedigree:

  Diabetes pedigree function

## Source

Derived from `data_PimaIndiansDiabetes`.

## Details

This dataset is intended for practice and teaching. Mediation estimates
from observational data should be interpreted causally only when the
temporal ordering and no-unmeasured-confounding assumptions are
justified by study design, DAGs, and subject-matter knowledge.
