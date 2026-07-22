# Birth Weight Data

A dataset from the MASS package containing risk factors associated with
low birth weight (LBW) in newborns. Originally collected at Baystate
Medical Center, Springfield, Massachusetts, USA.

## Usage

``` r
data_birthwt
```

## Format

A data frame with 189 observations and 10 variables:

- low:

  Indicator for birth weight \< 2500g (binary): `0 = normal`,
  `1 = low birth weight`

- age:

  Mother's age in years (numeric)

- lwt:

  Mother's weight in pounds at last menstrual period (numeric)

- race:

  Mother's race (factor): `1 = White`, `2 = Black`, `3 = Other`

- smoke:

  Smoking status during pregnancy (binary): `0 = No`, `1 = Yes`

- ptl:

  Number of previous premature labors (integer)

- ht:

  History of hypertension (binary): `0 = No`, `1 = Yes`

- ui:

  Presence of uterine irritability (binary): `0 = No`, `1 = Yes`

- ftv:

  no of physician visits during the 1st trimester (integer, 0–6)

- bwt:

  Birth weight in grams (numeric)

## Source

Hosmer, D.W., Lemeshow, S. (1989). \*Applied Logistic Regression.\* New
York: Wiley. Also available in MASS and described in detail in its
documentation.

## Details

The outcome variable is binary (\`low\`): birth weight \< 2500g (yes
= 1) or not (no = 0).
