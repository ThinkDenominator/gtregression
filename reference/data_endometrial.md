# Endometrial Cancer Histology Grade Data

A classic endometrial cancer dataset used to demonstrate separation in
logistic regression. The outcome is high histology grade.
Neovascularization is completely absent among low-grade cases in this
dataset, making it useful for teaching Firth penalized logistic
regression.

## Usage

``` r
data_endometrial
```

## Format

A data frame with 79 observations and 4 variables:

- NV:

  Neovascularization status (0 = absent, 1 = present)

- PI:

  Pulsatility index of the uterine artery

- EH:

  Endometrium height

- HG:

  Histology grade (0 = low grade, 1 = high grade)

## Source

brglm2 package. The packaged dataset was sourced from
<https://users.stat.ufl.edu/~aa/glm/data/>, the data repository used in
Agresti (2015). Originally analyzed in Heinze and Schemper (2002).

## References

Agresti A (2015). *Foundations of Linear and Generalized Linear Models*.
Wiley.

Heinze G, Schemper M (2002). A solution to the problem of separation in
logistic regression. *Statistics in Medicine*, 21, 2409-2419.
[doi:10.1002/sim.1047](https://doi.org/10.1002/sim.1047)
