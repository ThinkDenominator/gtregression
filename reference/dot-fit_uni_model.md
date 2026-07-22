# Fit Regression Model with One or More Predictors (Internal)

Fits a regression model based on the selected approach. Can handle a
single exposure or a vector of exposures.

## Usage

``` r
.fit_uni_model(data, outcome, exposures, approach)
```

## Arguments

- data:

  A \`data.frame\` with complete observations for outcome and exposures.

- outcome:

  A string. Name of the outcome variable.

- exposures:

  A string or character vector of predictor(s).

- approach:

  A string specifying the regression approach. One of \`"logit"\`,
  \`"logbinomial"\`, \`"poisson"\`, \`"linear"\`, \`"robpoisson"\`, or
  \`"negbin"\`.

## Value

A fitted model object (\`glm\`, \`lm\`, \`riskratio\`, or \`negbin\`) or
\`NULL\` if fitting fails.
