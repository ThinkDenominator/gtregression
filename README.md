
# gtregression

`gtregression` is an R package that simplifies regression modeling and
generates publication-ready tables using the `gtsummary` ecosystem. It
supports a variety of regression approaches with built-in tools for
model diagnostics, selection, and confounder identification—all designed
to provide beginner and intermediate R users with clean, interpretable
output.

This package was created with the aim of empowering R users in low- and
middle-income countries (LMICs) by offering a simpler and more
accessible coding experience. We sincerely thank the authors and
contributors of foundational R packages such as `gtsummary`, `MASS`,
`RISKS`, `dplyr`, and others—without whom this project would not have
been possible.

## Vision

At its core, `gtregression` is more than just a statistical tool—it is a
commitment to open access, simplicity, and inclusivity in health data
science. Our team is driven by the vision of empowering researchers,
students, and public health professionals in LMICs through
user-friendly, well-documented tools that minimize coding burden and
maximize interpretability.

We believe in the democratization of data science and aim to promote
open-source resources for impactful and equitable research globally.

## Features

- Supports multiple regression approaches:
  - Logistic (logit)
  - Log-binomial
  - Poisson / Robust Poisson
  - Negative Binomial
  - Linear Regression
- Univariable and multivariable regression
- Confounder identification using crude and adjusted estimates
- Stepwise model selection (AIC/BIC/adjusted R²)
- Stratified regression support
- Formatted outputs using `gtsummary`
- Built-in example datasets: `PimaIndiansDiabetes2`, `birthwt`, `epil`

## Installation

\`\`\`r

\# Install from GitHub

devtools::install_github(“drrubesh/gtregression”)

\`\`\`

## Quick Start

\`\`\`r

library(gtregression)

## Logistic regression

data(PimaIndiansDiabetes2, package = “mlbench”)  
  
multi_reg( data = PimaIndiansDiabetes2,  
outcome = “diabetes”,  
exposures = c(“age”, “mass”, “glucose”), approach = “logit” )

## Negative binomial regression

data(epil, package = “MASS”)  
  
multi_reg_nbin( data = epil, outcome = “y”, exposures = c(“trt”, “base”,
“age”) )

\`\`\`

# Key Functions

| Function Name             | Purpose                                        |
|---------------------------|------------------------------------------------|
| `uni_reg()`               | Univariable regression (OR/RR/IRR/β)           |
| `multi_reg()`             | Multivariable regression                       |
| `select_models()`         | Stepwise model selection                       |
| `identify_confounder()`   | Confounding assessment via % change            |
| `check_convergence()`     | Evaluate model convergence and max probability |
| `interaction_models()`    | Compare models with and without interactions   |
| `stratified_uni_reg()`    | Stratified univariable regression              |
| `stratified_multi_reg()`  | Stratified multivariable regression            |
| `uni_reg_nbin()`          | Univariable negative binomial regression       |
| `multi_reg_nbin()`        | Multivariable negative binomial regression     |
| `stratified_uni_nbin()`   | Stratified univariable NB regression           |
| `stratified_multi_nbin()` | Stratified multivariable NB regression         |

## Contributing

We welcome issues, feature requests, and pull requests.

1.  Fork the repository

2.  Create a new branch: git checkout -b feature/my-feature

3.  Commit your changes: git commit -m “Add feature”

4.  Push to GitHub: git push origin feature/my-feature

5.  Open a Pull Request

## Authors

Rubeshkumar Polani Chandrasekar — @drrubesh

Yuvaraj Krishnamoorthy

Marie Gilbert Majella

License MIT License. See LICENSE for details.

------------------------------------------------------------------------
