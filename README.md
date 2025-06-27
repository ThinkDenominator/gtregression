<p align="left">
  <img src="man/figures/gtregression_hex.png" width="200"/>
</p>


# gtregression

`gtregression` is an R package that simplifies regression modeling and generates publication-ready tables using the `gtsummary` ecosystem.     
It supports a wide range of regression approaches—including logistic, linear, 
log-binomial, Poisson, negative binomial, and poisson with robust standard error models—
alongside tools for model diagnostics, variable selection, and confounder assessment.    

Designed for students, public health professionals, and researchers—especially in low- and middle-income countries (LMICs)—`gtregression` helps reduce the burden of writing complex code and juggling multiple packages.
It integrates reliable tools like `gtsummary`, `risks`, `MASS`, `dplyr`, and `stats`, and wraps them into a cohesive, user-friendly interface.

## Why We Built It

Many R users find regression workflows intimidating and fragmented. 
Simple tasks like fitting a model and formatting output can require long scripts, multiple helper functions, and manual copying of results. 
These inefficiencies are especially felt in LMIC settings, where users often face time constraints, limited training, and inconsistent software access.

`gtregression` was created to streamline this process and improve the user experience without compromising rigor. It offers:

- Clean, readable syntax with minimal setup
- Publication-ready tables with built-in diagnostics
- Unified functions across multiple regression types
- Smart defaults that reduce the chance of error

## Our Vision

We believe in democratizing access to data science.  
`gtregression` reflects our commitment to open, accessible, and evidence-based tools that empower researchers to work efficiently and communicate their findings clearly—regardless of technical background or location.

We believe this package can help grow the R user base in low- and middle-income countries (LMICs) by lowering the barriers to performing high-quality statistical analysis.

We sincerely thank the authors of foundational R packages whose work made this project possible.

## Features

- Supports multiple regression approaches:
  - Logistic (logit)
  - Log-binomial
  - Poisson / Robpoisson
  - Negative Binomial
  - Linear Regression
- Univariable and multivariable regression
- Confounder identification using crude and adjusted estimates
- Stepwise model selection (AIC/BIC/adjusted R²)
- Stratified regression support
- Formatted outputs using `gtsummary`
- Built-in example datasets

## Installation

``` r
# Install from GitHub
devtools::install_github("drrubesh/gtregression")
```

## Quick Start

``` r
library(gtregression)

## Logistic regression
data(PimaIndiansDiabetes2, package = "mlbench")
multi_reg(
  data = PimaIndiansDiabetes2,
  outcome = "diabetes",
  exposures = c("age", "mass", "glucose"),
  approach = "logit"
)

## Negative binomial regression
data(epil, package = "MASS")
multi_reg_nbin(
  data = epil,
  outcome = "y",
  exposures = c("trt", "base", "age")
)
```

## Function Overview

### Regression Modeling

| Function Name             | Purpose                                        |
|---------------------------|------------------------------------------------|
| `uni_reg()`               | Univariable regression (OR, RR, IRR, or β)     |
| `multi_reg()`             | Multivariable regression                       |
| `uni_reg_nbin()`          | Univariable negative binomial regression       |
| `multi_reg_nbin()`        | Multivariable negative binomial regression     |
| `stratified_uni_reg()`    | Stratified univariable regression              |
| `stratified_multi_reg()`  | Stratified multivariable regression            |
| `stratified_uni_nbin()`   | Stratified univariable NB regression           |
| `stratified_multi_nbin()` | Stratified multivariable NB regression         |

### Model Diagnostics and Variable Assessment

| Function Name             | Purpose                                        |
|---------------------------|------------------------------------------------|
| `select_models()`         | Stepwise model selection (AIC/BIC/logLik)      |
| `identify_confounder()`   | Confounding assessment via % change or MH      |
| `check_convergence()`     | Evaluate model convergence and max probability |
| `interaction_models()`    | Compare models with and without interactions   |
| `check_collinearity()`    | Check multicollinearity via VIF and pairwise   |

## Plots and Exports

| Function Name        | Purpose                                             |
|----------------------|-----------------------------------------------------|
| `modify_table()`     | Modify `gtsummary` tables (e.g., labels, p-values)  |
| `plot_reg()`         | Forest plot of regression model estimates           |
| `plot_reg_combine()` | Side-by-side plot of univariable and multivariable models |
| `save_table()`       | Save regression tables to Word/HTML                  |
| `save_plot()`        | Save regression plots to file                        |
| `save_docx()`        | Save tables and plots in one report                  |

## Contributing

We welcome issues, feature requests, and pull requests.

1.  Fork the repository
2.  Create a new branch: `git checkout -b feature/my-feature`
3.  Commit your changes: `git commit -m "Add feature"`
4.  Push to GitHub: `git push origin feature/my-feature`
5.  Open a Pull Request

## Authors

The `gtregression` package is developed and maintained by a
collaborative team committed to making regression modeling accessible,
especially for public health professionals and researchers in LMICs.

- **Rubeshkumar Polani Chandrasekar**  
  <rubesh@thinkdenominator.com>  
  ORCID: [0000-0002-0418-7592](https://orcid.org/0000-0002-0418-7592)  
  *Creator and Author*

- **Salin K Eliyas**  
  <salins13@gmail.com>  
  ORCID: [0000-0002-8020-5860](https://orcid.org/0000-0002-8020-5860)  
  *Author*

- **Manikandanesan Sakthivel**  
  <nesanmbbs@gmail.com>  
  ORCID: [0000-0002-5438-3970](https://orcid.org/0000-0002-5438-3970)  
  *Author*

- **Yuvaraj Krishnamoorthy**  
  <yuvaraj@propulevidence.org>  
  ORCID: [0000-0003-4688-510X](https://orcid.org/0000-0003-4688-510X)  
  *Author*

- **Marie Gilbert Majella**  
  <gilbert2691@gmail.com>  
  ORCID: [0000-0003-4036-5162](https://orcid.org/0000-0003-4036-5162)  
  *Author*

## License

MIT License. See LICENSE for details.
