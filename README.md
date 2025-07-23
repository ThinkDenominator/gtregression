
<p align="left">

<img src="man/figures/gtregression_hex.png" 
alt="gtregression logo" width="180"/>

</p>

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

## Table of Contents

- [Vision](#vision)
- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Key Functions](#key-functions)
- [Contributing](#contributing)
- [Authors](#authors)
- [License](#license)

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

``` r
# Install from CRAN
install.packages("gtregression")

# Or install the development version from GitHub
devtools::install_github("ThinkDenominator/gtregression")
```

## Quick Start

``` r
# Load necessary libraries
library(gtregression)

# Load example dataset
data("data_PimaIndiansDiabetes", package="gtregression")

# Convert diabetes outcome to binary and create categorical variables
pima_data <- data_PimaIndiansDiabetes |>
  mutate(diabetes = ifelse(diabetes == "pos", 1, 0)) |>
  mutate(bmi = case_when(
    mass < 25 ~ "Normal",
    mass >= 25 & mass < 30 ~ "Overweight",
    mass >= 30 ~ "Obese",
    TRUE ~ NA_character_),                                       
    bmi = factor(bmi, levels = c("Normal", "Overweight", "Obese")),
    age_cat = case_when(
      age < 30 ~ "Young",
      age >= 30 & age < 50 ~ "Middle-aged",
      age >= 50 ~ "Older"),
    age_cat = factor(age_cat, levels = c("Young", "Middle-aged", "Older")),
    npreg_cat = ifelse(pregnant > 2, "High parity", "Low parity"),
    npreg_cat = factor(npreg_cat, levels = c("Low parity", "High parity")),
    glucose_cat= case_when(glucose<=140~ "Normal", glucose>140~"High"),
    glucose_cat= factor(glucose_cat, levels = c("Normal", "High")),
    bp_cat = case_when(
      pressure < 80 ~ "Normal",
      pressure >= 80 ~ "High"
    ),
    bp_cat= factor(bp_cat, levels = c("Normal", "High")),
    triceps_cat = case_when(
      triceps < 23 ~ "Normal",
      triceps >= 23 ~ "High"
    ),
    triceps_cat= factor(triceps_cat, levels = c("Normal", "High")),
    insulin_cat = case_when(
      insulin < 30 ~ "Low",
      insulin >= 30 & insulin < 150 ~ "Normal",
      insulin >= 150 ~ "High"
    ),
    insulin_cat = factor(insulin_cat, levels = c("Low", "Normal", "High"))
  ) |>
  mutate(
    dpf_cat = case_when(
      pedigree <= 0.2 ~ "Low Genetic Risk",
      pedigree > 0.2 & pedigree <= 0.5 ~ "Moderate Genetic Risk",
      pedigree > 0.5 ~ "High Genetic Risk"
    )
  ) |>
  mutate(dpf_cat = factor(dpf_cat, 
              levels = c("Low Genetic Risk", 
                          "Moderate Genetic Risk", 
                          "High Genetic Risk"))) |>
  mutate(diabetes_cat= case_when(diabetes== 1~ "Diabetes positive", 
                                TRUE~ "Diabetes negative")) |>
  mutate(diabetes_cat= factor(diabetes_cat, 
                        levels = c("Diabetes negative","Diabetes positive" )))

# Descriptive statistics table
exposures <- c("bmi", "age_cat", "npreg_cat", "bp_cat", "triceps_cat",
               "insulin_cat", "dpf_cat")

# Create a descriptive table by diabetes category
des_tbl = descriptive_table(data= pima_data, 
                             exposures = exposures, 
                             by= "diabetes_cat")
                             
# Check the data compatibility
dissect(pima_data)

# Univariable regression
uni_tbl = uni_reg(
  data = pima_data,
  outcome = "diabetes",
  exposures = exposures,
  approach = "logit"
)

# check models and summaries
uni_tbl$models
uni_tbl$model_summaries

# Plot univariable regression results
plot_reg(uni_tbl, 
         title = "Univariable Regression Results")
         
# multivariable regression
multi_tbl = multi_reg(
  data = pima_data,
  outcome = "diabetes",
  exposures = exposures,
  approach = "logit"
)

# check models and summaries
multi_tbl$models
multi_tbl$model_summaries

# Plot univariable regression results
plot_reg(multi_tbl, 
         title = "Multivariable Regression Results")

# combined plots
plot_reg_combine(
  uni_tbl, 
  multi_tbl, 
  title = "Univariable vs Multivariable Regression Results")
  
# combine the tables
merge_table(des_tbl, uni_tbl, multi_tbl, 
            spanners = c("**Descriptive**",
            "**Univariate**", 
            "**Multivariable**"))

# Save the table as a Word document
save_table(des_tbl, filename = "des_tbl", format = "docx")

save_docx(
  tables = list(des_tbl, uni_tbl, multi_tbl),
  filename = "Outputs.docx")
  
# Stratified regression
stratified_uni_reg(pima_data,
                     outcome= "diabetes",
                     exposures =c("bmi", "insulin_cat", "age_cat", "dpf_cat"),
                     approach = "logit",
                     stratifier = "glucose_cat")
                     
stratified_multi_reg(pima_data,
                     outcome= "diabetes",
                     exposures =c("bmi", "insulin_cat", "age_cat", "dpf_cat"),
                     approach = "logit",
                     stratifier = "glucose_cat")
                     
# Check model convergence
check_convergence(pima_data, 
                  exposures = exposures, 
                  outcome = "diabetes", 
                  approach = "logit", 
                  multivariate = F)
                  
check_convergence(pima_data, 
                  exposures = exposures, 
                  outcome = "diabetes", 
                  approach = "logit", 
                  multivariate = T)


# identify confounders
identify_confounder(pima_data,
                    outcome = "diabetes",
                    exposure = "npreg_cat",
                    potential_confounder = "bp_cat",
                    approach = "logit")
                     
# check interactions
interaction_models(pima_data,
                   outcome,
                   exposure = "bmi",
                   effect_modifier = "glucose_cat",
                   covariates = c("insulin_cat", "age_cat", "dpf_cat"),
                   approach = "logit")
```

## Key Functions

### Descriptive & Compatibility Tools

| Function Name | Purpose |
|----|----|
| `descriptive_table()` | Summarise exposures by outcome groups using gtsummary |
| `dissect()` | Check outcome-exposure compatibility |

### Regression Functions - Fit univariate and multivariable models

| Function Name | Purpose                              |
|---------------|--------------------------------------|
| `uni_reg()`   | Univariable regression (OR/RR/IRR/β) |
| `multi_reg()` | Multivariable regression             |

### Regression Functions by stratifier

| Function Name            | Purpose                             |
|--------------------------|-------------------------------------|
| `stratified_uni_reg()`   | Stratified univariable regression   |
| `stratified_multi_reg()` | Stratified multivariable regression |

### Model Diagnostics & Selection

| Function Name         | Purpose                                          |
|-----------------------|--------------------------------------------------|
| `check_convergence()` | Evaluate model convergence and max fitted values |
| `select_models()`     | Stepwise model selection (AIC/BIC/adjusted R²)   |

### Confounding & Interaction

| Function Name           | Purpose                                           |
|-------------------------|---------------------------------------------------|
| `identify_confounder()` | Confounding assessment via % change or MH method  |
| `interaction_models()`  | Compare models with and without interaction terms |

### Plots & Exports

| Function Name        | Purpose                                        |
|----------------------|------------------------------------------------|
| `plot_reg()`         | Forest plot for a single regression model      |
| `plot_reg_combine()` | Side-by-side forest plots for uni/multi models |
| `modify_table()`     | Customize column labels or output structure    |
| `save_table()`       | Export table to `.html`, `.csv`, `.docx`       |
| `save_docx()`        | Save table as Word document (`.docx`)          |
| `save_plot()`        | Save plot as `.png`, `.pdf`, etc.              |
| `merge_table()`      | Combine descriptive and regression tables      |

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

- **Rubeshkumar Polani**  
  <a href="mailto:rubesh@thinkdenominator.com"
  class="email">rubesh.pc@gmail.com</a>  
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
