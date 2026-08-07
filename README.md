<h1>
gtregression
</h1>
<p>
Publication-ready regression and survival analysis tables, plots, and
forest plots for real-world health data. Fit models, compare estimates,
visualise results, and export manuscript-ready outputs without
hand-formatting every coefficient.
</p>

<a href="articles/gtregression-intro.html">Start the
workflow</a><a class="secondary" href="reference/index.html">Explore
functions</a>

<img src="man/figures/gtregression_hex.png" alt="gtregression logo"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ThinkDenominator/gtregression/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThinkDenominator/gtregression/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/ThinkDenominator/gtregression/actions/workflows/pkgdown.yaml/badge.svg)](https://ThinkDenominator.github.io/gtregression/)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtregression)](https://CRAN.R-project.org/package=gtregression)
[![CRAN
checks](https://badges.cranchecks.info/worst/gtregression.svg)](https://cran.r-project.org/web/checks/check_results_gtregression.html)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/gtregression)](https://cranlogs.r-pkg.org/downloads/total/last-month/gtregression)
[![CRAN downloads
total](https://cranlogs.r-pkg.org/badges/grand-total/gtregression)](https://cranlogs.r-pkg.org/downloads/total/grand-total/gtregression)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![Codecov](https://codecov.io/gh/ThinkDenominator/gtregression/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ThinkDenominator/gtregression)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16905350.svg)](https://doi.org/10.5281/zenodo.16905350)

<!-- badges: end -->

## Publication-Ready Regression, Survival, and Mediation Outputs

`gtregression` helps you move from model to manuscript: fit regression
models, produce clean tables, visualise estimates, merge outputs, and
export results without hand-formatting every coefficient.

It supports logistic, log-binomial, Poisson, robust Poisson, negative
binomial, linear, Cox, parametric survival, and causal mediation
workflows, including adjusted and stratified models.

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Build</th>
<th>What you get</th>
</tr>
</thead>
<tbody>
<tr>
<td>Descriptive tables</td>
<td>Grouped summaries with row or column percentages</td>
</tr>
<tr>
<td>Regression tables</td>
<td>Crude, adjusted, stratified, linear, Cox, and parametric survival
outputs</td>
</tr>
<tr>
<td>Survival analysis</td>
<td>Kaplan-Meier curves, survival summaries, RMST, log-rank tests, Cox
PH checks, and survival predictions</td>
</tr>
<tr>
<td>Mediation analysis</td>
<td>Direct, indirect, total, and proportion mediated effects with causal
caveats</td>
</tr>
<tr>
<td>Visualisations</td>
<td>Regression plots, survival curves, fitted survival curves, and
forest tables</td>
</tr>
<tr>
<td>Interpretation helpers</td>
<td>Confounding, interaction, mediation, convergence, collinearity,
model selection, and survival diagnostics</td>
</tr>
<tr>
<td>Exports</td>
<td>HTML, PDF, PNG, and Word-ready outputs</td>
</tr>
</tbody>
</table>

<strong>Describe</strong>Build baseline tables with grouped summaries.

<strong>Model</strong>Fit crude, adjusted, and stratified regressions.

<strong>Visualise</strong>Create plots and forest tables for estimates.

<strong>Export</strong>Save polished tables, figures, and Word outputs.

## Why It Exists

Many students, researchers, and public health analysts need regression
outputs that are readable, reproducible, and report-ready.
`gtregression` keeps the R syntax approachable while preserving
transparent model objects underneath.

## Built on Trusted R Packages

`gtregression` is intentionally a readable interface over established R
packages. The package uses widely trusted modelling, tidying, plotting,
and reporting tools so users can inspect fitted models and understand
the statistical engines behind each output.

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Area</th>
<th>Core packages used</th>
</tr>
</thead>
<tbody>
<tr>
<td>Data handling and tidy workflows</td>
<td><code>dplyr</code>, <code>purrr</code>, <code>tibble</code>,
<code>rlang</code></td>
</tr>
<tr>
<td>Model fitting</td>
<td><code>stats</code>, <code>MASS</code>, <code>survival</code>,
<code>risks</code>, <code>logistf</code></td>
</tr>
<tr>
<td>Robust and diagnostic inference</td>
<td><code>sandwich</code>, <code>lmtest</code>, <code>broom</code>,
<code>broom.helpers</code></td>
</tr>
<tr>
<td>Tables and Word-ready reporting</td>
<td><code>flextable</code>, <code>officer</code>, <code>gt</code></td>
</tr>
<tr>
<td>Figures and forest plots</td>
<td><code>ggplot2</code>, <code>patchwork</code>,
<code>forestploter</code>, <code>scales</code></td>
</tr>
<tr>
<td>Optional development and checking tools</td>
<td><code>testthat</code>, <code>knitr</code>, <code>rmarkdown</code>,
<code>pkgdown</code>, <code>car</code>, <code>forcats</code>,
<code>ggtext</code></td>
</tr>
</tbody>
</table>

The user-facing functions return objects with fitted models, table
bodies, and display metadata that advanced users can audit, modify, or
reuse.

## Install

    install.packages("gtregression")

    # Development version
    devtools::install_github("ThinkDenominator/gtregression")

## Five-Minute Workflow

    library(gtregression)
    library(dplyr)

    data("data_birthwt", package = "gtregression")

    birthwt_data <- data_birthwt |>
      mutate(
        race = factor(race, levels = c(1, 2, 3),
                      labels = c("White", "Black", "Other")),
        smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
        ht = factor(ht, levels = c(0, 1), labels = c("No", "Yes")),
        ui = factor(ui, levels = c(0, 1), labels = c("No", "Yes")),
        low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW"))
      )

    exposures <- c("age", "lwt", "race", "smoke", "ht", "ui")

    attr(birthwt_data$age, "label") <- "Maternal age"
    attr(birthwt_data$lwt, "label") <- "Maternal weight"
    attr(birthwt_data$smoke, "label") <- "Smoking during pregnancy"

    desc <- descriptive_table(
      birthwt_data,
      exposures = exposures,
      by = "low",
      percent = "column",
      show_overall = "last"
    )

    uni <- uni_reg(
      birthwt_data,
      outcome = "low",
      exposures = exposures,
      approach = "logit"
    )

    multi <- multi_reg(
      birthwt_data,
      outcome = "low",
      exposures = c("smoke", "ht", "ui"),
      adjust_for = c("age", "lwt", "race"),
      approach = "logit"
    )

    plot_reg(multi, title = "Adjusted Regression for Low Birth Weight")

Variable labels set with `attr(x, "label")` or `labelled::var_label()`
are used automatically in display tables and plots, while original
column names remain available internally for merging, modification, and
testing.

Objects stay inspectable:

    desc$table
    uni$table
    multi$table
    multi$models

Optional model-fit statistics can be requested without changing the
publication table:

    uni_stats <- uni_reg(
      data = birthwt_data,
      outcome = "low",
      exposures = exposures,
      approach = "logit",
      model_stats = TRUE
    )

    uni_stats$model_stats

## Browse by Task

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Task</th>
<th>Start here</th>
</tr>
</thead>
<tbody>
<tr>
<td>First workflow</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/gtregression-intro.html">Start
Here</a></td>
</tr>
<tr>
<td>Descriptive summaries</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/descriptive-tables.html">Descriptive
Tables</a></td>
</tr>
<tr>
<td>Regression tables</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/regression-tables.html">Regression
Tables</a></td>
</tr>
<tr>
<td>Survival analysis</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/survival-analysis.html">Survival
Analysis</a></td>
</tr>
<tr>
<td>Causal mediation</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/causal-mediation.html">Causal
Mediation</a></td>
</tr>
<tr>
<td>Visualise estimates</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/visualise-results.html">Visualise
Results</a></td>
</tr>
<tr>
<td>Stratified models</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/stratified-analysis.html">Stratified
Analysis</a></td>
</tr>
<tr>
<td>Diagnostics and selection</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/diagnostics-selection.html">Diagnostics</a></td>
</tr>
<tr>
<td>Confounding and interaction</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/confounding-interaction.html">Interpret</a></td>
</tr>
<tr>
<td>Merge and export</td>
<td><a
href="https://gtregression.thinkdenominator.com/articles/customize-export.html">Customize
and Export</a></td>
</tr>
</tbody>
</table>

## Function Map

<table>
<colgroup>
<col style="width: 50%" />
<col style="width: 50%" />
</colgroup>
<thead>
<tr>
<th>Workflow</th>
<th>Functions</th>
</tr>
</thead>
<tbody>
<tr>
<td>Describe</td>
<td><code>descriptive_table()</code>, <code>dissect()</code></td>
</tr>
<tr>
<td>Model</td>
<td><code>uni_reg()</code>, <code>multi_reg()</code>,
<code>cox_reg()</code>, <code>surv_reg()</code></td>
</tr>
<tr>
<td>Survival</td>
<td><code>km_plot()</code>, <code>km_risk_table()</code>,
<code>survival_summary()</code>, <code>survival_quantiles()</code>,
<code>survival_prob()</code>, <code>rmst_table()</code>,
<code>logrank_test()</code>, <code>check_ph()</code>,
<code>surv_model_compare()</code>, <code>plot_surv_fit()</code>,
<code>surv_predict()</code></td>
</tr>
<tr>
<td>Stratify</td>
<td><code>stratified_uni_reg()</code>,
<code>stratified_multi_reg()</code></td>
</tr>
<tr>
<td>Visualise</td>
<td><code>plot_reg()</code>, <code>plot_reg_combine()</code>,
<code>forest_df()</code>, <code>forest_reg()</code></td>
</tr>
<tr>
<td>Diagnose</td>
<td><code>check_convergence()</code>, <code>check_collinearity()</code>,
<code>check_ph()</code>, <code>select_models()</code></td>
</tr>
<tr>
<td>Interpret</td>
<td><code>identify_confounder()</code>,
<code>interaction_models()</code>, <code>mediation_analysis()</code>,
<code>plot_mediation()</code></td>
</tr>
<tr>
<td>Polish and export</td>
<td><code>modify_table()</code>, <code>merge_tables()</code>,
<code>save_table()</code>, <code>save_plot()</code>,
<code>save_docx()</code></td>
</tr>
</tbody>
</table>

## Citation

If you use `gtregression` in your work, please cite it as:

Polani R, Eliyas SK, Sakthivel M, Krishnamoorthy Y, Majella MG.
*gtregression: Tools for Creating Publication-Ready Regression Tables.*
Zenodo. <https://doi.org/10.5281/zenodo.16905350>

## Acknowledgements

`gtregression` builds on the R ecosystem, especially `stats`,
`survival`, `MASS`, `risks`, `logistf`, `broom`, `broom.helpers`,
`sandwich`, `lmtest`, `dplyr`, `purrr`, `tibble`, `rlang`, `flextable`,
`officer`, `gt`, `ggplot2`, `patchwork`, `forestploter`, and `scales`.
