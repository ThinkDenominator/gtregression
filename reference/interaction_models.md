# Compare Models With and Without an Interaction Term

Fits two models, one with and one without an interaction term between an
exposure and a potential effect modifier. The models are compared using
a likelihood ratio test or Wald test to assess statistical evidence of
interaction.

## Usage

``` r
interaction_models(
  data,
  outcome = NULL,
  exposure,
  covariates = NULL,
  effect_modifier,
  approach = "logit",
  time = NULL,
  event = NULL,
  distribution = "weibull",
  test = c("LRT", "Wald"),
  alpha = 0.05,
  verbose = FALSE,
  format = c("flextable", "gt", "tibble")
)
```

## Arguments

- data:

  A data frame containing all required variables.

- outcome:

  Outcome variable name. Quoted and bare names are accepted. Required
  for ordinary regression approaches. Leave unset for Cox and parametric
  survival approaches and supply `time` and `event` instead.

- exposure:

  Main exposure variable name. Quoted and bare names are accepted.

- covariates:

  Optional character vector of additional covariates. Quoted names are
  recommended in scripts, and bare names are also accepted.

- effect_modifier:

  Variable name for the potential effect modifier. Quoted and bare names
  are accepted.

- approach:

  Regression approach. One of `"logit"`, `"logbinomial"`, `"poisson"`,
  `"robpoisson"`, `"negbin"`, `"linear"`, `"cox"`, or `"survreg"`.

- time:

  Survival time variable name for `approach = "cox"` or
  `approach = "survreg"`. Quoted and bare names are accepted.

- event:

  Event indicator variable name for survival approaches.

- distribution:

  Parametric survival distribution for `approach = "survreg"`. One of
  `"weibull"`, `"exponential"`, `"lognormal"`, or `"loglogistic"`.

- test:

  Statistical test for model comparison. One of `"LRT"` or `"Wald"`.

- alpha:

  Significance threshold used to classify the interaction result.

- verbose:

  Logical; if `TRUE`, prints a short interpretation.

- format:

  Output format for the viewing table. One of `"flextable"` (default),
  `"gt"`, or `"tibble"`. Use `format = "tibble"` to keep only the
  original list structure.

## Value

A list with model objects, formulas, p-value, decision, and a one-row
summary tibble. When `format` is `"gt"` or `"flextable"`, the list also
includes `table`.

## Details

Use this function when the interaction is planned or clinically/causally
motivated and you want a focused model comparison. Mantel-Haenszel
estimation is not used here because this function tests an explicit
interaction term in a regression model. For broader screening of
candidate confounders or effect modifiers, including
Mantel-Haenszel-supported checks when appropriate, use
[`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md).

## See also

[`identify_confounder()`](https://thinkdenominator.github.io/gtregression/reference/identify_confounder.md)
for broader confounding and effect-modification screening.

## Examples

``` r
birthwt_data <- data_birthwt |>
  dplyr::mutate(
    low = factor(low, levels = c(0, 1), labels = c("Normal BW", "Low BW")),
    smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")),
    race = factor(race, levels = c(1, 2, 3),
                  labels = c("White", "Black", "Other"))
  )

interaction_models(
  data = birthwt_data,
  outcome = low,
  exposure = smoke,
  effect_modifier = race,
  covariates = c(age, lwt),
  approach = logit
)
#> $summary
#> # A tibble: 1 × 10
#>   outcome exposure effect_modifier approach test   p_value alpha has_interaction
#>   <chr>   <chr>    <chr>           <chr>    <chr>    <dbl> <dbl> <lgl>          
#> 1 low     smoke    race            logit    Likel…   0.319  0.05 FALSE          
#> # ℹ 2 more variables: decision <chr>, interpretation <chr>
#> 
#> $model_no_interaction
#> 
#> Call:  stats::glm(formula = formula, family = stats::binomial("logit"), 
#>     data = model_data)
#> 
#> Coefficients:
#> (Intercept)     smokeYes    raceBlack    raceOther          age          lwt  
#>     0.33245      1.05444      1.23167      0.94326     -0.02248     -0.01253  
#> 
#> Degrees of Freedom: 188 Total (i.e. Null);  183 Residual
#> Null Deviance:       234.7 
#> Residual Deviance: 214.6     AIC: 226.6
#> 
#> $model_with_interaction
#> 
#> Call:  stats::glm(formula = formula, family = stats::binomial("logit"), 
#>     data = model_data)
#> 
#> Coefficients:
#>        (Intercept)            smokeYes           raceBlack           raceOther  
#>           -0.18086             1.56250             1.51167             1.47279  
#>                age                 lwt  smokeYes:raceBlack  smokeYes:raceOther  
#>           -0.01986            -0.01189            -0.29633            -1.31147  
#> 
#> Degrees of Freedom: 188 Total (i.e. Null);  181 Residual
#> Null Deviance:       234.7 
#> Residual Deviance: 212.3     AIC: 228.3
#> 
#> $robust_no_interaction
#> NULL
#> 
#> $robust_with_interaction
#> NULL
#> 
#> $formula_no_interaction
#> low ~ smoke + race + age + lwt
#> <environment: 0x561c5f271640>
#> 
#> $formula_with_interaction
#> low ~ smoke + race + age + lwt + smoke:race
#> <environment: 0x561c5f271640>
#> 
#> $interaction_terms
#> [1] "smokeYes:raceBlack" "smokeYes:raceOther"
#> 
#> $comparison
#> Analysis of Deviance Table
#> 
#> Model 1: low ~ smoke + race + age + lwt
#> Model 2: low ~ smoke + race + age + lwt + smoke:race
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       183     214.58                     
#> 2       181     212.29  2   2.2829   0.3194
#> 
#> $p_value
#> [1] 0.3193602
#> 
#> $alpha
#> [1] 0.05
#> 
#> $has_interaction
#> [1] FALSE
#> 
#> $decision
#> [1] "no_interaction"
#> 
#> $interpretation
#> [1] "No statistical evidence of interaction between smoke and race at alpha = 0.05."
#> 
#> $test
#> [1] "Likelihood Ratio Test"
#> 
#> $approach
#> [1] "logit"
#> 
#> $source
#> [1] "interaction_models"
#> 
#> $table
#> 
#> attr(,"class")
#> [1] "interaction_models_result" "list"                     

lung_data <- data_lungcancer |>
  dplyr::mutate(
    trt = factor(trt, levels = c(1, 2),
                 labels = c("Standard treatment", "Test treatment")),
    prior = factor(prior, levels = c(0, 10), labels = c("No", "Yes"))
  )

interaction_models(
  data = lung_data,
  time = time,
  event = status,
  exposure = trt,
  effect_modifier = prior,
  covariates = c(age, karno),
  approach = cox
)
#> $summary
#> # A tibble: 1 × 10
#>   outcome  exposure effect_modifier approach test  p_value alpha has_interaction
#>   <chr>    <chr>    <chr>           <chr>    <chr>   <dbl> <dbl> <lgl>          
#> 1 time/st… trt      prior           cox      Like…  0.0683  0.05 FALSE          
#> # ℹ 2 more variables: decision <chr>, interpretation <chr>
#> 
#> $model_no_interaction
#> Call:
#> survival::coxph(formula = formula, data = model_data, model = TRUE)
#> 
#>                        coef exp(coef)  se(coef)      z        p
#> trtTest treatment  0.193793  1.213846  0.186309  1.040    0.298
#> priorYes          -0.060857  0.940958  0.202705 -0.300    0.764
#> age               -0.004033  0.995975  0.009202 -0.438    0.661
#> karno             -0.034266  0.966314  0.005253 -6.523 6.89e-11
#> 
#> Likelihood ratio test=43.23  on 4 df, p=9.257e-09
#> n= 137, number of events= 128 
#> 
#> $model_with_interaction
#> Call:
#> survival::coxph(formula = formula, data = model_data, model = TRUE)
#> 
#>                                 coef exp(coef)  se(coef)      z       p
#> trtTest treatment           0.420717  1.523053  0.224505  1.874  0.0609
#> priorYes                    0.305208  1.356907  0.275497  1.108  0.2679
#> age                        -0.008447  0.991589  0.009468 -0.892  0.3723
#> karno                      -0.034908  0.965694  0.005317 -6.565 5.2e-11
#> trtTest treatment:priorYes -0.757259  0.468950  0.417582 -1.813  0.0698
#> 
#> Likelihood ratio test=46.56  on 5 df, p=6.999e-09
#> n= 137, number of events= 128 
#> 
#> $robust_no_interaction
#> NULL
#> 
#> $robust_with_interaction
#> NULL
#> 
#> $formula_no_interaction
#> survival::Surv(time, status) ~ trt + prior + age + karno
#> <environment: 0x561c63360ed8>
#> 
#> $formula_with_interaction
#> survival::Surv(time, status) ~ trt + prior + age + karno + trt * 
#>     prior
#> <environment: 0x561c63360ed8>
#> 
#> $interaction_terms
#> [1] "trtTest treatment:priorYes"
#> 
#> $comparison
#> Analysis of Deviance Table
#>  Cox model: response is  survival::Surv(time, status)
#>  Model 1: ~ trt + prior + age + karno
#>  Model 2: ~ trt + prior + age + karno + trt * prior
#>    loglik  Chisq Df Pr(>|Chi|)  
#> 1 -483.83                       
#> 2 -482.17 3.3227  1    0.06833 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $p_value
#> [1] 0.06832996
#> 
#> $alpha
#> [1] 0.05
#> 
#> $has_interaction
#> [1] FALSE
#> 
#> $decision
#> [1] "no_interaction"
#> 
#> $interpretation
#> [1] "No statistical evidence of interaction between trt and prior at alpha = 0.05."
#> 
#> $test
#> [1] "Likelihood Ratio Test"
#> 
#> $approach
#> [1] "cox"
#> 
#> $source
#> [1] "interaction_models"
#> 
#> $table
#> 
#> attr(,"class")
#> [1] "interaction_models_result" "list"                     
```
