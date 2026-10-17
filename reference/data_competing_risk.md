# Synthetic Competing-Risk Oncology Dataset

A synthetic clinical follow-up dataset for teaching cumulative incidence
and competing-risk regression. The primary event is cancer relapse,
while death before relapse is a competing event because it prevents a
later relapse from being observed.

## Usage

``` r
data_competing_risk
```

## Format

A data frame with 500 observations and 8 variables:

- patient_id:

  Synthetic patient identifier.

- follow_up_months:

  Observed follow-up time in months.

- outcome:

  First observed outcome: `Censored`, `Relapse`, or
  `Death without relapse`.

- treatment:

  Randomized treatment group: `Standard` or `Intervention`.

- age:

  Age at baseline in years.

- sex:

  Recorded sex: `Female` or `Male`.

- stage:

  Disease stage at baseline: `I`, `II`, or `III`.

- performance_status:

  Baseline performance score from 0 to 2, with higher values indicating
  greater functional limitation.

## Source

Generated deterministically for the gtregression package.

## Details

These data are entirely synthetic and do not correspond to real people.
Latent times to relapse and death were generated from separate
exponential event processes and compared with an independently generated
censoring time. The intervention lowers the simulated relapse rate,
while age, disease stage, and performance status influence one or both
event processes. The dataset is intended for teaching workflow and
interpretation, not for clinical inference or benchmarking estimators.
