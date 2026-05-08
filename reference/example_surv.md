# Simulated Survival Data

A data frame containing simulated data from a clinical trial with a
treatment arm (n=200) and a control arm (n=158), as well as data from an
external control (n=242).

## Usage

``` r
example_surv
```

## Format

A data frame with 600 rows and 6 variables:

- trt:

  0/1, flag for treatment arm

- ext:

  0/1, flag for external controls

- eventtime:

  numeric \>0, survival time

- status:

  0/1, event indicator

- censor:

  0/1, censoring indicator

- cov1:

  0/1, binary baseline covariate 1

- cov2:

  integer in \[0, 15\], baseline covariate 2

- cov3:

  continuous numeric, baseline covariate 3
