# Summarize the number of continuous and binary covariates in a `SimCovariates` object created by `sim_covariates()`

Summarize the number of continuous and binary covariates in a
`SimCovariates` object created by
[`sim_covariates()`](https://genentech.github.io/psborrow2/reference/sim_covariates.md)

## Usage

``` r
sim_covariates_summ(sim_covariates_obj)
```

## Arguments

- sim_covariates_obj:

  `SimCovariates`. Object returned by
  [`sim_covariates()`](https://genentech.github.io/psborrow2/reference/sim_covariates.md).

## Value

data.frame showing covariate names and types as well as counts of binary
and continuous covariates.
