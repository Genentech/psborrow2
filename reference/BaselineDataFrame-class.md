# Baseline Data Frame Object

Contains a generated baseline dataset for a single arm.

## Value

A `BaselineDataFrame`

## Slots

- `cov_names`:

  `character` contains the names of covariates generated from the
  multivariate normal distribution

- `means`:

  `numeric` contains the means of generating distribution for the
  covariates in `cov_names`

- `variances`:

  `numeric` contains the marginal variances of generating distribution
  for the covariates in `cov_names`.
