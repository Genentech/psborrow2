# `SimCovariates` Class

A class for specifying covariate distributions and covariance for
simulation studies.

## Slots

- `covariates`:

  list. List of covariate mean values or probabilities as generated
  through
  [`bin_var()`](https://genentech.github.io/psborrow2/reference/bin_var.md)
  (class `SimVarBin` or
  [`cont_var()`](https://genentech.github.io/psborrow2/reference/cont_var.md)
  (class `SimVarCont`).

- `covariance_internal`:

  matrix. Covariance matrix before binarization for internal patients.

- `covariance_external`:

  matrix. Covariance matrix before binarization for external patients.
