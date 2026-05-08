# Specify covariates for simulation study

Provide details on the desired covariate distributions and covariance
for for a simulation study.

## Usage

``` r
sim_covariates(
  covariates,
  covariance_internal,
  covariance_external = covariance_internal
)
```

## Arguments

- covariates:

  list. Named list of covariate mean values or probabilities as
  generated through
  [`bin_var()`](https://genentech.github.io/psborrow2/reference/bin_var.md)
  (class `SimVarBin` or
  [`cont_var()`](https://genentech.github.io/psborrow2/reference/cont_var.md)
  (class `SimVarCont`). See `details` for more information.

- covariance_internal:

  matrix. Covariance matrix before binarization for internal patients.

- covariance_external:

  matrix. Covariance matrix before binarization for external patients.
  Defaults to the internal covariance.

## Value

Object of class `SimCovariates`

## Details

This function is intended to specify the number of covariates and
relationships between them for the purposes of designing a simulation
study in `psborrow2`. Because the outcome model does not necessarily
need to adjust for covariates, this function is not necessary in
[`create_simulation_obj()`](https://genentech.github.io/psborrow2/reference/create_simulation_obj.md).
The relationship between the treatment and the outcome is specified
elsewhere (i.e, in `sim_survival()` or `sim_binary_event()`).

We need a few things to

## See also

Other simulation:
[`sim_samplesize()`](https://genentech.github.io/psborrow2/reference/sim_samplesize.md)

## Examples

``` r

set.seed(123)
covmat <- matrix(rWishart(1, 2, diag(2)), ncol = 2)

covset1 <- sim_covariates(
  covariates = list(
    cov1 = bin_var(0.5, 0.5),
    cov2 = cont_var(100, 130)
  ),
  covariance_internal = covmat
)
```
