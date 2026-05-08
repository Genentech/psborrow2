# Set simulation study parameters for sample size

Set simulation study parameters for sample size

## Usage

``` r
sim_samplesize(n_internal_control, n_external_control, n_internal_experimental)
```

## Arguments

- n_internal_control:

  integer. Number of patients to be simulated in the internal control
  arm.

- n_external_control:

  integer. Number of patients to be simulated in the external control
  arm.

- n_internal_experimental:

  integer. Number of patients to be simulated in the internal
  experimental arm.

## Value

Object of class `SimSampleSize`

## See also

Other simulation:
[`sim_covariates()`](https://genentech.github.io/psborrow2/reference/sim_covariates.md)

## Examples

``` r
ss <- sim_samplesize(200, 200, 500)
```
