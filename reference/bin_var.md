# Create binary covariate

Create an object of class `SimVarBin` to hold proportions of binary
variables specified in a simulation study.

## Usage

``` r
bin_var(
  prob_internal,
  prob_external,
  mu_internal_before_bin = 0,
  mu_external_before_bin = 0
)
```

## Arguments

- prob_internal:

  numeric. Proportion for the internal arms.

- prob_external:

  numeric. Proportion for the external arm.

- mu_internal_before_bin:

  numeric. Mean value of the covariate before binarization for the
  internal arms. The default is 0. See `details` for more information.

- mu_external_before_bin:

  numeric. Mean value of the covariate before binarization for the
  external arm. The default is 0. See `details` for more information.

## Value

Object of class
[`SimVarBin`](https://genentech.github.io/psborrow2/reference/SimVarBin-class.md).

## Details

This function contains information necessary to create binary covariates
as part of a simulation study. The binary covariates are created by
binarizing multivariate normal distributions to achieve the
probabilities specified in `prob_internal` and `prob_external`. The user
may choose to change the default mean value of each variable prior to
binarization by specifying `mu_internal_before_bin` or
`mu_external_before_bin` to ensure the correct scales are used in the
covariance matrix, though the ultimate proportions will depend on
`prob_internal` and `prob_external`. The default values for
`mu_internal_before_bin` and `mu_external_before_bin` are `0`, and it is
not recommended to change these without good reason.

## See also

Other simvar:
[`cont_var()`](https://genentech.github.io/psborrow2/reference/cont_var.md)

## Examples

``` r
cv1 <- bin_var(0.50, 0.80)
#> Loading required namespace: testthat
cv2 <- bin_var(.95, .92)
```
