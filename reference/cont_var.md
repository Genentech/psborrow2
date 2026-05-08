# Create continuous covariate

Create an object of class `SimVarCont` to hold mean values of of
continuous variables specified in a simulation study.

## Usage

``` r
cont_var(mu_internal, mu_external)
```

## Arguments

- mu_internal:

  numeric. Mean covariate value for the internal arms.

- mu_external:

  numeric. Mean covariate value for the external arm.

## Value

Object of class
[`SimVarCont`](https://genentech.github.io/psborrow2/reference/SimVarCont-class.md).

## See also

Other simvar:
[`bin_var()`](https://genentech.github.io/psborrow2/reference/bin_var.md)

## Examples

``` r
cv1 <- cont_var(0.5, 1)
cv2 <- cont_var(10, 10)
```
