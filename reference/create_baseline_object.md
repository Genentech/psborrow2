# Create Baseline Data Simulation Object

Create Baseline Data Simulation Object

## Usage

``` r
create_baseline_object(
  n_trt_int,
  n_ctrl_int,
  n_ctrl_ext,
  covariates,
  transformations
)
```

## Arguments

- n_trt_int:

  Number of internal treated patients

- n_ctrl_int:

  Number of internal control patients

- n_ctrl_ext:

  Number of external control patients

- covariates:

  List of correlated covariates objects, see
  [`baseline_covariates()`](https://genentech.github.io/psborrow2/reference/baseline_covariates.md)

- transformations:

  List of named transformation functions.

## Value

A
[BaselineObject](https://genentech.github.io/psborrow2/reference/BaselineObject-class.md)

## Details

Transformation functions are evaluated in order and create or overwrite
a column in the data.frame with that name. The function should take a
`data.frame` (specifically a `BaselineDataFrame` object from
`generate(BaselineObject)`) and return a vector with length identical to
the total number of patients. The `@BaselineObject` slot may be accessed
directly or with
[`get_quantiles()`](https://genentech.github.io/psborrow2/reference/get_quantiles.md)
to create transformations. See
[`binary_cutoff()`](https://genentech.github.io/psborrow2/reference/binary_cutoff.md)

## Examples

``` r
bl_no_covs <- create_baseline_object(
  n_trt_int = 100,
  n_ctrl_int = 50,
  n_ctrl_ext = 100
)


bl_biomarkers <- create_baseline_object(
  n_trt_int = 100,
  n_ctrl_int = 50,
  n_ctrl_ext = 100,
  covariates = baseline_covariates(
    c("b1", "b2", "b3"),
    means_int = c(0, 0, 0),
    covariance_int = covariance_matrix(c(1, 1, 1), c(.8, .3, .8))
  ),
  transformations = list(
    exp_b1 = function(data) exp(data$b1),
    b2 = binary_cutoff("b2", int_cutoff = 0.7, ext_cutoff = 0.5)
  )
)
```
