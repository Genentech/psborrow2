# Specify Correlated Baseline Covariates

Set parameters to generate correlated multivariate normal data for
internal and external patients.

## Usage

``` r
baseline_covariates(
  names,
  means_int,
  means_ext = means_int,
  covariance_int,
  covariance_ext = covariance_int
)
```

## Arguments

- names:

  character vector of variable names.

- means_int:

  numeric vector of means for internal patients. Must have same length
  as `names`

- means_ext:

  numeric vector of means for external patients. Must have same length
  as `names`

- covariance_int:

  variance-covariance matrix for generating multivariate normal for
  internal patients. Must be square matrix with same number of rows and
  `length(names)`

- covariance_ext:

  variance-covariance matrix for generating multivariate normal data for
  external patients. Must be square matrix with same number of rows and
  `length(names)`

## Value

[BaselineObject](https://genentech.github.io/psborrow2/reference/BaselineObject-class.md)
to build simulated dataset

## Examples

``` r
corr_covs <- baseline_covariates(
  names = c("b1", "b2"),
  means_int = c(5, 25),
  covariance_int = covariance_matrix(diag = c(1, 1), upper_tri = 0.4)
)
```
