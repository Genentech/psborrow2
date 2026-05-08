# Input covariate adjustment details for a simulation study

A function for defining which covariate adjustment scenarios should be
evaluated as part of a simulation study.

## Usage

``` r
sim_covariate_list(covariate_list)
```

## Arguments

- covariate_list:

  named list of objects of class `Covariate` created by
  [`add_covariates()`](https://genentech.github.io/psborrow2/reference/add_covariates.md).

## Value

Object of class
[`SimCovariateList`](https://genentech.github.io/psborrow2/reference/SimCovariateList-class.md).

## Details

This function allows the user to specify covariate adjustment details
that will be included as part of a simulation study. It is often of
interest to compare several adjustment methods to no adjustment. To
specify no adjustment, pass `NULL` as a list item to `covariate_list`.

## See also

Other simulation classes:
[`sim_borrowing_list()`](https://genentech.github.io/psborrow2/reference/sim_borrowing_list.md),
[`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md),
[`sim_outcome_list()`](https://genentech.github.io/psborrow2/reference/sim_outcome_list.md),
[`sim_treatment_list()`](https://genentech.github.io/psborrow2/reference/sim_treatment_list.md)

## Examples

``` r

covariates <- sim_covariate_list(
  list(
    "No adjustment" = NULL,
    "Covariates 1 and 2" = add_covariates(c("cov1", "cov2"), prior_normal(0, 1000))
  )
)
```
