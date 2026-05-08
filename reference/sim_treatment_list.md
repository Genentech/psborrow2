# Input treatment details for a simulation study

A function for defining which treatment scenarios should be evaluated as
part of a simulation study.

## Usage

``` r
sim_treatment_list(treatment_list)
```

## Arguments

- treatment_list:

  named list of objects of class `Treatment` created by
  [`treatment_details()`](https://genentech.github.io/psborrow2/reference/treatment_details.md).

## Value

Object of class
[`SimTreatmentList`](https://genentech.github.io/psborrow2/reference/SimTreatmentList-class.md).

## See also

Other simulation classes:
[`sim_borrowing_list()`](https://genentech.github.io/psborrow2/reference/sim_borrowing_list.md),
[`sim_covariate_list()`](https://genentech.github.io/psborrow2/reference/sim_covariate_list.md),
[`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md),
[`sim_outcome_list()`](https://genentech.github.io/psborrow2/reference/sim_outcome_list.md)

## Examples

``` r

treatment_scenarios <- sim_treatment_list(
  list(
    "Standard" = treatment_details("trt", prior_normal(0, 1000))
  )
)
```
