# Input outcome details for a simulation study

A function for defining which outcome scenarios should be evaluated as
part of a simulation study.

## Usage

``` r
sim_outcome_list(outcome_list)
```

## Arguments

- outcome_list:

  named list of objects of class `Outcome` created by
  `outcome_details()`.

## Value

Object of class
[`SimOutcomeList`](https://genentech.github.io/psborrow2/reference/SimOutcomeList-class.md).

## See also

Other simulation classes:
[`sim_borrowing_list()`](https://genentech.github.io/psborrow2/reference/sim_borrowing_list.md),
[`sim_covariate_list()`](https://genentech.github.io/psborrow2/reference/sim_covariate_list.md),
[`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md),
[`sim_treatment_list()`](https://genentech.github.io/psborrow2/reference/sim_treatment_list.md)

## Examples

``` r

outcome_scenarios <- sim_outcome_list(
  list(
    "Exponential" = outcome_surv_exponential("time", "cnsr", prior_normal(0, 10000))
  )
)
```
