# Input borrowing details for a simulation study

A function for defining which borrowing scenarios should be evaluated as
part of a simulation study.

## Usage

``` r
sim_borrowing_list(borrowing_list)
```

## Arguments

- borrowing_list:

  named list of objects of class `Borrowing` created by
  [`borrowing_full()`](https://genentech.github.io/psborrow2/reference/borrowing_full.md),
  [`borrowing_none()`](https://genentech.github.io/psborrow2/reference/borrowing_none.md),
  or
  [`borrowing_hierarchical_commensurate()`](https://genentech.github.io/psborrow2/reference/borrowing_hierarchical_commensurate.md).

## Value

Object of class
[`SimBorrowingList`](https://genentech.github.io/psborrow2/reference/SimBorrowingList-class.md).

## See also

Other simulation classes:
[`sim_covariate_list()`](https://genentech.github.io/psborrow2/reference/sim_covariate_list.md),
[`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md),
[`sim_outcome_list()`](https://genentech.github.io/psborrow2/reference/sim_outcome_list.md),
[`sim_treatment_list()`](https://genentech.github.io/psborrow2/reference/sim_treatment_list.md)

## Examples

``` r

borrow_scenarios <- sim_borrowing_list(
  list(
    "No borrowing" = borrowing_none("ext"),
    "Full borrowing" = borrowing_full("ext"),
    "BDB, uninformative prior" = borrowing_hierarchical_commensurate(
      "ext",
      prior_gamma(0.001, 0.001)
    ),
    "BDB, informative prior" = borrowing_hierarchical_commensurate(
      "ext",
      prior_gamma(1, 0.001)
    )
  )
)
```
