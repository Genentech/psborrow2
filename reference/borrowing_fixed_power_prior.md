# Fixed Power Prior Borrowing

Fixed Power Prior Borrowing

## Usage

``` r
borrowing_fixed_power_prior(ext_flag_col, power_par)
```

## Arguments

- ext_flag_col:

  character. Name of the external flag column in the matrix.

- power_par:

  numeric. Fixed power parameter for all external data. Must be between
  0 and 1.

## Value

Object of class
[`BorrowingFixedPowerPrior`](https://genentech.github.io/psborrow2/reference/BorrowingFixedPowerPrior-class.md).

## Examples

``` r
borrowing_fixed_power_prior(
  ext_flag_col = "ext",
  power_par = 0.5
)
#> Borrowing object using the  Borrowing with fixed power prior  approach
#> 
#> External control flag: ext 
#> 
```
