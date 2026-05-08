# Constant Enrollment Rates

Constant Enrollment Rates

## Usage

``` r
enrollment_constant(rate, for_time = rep(1, length(rate)))
```

## Arguments

- rate:

  Number of patients to enroll per unit time

- for_time:

  Number of time periods for each rate. Must be equal length to `rate`

## Value

An object of class
[DataSimEnrollment](https://genentech.github.io/psborrow2/reference/DataSimEnrollment-class.md)
to be passed to
[`create_data_simulation()`](https://genentech.github.io/psborrow2/reference/create_data_simulation.md)

## Examples

``` r
# 10 patients/month for 6 months, then 5/month for 6 months
enroll_obj <- enrollment_constant(rate = c(10, 5), for_time = c(6, 6))
enroll_obj@fun(n = 80)
#>  [1]  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3
#> [26]  3  3  3  3  3  4  4  4  4  4  4  4  4  4  4  5  5  5  5  5  5  5  5  5  5
#> [51]  6  6  6  6  6  6  6  6  6  6  7  7  7  7  7  8  8  8  8  8  9  9  9  9  9
#> [76] 10 10 10 10 10
```
