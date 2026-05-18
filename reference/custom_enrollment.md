# Create a `DataSimEnrollment` Object

Create a `DataSimEnrollment` Object

## Usage

``` r
custom_enrollment(fun, label)
```

## Arguments

- fun:

  A function that takes one argument `n` the number of enrollment times
  to observe and returns a vector of times.

- label:

  A user-friendly label

## Value

A
[DataSimEnrollment](https://genentech.github.io/psborrow2/reference/DataSimEnrollment-class.md)
object

## Examples

``` r
custom_enrollment(
  fun = function(n) rpois(n, lambda = 5),
  label = "Poisson enrollment distribution"
)
#> An object of class "DataSimEnrollment"
#> Slot "fun":
#> function (n) 
#> rpois(n, lambda = 5)
#> <environment: 0x55aa6e1a9d00>
#> 
#> Slot "label":
#> [1] "Poisson enrollment distribution"
#> 
```
