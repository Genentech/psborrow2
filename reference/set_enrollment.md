# Set Enrollment Rates for Internal and External Trials

Set Enrollment Rates for Internal and External Trials

## Usage

``` r
set_enrollment(object, internal, external = internal)
```

## Arguments

- object:

  A `DataSimObject` from
  [create_data_simulation](https://genentech.github.io/psborrow2/reference/create_data_simulation.md)

- internal:

  `DataSimEnrollment` object to define the enrollment times for internal
  data

- external:

  `DataSimEnrollment` object to define the enrollment times for external
  data. Defaults to be the same as internal.

## Value

A `DataSimObject` with updated `enrollment_internal` and
`enrollment_external` slots.

## Examples

``` r
data_sim <- create_data_simulation(
  create_baseline_object(10, 10, 10),
  event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
)
set_enrollment(
  data_sim,
  internal = enrollment_constant(rate = c(10, 5), for_time = c(6, 6)),
  external = enrollment_constant(rate = c(5), for_time = c(20))
)
#> DataSimObject
#> -------------
#> Baseline object:
#> Baseline Data Simulation Object
#>   N internal treated:  10 
#>   N internal control:  10 
#>   N external control:  10 
#> 
#> Covariates: 
#> [[1]]
#> [1] covariate      means_internal means_external
#> <0 rows> (or 0-length row.names)
#> 
#> Covariance Matrices
#> Internal         External
#> <0 x 0 matrix>   <0 x 0 matrix> 
#> 
#> 
#> Event distribution:
#> exponential distribution with lambda = 0.0277777777777778 
#> 
#> Treatment HR:  1 
#> Drift HR:  1 
#> 
#> Enrollment:
#>  Internal: Enrolling patients per time at rates: 10, 10, 10, 10, 10, 10, 5, 5, 5, 5, 5, 5 
#>  External: Enrolling patients per time at rates: 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5.... 
#> 
#> Dropout:
#>  Internal treated: No distribution specified 
#>  Internal control: No distribution specified 
#>  External control: No distribution specified 
#> 
#> Clinical cut off:
#>  Internal: No cut off 
#>  External: No cut off 
#> 
```
