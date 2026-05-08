# Set Drop Out Distribution

Set Drop Out Distribution

## Usage

``` r
set_dropout(object, internal_treated, internal_control, external_control)
```

## Arguments

- object:

  `DataSimObject`

- internal_treated:

  `DataSimEvent` object specifying distribution for internal treated
  patients.

- internal_control:

  `DataSimEvent` object specifying distribution for internal control
  patients.

- external_control:

  `DataSimEvent` object specifying distribution for external control
  patients.

## Value

A `DataSimObject` with updated `internal_treated`, `internal_control`
and `external_control` slots.

## Details

`DataSimEvent` objects can be specified with
[`create_event_dist()`](https://genentech.github.io/psborrow2/reference/create_event_dist.md).
Currently no `beta` parameters can be used in drop out distributions
(unlike for the survival outcome).

## Examples

``` r
data_sim <- create_data_simulation(
  create_baseline_object(10, 10, 10),
  event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
)
set_dropout(
  data_sim,
  internal_treated = create_event_dist(dist = "exponential", lambdas = 1 / 55),
  internal_control = create_event_dist(dist = "exponential", lambdas = 1 / 50),
  external_control = create_event_dist(dist = "exponential", lambdas = 1 / 40)
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
#>  Internal: Enrolling 1 patient per time 
#>  External: Enrolling 1 patient per time 
#> 
#> Dropout:
#>  Internal treated: exponential distribution with lambda = 0.0181818181818182 
#>  Internal control: exponential distribution with lambda = 0.02 
#>  External control: exponential distribution with lambda = 0.025 
#> 
#> Clinical cut off:
#>  Internal: No cut off 
#>  External: No cut off 
#> 
```
