# Set Clinical Cut Off Rule

Set Clinical Cut Off Rule

## Usage

``` r
set_cut_off(object, internal = cut_off_none(), external = cut_off_none())
```

## Arguments

- object:

  `DataSimObject`

- internal:

  `DataSimCutOff` object specified by one of the cut off functions:
  [`cut_off_after_events()`](https://genentech.github.io/psborrow2/reference/cut_off_funs.md),
  [`cut_off_after_first()`](https://genentech.github.io/psborrow2/reference/cut_off_funs.md),
  [`cut_off_after_last()`](https://genentech.github.io/psborrow2/reference/cut_off_funs.md),
  [`cut_off_none()`](https://genentech.github.io/psborrow2/reference/cut_off_funs.md).

- external:

  `DataSimCutOff` for the external data.

## Value

A `DataSimObject` with updated `cut_off_internal` and `cut_off_external`
slots.

## Examples

``` r
data_sim <- create_data_simulation(
  create_baseline_object(10, 10, 10),
  event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
)
set_cut_off(
  data_sim,
  cut_off_after_events(n = 10),
  cut_off_after_first(time = 30)
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
#>  Internal treated: No distribution specified 
#>  Internal control: No distribution specified 
#>  External control: No distribution specified 
#> 
#> Clinical cut off:
#>  Internal: Cut off after 10 events 
#>  External: Cut off after first enrolled patient reaches time = 30 
#> 
```
