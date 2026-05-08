# Generate Data for a `DataSimObject`

Generate Data for a `DataSimObject`

## Usage

``` r
# S4 method for class 'DataSimObject'
generate(x, n = 1, treatment_hr = NULL, drift_hr = NULL)
```

## Arguments

- x:

  a `DataSimObject` object created by
  [create_data_simulation](https://genentech.github.io/psborrow2/reference/create_data_simulation.md)

- n:

  number of data sets to simulate

- treatment_hr:

  vector of numeric treatment effects

- drift_hr:

  vector of numeric drift effects

## Value

A
[SimDataList](https://genentech.github.io/psborrow2/reference/SimDataList-class.md)
object for use with
[`create_simulation_obj()`](https://genentech.github.io/psborrow2/reference/create_simulation_obj.md).

## Examples

``` r
baseline_obj <- create_baseline_object(
  n_trt_int = 100,
  n_ctrl_int = 50,
  n_ctrl_ext = 10,
  covariates = baseline_covariates(
    names = c("age", "score"),
    means_int = c(55, 5),
    means_ext = c(60, 5),
    covariance_int = covariance_matrix(c(5, 1))
  )
)
sim_obj <- create_data_simulation(
  baseline_obj,
  coefficients = c(age = 0.001, score = 1.5),
  event_dist = create_event_dist(dist = "exponential", lambdas = 1 / 36)
)
data_sim_list <- generate(sim_obj, treatment_hr = c(0, 1), drift_hr = 0.5)
#> Warning: There were 100 event times evaluated at infinity (likely due to the hazard approaching zero). Perhaps consider specifying a finite censoring time using the 'maxt' argument.
```
