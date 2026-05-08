# Data Simulation

Data Simulation

## Usage

``` r
create_data_simulation(
  baseline,
  coefficients = numeric(),
  treatment_hr = 1,
  drift_hr = 1,
  event_dist,
  fixed_external_data
)
```

## Arguments

- baseline:

  `BaselineObject` from
  [`create_baseline_object()`](https://genentech.github.io/psborrow2/reference/create_baseline_object.md)

- coefficients:

  Named vector of coefficients for linear predictor. Must correspond to
  variables in baseline object

- treatment_hr:

  Default treatment hazard ratio for simulations. Alternative simulation
  settings can be specified in
  [generate](https://genentech.github.io/psborrow2/reference/generate.md).

- drift_hr:

  Default drift hazard ratio between internal and external arms.
  Alternative simulation settings can be specified in
  [generate](https://genentech.github.io/psborrow2/reference/generate.md).

- event_dist:

  Specify time to event distribution with `SimDataEvent` object from
  [`create_event_dist()`](https://genentech.github.io/psborrow2/reference/create_event_dist.md)

- fixed_external_data:

  A `data.frame` containing external control data. It must contain
  columns `eventtime`, `status` and all of the variables named in
  `coefficients`. If present, `trt` must be 0 and `ext` must be 1 for
  all rows.

## Value

`DataSimObject`

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
data_sim_list <- generate(sim_obj, treatment_hr = c(0.5, 1), drift_hr = 0.5)
```
