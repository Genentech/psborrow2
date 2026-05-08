# Data Simulation Object Class

Data Simulation Object Class

## Value

A `DataSimObject`

## Slots

- `baseline`:

  `BaselineObject` from
  [create_baseline_object](https://genentech.github.io/psborrow2/reference/create_baseline_object.md)

- `coefficients`:

  Named `numeric` vector of `beta` coefficients for survival model. See
  `beta` at
  [`?simsurv::simsurv`](https://rdrr.io/pkg/simsurv/man/simsurv.html)

- `treatment_hr`:

  `numeric` treatment effect as a hazard ration. `log(treatment_hr)` is
  included in `beta` with `coefficients` and `log(drift_hr)`. This
  default is overridden by
  [generate](https://genentech.github.io/psborrow2/reference/generate-DataSimObject-method.md)
  arguments

- `drift_hr`:

  `numeric` hazard ratio between internal and external arms. Included as
  `log(drift_hr)`.

- `fixed_external_data`:

  `data.frame` for external data. Currently unused.

- `event_dist`:

  `DataSimEvent` parameters for outcome distribution from
  [`create_event_dist()`](https://genentech.github.io/psborrow2/reference/create_event_dist.md)

- `enrollment`:

  `DataSimEnrollment` object.

- `cut_off`:

  `DataSimCutOff`
