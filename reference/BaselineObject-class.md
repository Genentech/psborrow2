# `BaselineObject` class for data simulation

`BaselineObject` class for data simulation

## Slots

- `n_trt_int`:

  integer. Number of internal treated patients

- `n_ctrl_int`:

  integer. Number of internal control patients

- `n_ctrl_ext`:

  integer. Number of external control patients

- `covariates`:

  list. List of correlated covariates objects, see
  [`baseline_covariates()`](https://genentech.github.io/psborrow2/reference/baseline_covariates.md)

- `transformations`:

  list. List of named transformation functions.
