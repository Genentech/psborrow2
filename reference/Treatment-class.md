# `Treatment` Class

A class for defining treatment details. Objects of class `Treatment`
should not be created directly but by the constructor
[`treatment_details()`](https://genentech.github.io/psborrow2/reference/treatment_details.md).

## Slots

- `trt_flag_col`:

  character. Character specifying the name of the column in the model
  matrix that corresponds to the treatment flag (`1`/`0` or
  `TRUE`/`FALSE`). This identifies patients as belonging to the
  experimental treatment arm.

- `trt_prior`:

  `Prior`. Object of class `Prior` specifying the prior distribution of
  the log effect estimate (log hazard ratio for time to event endpoints
  and log odds ratio for binary endpoints).
