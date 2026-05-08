# `SimSampleSize` Class

A class for creating matrices for simulation studies containing flags
specifying whether the patient is from the concurrent trial or not
(`ext` = 0 for concurrent trial, `ext` = 1 for historical data) and
whether the patient is on the experimental therapy or not (`trt` = 0 for
no experimental therapy, `trt` = 1 for experimental therapy).

## Slots

- `n_internal_control`:

  integer. Number of patients to be simulated in the internal control
  arm.

- `n_external_control`:

  integer. Number of patients to be simulated in the external control
  arm.

- `n_internal_experimental`:

  integer. Number of patients to be simulated in the internal
  experimental arm.

- `mat`:

  matrix. Matrix with two columns, `ext` (flag for being from external
  data source) and `trt` (flag for receiving experimental treatment)
