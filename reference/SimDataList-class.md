# `SimDataList` Class

A class for defining generated data for use as part of a simulation
study. Objects of class `SimDataList` should not be created directly but
by the constructor
[`sim_data_list()`](https://genentech.github.io/psborrow2/reference/sim_data_list.md).

## Slots

- `data_list`:

  list of lists of matrices. The lists at the highest level differ in
  that the parameters used to generate the data. The matrices at lowest
  level are different iterations of the same data generation parameters.

- `guide`:

  data.frame. `guide` contains information on the parameters that differ
  at the highest level of `data_list`.

- `effect`:

  character. The column in `guide` that corresponds to the true
  treatment effect estimate (hazard ratio or odds ratio).

- `drift`:

  character. The column in `guide` that corresponds to the drift between
  external and internal control arms. A drift \>1 means the external arm
  experiences greater effects.

- `index`:

  character. The column in `guide` that corresponds to the index of the
  parameter situations in `data_list`.
